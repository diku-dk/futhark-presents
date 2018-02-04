import "/futlib/colour"
import "/futlib/vec2"

module vec2 = mk_vec2 f32

type mass = f32
type position = vec2.vec
type acceleration = vec2.vec
type velocity = vec2.vec
type body = (position, mass, velocity, argb.colour)

let mass_from_colour (c: argb.colour): f32 =
  let (r,g,b,_) = argb.to_rgba c
  in f32.max 0.3f32 (3f32 - (r + g + b))

let dist (x: body) (y: body) =
  vec2.dot (x.1 vec2.- y.1) (x.1 vec2.- y.1)

let accel (epsilon: f32) ((pi, _, _, _):body) ((pj, mj, _, _): body)
        : velocity =
  let r = pj vec2.- pi
  let rsqr = vec2.dot r r + epsilon * epsilon
  let invr = 1.0f32 / f32.sqrt rsqr
  let invr3 = invr * invr * invr
  let s = mj * invr3
  in vec2.scale s r

let bound_pos (maxx: f32) (maxy: f32) (({x,y}, mass, vel, c): body): body =
  ({x=f32.min (f32.max 0f32 x) maxy,
    y=f32.min (f32.max 0f32 y) maxx},
   mass, vel, c)

let calc_accels [n] (epsilon: f32) (bodies: [n]body) (attractors: []body): [n]acceleration =
  let move (body: body) =
        let accels = map (accel epsilon body) attractors
        in (reduce_comm (vec2.+) {x=0f32, y=0f32} accels)
  in map move bodies

let advance_body (maxx: f32) (maxy: f32) (time_step: f32) ((pos, mass, vel, c):body) (acc:acceleration): body =
  let acc' = vec2.scale mass acc
  let pos' = pos vec2.+ vec2.scale time_step vel
  let vel' = vel vec2.+ vec2.scale time_step acc'
  in bound_pos maxx maxy (pos', mass, vel', c)

let advance_bodies [n] (maxx: f32) (maxy: f32) (epsilon: f32) (time_step: f32) (bodies: [n]body) (attractors: []body): [n]body =
  let accels = calc_accels epsilon bodies attractors
  in map (advance_body maxx maxy time_step) bodies accels

let calc_revert_accels [n] (epsilon: f32) (bodies: []body) (orig_bodies: [n]body): []acceleration =
  let weighten ((pos, _mass, vel, c): body) =
        (pos, 3000f32, vel, c)
  let move (body: body) (orig_body: body) =
        (accel epsilon body (weighten orig_body))
  in map move bodies orig_bodies

let revert_bodies [n] (maxx: f32) (maxy: f32) (epsilon: f32) (time_step: f32) (bodies: [n]body) (orig_bodies: [n]body): [n]body =
  let accels = calc_revert_accels epsilon bodies orig_bodies
  let friction ((pos, mass, vel, c): body) (_orig_body: body) =
        (pos, mass, vec2.scale 0.9f32 vel, c)
  let maybe_jump (body: body) (orig_body: body) =
        if dist body orig_body < 1f32
        then (orig_body.1, body.2, body.3, body.4)
        else body
  in map maybe_jump
         (map (advance_body maxx maxy time_step) (map friction bodies orig_bodies) accels)
         orig_bodies

let only_nonwhites (bodies: []body) =
  let not_white ((_, _, _, col): body) = col != argb.white
  in filter not_white bodies

let bodies_from_pixels [h][w] (image: [h][w]i32): []body =
  let body_from_pixel (x: i32, y: i32) (pix: argb.colour) =
        ({x=f32.i32 x, y=f32.i32 y}, mass_from_colour pix, {x=0f32, y=0f32}, pix)
  in reshape (h*w)
     (map (\(row, x) -> map (\(pix, y) -> body_from_pixel (x,y) pix) (zip row (iota w)))
            (zip image (iota h)))

let bodies_from_image [h][w] (image: [h][w]i32): []body =
  only_nonwhites (bodies_from_pixels image)

let render_body (_h: i32) (w: i32) (({x,y}, _, _, c): body): (i32, i32) =
  if c == argb.white then (-1, c) else (i32.f32 x * w + i32.f32 y, c)

type state [h][w] = { image: [h][w]i32
                    , bodies: []body
                    , orig_bodies: []body
                    , offset: i32
                    , reverting: bool
                    , background: argb.colour }

entry load_image [h][w] (image: [w][h][3]u8) (background: argb.colour): state [h][w] =
 let pack (pix: [3]u8) = argb.from_rgba (f32.u8 pix[0] / 255f32)
                                        (f32.u8 pix[1] / 255f32)
                                        (f32.u8 pix[2] / 255f32)
                                        1f32
 in { image = transpose (map (\row -> map pack row) image)
    , bodies = empty(body)
    , orig_bodies = empty(body)
    , offset = 0
    , reverting = false
    , background }

entry render [h][w] (state: state [h][w]): [h][w]i32 =
 if length state.bodies == 0
 then state.image
 else let (is, vs) = unzip (map (render_body h w) state.bodies)
      in reshape (h,w) (scatter (replicate (w*h) state.background) is vs)

entry start_nbody [h][w] (state: state [h][w]): state [h][w] =
  let bodies = bodies_from_image state.image
  in { image = state.image,
       bodies = bodies,
       orig_bodies = bodies,
       offset = length bodies / 2,
       reverting = false,
       background = state.background }

entry bodies_and_flags (state: state [][]): ([]i32, []bool) =
  let bodies = bodies_from_pixels state.image
  in (0..<length bodies, map (\b -> b.4 != state.background) bodies)

entry start_nbody_prefiltered [h][w] (state: state [h][w]) (is: []i32): state [h][w] =
  let all_bodies = bodies_from_pixels state.image
  let bodies = map (\i -> unsafe all_bodies[i]) is
  in { image = state.image,
       bodies = bodies,
       offset = length bodies / 2,
       orig_bodies = bodies,
       reverting = false,
       background = state.background }

let chunk_size = 10000
let num_attractors [n] (_bodies: [n]body) = n / chunk_size

entry revert [h][w] (state: state [h][w]): state [h][w] =
  let {image, bodies, offset, orig_bodies, reverting, background} = state
  in { image, bodies, offset, orig_bodies, reverting = !reverting, background }

entry advance [h][w] ({image, bodies, offset, orig_bodies, reverting, background}: state [h][w]): state [h][w] =
  let chunk_size = i32.min (num_attractors bodies) (length bodies - offset)
  let attractors = bodies[offset:offset+chunk_size]
  let bodies' = if reverting
                then revert_bodies (r32 w) (r32 h) 100f32 2f32 bodies orig_bodies
                else advance_bodies (r32 w) (r32 h) 50f32 1f32 bodies attractors
  in { image,
       orig_bodies,
       reverting,
       bodies = bodies',
       offset = if length bodies > 0
                then (offset + num_attractors bodies) % length bodies
                else 0,
       background }
