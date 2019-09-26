import "lib/github.com/athas/matte/colour"
import "lib/github.com/athas/vector/vspace"

module vec2 = mk_vspace_2d f32

type mass = f32
type position = vec2.vector
type acceleration = vec2.vector
type velocity = vec2.vector
type body = (position, mass, velocity, argb.colour)

let mass_from_colour (c: argb.colour): f32 =
  let (r,g,b,_) = argb.to_rgba c
  in f32.max 0.3 (3 - (r + g + b))

let dist (x: body) (y: body) =
  vec2.dot (x.1 vec2.- y.1) (x.1 vec2.- y.1)

let accel (epsilon: f32) ((pi, _, _, _):body) ((pj, mj, _, _): body)
        : velocity =
  let r = pj vec2.- pi
  let rsqr = vec2.dot r r + epsilon * epsilon
  let invr = 1.0 / f32.sqrt rsqr
  let invr3 = invr * invr * invr
  let s = mj * invr3
  in vec2.scale s r

let bound_pos (maxy: f32) (maxx: f32) (({x,y}, mass, {x=dx,y=dy}, c): body): body =
  let x' = f32.min (f32.max 0 x) maxx
  let y' = f32.min (f32.max 0 y) maxy
  let dx' = if x != x' then -dx else dx
  let dy' = if y != y' then -dy else dy
  in ({x=x',y=y'}, mass, {x=dx',y=dy'}, c)

let calc_accels [n] (epsilon: f32) (bodies: [n]body) (attractors: []body): [n]acceleration =
  let move (body: body) =
        let accels = map (accel epsilon body) attractors
        in (reduce_comm (vec2.+) {x=0, y=0} accels)
  in map move bodies

let advance_body (maxy: f32) (maxx: f32) (time_step: f32) ((pos, mass, vel, c):body) (acc:acceleration): body =
  let acc' = vec2.scale mass acc
  let pos' = pos vec2.+ vec2.scale time_step vel
  let vel' = vel vec2.+ vec2.scale time_step acc'
  in bound_pos maxy maxx (pos', mass, vel', c)

let advance_bodies [n] (maxy: f32) (maxx: f32) (epsilon: f32) (time_step: f32) (bodies: [n]body) (attractors: []body): [n]body =
  let accels = calc_accels epsilon bodies attractors
  in map2 (advance_body maxy maxx time_step) bodies accels

let calc_revert_accels [n] (epsilon: f32) (bodies: []body) (orig_bodies: [n]body): []acceleration =
  let weighten ((pos, _mass, vel, c): body) =
        (pos, 3000, vel, c)
  let move (body: body) (orig_body: body) =
        (accel epsilon body (weighten orig_body))
  in map2 move bodies orig_bodies

let revert_bodies [n] (maxy: f32) (maxx: f32) (epsilon: f32) (time_step: f32) (bodies: [n]body) (orig_bodies: [n]body): [n]body =
  let accels = calc_revert_accels epsilon bodies orig_bodies
  let friction ((pos, mass, vel, c): body) (_orig_body: body) =
        (pos, mass, vec2.scale 0.99 vel, c)
  in map2 (advance_body maxy maxx time_step) (map2 friction bodies orig_bodies) accels

let only_foreground (bg: argb.colour) (bodies: []body) =
  let not_bg ((_, _, _, col): body) = col != bg
  in filter not_bg bodies

let bodies_from_pixels [h][w] (image: [h][w]i32): []body =
  let body_from_pixel (x: i32, y: i32) (pix: argb.colour) =
        ({x=f32.i32 x, y=f32.i32 y}, mass_from_colour pix, {x=0, y=0}, pix)
  in flatten
     (map (\(row, x) -> map (\(pix, y) -> body_from_pixel (x,y) pix) (zip row (iota w)))
            (zip image (iota h)))

let bodies_from_image [h][w] (bg: argb.colour) (image: [h][w]i32): []body =
  only_foreground bg (bodies_from_pixels image)

let render_body (_h: i32) (w: i32) (({x,y}, _, _, c): body): (i32, i32) =
  (t32 (f32.round x) * w + t32 (f32.round y), c)
