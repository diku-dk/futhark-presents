import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/statistics/statistics"
import "lib/github.com/diku-dk/sorts/merge_sort"
import "nbody"
import "sand"

type state [h][w] = { image: [h][w]argb.colour
                    , bodies: []body
                    , orig_bodies: []body
                    , offset: i32
                    , reverting: bool
                    , background: argb.colour }

module statistics = mk_statistics f32

entry load_image [h][w] (image: [h][w]argb.colour): state [h][w] =
 { image = image
 , bodies = []
 , orig_bodies = []
 , offset = 0
 , reverting = false
 , background = argb.black -- dummy
 }

entry render [h][w] (state: state [h][w]): [h][w]i32 =
 if length state.bodies == 0
 then state.image
 else let (is, vs) = unzip (map (render_body h w) state.bodies)
      in unflatten h w (scatter (replicate (w*h) state.background) is vs)

let most_common_colour [h][w] (image: [h][w]argb.colour) =
 image |> flatten
       |> map (u32.i32 >-> f32.from_bits)
       |> merge_sort (<=) |> statistics.mode_sorted
       |> f32.to_bits
       |> i32.u32

entry start_nbody [h][w] (state: state [h][w]): state [h][w] =
  let bodies = bodies_from_image state.background state.image
  in { image = state.image,
       bodies = bodies,
       orig_bodies = bodies,
       offset = length bodies / 2,
       reverting = false,
       background = most_common_colour state.image }

entry bodies_and_flags [h][w] (state: state [h][w]): ([]i32, []bool) =
  let bodies = bodies_from_pixels state.image
  let background = most_common_colour state.image
  in (0..<length bodies, map (\(_,_,_,bg) -> bg != background) bodies)

entry start_nbody_prefiltered [h][w] (state: state [h][w]) (is: []i32): state [h][w] =
  let all_bodies = bodies_from_pixels state.image
  let bodies = map (\i -> unsafe all_bodies[i]) is
  in { image = state.image,
       bodies = bodies,
       offset = length bodies / 2,
       orig_bodies = bodies,
       reverting = false,
       background = most_common_colour state.image }

let num_attractors (n: i32) = i32.max 64 (t32 (8000 / r32 (i32.max 1 n)))

entry revert [h][w] (state: state [h][w]): state [h][w] =
  let {image, bodies, offset, orig_bodies, reverting, background} = state
  in { image, bodies, offset, orig_bodies, reverting = !reverting, background }

entry advance [h][w] ({image, bodies, offset, orig_bodies, reverting, background}: state [h][w]): state [h][w] =
  let chunk_size = i32.min (num_attractors (length bodies)) (length bodies - offset)
  let attractors = bodies[offset:offset+chunk_size]
  let bodies' = if reverting
                then revert_bodies (r32 w) (r32 h) 50 0.2 bodies orig_bodies
                else advance_bodies (r32 w) (r32 h) 50 0.1 bodies attractors
  in { image,
       orig_bodies,
       reverting,
       bodies = bodies',
       offset = if length bodies > 0
                then (offset + num_attractors (length bodies)) % length bodies
                else 0,
       background }

import "lib/github.com/diku-dk/cpprandom/random"
module engine = minstd_rand
module distribution = uniform_real_distribution f32 engine

entry shuffle [h][w] ({image, bodies, offset, orig_bodies, reverting, background}: state [h][w]) (seed: f32): state [h][w] =
  let rng = engine.rng_from_seed [i32.u32 (f32.to_bits seed)]
  let move (rng: engine.rng) ((_, mass, velocity, colour) : body) =
    let (rng, x) = distribution.rand (0, r32 h) rng
    let (_, y) = distribution.rand (0, r32 w) rng
    in ({x, y}, mass, velocity, colour)
  let bodies = map2 move (engine.split_rng (length bodies) rng) bodies
  in {image, bodies, offset, orig_bodies, reverting, background}

entry drop_pixels [h][w] (i: i32) ({image, bodies, offset, orig_bodies, reverting, background}: state [h][w]): state [h][w] =
  {image = drop_pixels i image, bodies, offset, orig_bodies, reverting, background}
