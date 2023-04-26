import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/statistics/statistics"
import "lib/github.com/diku-dk/sorts/merge_sort"
import "nbody"
import "sand"

type~ state [h][w][n] = { image: [h][w]argb.colour
                        , bodies: [n]body
                        , orig_bodies: [n]body
                        , offset: i64
                        , reverting: bool
                        , background: argb.colour }

module statistics = mk_statistics f32

entry load_image [h][w] (image: [h][w]argb.colour): state [h][w][0] =
 { image = image
 , bodies = []
 , orig_bodies = []
 , offset = 0
 , reverting = false
 , background = argb.black -- dummy
 }

entry render [h][w][n] (s: state [h][w][n]): [h][w]argb.colour =
 if length s.bodies == 0
 then s.image
 else let (is, vs) = unzip (map (render_body h w) s.bodies)
      in unflatten h w (scatter (replicate (h*w) s.background) is vs)

let most_common_colour [h][w] (image: [h][w]argb.colour) =
 image |> flatten
       |> map f32.from_bits
       |> merge_sort (<=) |> statistics.mode_sorted
       |> f32.to_bits

entry start_nbody [h][w] (s: state [h][w][]): state [h][w][] =
  let background = most_common_colour s.image
  let bodies = bodies_from_image background s.image
  in s with bodies = bodies
       with orig_bodies = bodies
       with background = background
       with offset = length s.bodies / 2
       with reverting = false

let num_attractors (n: i64) = i64.max 64 (i64.f32 (8000 / f32.i64 (i64.max 1 n)))

entry revert [h][w][n] (s: state [h][w][n]): state [h][w][n] =
  s with reverting = !s.reverting

entry advance [h][w][n] (s: state [h][w][n]): state [h][w][n] =
  let chunk_size = i64.min (num_attractors (length s.bodies)) (length s.bodies - s.offset)
  let attractors = s.bodies[s.offset:s.offset+chunk_size]
  let fwd_time_step = 0.1 * 5
  let rev_time_step = 0.2
  in s with bodies = (if s.reverting
                      then revert_bodies (f32.i64 w) (f32.i64 h) 50 rev_time_step s.bodies s.orig_bodies
                      else advance_bodies (f32.i64 w) (f32.i64 h) 50 fwd_time_step s.bodies attractors)
       with offset = if length s.bodies > 0
                     then (s.offset + num_attractors (length s.bodies)) % length s.bodies
                     else 0

import "lib/github.com/diku-dk/cpprandom/random"
module engine = minstd_rand
module distribution = uniform_real_distribution f32 engine

entry shuffle [h][w][n] (s: state [h][w][n]) (seed: f32): state [h][w][n] =
  let rng = engine.rng_from_seed [i32.u32 (f32.to_bits seed)]
  let move (rng: engine.rng) ((_, mass, velocity, colour) : body) =
    let (rng, x) = distribution.rand (0, f32.i64 h) rng
    let (_, y) = distribution.rand (0, f32.i64 w) rng
    in ({x, y}, mass, velocity, colour)
  in s with bodies = map2 move (engine.split_rng n rng) s.bodies

entry drop_pixels [h][w][n] (i: i32) (s: state [h][w][n]): state [h][w][n] =
  s with image = drop_pixels i s.image
