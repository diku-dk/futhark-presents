import "/futlib/math"
import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/cpprandom/random"

-- [sgm_scan_add xs fs] returns the sum-scan of the argument xs but
-- reset at points i where xs[i] is true.

let sgm_scan_add [n] (vals:[n]i32) (flags:[n]bool) : [n]i32 =
  let pairs = scan (\(v1,f1) (v2,f2) ->
                       let f = f1 || f2
                       let v = if f2 then v2 else v1+v2
                       in (v,f) ) (0,false) (zip vals flags)
  let (res,_) = unzip pairs
  in res

let repl_idx [n] (reps:[n]i32) : []i32 =
  let s1 = scan (+) 0 reps
  let s2 = map (\i -> if i==0 then 0 else unsafe s1[i-1]) (iota n)
  let tmp = scatter (replicate (unsafe s1[n-1]) 0) s2 (iota n)
  let flags = map (>0) tmp
  in sgm_scan_add tmp flags

let sgm_iota [n] (flags:[n]bool) : [n]i32 =
  let iotas = sgm_scan_add (replicate n 1) flags
  in map (\x->x-1) iotas

let expand 'a 'b (sz: a -> i32) (get: a -> i32 -> b) (arr:[]a) : []b =
  let szs = map sz arr
  let idxs = repl_idx szs
  let iotas = sgm_iota (map2 (!=) idxs (rotate (i32.negate 1) idxs))
  in map2 (\i j -> get arr[i] j) idxs iotas

-- Drawing lines
type point = {x:i32,y:i32}
type line = (point,point)

let xmax ({x=x1,y=y1}:point) ({x=x2,y=y2}:point) : bool =
  i32.abs(x1-x2) > i32.abs(y1-y2)

let swap ({x,y}:point) : point = {x=y,y=x}

-- Write to grid
let update [h][w][n] (grid:[h][w]i32) (ps:[n]point) : [h][w]i32 =
  let is = map (\ p -> w*p.y+p.x) ps
  let flatgrid = flatten grid
  let ones = map (\_ -> 255*255*255) is
  in unflatten h w (scatter (copy flatgrid) is ones)

let compare (v1:i32) (v2:i32) : i32 =
  if v2 > v1 then 1 else if v1 > v2 then -1 else 0

let slo ({x=x1,y=y1}:point) ({x=x2,y=y2}:point) : f32 =
  if x2==x1 then if y2>y1 then r32(1) else r32(-1)
                 else r32(y2-y1) / r32(i32.abs(x2-x1))

let points_in_line ({x=x1,y=y1},{x=x2,y=y2}) : i32 =
  i32.(1 + max (abs(x2-x1)) (abs(y2-y1)))

-- Sequential algorithm for drawing multiple lines
let linepoints (p1:point, p2:point) : []point =
  let len = points_in_line (p1,p2)
  let xmax = xmax p1 p2
  let (dir,slop) =
    if xmax then (compare p1.x p2.x, slo p1 p2)
    else (compare p1.y p2.y, slo (swap p1) (swap p2))
  in map (\i -> if xmax then {x=p1.x+i*dir,
                              y=p1.y+t32(slop*r32 i)}
                else {x=p1.x+t32(slop*r32 i),
                      y=p1.y+i*dir}) (iota len)

let drawlines_seq [h] [w] [n] (grid: *[h][w]i32) (lines:[n]line) : [h][w]i32 =
  loop (grid) for i < n do -- find points for line i
    let points = linepoints (lines[i])
    in update grid points

-- Parallel flattened algorithm for drawing multiple lines,
-- using expansion.
let get_point ((p1,p2):line) (i:i32) : point =
  if i32.abs(p1.x-p2.x) > i32.abs(p1.y-p2.y)
  then let dir = compare (p1.x) (p2.x)
       let sl = slo p1 p2
       in {x=p1.x+dir*i,
           y=p1.y+t32(sl*r32 i)}
  else let dir = compare (p1.y) (p2.y)
       let sl = slo (swap p1) (swap p2)
       in {x=p1.x+t32(sl*r32 i),
           y=p1.y+i*dir}

let drawlines_flat [h][w][n] (grid:*[h][w]i32)
                              (lines:[n]line) :[h][w]i32 =
  let points = expand points_in_line get_point lines
  in update grid points

-- Triangles
module type Triangle = {
  type t
  val mk : point -> point -> point -> t
  val mk' : (i32,i32) -> (i32,i32) -> (i32,i32) -> t
  val lines_n : t -> i32
  val getline : t -> i32 -> line  -- horizontal line n
  val transl : i32 -> i32 -> t -> t
}
module triangle : Triangle = {
  type t = (point, point, point)  -- ordered wrt y axis

  let bubble (a:point) (b:point) =
    if b.y < a.y then (b,a) else (a,b)

  let mk (p:point) (q:point) (r:point) : t =
    let (p,q) = bubble p q
    let (q,r) = bubble q r
    let (p,q) = bubble p q
    in (p,q,r)

  let mk' (x1,y1) (x2,y2) (x3,y3) : t =
    mk {x=x1,y=y1} {x=x2,y=y2} {x=x3,y=y3}

  let lines_n ((p,_,r):t) = r.y - p.y + 1

  let dxdy (a:point) (b:point) : f32 =
    let dx = b.x - a.x
    let dy = b.y - a.y
    in if dy == 0 then f32.i32 0
       else f32.i32 dx f32./ f32.i32 dy

  let getline ((p,q,r):t) (i:i32) : (point,point) =
    let y = p.y + i
    in if i <= q.y - p.y then     -- upper half
         let sl1 = dxdy p q
         let sl2 = dxdy p r
         let x1 = p.x + i32.f32(sl1 * f32.i32 i)
         let x2 = p.x + i32.f32(sl2 * f32.i32 i)
         in ({x=x1,y},{x=x2,y})
       else                       -- lower half
         let sl1 = dxdy r p
         let sl2 = dxdy r q
         let dy = (r.y - p.y) - i
         let x1 = r.x - i32.f32(sl1 * f32.i32 dy)
         let x2 = r.x - i32.f32(sl2 * f32.i32 dy)
         in ({x=x1,y},{x=x2,y})

  let transl_p x y {x=a,y=b} = {x=a+x,y=b+y}
  let transl x y (p,q,r) =
    (transl_p x y p,
     transl_p x y q,
     transl_p x y r)
}

type state = { lines: []line
             , triangles: []triangle.t
             , h: i32
             , w: i32 }

module engine = minstd_rand
module distribution = uniform_int_distribution i32 engine

let mk_lines (h:i32) (w:i32) (n:i32) (rng:engine.rng) : ([n]line, engine.rng) =
  let rngs = engine.split_rng n rng
  let pairs = map (\ rng ->
                   let (rng,a) = distribution.rand (0, w - w / 10) rng
                   let (rng,b) = distribution.rand (0, h - h / 10) rng
                   let (rng,c) = distribution.rand (0, w / 10) rng
                   let (rng,d) = distribution.rand (0, h / 10) rng
                   let c = a + c
                   let d = b + d
                   in (({x=a,y=b},{x=c,y=d}),rng)) rngs
  let (res, rngs) = unzip pairs
  in (res, engine.join_rng rngs)

entry load_image (h:i32) (w:i32) : state =
  let seed = 43.0
  let rng = engine.rng_from_seed [i32.u32 (f32.to_bits seed)]
  let (lines,_rng) = mk_lines h w 5000 rng
  let triangles = --[triangle.mk' (50,10) (10,80) (100,80)]
                  [triangle.mk' (50,110) (10,180) (100,180)]
               ++ [triangle.mk' (100,20) (30,40) (200,70)]
               ++ [triangle.mk' (200,20) (280,40) (300,80)]
               ++ [triangle.mk' (100,100) (300,500) (800,100)]
  in { lines
     , triangles
     --    [((58,20),(2,3)),((27,3),(2,28)),((5,20),(20,20)),
     --     ((4,10),(6,25)),((26,25),(26,2)),((58,20),(52,3))]
     , h
     , w }

entry render (state: state): [][]i32 = unsafe
  let grid : *[][]i32 = replicate state.h (replicate state.w 0)
  let lines = state.lines
  let lines2 = expand triangle.lines_n triangle.getline state.triangles
  --let lines = [((10,10),(20,80))]
  in drawlines_flat grid (lines ++ lines2)

entry advance (state: state): state =
  {lines=map (\ ({x=a,y=b},{x=c,y=d}) ->
              let a = a +1
              let c = c +1
              let a = if a >= state.w then 0 else a
              let c = if c >= state.w then 0 else c
              in ({x=a,y=b},{x=c,y=d})) state.lines
  , triangles=map (triangle.transl 1 0) state.triangles
  , h=state.h
  , w=state.w }
