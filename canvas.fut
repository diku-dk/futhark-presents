import "/futlib/math"
import "lib/github.com/athas/matte/colour"

module type point = {
  type t = {x:i32,y:i32}
  val rotate : t -> f32 -> t -> t
  val transl : t -> t -> t
}

module type line = {
  type point
  type t = (point,point)
  val rotate : point -> f32 -> t -> t
  val transl : point -> t -> t
  val scale  : f32 -> t -> t
}

module type triangle = {
  type point
  type t = (point,point,point)
  val rotate : point -> f32 -> t -> t
  val transl : point -> t -> t
  val scale  : f32 -> t -> t
}

module type canvas = {
  type t[h][w]
  val mk         : (h:i32) -> (w:i32) -> *(t[h][w])
  val raw [h][w] : t[h][w] -> [h][w]i32

  module point : point
  module line : line with point = point.t
  module triangle : triangle with point = line.point

  -- Grouping
  type grp
  val &&&              : grp -> grp -> grp

  type colour
  val lines            : [](line.t,colour) -> grp
  val triangles        : [](triangle.t,colour) -> grp
  val points           : [](point.t,colour) -> grp

  val draw      [h][w] : grp -> t[h][w] -> t[h][w]
}

module point : point = {
  type t = {x:i32,y:i32}
  let rotate (o:t) (d:f32) (p:t) : t =
    let p = {x=p.x-o.x,y=p.y-o.y}
    let x = f32.(i32 p.x * cos d - i32 p.y * sin d)
    let y = f32.(i32 p.y * cos d + i32 p.x * sin d)
    let p = i32.({x=f32 x,y=f32 y})
    in {x=p.x+o.x,y=p.y+o.y}
  let transl (o:t) (p:t) : t =
    {x=o.x+p.x,y=o.y+p.y}
}

module line : line with point = point.t = {
  type point = point.t
  type t = (point,point)
  let rotate (o:point) (d:f32) (l:t) : t =
    (point.rotate o d l.1,
     point.rotate o d l.2)
  let transl (o:point) (l:t) : t =
    (point.transl o l.1,
     point.transl o l.2)
  let neg {x,y} = i32.({x=negate x,y=negate y})
  let scale (d:f32) (p1,p2) : t =
    let p2 = point.transl (neg p1) p2
    let p2 = f32.({x=d*i32 p2.x,y=d*i32 p2.y})
    let p2 = i32.({x=f32 p2.x,y=f32 p2.y})
    in (p1,point.transl p1 p2)
}

module triangle : triangle with point = point.t = {
  type point = point.t
  type t = (point,point,point)
  let rotate (o:point) (d:f32) (t:t) : t =
    (point.rotate o d t.1,
     point.rotate o d t.2,
     point.rotate o d t.3)
  let transl (o:point) (t:t) : t =
    (point.transl o t.1,
     point.transl o t.2,
     point.transl o t.3)
  let scale (_:f32) (t:t) = t
}

module canvas : canvas with colour = argb.colour = {
  type t[h][w] = [h][w]i32

  let mk (h:i32) (w:i32) : *(t[h][w]) =
    replicate h (replicate w 0)

  let raw [h][w] (x: t[h][w]) = x

  module point = point
  module line = line
  module triangle = triangle

  -- Grouping
  type colour = argb.colour
  type grp = {points:[](point.t,colour),
              lines:[](line.t,colour),
              triangles:[](triangle.t,colour)}

  let (&&&) ({points=p1,lines=l1,triangles=t1}:grp)
            ({points=p2,lines=l2,triangles=t2}:grp) : grp =
    {points=p1++p2,lines=l1++l2,triangles=t1++t2}

  let lines (xs:[](line.t,colour)) : grp = {lines=xs,points=[],triangles=[]}
  let triangles (xs:[](triangle.t,colour)) : grp = {triangles=xs,lines=[],points=[]}
  let points (xs: [](point.t,colour)) : grp = {points=xs,triangles=[],lines=[]}

  -- Segmented utlity functions (memo: instead, refer to segmented library)

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

  -- Converting lines to points (flattened)

  let swap ({x,y}:point.t) : point.t = {x=y,y=x}

  let compare (v1:i32) (v2:i32) : i32 =
    if v2 > v1 then 1 else if v1 > v2 then -1 else 0

  let slo ({x=x1,y=y1}:point.t) ({x=x2,y=y2}:point.t) : f32 =
    if x2==x1 then if y2>y1 then r32 1 else r32(-1)
                   else r32(y2-y1) / r32(i32.abs(x2-x1))

  -- Parallel flattened algorithm for turning lines into
  -- points, using expansion.

  let points_in_line ((({x=x1,y=y1},{x=x2,y=y2}),_):(line.t,colour)) : i32 =
    i32.(1 + max (abs(x2-x1)) (abs(y2-y1)))

  let get_point_in_line (((p1,p2),c):(line.t,colour)) (i:i32) : (point.t,colour) =
    if i32.abs(p1.x-p2.x) > i32.abs(p1.y-p2.y)
    then let dir = compare (p1.x) (p2.x)
         let sl = slo p1 p2
         in ({x=p1.x+dir*i,
              y=p1.y+t32(sl*r32 i)},c)
    else let dir = compare (p1.y) (p2.y)
         let sl = slo (swap p1) (swap p2)
         in ({x=p1.x+t32(sl*r32 i),
              y=p1.y+i*dir},c)

  let points_of_lines (xs:[](line.t,colour)) : [](point.t,colour) =
    expand points_in_line get_point_in_line xs

  -- Parallel flattened algorithm for turning triangles into
  -- lines, using expansion.

  let bubble (a:point.t) (b:point.t) =
    if b.y < a.y then (b,a) else (a,b)

  let normalize (((p,q,r),c):(triangle.t,colour)) =
    let (p,q) = bubble p q
    let (q,r) = bubble q r
    let (p,q) = bubble p q
    in ((p,q,r),c)

  let lines_in_triangle (((p,_,r),_):(triangle.t,colour)) =
    r.y - p.y + 1

  let dxdy (a:point.t) (b:point.t) : f32 =
    let dx = b.x - a.x
    let dy = b.y - a.y
    in if dy == 0 then f32.i32 0
       else f32.i32 dx f32./ f32.i32 dy

  let get_line_in_triangle (((p,q,r),c):(triangle.t,colour)) (i:i32) : (line.t,colour) =
    let y = p.y + i
    in if i <= q.y - p.y then     -- upper half
         let sl1 = dxdy p q
         let sl2 = dxdy p r
         let x1 = p.x + i32.f32(sl1 * f32.i32 i)
         let x2 = p.x + i32.f32(sl2 * f32.i32 i)
         in (({x=x1,y},{x=x2,y}),c)
       else                       -- lower half
         let sl1 = dxdy r p
         let sl2 = dxdy r q
         let dy = (r.y - p.y) - i
         let x1 = r.x - i32.f32(sl1 * f32.i32 dy)
         let x2 = r.x - i32.f32(sl2 * f32.i32 dy)
         in (({x=x1,y},{x=x2,y}),c)

  let lines_of_triangles (xs:[](triangle.t,colour)) : [](line.t,colour) =
    expand lines_in_triangle get_line_in_triangle
           (map normalize xs)

  -- Write to grid
  let update [h][w][n] (ps:[n](point.t,colour)) (grid:[h][w]i32) : [h][w]i32 =
    let ps = filter (\ ((p,_):(point.t,colour)) -> (0 <= p.x && p.x < w && 0 <= p.y && p.y < h)) ps
    let ps = map (\ (p,c) -> (w*p.y+p.x,c)) ps
    let flatgrid = flatten grid
    let is = map (.1) ps
    let cs = map (.2) ps
    in unflatten h w (scatter (copy flatgrid) is cs)

  let draw [h][w] ({points,lines,triangles}:grp) (grid: (t[h][w])) : (t[h][w]) =
    let ls = lines_of_triangles triangles
    let ps = points_of_lines (ls ++ lines)
    in update (points ++ ps) grid
}
