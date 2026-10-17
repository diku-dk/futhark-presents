import "lib/github.com/athas/matte/colour"

type marg_pos = i64
type element = argb.colour
def oob : element = 0x12345678
type hood = (element, element, element, element)

def hood_quadrants ((ul, ur, dl, dr): hood) : (element, element, element, element) =
  (ul, ur, dl, dr)

def hood_from_quadrants (ul: element) (ur: element) (dl: element) (dr: element) : hood =
  (ul, ur, dl, dr)

def hood_quadrant (h: hood) (i: marg_pos) : element =
  let (ul0, ur0, dl0, dr0) = hood_quadrants h
  in if i == 0
     then ul0
     else if i == 1
     then ur0
     else if i == 2
     then dl0
     else dr0

def index_to_hood (offset: i64) (i: i64) : (i64, i64) =
  if offset == 0
  then (i / 2, i % 2)
  else ((i + 1) / 2, (i + 1) % 2)

def mk_hoods [h] [w] (offset: i64) (pixels: [h][w]argb.colour) : [][]hood =
  let get (i, j) =
    if i >= 0 && i < h && j >= 0 && j < w
    then #[unsafe] pixels[i, j]
    else oob
  in tabulate_2d (h / 2 - offset)
                 (w / 2 - offset)
                 (\i j ->
                    hood_from_quadrants (get (i * 2 + offset, j * 2 + offset))
                                        (get (i * 2 + offset, j * 2 + 1 + offset))
                                        (get (i * 2 + 1 + offset, j * 2 + offset))
                                        (get (i * 2 + 1 + offset, j * 2 + 1 + offset)))

def world_index [h] [w] (offset: i64) (elems: [h][w]hood) ((i, j): (i64, i64)) : element =
  let (hi, ii) = index_to_hood offset i
  let (hj, ij) = index_to_hood offset j
  in if hi < 0 || hi >= h || hj < 0 || hj >= w
     then oob
     else hood_quadrant (#[unsafe] elems[hi, hj]) (ii * 2 + ij)

def un_hoods [h] [w] (offset: i64) (hoods: [h][w]hood) : [][]argb.colour =
  let particle_pixel i j = world_index offset hoods (i, j)
  in tabulate_2d ((h + offset) * 2) ((w + offset) * 2) particle_pixel

def is_wall = (== oob)

def weight c =
  let (r, g, b, _) = argb.to_rgba c
  in -(0.2126 * r + 0.7152 * g + 0.0722 * b)

def check_if_drop (above: element) (below: element) : (element, element) =
  if is_wall above || is_wall below || weight below >= weight above
  then (above, below)
  else (below, above)

def gravity (h: hood) : hood =
  let (ul, ur, dl, dr) = hood_quadrants h
  let (ul, dl) = check_if_drop ul dl
  let (ur, dr) = check_if_drop ur dr
  let (ul, dr) = check_if_drop ul dr
  let (ur, dl) = check_if_drop ur dl
  in hood_from_quadrants ul ur dl dr

def drop_pixels [h] [w] (i: i32) (pixels: [h][w]argb.colour) : [h][w]argb.colour =
  let offset = (i64.i32 i % 2) - 1
  in (mk_hoods offset pixels |> map (map gravity) |> un_hoods offset) :> [h][w]argb.colour
