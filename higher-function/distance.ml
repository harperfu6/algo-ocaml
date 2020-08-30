let distance (x1, y1) (x2, y2) =
  let dx = x1 -. x2 in
  let dy = y1 -. y2 in
  sqrt (dx *. dx +. dy *. dy)

let distance3d (x1, y1, z1) (x2, y2, z2) =
  let dx = x1 -. x2 in
  let dy = y1 -. y2 in
  let dz = z1 -. z2 in
  sqrt (dx *. dx +. dy *. dy +. dz *. dz)

let norm = distance (0., 0.)
let norm3d = distance3d (0., 0., 0.)
