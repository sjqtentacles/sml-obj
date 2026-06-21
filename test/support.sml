(* support.sml -- shared helpers for mesh tests. *)

structure Support =
struct
  structure M = Mesh
  structure V3 = Mesh.Glm.Vec3
  structure V2 = Mesh.Glm.Vec2

  val eps = 1e~6

  fun rApprox (a, b) = Real.abs (a - b) < eps

  fun checkV3 name (expected as (ex,ey,ez)) v =
    Harness.check name
      (rApprox (V3.x v, ex) andalso rApprox (V3.y v, ey) andalso rApprox (V3.z v, ez))

  fun checkTri name (a, b, c) (t : M.tri) =
    Harness.check name (#a t = a andalso #b t = b andalso #c t = c)

  fun nth (vec, i) = Vector.sub (vec, i)
end
