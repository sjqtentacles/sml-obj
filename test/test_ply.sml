(* test_ply.sml -- PLY ascii and binary-little-endian produce identical meshes. *)

structure PlyTests =
struct
  structure M = Mesh
  open Support

  fun sameMesh (a : M.mesh, b : M.mesh) =
    Vector.length (#positions a) = Vector.length (#positions b)
    andalso Vector.length (#tris a) = Vector.length (#tris b)
    andalso
      let
        fun posEq i =
          let val pa = nth (#positions a, i) val pb = nth (#positions b, i)
          in rApprox (V3.x pa, V3.x pb) andalso rApprox (V3.y pa, V3.y pb)
             andalso rApprox (V3.z pa, V3.z pb) end
        fun all p 0 = true | all p k = p (k-1) andalso all p (k-1)
      in all posEq (Vector.length (#positions a)) end

  fun run () =
    let
      val _ = Harness.section "PLY ASCII"
      val pa = M.parsePly Fixtures.ply_ascii
      val () = Harness.checkInt "ascii vertex count" (4, M.vertexCount pa)
      val () = Harness.checkInt "ascii quad -> 2 tris" (2, M.triCount pa)
      val () = checkV3 "ascii v2" (1.0, 1.0, 0.0) (nth (#positions pa, 2))

      val _ = Harness.section "PLY binary little-endian"
      val pb = M.parsePly Fixtures.ply_binary
      val () = Harness.checkInt "binary vertex count" (4, M.vertexCount pb)
      val () = checkV3 "binary float32 v2 decodes" (1.0, 1.0, 0.0) (nth (#positions pb, 2))
      val () = checkTri "binary face fan tri 1" (0, 2, 3) (nth (#tris pb, 1))

      val _ = Harness.section "PLY ascii == binary"
      val () = Harness.check "same mesh from both encodings" (sameMesh (pa, pb))
    in () end
end
