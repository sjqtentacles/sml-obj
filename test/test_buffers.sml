(* test_buffers.sml -- flat position/index buffers reconstruct the triangles. *)

structure BufferTests =
struct
  structure M = Mesh
  open Support

  fun run () =
    let
      val _ = Harness.section "buffer views"
      val m = M.parseObj Fixtures.obj_quad
      val pb = M.positionBuffer m
      val ib = M.indexBuffer m

      val () = Harness.checkInt "position buffer length = 3*verts"
                 (3 * M.vertexCount m, Vector.length pb)
      val () = Harness.checkInt "index buffer length = 3*tris"
                 (3 * M.triCount m, Vector.length ib)

      (* position buffer interleaves x,y,z in vertex order *)
      val () = Harness.check "pb[6..8] = v2 (1,1,0)"
                 (rApprox (Vector.sub (pb, 6), 1.0)
                  andalso rApprox (Vector.sub (pb, 7), 1.0)
                  andalso rApprox (Vector.sub (pb, 8), 0.0))

      (* index buffer matches the triangle list, flattened *)
      val () = Harness.checkIntList "index buffer matches tris"
                 ([0,1,2, 0,2,3],
                  Vector.foldr (op ::) [] ib)

      (* reconstruct triangle 1's third vertex position via the buffers *)
      val i = Vector.sub (ib, 5)   (* tri1.c = vertex 3 *)
      val () = Harness.check "reconstructed v3 position = (0,1,0)"
                 (rApprox (Vector.sub (pb, i*3), 0.0)
                  andalso rApprox (Vector.sub (pb, i*3+1), 1.0)
                  andalso rApprox (Vector.sub (pb, i*3+2), 0.0))
    in () end
end
