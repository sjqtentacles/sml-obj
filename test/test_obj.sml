(* test_obj.sml -- Wavefront OBJ parsing: vertices, triangulation, index forms. *)

structure ObjTests =
struct
  structure M = Mesh
  open Support

  fun run () =
    let
      val _ = Harness.section "OBJ quad parsing"
      val m = M.parseObj Fixtures.obj_quad
      val () = Harness.checkInt "vertex count" (4, M.vertexCount m)
      val () = Harness.checkInt "quad triangulated to 2 tris" (2, M.triCount m)
      val () = checkV3 "v0 position" (0.0, 0.0, 0.0) (nth (#positions m, 0))
      val () = checkV3 "v2 position" (1.0, 1.0, 0.0) (nth (#positions m, 2))
      val () = Harness.checkInt "texcoord count" (4, Vector.length (#texcoords m))
      val () = Harness.checkInt "normal count" (1, Vector.length (#normals m))
      val () = checkTri "fan tri 0 (1-based -> 0-based)" (0, 1, 2) (nth (#tris m, 0))
      val () = checkTri "fan tri 1" (0, 2, 3) (nth (#tris m, 1))

      val _ = Harness.section "OBJ index forms"
      val mn = M.parseObj Fixtures.obj_neg
      val () = Harness.checkInt "negative-index mesh vertex count" (3, M.vertexCount mn)
      val () = checkTri "negative (relative) indices resolve" (0, 1, 2) (nth (#tris mn, 0))

      val _ = Harness.section "OBJ whitespace & comments"
      val crlf = M.parseObj "v 0 0 0\r\nv 1 0 0\r\nv 0 1 0\r\n# c\r\nf 1 2 3\r\n"
      val () = Harness.checkInt "CRLF line endings handled" (1, M.triCount crlf)
    in () end
end
