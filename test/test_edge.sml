(* test_edge.sml -- malformed / degenerate inputs raise Mesh or parse cleanly. *)

structure EdgeTests =
struct
  structure M = Mesh
  open Support

  fun run () =
    let
      val _ = Harness.section "OBJ edge cases"
      val empty = M.parseObj "# only comments\n\n   \n"
      val () = Harness.checkInt "empty mesh has 0 vertices" (0, M.vertexCount empty)
      val () = Harness.checkInt "empty mesh has 0 tris" (0, M.triCount empty)

      val () = Harness.checkRaises "out-of-range face index"
                 (fn () => M.parseObj "v 0 0 0\nv 1 0 0\nf 1 2 5\n")
      val () = Harness.checkRaises "face with < 3 vertices"
                 (fn () => M.parseObj "v 0 0 0\nv 1 0 0\nf 1 2\n")
      val () = Harness.checkRaises "non-numeric vertex"
                 (fn () => M.parseObj "v 0 zero 0\n")

      val _ = Harness.section "OBJ missing attributes"
      val noNorm = M.parseObj "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n"
      val () = Harness.checkInt "no normals -> empty normals array"
                 (0, Vector.length (#normals noNorm))
      val () = Harness.checkInt "no texcoords -> empty texcoords array"
                 (0, Vector.length (#texcoords noNorm))

      val _ = Harness.section "PLY edge cases"
      val () = Harness.checkRaises "PLY without magic"
                 (fn () => M.parsePly (Byte.stringToBytes "format ascii 1.0\nend_header\n"))
      val () = Harness.checkRaises "PLY without end_header"
                 (fn () => M.parsePly (Byte.stringToBytes "ply\nformat ascii 1.0\n"))
      val () = Harness.checkRaises "PLY without vertex element"
                 (fn () => M.parsePly (Byte.stringToBytes
                            "ply\nformat ascii 1.0\nelement face 0\nend_header\n"))

      val _ = Harness.section "large coordinate precision"
      val big = M.parseObj "v 1234567.5 -9876543.25 0.0009765625\nv 1 0 0\nv 0 1 0\nf 1 2 3\n"
      val () = checkV3 "large/small coords survive" (1234567.5, ~9876543.25, 0.0009765625)
                 (nth (#positions big, 0))
    in () end
end
