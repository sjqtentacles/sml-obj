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

      val _ = Harness.section "OBJ oversized indices (cross-compiler bounded parse)"
      (* A face vertex index past the fixed 32-bit range must raise `Mesh`
         (the documented failure), never a raw `Overflow`. MLton's default
         `int` is 32-bit and `Int.fromString` raises `Overflow` past 2^31,
         while Poly/ML's 63-bit `int` silently accepts it -- so an unbounded
         parse both crashes MLton and diverges across compilers. `parseInt`
         must reject out-of-range digits as `Mesh`. A real mesh never has 2^31
         vertices, so bounding to 32-bit loses nothing. *)
      fun objErr src =
        (ignore (M.parseObj src); "no-exn")
        handle M.Mesh _ => "mesh"
             | Overflow  => "overflow"
             | _         => "other"
      val () = Harness.checkString "face index 2147483648 -> Mesh, not Overflow"
                 ("mesh", objErr "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 2147483648\n")
      val () = Harness.checkString "face index 999999999999 -> Mesh, not Overflow"
                 ("mesh", objErr "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 999999999999\n")
      val () = Harness.checkString "negative index -999999999999 -> Mesh, not Overflow"
                 ("mesh", objErr "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 -999999999999\n")
      (* an in-range (if out-of-bounds) index still fails as Mesh, not Overflow;
         and a valid small face still parses to one triangle. *)
      val okSmall = M.parseObj "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n"
      val () = Harness.checkInt "valid small face still parses (1 tri)"
                 (1, M.triCount okSmall)

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
