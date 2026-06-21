(* test_mtl.sml -- Wavefront MTL material parsing. *)

structure MtlTests =
struct
  structure M = Mesh
  open Support

  fun run () =
    let
      val _ = Harness.section "MTL parsing"
      val mats = M.parseMtl Fixtures.mtl
      val () = Harness.checkInt "two materials" (2, List.length mats)
      val red = List.nth (mats, 0)
      val green = List.nth (mats, 1)
      val () = Harness.checkString "first material name" ("red", #name red)
      val () = Harness.checkString "second material name" ("green", #name green)
      val () = checkV3 "red Ka" (0.1, 0.0, 0.0) (#ambient red)
      val () = checkV3 "red Kd" (0.8, 0.1, 0.1) (#diffuse red)
      val () = checkV3 "red Ks" (1.0, 1.0, 1.0) (#specular red)
      val () = checkV3 "green Kd" (0.1, 0.7, 0.2) (#diffuse green)
      val () = checkV3 "green Ka defaults to zero" (0.0, 0.0, 0.0) (#ambient green)
    in () end
end
