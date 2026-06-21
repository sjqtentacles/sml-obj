(* sml-obj demo: generates a torus as Wavefront OBJ text, parses it with
   Mesh.parseObj, projects every vertex through a sml-glm model/view/projection
   pipeline, and draws the triangle edges as a depth-shaded wireframe to
   assets/wireframe.png. *)

open Mesh.Glm

fun rgba (r, g, b, a) : Image.rgba8 =
  { r = Word8.fromInt r, g = Word8.fromInt g
  , b = Word8.fromInt b, a = Word8.fromInt a }

(* ----- build a torus as OBJ text ----- *)
val nu = 28   (* segments around the major ring *)
val nv = 14   (* segments around the tube *)
val majorR = 1.0
val minorR = 0.42
val tau = 2.0 * Math.pi

val objText =
  let
    val sb = ref []
    fun emit s = sb := s :: !sb
    fun vertex (i, j) =
      let
        val u = tau * real i / real nu
        val v = tau * real j / real nv
        val x = (majorR + minorR * Math.cos v) * Math.cos u
        val y = minorR * Math.sin v
        val z = (majorR + minorR * Math.cos v) * Math.sin u
      in
        emit (String.concat
          ["v ", Real.toString x, " ", Real.toString y, " ", Real.toString z, "\n"])
      end
    fun idx (i, j) = (i mod nu) * nv + (j mod nv) + 1   (* 1-based *)
    fun face (i, j) =
      let
        val a = idx (i, j)
        val b = idx (i + 1, j)
        val c = idx (i + 1, j + 1)
        val d = idx (i, j + 1)
        fun tri (p, q, r) =
          emit (String.concat
            ["f ", Int.toString p, " ", Int.toString q, " ", Int.toString r, "\n"])
      in
        tri (a, b, c); tri (a, c, d)
      end
    fun loopV i =
      if i >= nu then ()
      else
        let fun loopJ j = if j >= nv then () else (vertex (i, j); loopJ (j + 1))
        in loopJ 0; loopV (i + 1) end
    fun faceV i =
      if i >= nu then ()
      else
        let fun faceJ j = if j >= nv then () else (face (i, j); faceJ (j + 1))
        in faceJ 0; faceV (i + 1) end
  in
    loopV 0; faceV 0;
    String.concat (rev (!sb))
  end

val mesh = Mesh.parseObj objText

(* ----- camera / projection (sml-glm) ----- *)
val width = 640
val height = 512

val proj = Mat4.perspective
  { fovy = radians 42.0, aspect = real width / real height, near = 0.1, far = 100.0 }
val viewM = Mat4.lookAt
  { eye = Vec3.v (2.7, 1.8, 2.7), center = Vec3.v (0.0, 0.0, 0.0), up = Vec3.v (0.0, 1.0, 0.0) }
val model = Mat4.mul (Mat4.rotateY 0.7, Mat4.rotateX 0.45)
val mvp = Mat4.mul (proj, Mat4.mul (viewM, model))

(* project a position -> (screen x, screen y, ndc depth) *)
fun project p =
  let
    val c = Mat4.mulV (mvp, Vec4.v (Vec3.x p, Vec3.y p, Vec3.z p, 1.0))
    val w = Vec4.w c
    val nx = Vec4.x c / w
    val ny = Vec4.y c / w
    val nz = Vec4.z c / w
  in
    ( Real.round ((nx * 0.5 + 0.5) * real (width - 1))
    , Real.round ((1.0 - (ny * 0.5 + 0.5)) * real (height - 1))
    , nz )
  end

val projected = Vector.map project (#positions mesh)

(* depth -> color: near edges bright cyan, far edges dim. *)
fun clampi v = if v < 0 then 0 else if v > 255 then 255 else v
fun shade d =
  let
    val t = (d + 1.0) / 2.0           (* roughly 0 (near) .. 1 (far) *)
    fun mix (a, b) = clampi (Real.round (real a + (real b - real a) * t))
  in
    rgba (mix (150, 32), mix (224, 70), mix (255, 96), 255)
  end

val img =
  let
    val base = Raster.blank (width, height) (rgba (16, 18, 24, 255))
    fun line img (i, j) =
      let
        val (x0, y0, d0) = Vector.sub (projected, i)
        val (x1, y1, d1) = Vector.sub (projected, j)
      in
        Raster.line img { x0 = x0, y0 = y0, x1 = x1, y1 = y1 } (shade ((d0 + d1) * 0.5))
      end
    fun edges (img, { a, b, c } : Mesh.tri) =
      let val img = line img (a, b)
          val img = line img (b, c)
      in line img (c, a) end
  in
    Vector.foldl (fn (t, img) => edges (img, t)) base (#tris mesh)
  end

val () =
  let
    val os = BinIO.openOut "assets/wireframe.png"
  in
    BinIO.output (os, Image.encodePng img);
    BinIO.closeOut os;
    print (String.concat
      ["wrote assets/wireframe.png  (",
       Int.toString (Mesh.vertexCount mesh), " verts, ",
       Int.toString (Mesh.triCount mesh), " tris)\n"])
  end
