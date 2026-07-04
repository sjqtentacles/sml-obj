(* mesh.sml

   Implementation of the MESH signature: OBJ/MTL and PLY parsers over the
   vendored sml-glm. Pure Basis; deterministic across MLton and Poly/ML. *)

structure Mesh :> MESH =
struct
  exception Mesh of string

  structure Glm = Glm

  type tri = { a : int, b : int, c : int }

  type mesh =
    { positions : Glm.Vec3.t vector,
      normals   : Glm.Vec3.t vector,
      texcoords : Glm.Vec2.t vector,
      tris      : tri vector }

  type material =
    { name : string,
      ambient  : Glm.Vec3.t,
      diffuse  : Glm.Vec3.t,
      specular : Glm.Vec3.t }

  (* ---------- small text utilities ---------- *)

  (* split into lines, tolerating both LF and CRLF (and lone CR). *)
  fun lines s =
    let
      val s = String.map (fn c => if c = #"\r" then #"\n" else c) s
    in String.fields (fn c => c = #"\n") s end

  fun isBlankOrComment ln =
    case String.tokens Char.isSpace ln of
      [] => true
    | (first :: _) => String.isPrefix "#" first

  fun words ln = String.tokens Char.isSpace ln

  fun parseReal s =
    case Real.fromString s of
      SOME r => r
    | NONE => raise Mesh ("expected a real number, got: " ^ s)

  (* Parse via IntInf and bound to the fixed 32-bit range so the result is
     identical on MLton (default 32-bit int) and Poly/ML (63-bit int): an
     integer field outside [~2^31, 2^31-1] raises `Mesh` rather than raising
     Overflow on MLton / silently accepting a huge value on Poly/ML. Mesh
     indices and counts always fit `int` on every target -- a real asset never
     has 2^31 vertices. *)
  fun parseInt s =
    case IntInf.fromString s of
      SOME n =>
        if n >= ~2147483648 andalso n <= 2147483647
        then IntInf.toInt n
        else raise Mesh ("expected an integer, got: " ^ s)
    | NONE => raise Mesh ("expected an integer, got: " ^ s)

  (* ---------- OBJ ---------- *)

  (* Resolve a (possibly negative / 1-based) OBJ index against the current
     count of that attribute. OBJ indices are 1-based; negative means relative
     to the end (-1 = last). Returns a 0-based index. *)
  fun resolveIndex (raw, count) =
    let
      val idx = if raw < 0 then count + raw else raw - 1
    in
      if idx < 0 orelse idx >= count
      then raise Mesh ("OBJ index out of range: " ^ Int.toString raw)
      else idx
    end

  (* parse one face vertex token "v", "v/vt", "v//vn", or "v/vt/vn" into the
     position index (the only one we thread into tris). counts = #positions. *)
  fun faceVertPos (tok, nPos) =
    let
      val parts = String.fields (fn c => c = #"/") tok
    in
      case parts of
        (vs :: _) =>
          if vs = "" then raise Mesh "OBJ face: empty vertex index"
          else resolveIndex (parseInt vs, nPos)
      | [] => raise Mesh "OBJ face: empty token"
    end

  fun parseObj text =
    let
      val posR = ref ([] : Glm.Vec3.t list)   (* reversed accumulators *)
      val norR = ref ([] : Glm.Vec3.t list)
      val texR = ref ([] : Glm.Vec2.t list)
      val triR = ref ([] : tri list)
      val nPos = ref 0

      fun addPos v = (posR := v :: !posR; nPos := !nPos + 1)
      fun addNor v = norR := v :: !norR
      fun addTex v = texR := v :: !texR

      fun handleFace ws =
        let
          val verts = List.map (fn t => faceVertPos (t, !nPos)) ws
        in
          case verts of
            (v0 :: v1 :: v2 :: rest) =>
              (* fan-triangulate: (v0, v_i, v_{i+1}) over [v1,v2,...] *)
              let
                fun fan (prev, x :: xs) =
                      ( triR := { a = v0, b = prev, c = x } :: !triR
                      ; fan (x, xs) )
                  | fan (_, []) = ()
              in fan (v1, v2 :: rest) end
          | _ => raise Mesh "OBJ face needs at least 3 vertices"
        end

      fun handleLine ln =
        if isBlankOrComment ln then ()
        else
          case words ln of
            ("v" :: x :: y :: z :: _) =>
              addPos (Glm.Vec3.v (parseReal x, parseReal y, parseReal z))
          | ("vn" :: x :: y :: z :: _) =>
              addNor (Glm.Vec3.v (parseReal x, parseReal y, parseReal z))
          | ("vt" :: x :: y :: _) =>
              addTex (Glm.Vec2.v (parseReal x, parseReal y))
          | ("vt" :: x :: _) =>
              addTex (Glm.Vec2.v (parseReal x, 0.0))
          | ("f" :: rest) => handleFace rest
          | (_ :: _) => ()   (* ignore o/g/s/usemtl/mtllib/etc. *)
          | [] => ()
    in
      List.app handleLine (lines text);
      { positions = Vector.fromList (List.rev (!posR)),
        normals   = Vector.fromList (List.rev (!norR)),
        texcoords = Vector.fromList (List.rev (!texR)),
        tris      = Vector.fromList (List.rev (!triR)) }
    end

  (* ---------- MTL ---------- *)

  fun parseMtl text =
    let
      val matsR = ref ([] : material list)
      val cur = ref (NONE : { name : string, ka : Glm.Vec3.t ref,
                              kd : Glm.Vec3.t ref, ks : Glm.Vec3.t ref } option)

      fun flush () =
        case !cur of
          NONE => ()
        | SOME m =>
            matsR := { name = #name m, ambient = !(#ka m),
                       diffuse = !(#kd m), specular = !(#ks m) } :: !matsR

      fun handleLine ln =
        if isBlankOrComment ln then ()
        else
          case words ln of
            ("newmtl" :: nm :: _) =>
              ( flush ()
              ; cur := SOME { name = nm, ka = ref Glm.Vec3.zero,
                              kd = ref Glm.Vec3.zero, ks = ref Glm.Vec3.zero } )
          | ("Ka" :: r :: g :: b :: _) =>
              (case !cur of SOME m => (#ka m) := Glm.Vec3.v (parseReal r, parseReal g, parseReal b) | NONE => ())
          | ("Kd" :: r :: g :: b :: _) =>
              (case !cur of SOME m => (#kd m) := Glm.Vec3.v (parseReal r, parseReal g, parseReal b) | NONE => ())
          | ("Ks" :: r :: g :: b :: _) =>
              (case !cur of SOME m => (#ks m) := Glm.Vec3.v (parseReal r, parseReal g, parseReal b) | NONE => ())
          | _ => ()
    in
      List.app handleLine (lines text);
      flush ();
      List.rev (!matsR)
    end

  (* ---------- PLY ---------- *)

  (* PLY support: format ascii 1.0 and binary_little_endian 1.0. We read the
     `vertex` element (x,y,z required; nx,ny,nz and s,t / u,v optional) and the
     `face` element's list property of vertex indices, triangulating fans. *)

  local
    structure W8V = Word8Vector
    structure W8S = Word8VectorSlice

    datatype scalar =
        Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | F32 | F64

    fun scalarOf s =
      case s of
        "char" => Int8 | "int8" => Int8
      | "uchar" => UInt8 | "uint8" => UInt8
      | "short" => Int16 | "int16" => Int16
      | "ushort" => UInt16 | "uint16" => UInt16
      | "int" => Int32 | "int32" => Int32
      | "uint" => UInt32 | "uint32" => UInt32
      | "float" => F32 | "float32" => F32
      | "double" => F64 | "float64" => F64
      | _ => raise Mesh ("PLY: unknown scalar type " ^ s)

    fun scalarSize t =
      case t of
        Int8 => 1 | UInt8 => 1 | Int16 => 2 | UInt16 => 2
      | Int32 => 4 | UInt32 => 4 | F32 => 4 | F64 => 8

    (* a property is either a scalar or a list (count-type, elem-type) *)
    datatype prop =
        Scalar of string * scalar
      | List of string * scalar * scalar   (* name, count type, elem type *)

    type element = { name : string, count : int, props : prop list }

    (* find the byte offset just past the "end_header\n". *)
    fun headerEnd v =
      let
        val n = W8V.length v
        val needle = "end_header"
        fun matchAt i =
          let
            fun go j = j >= String.size needle
                       orelse (i + j < n
                               andalso W8V.sub (v, i + j) = Byte.charToByte (String.sub (needle, j))
                               andalso go (j + 1))
          in go 0 end
        fun scan i =
          if i >= n then raise Mesh "PLY: no end_header"
          else if matchAt i then
            (* advance to the newline after end_header *)
            let
              fun toNl j = if j >= n then j
                           else if W8V.sub (v, j) = 0wx0A then j + 1 else toNl (j + 1)
            in toNl (i + String.size needle) end
          else scan (i + 1)
      in scan 0 end

    (* read little-endian scalar at byte offset; return (realValue, intValue, nextOffset). *)
    fun readScalar (v, off, t) =
      let
        fun u8 i = Word8.toInt (W8V.sub (v, i))
        val sz = scalarSize t
        fun uintLE k =
          let
            fun go (j, acc, mul) =
              if j >= k then acc
              else go (j + 1, acc + u8 (off + j) * mul,
                       if j + 1 >= k then mul else mul * 256)
          in go (0, 0, 1) end
      in
        case t of
          F32 =>
            let
              val w = Word32.fromInt (u8 off)
                      + Word32.<< (Word32.fromInt (u8 (off+1)), 0w8)
                      + Word32.<< (Word32.fromInt (u8 (off+2)), 0w16)
                      + Word32.<< (Word32.fromInt (u8 (off+3)), 0w24)
              val r = float32FromBits w
            in (r, Real.round r, off + 4) end
        | F64 => raise Mesh "PLY: binary double not supported"
        | _ =>
            let
              val n = uintLE sz
              val n =
                case t of
                  Int8  => if n >= 128 then n - 256 else n
                | Int16 => if n >= 32768 then n - 65536 else n
                | Int32 => if Word32.andb (Word32.fromInt n, 0wx80000000) <> 0w0
                           then n - (65536 * 65536) else n
                | _ => n
            in (Real.fromInt n, n, off + sz) end
      end

    and float32FromBits (w : Word32.word) =
      let
        val sign = if Word32.andb (w, 0wx80000000) <> 0w0 then ~1.0 else 1.0
        val exp = Word32.toInt (Word32.andb (Word32.>> (w, 0w23), 0wxFF))
        val mant = Word32.toInt (Word32.andb (w, 0wx7FFFFF))
      in
        if exp = 0 then
          sign * Math.pow (2.0, ~126.0) * (Real.fromInt mant / Math.pow (2.0, 23.0))
        else
          sign * Math.pow (2.0, Real.fromInt (exp - 127))
               * (1.0 + Real.fromInt mant / Math.pow (2.0, 23.0))
      end
  in
    fun parsePly v =
      let
        val hdrEnd = headerEnd v
        val headerText =
          Byte.bytesToString (W8S.vector (W8S.slice (v, 0, SOME hdrEnd)))
        val hlines = List.filter (fn l => l <> "")
                       (List.map (fn l => l) (lines headerText))
        val () = case hlines of
                   (h :: _) => if String.isPrefix "ply" h then ()
                               else raise Mesh "PLY: missing magic"
                 | [] => raise Mesh "PLY: empty header"

        (* parse format + element/property declarations *)
        val fmtRef = ref "ascii"
        fun parseHeader (ls, elems, curName, curCount, curProps) =
          case ls of
            [] => List.rev (case curName of
                              SOME nm => { name = nm, count = curCount,
                                           props = List.rev curProps } :: elems
                            | NONE => elems)
          | (l :: rest) =>
              (case words l of
                 ("format" :: f :: _) => (fmtRef := f; parseHeader (rest, elems, curName, curCount, curProps))
               | ("element" :: nm :: cnt :: _) =>
                   let
                     val elems' = case curName of
                                    SOME pn => { name = pn, count = curCount,
                                                 props = List.rev curProps } :: elems
                                  | NONE => elems
                   in parseHeader (rest, elems', SOME nm, parseInt cnt, []) end
               | ("property" :: "list" :: ct :: et :: pn :: _) =>
                   parseHeader (rest, elems, curName, curCount,
                                List (pn, scalarOf ct, scalarOf et) :: curProps)
               | ("property" :: ty :: pn :: _) =>
                   parseHeader (rest, elems, curName, curCount,
                                Scalar (pn, scalarOf ty) :: curProps)
               | _ => parseHeader (rest, elems, curName, curCount, curProps))
        val elements = parseHeader (hlines, [], NONE, 0, [])
        val binary = String.isPrefix "binary_little_endian" (!fmtRef)
        val ascii = String.isPrefix "ascii" (!fmtRef)
        val () = if binary orelse ascii then ()
                 else raise Mesh ("PLY: unsupported format " ^ !fmtRef)

        fun findElem nm = List.find (fn e => #name e = nm) elements
        val vElem = case findElem "vertex" of SOME e => e | NONE => raise Mesh "PLY: no vertex element"
        val fElem = findElem "face"

        (* accumulate vertex attributes and faces *)
        val posR = ref ([] : Glm.Vec3.t list)
        val norR = ref ([] : Glm.Vec3.t list)
        val texR = ref ([] : Glm.Vec2.t list)
        val triR = ref ([] : tri list)
        val hasN = List.exists (fn Scalar ("nx", _) => true | _ => false) (#props vElem)
        val hasST = List.exists (fn Scalar (n, _) => n = "s" orelse n = "u" | _ => false) (#props vElem)

        fun emitVertex fields =
          (* fields: (name -> real) lookups by position; we read by prop order *)
          let
            fun get nm = case List.find (fn (n, _) => n = nm) fields of
                           SOME (_, r) => r | NONE => 0.0
          in
            posR := Glm.Vec3.v (get "x", get "y", get "z") :: !posR;
            if hasN then norR := Glm.Vec3.v (get "nx", get "ny", get "nz") :: !norR else ();
            if hasST then texR := Glm.Vec2.v
                           (case List.find (fn (n,_) => n="s" orelse n="u") fields of
                              SOME (_, r) => r | NONE => 0.0,
                            case List.find (fn (n,_) => n="t" orelse n="v") fields of
                              SOME (_, r) => r | NONE => 0.0) :: !texR
            else ()
          end

        fun fanFace idxs =
          case idxs of
            (i0 :: i1 :: rest) =>
              let fun fan (prev, x :: xs) =
                        (triR := { a = i0, b = prev, c = x } :: !triR; fan (x, xs))
                    | fan (_, []) = ()
              in fan (i1, rest) end
          | _ => raise Mesh "PLY: face with <3 vertices"
      in
        if ascii then
          let
            val bodyText =
              Byte.bytesToString (W8S.vector (W8S.slice (v, hdrEnd, NONE)))
            val toks = List.filter (fn t => t <> "")
                         (String.tokens Char.isSpace bodyText)
            val cursor = ref toks
            fun next () = case !cursor of
                            (t :: rest) => (cursor := rest; t)
                          | [] => raise Mesh "PLY: unexpected end of body"
            fun readScalarA t =
              let val s = next () in
                case t of
                  F32 => parseReal s | F64 => parseReal s
                | _ => Real.fromInt (parseInt s)
              end
            fun readVertex () =
              let
                val fields =
                  List.map
                    (fn Scalar (nm, t) => (nm, readScalarA t)
                      | List _ => raise Mesh "PLY: list property in vertex unsupported")
                    (#props vElem)
              in emitVertex fields end
            fun readFace () =
                  (* assume a single list property (vertex_indices) *)
                  let
                    val cnt = Real.round (readScalarA UInt8)  (* count type read as number *)
                    val idxs = List.tabulate (cnt, fn _ => Real.round (readScalarA Int32))
                  in fanFace idxs end
            val () = let fun go 0 = () | go k = (readVertex (); go (k - 1))
                     in go (#count vElem) end
            val () = case fElem of
                       SOME fe => let fun go 0 = () | go k = (readFace (); go (k-1))
                                  in go (#count fe) end
                     | NONE => ()
          in
            { positions = Vector.fromList (List.rev (!posR)),
              normals = Vector.fromList (List.rev (!norR)),
              texcoords = Vector.fromList (List.rev (!texR)),
              tris = Vector.fromList (List.rev (!triR)) }
          end
        else
          let
            val off = ref hdrEnd
            fun rd t = let val (r, i, off') = readScalar (v, !off, t) in off := off'; (r, i) end
            fun readVertex () =
              let
                val fields =
                  List.map
                    (fn Scalar (nm, t) => (nm, #1 (rd t))
                      | List _ => raise Mesh "PLY: list property in vertex unsupported")
                    (#props vElem)
              in emitVertex fields end
            fun readFace fe =
              case #props fe of
                (List (_, ct, et) :: _) =>
                  let
                    val (_, cnt) = rd ct
                    val idxs = List.tabulate (cnt, fn _ => #2 (rd et))
                  in fanFace idxs end
              | _ => raise Mesh "PLY: face element lacks a list property"
            val () = let fun go 0 = () | go k = (readVertex (); go (k-1))
                     in go (#count vElem) end
            val () = case fElem of
                       SOME fe => let fun go 0 = () | go k = (readFace fe; go (k-1))
                                  in go (#count fe) end
                     | NONE => ()
          in
            { positions = Vector.fromList (List.rev (!posR)),
              normals = Vector.fromList (List.rev (!norR)),
              texcoords = Vector.fromList (List.rev (!texR)),
              tris = Vector.fromList (List.rev (!triR)) }
          end
      end
  end

  (* ---------- buffer views ---------- *)

  fun vertexCount (m : mesh) = Vector.length (#positions m)
  fun triCount (m : mesh) = Vector.length (#tris m)

  fun positionBuffer (m : mesh) =
    let
      val ps = #positions m
      val n = Vector.length ps
    in
      Vector.tabulate (n * 3,
        fn i =>
          let val p = Vector.sub (ps, i div 3) in
            case i mod 3 of
              0 => Glm.Vec3.x p | 1 => Glm.Vec3.y p | _ => Glm.Vec3.z p
          end)
    end

  fun indexBuffer (m : mesh) =
    let
      val ts = #tris m
      val n = Vector.length ts
    in
      Vector.tabulate (n * 3,
        fn i =>
          let val t = Vector.sub (ts, i div 3) in
            case i mod 3 of 0 => #a t | 1 => #b t | _ => #c t
          end)
    end
end
