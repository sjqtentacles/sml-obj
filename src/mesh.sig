(* mesh.sig

   Pure-SML parsers for common mesh asset formats, producing index buffers over
   sml-glm vectors:

     - Wavefront OBJ (.obj) + materials (.mtl)
     - PLY (.ply), both ASCII and binary little-endian

   A parsed `mesh` keeps the raw vertex attribute arrays (positions, optional
   normals and texcoords) plus a flat list of triangle index triples. Faces are
   triangulated (fans) and indices are normalized to 0-based on parse.

   All operations are pure and total; malformed input raises `Mesh`.
   Deterministic and byte-identical across MLton and Poly/ML. *)

signature MESH =
sig
  exception Mesh of string

  structure Glm : GLM

  (* One triangle's vertex references into the attribute arrays (0-based). *)
  type tri = { a : int, b : int, c : int }

  type mesh =
    { positions : Glm.Vec3.t vector,
      normals   : Glm.Vec3.t vector,    (* empty if absent *)
      texcoords : Glm.Vec2.t vector,    (* empty if absent *)
      tris      : tri vector }

  (* --- Wavefront OBJ --- *)

  (* Parse OBJ text. Supports v / vt / vn, f with v, v/vt, v//vn, v/vt/vn forms,
     1-based and negative (relative) indices, quad+ face triangulation, comments
     (#) and blank lines, CRLF or LF. The returned `tris` index into the parsed
     positions; normals/texcoords arrays are parallel where present. *)
  val parseObj : string -> mesh

  (* A single material from a .mtl file. *)
  type material =
    { name : string,
      ambient  : Glm.Vec3.t,            (* Ka *)
      diffuse  : Glm.Vec3.t,            (* Kd *)
      specular : Glm.Vec3.t }           (* Ks *)

  val parseMtl : string -> material list

  (* --- PLY --- *)

  (* Parse PLY (format ascii 1.0 or binary_little_endian 1.0). Reads the
     `vertex` element's x/y/z (and nx/ny/nz, s/t or u/v if present) and the
     `face` element's vertex_indices list, triangulating polygons. *)
  val parsePly : Word8Vector.vector -> mesh

  (* --- buffer views --- *)

  (* Flatten to an interleaved positions buffer [x,y,z, x,y,z, ...] in vertex
     order, and a flat index buffer [a,b,c, a,b,c, ...] in triangle order. *)
  val positionBuffer : mesh -> real vector
  val indexBuffer    : mesh -> int vector

  (* Convenience counts. *)
  val vertexCount : mesh -> int
  val triCount    : mesh -> int
end
