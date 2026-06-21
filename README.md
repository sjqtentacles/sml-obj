# sml-obj

Pure-Standard-ML **mesh asset parsers** producing index buffers over
[`sml-glm`](https://github.com/sjqtentacles/sml-glm) vectors:

- **Wavefront OBJ** (`.obj`) — `v` / `vt` / `vn`, all face index forms
  (`v`, `v/vt`, `v//vn`, `v/vt/vn`), 1-based and negative (relative) indices,
  polygon fan-triangulation, comments, blank lines, LF or CRLF.
- **Wavefront MTL** (`.mtl`) — `newmtl` with `Ka` / `Kd` / `Ks`.
- **PLY** (`.ply`) — `format ascii 1.0` and `binary_little_endian 1.0`; reads the
  `vertex` element (`x`/`y`/`z`, optional `nx`/`ny`/`nz`, `s`/`t` or `u`/`v`) and
  the `face` element's `vertex_indices` list, with fan-triangulation.

No FFI, no C — just the Basis Library plus the vendored `sml-glm`. Deterministic
and byte-identical across [MLton](http://mlton.org/) and
[Poly/ML](https://www.polyml.org/).

## Installation

With [`smlpkg`](https://github.com/diku-dk/smlpkg):

```sh
smlpkg add github.com/sjqtentacles/sml-obj
smlpkg sync
```

Then build from `src/mesh.mlb` (which pulls in the vendored `sml-glm`).

## The mesh type

```sml
type tri = { a : int, b : int, c : int }   (* 0-based vertex indices *)

type mesh =
  { positions : Glm.Vec3.t vector,
    normals   : Glm.Vec3.t vector,    (* empty if absent *)
    texcoords : Glm.Vec2.t vector,    (* empty if absent *)
    tris      : tri vector }
```

## API

```sml
exception Mesh of string
structure Glm : GLM

val parseObj : string -> mesh

type material =
  { name : string, ambient : Glm.Vec3.t, diffuse : Glm.Vec3.t, specular : Glm.Vec3.t }
val parseMtl : string -> material list

val parsePly : Word8Vector.vector -> mesh   (* ascii or binary little-endian *)

val positionBuffer : mesh -> real vector    (* [x,y,z, x,y,z, ...] *)
val indexBuffer    : mesh -> int vector     (* [a,b,c, a,b,c, ...] *)
val vertexCount : mesh -> int
val triCount    : mesh -> int
```

Faces are fan-triangulated and indices normalized to 0-based on parse. Malformed
input (out-of-range index, face with fewer than 3 vertices, non-numeric data,
missing PLY header) raises `Mesh msg`.

## Example

```sml
val m   = Mesh.parseObj objText
val pos = Mesh.positionBuffer m   (* upload as a GL_ARRAY_BUFFER *)
val idx = Mesh.indexBuffer m      (* upload as a GL_ELEMENT_ARRAY_BUFFER *)
```

## Building & testing

```sh
make test        # build + run under MLton
make test-poly   # run under Poly/ML
make all-tests   # both compilers
make fixtures    # regenerate test/fixtures.sml (needs python3)
```

### Test fixtures

A small unit quad is provided as OBJ/MTL/ASCII-PLY (hand-authored, human
readable) plus a **binary little-endian PLY** packed with Python's `struct`
(`bin/gen_fixtures.py`) so the binary float32/index reader is validated against
an independent encoder. Tests assert vertex/triangle counts and coordinates,
that ASCII and binary PLY yield identical meshes, that the flattened
position/index buffers reconstruct the triangles, and that edge cases (negative
indices, quad triangulation, CRLF, missing normals, out-of-range indices, empty
meshes, large coordinates) behave correctly.

## License

See [LICENSE](LICENSE).
