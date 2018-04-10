ocplib-cmi
==========

A library `ocplib-cmi` with mostly one useful module:
* `CmiCompress`: `CmiCompress.signature sg` will compress a signature by
  increasing the sharing within it. It decreases the size of `.cmi` files

A program `ocp-cmi-compress` to compress specific `.cmi` files or
`.cmi` files included in directories. A backup is done and can be
restored.

Dependencies
------------

The only dependencies are the OCaml distribution and `ocp-build`.

Use `make` to build. It makes the library in `_obuild/ocplib-cmi/` and
the program in `_obuild/ocp-cmi-compress/`.



