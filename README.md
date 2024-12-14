Advent of Code 2024 in OCaml

Building:
Run `eval $(opam config env)`

Rough edges I noticed:
- I add a library to dune. But I still need to download it manually via opam.
- Owl needs to be pinned to source: 
```
opam pin add owl https://github.com/owlbarn/owl.git
```