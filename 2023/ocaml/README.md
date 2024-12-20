## OCaml Setup

- Install opam
- Install dune
- Create opam switch
- Install packages
- Execute binaries

```shell
cd 2023/ocaml
dune init project advent .

opam switch create . 5.2.0
eval $(opam env)
opam install dune utop ocamlformat ocaml-lsp-server merlin user-setup core
dune exec day<XX> -w
```
