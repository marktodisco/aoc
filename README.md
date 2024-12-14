### Setup Steps

- Install opam
- Install dune
- Create opam switch
- Install packages
- Execute binaries



```shell
cd ocaml/<year>
dune init project advent .
opam switch create . 5.2.0
eval $(opam env)
opam install utop ocamlformat ocaml-lsp-server core
```

### How to Run Code

```shell
cd ocaml/<year>
eval $(opam env)
dune exec dayXX -w  # replace XX with the day number to run
```