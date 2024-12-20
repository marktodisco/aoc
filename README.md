# Advent of Code

Code is organized by year and language, where languages from the same year share a common data folder.

## OCaml Setup

- Install opam
- Install dune
- Create opam switch
- Install packages
- Execute binaries

```shell
cd <year>/ocaml
dune init project advent .

opam switch create . 5.2.0
eval $(opam env)
opam install dune utop ocamlformat ocaml-lsp-server merlin user-setup core
dune exec day<XX> -w
```

## Python Setup

Since Python has such a massive standard library, don't install any 3rd-party dependencies.

```shell
cd <year>/python
uv init --package --name advent
uv venv
source .venv/bin/activate
# only use the python standard library or custom code
python main.py <XX>
# alternatively
uv add watchdog
watchmedo shell-command . --recursive --command='rebuilding... && python main.py <XX>' --wait --drop
```