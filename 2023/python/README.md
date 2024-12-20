## Python Setup

Since Python has such a massive standard library, don't install any 3rd-party dependencies.

```shell
cd 2023/python
uv init --package --name advent
uv venv
source .venv/bin/activate
# only use the python standard library or custom code
python main.py <XX>
# alternatively
uv add watchdog
watchmedo shell-command . --recursive --command='rebuilding... && python main.py <XX>' --wait --drop
```