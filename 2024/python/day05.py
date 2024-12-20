from collections import defaultdict
from pprint import pprint
from textwrap import dedent

lines = """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13""".split("\n")

rules = []
for line in lines:
    rules.append(list(map(int, line.split("|"))))
pprint(rules)

graph: dict[str, set[int]] = defaultdict(set)

for x, y in rules:
    graph[y].add(x)

# graph = dict(graph)
pprint(graph)

update_lines = dedent(
    """75,47,61,53,29
    97,61,53,29,13
    75,29,13
    75,97,47,61,53
    61,13,29
    97,13,75,29,47"""
).split("\n")
updates = []
for ul in update_lines:
    updates.append(list(map(int, ul.split(","))))
print(updates)

checks = []
for pages in updates:
    is_valid = True
    for i, x in enumerate(pages):
        for j, y in enumerate(pages):
            if i > j and x in graph[y]:
                is_valid = False
                print(f"<{(i, x) = }> <{(j, y) = }> <{x in graph[y] = }>")
    checks.append(is_valid)

print(checks)
