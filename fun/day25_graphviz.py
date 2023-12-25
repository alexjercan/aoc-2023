from collections import defaultdict
import subprocess

with open("../input/day25.input") as f:
    data = f.read().strip()

lines = data.split("\n")

graph = defaultdict(list)
for line in lines:
    [name, rest] = line.split(": ", maxsplit=1)
    children = rest.split(" ")
    graph[name] = children

graphviz = "graph G {\n"
for name, children in graph.items():
    for child in children:
        graphviz += f'  "{name}" -- "{child}";\n'
graphviz += "}"

print(graphviz)

subprocess.run(["dot", "-Tpng", "-o", "day25.png"], input=graphviz.encode("utf-8"))
