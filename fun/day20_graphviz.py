import subprocess

with open("../input/day20.input") as f:
    data = f.read().strip()

lines = data.split("\n")

modules = {}
for line in lines:
    [name, rest] = line.split(" -> ", maxsplit=1)
    children = rest.split(", ")
    modules[name] = children


def add_prefix(modules, child):
    for name in modules:
        if name.endswith(child):
            return name[0]

    return None


modules2 = {}
for name, children in modules.items():
    modules2[name] = []
    for child in children:
        if child == "rx":
            prefix = ""
        else:
            prefix = add_prefix(modules, child)

        assert prefix is not None, "No prefix found for " + child
        modules2[name].append(prefix + child)

modules = modules2

graphviz = "digraph G {\n"
for name, children in modules.items():
    for child in children:
        graphviz += f'  "{name}" -> "{child}";\n'
graphviz += "}"

print(graphviz)

subprocess.run(["dot", "-Tpng", "-o", "day20.png"], input=graphviz.encode("utf-8"))
