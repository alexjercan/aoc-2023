def parse_line(line: str):
    pos_str, vel_str = line.strip().split(" @ ", 1)
    pos = tuple(map(int, pos_str.split(",")))
    vel = tuple(map(int, vel_str.split(",")))
    return pos, vel

def parse(input_str: str):
    return list(map(parse_line, input_str.splitlines()))

def intersect(cell1, cell2):
    ((x1, y1, _), (dx1, dy1, _)) = cell1
    ((x2, y2, _), (dx2, dy2, _)) = cell2

    xdiff = (-dx1, -dx2)
    ydiff = (-dy1, -dy2)

    def det(a, b):
        return a[0] * b[1] - a[1] * b[0]

    div = det(xdiff, ydiff)
    if div == 0:
        return False

    d = (det((x1, y1), (x1 + dx1, y1 + dy1)), det((x2, y2), (x2 + dx2, y2 + dy2)))
    x = det(d, xdiff) / div
    y = det(d, ydiff) / div

    if 200000000000000 <= x <= 400000000000000 and 200000000000000 <= y <= 400000000000000 \
            and (x - x1 > 0) == (dx1 > 0) and (y - y1 > 0) == (dy1 > 0) \
            and (x - x2 > 0) == (dx2 > 0) and (y - y2 > 0) == (dy2 > 0):
        return True

    return False


def part1(xs):
    n = len(xs)

    count = 0
    for i in range(n):
        for j in range(i + 1, n):
            cell1 = xs[i]
            cell2 = xs[j]

            if intersect(cell1, cell2):
                count += 1

    return count


if __name__ == "__main__":
    input_str = open("../input/day24.input").read()

    # Part 1
    xs = parse(input_str)

    print(part1(xs))
