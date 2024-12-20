def main() -> None:
    file = "../data/day01.txt"

    with open(file, "r") as fp:
        lines = fp.read().strip().split("\n")

    def find_substrings(line: str) -> list[tuple[str, int]]:
        ret = []
        for i in range(0, len(line)):
            sub = line[i:]
            if sub.startswith("1") or sub.startswith("one"):
                ret.append(("1", i))
                continue
            elif sub.startswith("2") or sub.startswith("two"):
                ret.append(("2", i))
                continue
            elif sub.startswith("3") or sub.startswith("three"):
                ret.append(("3", i))
                continue
            elif sub.startswith("4") or sub.startswith("four"):
                ret.append(("4", i))
                continue
            elif sub.startswith("5") or sub.startswith("five"):
                ret.append(("5", i))
                continue
            elif sub.startswith("6") or sub.startswith("six"):
                ret.append(("6", i))
                continue
            elif sub.startswith("7") or sub.startswith("seven"):
                ret.append(("7", i))
                continue
            elif sub.startswith("8") or sub.startswith("eight"):
                ret.append(("8", i))
                continue
            elif sub.startswith("9") or sub.startswith("nine"):
                ret.append(("9", i))
                continue
            else:
                continue

        return ret

    def process(line: str) -> int:
        substr_matches: list[tuple[str, int]] = [
            m
            for m in sorted(find_substrings(line), key=lambda digit_idx: digit_idx[1])
            if m[1] != -1
        ]
        line_calib = substr_matches[0][0] + substr_matches[-1][0]

        print("=" * 20)
        print(f"{line = }")
        print(f"{substr_matches = }")
        print(f"{line_calib = }")
        print("=" * 20)
        print()

        return int(line_calib)

    calibration = sum(map(process, lines))
    print(f"{calibration = }")


if __name__ == "__main__":
    main()
