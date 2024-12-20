import argparse as ap

import advent


def main() -> None:
    parser = ap.ArgumentParser(formatter_class=ap.ArgumentDefaultsHelpFormatter)
    parser.add_argument("day", type=int, help="Advent of Code day number to run.")

    match parser.parse_args().day:
        case 1:
            advent.day01.main()
        case 2:
            advent.day02.main()


if __name__ == "__main__":
    main()
