from __future__ import annotations
from dataclasses import dataclass, field
from functools import reduce
import operator
import re
from typing import Self

MAX_RED = 12
MAX_GREEN = 13
MAX_BLUE = 14


def main() -> None:
    file = "../data/day02.txt"

    total_id = 0
    total_power = 0
    with open(file, "r") as fp:
        for line in fp:
            line = line.strip()
            game = Game.from_text(line)

            total_power += game.power
            if game.is_possible():
                total_id += game.id

            print("=" * 80)
            print(f"{line = }")
            print(f"{game.id = }")
            print(f"{game.is_possible() = }")
            for cs in game.cube_sets:
                print(f"    {cs = }, {cs.is_possible() = }")
            print(f"{game.minimum_cube_set = }")
            print(f"{game.power = }")
            print("=" * 80)
            print()

    print(f"{total_id = }")
    print(f"{total_power = }")


@dataclass
class CubeSet:
    red: int
    green: int
    blue: int

    @classmethod
    def from_text(cls, text: str) -> Self:
        # 3 green, 15 blue, 14 red
        [red_count] = re.compile(r"([+-]*\d+) red").findall(text) or ["0"]
        [green_count] = re.compile(r"([+-]*\d+) green").findall(text) or ["0"]
        [blue_count] = re.compile(r"([+-]*\d+) blue").findall(text) or ["0"]
        return cls(red=int(red_count), green=int(green_count), blue=int(blue_count))

    def is_possible(self) -> bool:
        is_red_possible = 0 <= self.red <= MAX_RED
        is_green_possible = 0 <= self.green <= MAX_GREEN
        is_blue_possible = 0 <= self.blue <= MAX_BLUE
        return is_red_possible and is_green_possible and is_blue_possible


@dataclass
class Game:
    id: int
    cube_sets: list[CubeSet]
    minimum_cube_set: tuple[int, int, int] = field(init=False)
    power: int = field(init=False)

    def __post_init__(self) -> None:
        self.minimum_cube_set = self._minimum_cube_set()
        self.power = reduce(operator.mul, self.minimum_cube_set, 1)

    @classmethod
    def from_text(cls, text: str) -> Self:
        # Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
        text = text.lower()
        game_id_text, game_text = text.split(":")
        cube_sets_text = game_text.split(";")

        [game_id] = re.compile(r"game (\d+)").findall(game_id_text)
        cube_sets = [CubeSet.from_text(t) for t in cube_sets_text]

        return cls(id=int(game_id), cube_sets=cube_sets)

    def is_possible(self) -> bool:
        return all(cs.is_possible() for cs in self.cube_sets)

    def _minimum_cube_set(self) -> tuple[int, int, int]:
        red, green, blue = 0, 0, 0
        for cs in self.cube_sets:
            red = max(red, cs.red)
            green = max(green, cs.green)
            blue = max(blue, cs.blue)
        return red, green, blue


if __name__ == "__main__":
    main()
