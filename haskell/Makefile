aoc=.stack-work/dist/x86_64-linux-nopie/Cabal-1.24.2.0/build/AOC/AOC
stack = stack

ghcflags = --ghc-options -O2
targs = --test-arguments=--match
rtsopts = +RTS -s

.PHONY: default clean

default:;

clean: 
	$(stack) clean

testall: 
	$(stack) test --fast

$(aoc): app/Main.hs
	$(stack) build $(ghcflags)

day1test:
	$(stack) test $(targs)=Day1
day1p1: $(aoc)
	$(aoc) 1 1 input/day1.txt $(rtsopts)
day1p2: $(aoc)
	$(aoc) 1 2 input/day1.txt $(rtsopts)

day2test:
	$(stack) test $(targs)=Day2
day2p1: $(aoc)
	$(aoc) 2 1 input/day2.txt $(rtsopts)
day2p2: $(aoc)
	$(aoc) 2 2 input/day2.txt $(rtsopts)

day3test:
	$(stack) test $(targs)=Day3
day3p1: $(aoc)
	$(aoc) 3 1 input/day3.txt $(rtsopts)
day3p2: $(aoc)
	$(aoc) 3 2 input/day3.txt $(rtsopts)

day4test:
	$(stack) test $(targs)=Day4
day4p1: $(aoc)
	$(aoc) 4 1 input/day4.txt $(rtsopts)
day4p2: $(aoc)
	$(aoc) 4 2 input/day4.txt $(rtsopts)

day5test:
	$(stack) test --fast $(targs)=Day5
day5p1: $(aoc)
	$(aoc) 5 1 input/day5.txt $(rtsopts)
day5p2: $(aoc)
	$(aoc) 5 2 input/day5.txt $(rtsopts)
