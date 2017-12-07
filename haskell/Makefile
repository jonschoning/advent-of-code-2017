aoc=.stack-work/dist/x86_64-linux-nopie/Cabal-1.24.2.0/build/AOC2017/AOC2017
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
day1test-doc:
	$(stack) exec -- doctest --fast src/Day1.hs
day1p1: $(aoc)
	$(aoc) 1 1 input/day1.txt $(rtsopts)
day1p2: $(aoc)
	$(aoc) 1 2 input/day1.txt $(rtsopts)

day2test:
	$(stack) test $(targs)=Day2
day2test-doc:
	$(stack) exec -- doctest --fast src/Day2.hs
day2p1: $(aoc)
	$(aoc) 2 1 input/day2.txt $(rtsopts)
day2p2: $(aoc)
	$(aoc) 2 2 input/day2.txt $(rtsopts)

day3test:
	$(stack) test $(targs)=Day3
day3test-doc:
	$(stack) exec -- doctest --fast src/Day3.hs
day3p1: $(aoc)
	$(aoc) 3 1 input/day3.txt $(rtsopts)
day3p2: $(aoc)
	$(aoc) 3 2 input/day3.txt $(rtsopts)

day4test:
	$(stack) test $(targs)=Day4
day4test-doc:
	$(stack) exec -- doctest --fast src/Day4.hs
day4p1: $(aoc)
	$(aoc) 4 1 input/day4.txt $(rtsopts)
day4p2: $(aoc)
	$(aoc) 4 2 input/day4.txt $(rtsopts)

day5test:
	$(stack) test --fast $(targs)=Day5
day5test-doc:
	$(stack) exec -- doctest --fast src/Day5.hs
day5p1: $(aoc)
	$(aoc) 5 1 input/day5.txt $(rtsopts)
day5p2: $(aoc)
	$(aoc) 5 2 input/day5.txt $(rtsopts)

day6test:
	$(stack) test --fast $(targs)=Day6
day6test-doc:
	$(stack) exec -- doctest --fast src/Day6.hs
day6p1: $(aoc)
	$(aoc) 6 1 input/day6.txt $(rtsopts)
day6p2: $(aoc)
	$(aoc) 6 2 input/day6.txt $(rtsopts)

day7test:
	$(stack) test --fast $(targs)=Day7
day7test-doc:
	$(stack) exec -- doctest --fast src/Day7.hs
day7p1: $(aoc)
	$(aoc) 7 1 input/day7.txt $(rtsopts)
day7p2: $(aoc)
	$(aoc) 7 2 input/day7.txt $(rtsopts)
