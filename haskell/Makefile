.PHONY: default clean
aoc=.stack-work/dist/x86_64-linux-nopie/Cabal-1.24.2.0/build/AOC/AOC

default:;

clean: 
	stack clean

test: 
	stack test --fast

$(aoc): app/Main.hs
	stack build

day1p1: $(aoc)
	$(aoc) 1 1 input/day1.txt
day1p2: $(aoc)
	$(aoc) 1 2 input/day1.txt
