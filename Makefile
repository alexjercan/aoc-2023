all:
	stack build

Day%:
	stack run day$* < input/day$*.input

%:
	stack build
