ALLEXECS += root.bsc
root.bsc: $(shell ls *.sbr)
	BSCMAKE /o$@ *.sbr
