all:
	ghc -O2 -Wall -Werror --make qs

prof:
	ghc -O2 -rtsopts -prof -auto-all --make qs -v

clean:
	rm -rf gen* *.o *.hi Data/*.o Data/*.hi Recruit/*.o Recruit/*.hi Regex/*.o Regex/*.hi
