all:
	ghc -O2 -Wall -Werror --make qs

prof:
	ghc -O2 -rtsopts -prof -auto-all --make qs -v

clean:
	rm -rf gen *.o *.hi

refresh:
	rm -rf gen
