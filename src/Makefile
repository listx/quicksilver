all:
	ghc -O2 -Wall -Werror --make qs

prof:
	ghc -O2 -rtsopts -prof -auto-all --make qs -v

clean:
	find -type f -iregex ".+\.\(o\|hi\|mix\|tix\|html\)" -exec rm -v {} \;
