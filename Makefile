all:
	ghc -O2 --make qs

mod:
	$(RM) gen/*
	./qs

prof:
	ghc -O2 -rtsopts -prof -auto-all --make qs -v

clean:
	$(RM) gen/* *.o *.hi
