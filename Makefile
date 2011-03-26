all:
	ghc -O2 --make qs

clean:
	$(RM) gen/*
