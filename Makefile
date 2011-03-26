all:
	ghc -O2 --make qs

mod:
	$(RM) gen/*
	./qs

clean:
	$(RM) gen/*
