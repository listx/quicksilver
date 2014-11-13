OBJ = dist/build/qs/qs-tmp/Main.o
all: $(OBJ)
	cabal build --ghc-options "-O2 -Wall" qs
	cp dist/build/qs/qs .

srclist:
	$(shell find -type f -regex ".*\.hs" > srclist)

clean:
	$(RM) qs
