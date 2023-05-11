lib_files = Main.hs

compile:
	ghc $(lib_files)

interp:
	ghci $(lib_files)

format:
	ormolu -i -c $(find . | grep .hs)

clean:
	rm *.o *.hi
