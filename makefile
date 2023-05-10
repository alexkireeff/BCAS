lib_files = Main.hs

compile:
	ghc $(lib_files)

interp:
	ghci $(lib_files)

clean:
	rm *.o *.hi
