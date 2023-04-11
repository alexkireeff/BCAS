lib_files = CAS.hs

compile:
	ghc $(lib_files)

interp:
	ghci $(lib_files)

clean:
	rm *.o *.hi
