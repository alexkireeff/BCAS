lib_files = HelloWorld.hs

compile:
	ghc $(lib_files)

interpret:
	ghci $(lib_files)

clean:
	rm *.o *.hi
