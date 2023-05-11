lib_files = Main.hs
format_files = $$(find . | grep .hs)

compile:
	ghc $(lib_files)

interp:
	ghci $(lib_files)

format:
	ormolu --no-cabal -i -c $(format_files)

clean:
	rm *.o *.hi
