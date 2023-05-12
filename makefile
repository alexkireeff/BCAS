translator_config = -isrc -O3 -threaded -rtsopts -o Main
run_config = +RTS -N10
lib_files = src/Main.hs
format_files = $$(find . | grep .hs)

compile:
	ghc $(translator_config) $(lib_files)

run: compile clean
	./Main $(run_config)

interp:
	ghci $(translator_config) $(lib_files)

format:
	ormolu --no-cabal -i -c $(format_files)

clean:
	rm **/*.o **/*.hi
