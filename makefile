compiler_config = -O3 -threaded -rtsopts
run_config = +RTS -N10
lib_files = Main.hs
format_files = $$(find . | grep .hs)

compile:
	ghc $(compiler_config) $(lib_files)

run: compile clean
	./Main $(run_config)

interp:
	ghci $(lib_files)

format:
	ormolu --no-cabal -i -c $(format_files)

clean:
	rm *.o *.hi
