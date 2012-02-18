PROG=TestPokerhands

all: run_tests
	./${PROG}

run_tests: *.hs
	ghc -fhpc -o ${PROG} --make TestPokerhands.hs

hpc: run_tests
	hpc report ${PROG} --exclude=Main
	hpc markup ${PROG} --exclude=Main

doc:
	touch doc
	rm -rf doc
	haddock -o doc -h *.hs
	hlint . --report

clean:
	rm ${PROG}
	rm *.o
	rm *.tix

