PROG=TestPokerhands

all: run_tests
	./${PROG}

run_tests: *.hs
	ghc -fhpc -o ${PROG} --make TestPokerhands.hs

hpc: run_tests
	hpc report ${PROG} --exclude=Main
	hpc markup ${PROG} --exclude=Main

doc:
	haddock -o doc -h *.hs
	hlint . --report

clean:
	rm -rf doc ${PROG} *.o *.tix *.hi .hpc *.html

