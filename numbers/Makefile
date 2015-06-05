HSFLAGS=-w

all: isPrime primeGen notBombe mersenneGen

notBombe:
	ghc notBombe	$(HSFLAGS)

isPrime:
	ghc isPrime	$(HSFLAGS)

primeGen:
	ghc primeGen	$(HSFLAGS)

mersenneGen:
	ghc mersenneGen $(HSFLAGS)

clean:
	-rm primeGen isPrime notBombe *.{o,hi}

