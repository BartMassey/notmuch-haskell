jmh:
	ghc --make -o jmh Main.hs

clean:
	rm -f jmh *.hi *.o

.PHONY: jmh clean
