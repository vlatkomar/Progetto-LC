all:
	happy -gca Pares1mod.y
	alex -g Lexes1mod.x
	latex Doces1mod.tex; dvips Doces1mod.dvi -o Doces1mod.ps
	ghc --make Testes1mod.hs -o Testes1mod
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Doces1mod.ps
distclean: clean
	-rm -f Doces1mod.* Lexes1mod.* Pares1mod.* Layoutes1mod.* Skeles1mod.* Printes1mod.* Testes1mod.* Abses1mod.* Testes1mod ErrM.* SharedString.* es1mod.dtd XMLes1mod.* Makefile*

