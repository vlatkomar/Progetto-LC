all:
	happy -gca ParRuby.y
	alex -g LexRuby.x
	ghc --make Test.hs -o Test

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocRuby.* LexRuby.* ParRuby.* LayoutRuby.* SkelRuby.* PrettyPrinter.* TestRuby.* AbsRuby.* TestRuby ErrM.* SharedString.* ComposOp.* Ruby.dtd XMLRuby.* Makefile*

demo:
	./Test < Prova.txt
	

