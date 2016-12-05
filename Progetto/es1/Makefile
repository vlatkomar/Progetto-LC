all:
	happy -gca Partree.y
	alex -g Lextree.x
	latex Doctree.tex; dvips Doctree.dvi -o Doctree.ps
	ghc --make Testtree.hs -o Testtree
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Doctree.ps
distclean: clean
	-rm -f Doctree.* Lextree.* Partree.* Layouttree.* Skeltree.* Printtree.* Testtree.* Abstree.* Testtree ErrM.* SharedString.* tree.dtd XMLtree.* Makefile*

