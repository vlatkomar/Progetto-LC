all:
	happy -gca ParTree2.y
	alex -g LexTree2.x
	latex DocTree2.tex; dvips DocTree2.dvi -o DocTree2.ps
	ghc --make TestTree2.hs -o TestTree2
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocTree2.ps
distclean: clean
	-rm -f DocTree2.* LexTree2.* ParTree2.* LayoutTree2.* SkelTree2.* PrintTree2.* TestTree2.* AbsTree2.* TestTree2 ErrM.* SharedString.* Tree2.dtd XMLTree2.* Makefile*

demo:          
	echo -n "4.2 2.2 3.2 (3.2 4.4 2.6 (4.8 3.1 2.123 (3.123 (3.52 (5.27 (1.83 8.35))))))" | ./TestTree2 
	echo -n "7 8 (23 56 (345 34)(723 35 (11 397 256)))" | ./TestTree2 
	echo -n "6 (8 15) (9 (23 87 45)) 7"| ./TestTree2 
	echo -n "9.87 7.24 (398 (45 (65 223.34)))" | ./TestTree2 
	echo -n "234 (3 (2 36) 76) (23 (854 934))" | ./TestTree2
