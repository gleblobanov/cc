all:
	bnfc JL.cf
	happy -gca ParJL.y
	alex -g LexJL.x
	ghc --make jlc.hs -o ../jlc

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.bak
	-rm -f DocJL.ps

distclean: clean
	-rm -f DocJL.* LexJL.* ParJL.* LayoutJL.* SkelJL.* PrintJL.* TestJL.* AbsJL.* TestJL ErrM.* SharedString.* JL.dtd XMLJL.*

