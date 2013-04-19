FILE=hw

default: compile

compile:
	ghc --make $(FILE)

clean:
	rm *.o *.hi $(FILE)

run:
	./$(FILE)

runh:
	runhaskell $(FILE).hs

caps:
	cat resources/quotes | runhaskell capslock

short:
	cat resources/quotes | runhaskell shortline

pal:
	cat resources/palindromes | runhaskell palindrome

open:
	runhaskell openfile
