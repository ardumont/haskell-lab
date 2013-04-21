FILE=hw

default: compile

compile:
	time ghc --make $(FILE)

clean:
	rm *.o *.hi $(FILE)

run: compile
	./$(FILE)

runh:
	runhaskell $(FILE).hs

caps:
	cat resources/quotes | runhaskell capslock

short:
	cat resources/quotes | runhaskell shortline

pal:
	cat resources/palindromes | runhaskell palindrome

file:
	runhaskell file
