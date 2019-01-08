all: Chapter.hs
	ghc -O3 Chapter.hs && ./Chapter ten-hundred > 10h.js && ./Chapter wordlist.asc > wl.js
