build: 
	cd src && ocamlopt -o ../gbcc str.cmxa CTypes.ml tokenTypes.ml utils.ml lexer.ml parser.ml generate.ml gbcc.ml

install:
	cp gbcc /usr/local/bin

uninstall:
	rm /usr/local/bin/gbcc

clean: 
	cd src && rm *.o && rm *.cmx && rm *.cmi
	rm gbcc
