all: clean compile run
readfile: clean compile read

compile:
	@flex gpp_lexer.l
	@yacc -d gpp_parser.y
	@gcc lex.yy.c y.tab.c -o gpp_interpreter

run: 
	@./gpp_interpreter

read:
	@./gpp_interpreter < input.g++

clean:
	@rm -f *.o
	clear

