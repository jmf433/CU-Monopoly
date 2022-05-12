.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/game_board_test.exe
	OCAMLRUNPARAM=b dune exec test/player_test.exe
	OCAMLRUNPARAM=b dune exec test/draw_test.exe


play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	zip -r monopoly.zip . -x _build/\* .git/\*