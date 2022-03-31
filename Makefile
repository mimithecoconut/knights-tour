# Makefile for CS 4 final exam code, Winter 2022.

OCAML = ocamlfind ocamlc

default: main

storage:
	${OCAML} -c storage.mli
	${OCAML} -c storage.ml

board: storage
	${OCAML} -c board.mli
	${OCAML} -c board.ml

utils: storage board
	${OCAML} -c utils.mli
	${OCAML} -c utils.ml

interact: storage board utils
	${OCAML} -c interact.ml
	${OCAML} -o interact storage.cmo board.cmo utils.cmo interact.cmo

search: storage board utils
	${OCAML} -c search.mli
	${OCAML} -c search.ml

main: storage board utils search
	${OCAML} -c main.ml
	${OCAML} -o knights_tour storage.cmo board.cmo utils.cmo search.cmo main.cmo

run_test_storage: storage
	${OCAML} -c -package ounit2 test_storage.ml
	${OCAML} -o test_storage \
	  -package ounit2 -linkpkg \
	  storage.cmo test_storage.cmo
	./test_storage

run_test_board: storage board
	${OCAML} -c -package ounit2 test_board.ml
	${OCAML} -o test_board \
	  -package ounit2 -linkpkg \
	  storage.cmo board.cmo test_board.cmo
	./test_board

run_test_search: storage board utils search
	${OCAML} -c -package ounit2 test_search.ml
	${OCAML} -o test_search \
	  -package ounit2 -linkpkg \
	  storage.cmo board.cmo utils.cmo search.cmo test_search.cmo
	./test_search

run_test_all: run_test_storage run_test_board run_test_search

clean:
	rm -f *.cmi *.cmo *.log *.cache
	rm -f interact knights_tour test_storage test_board test_search 

