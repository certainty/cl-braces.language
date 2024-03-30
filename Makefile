.PHONY: test repl build deps

test:
	ros run -e '(progn (ql:quickload :cl-braces.language) (asdf:test-system :cl-braces.language))' -q

repl:
	ros run -e '(ql:quickload :cl-braces.language)'

build:
	ros run -e '(asdf:make :cl-braces.language)'

deps:
	ros run -e '(ql:quickload :cl-braces.language)' -q
