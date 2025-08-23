test:
	ros run --eval "(progn (asdf:load-asd (truename \"sexpline.asd\")) (asdf:test-system :sexpline))" --quit

.PHONY: test
