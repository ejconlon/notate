
install:
	rm -rf .notate
	stack build && stack exec notate -- install stack.yaml .notate

notebook:
	stack build && stack exec notate -- notebook stack.yaml .notate

eval:
	stack build && stack exec notate -- eval stack.yaml .notate