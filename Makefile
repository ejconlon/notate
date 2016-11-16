
install:
	rm -rf .notate
	stack build && stack exec notate -- install .notate notate

notebook:
	stack build && stack exec notate -- notebook .notate notate
