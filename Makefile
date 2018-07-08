CASK ?= cask


.PHONY: test

test:
	$(CASK) exec ecukes --no-win
