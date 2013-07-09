# -*- Makefile -*-

EMACS = emacs

TEST_DIR = src
TRAVIS_FILE = .travis.yml
BATCHFLAGS = -batch -q --no-site-file

test:
	${EMACS} -L src $(BATCHFLAGS) -f batch-byte-compile $(TEST_DIR)/*.el
