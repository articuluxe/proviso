.POSIX:
EMACS=$(EMACS_BIN) -nw
# override: make EMACS=emacs-27.1 check
# override from environment: export EMACS=emacs-27.1 ; make -e
PACKAGE=proviso
ROOT=$(HOME)/.emacs.d
VERSION=0.1.1
DEPS=-L . -L $(ROOT)/ext/xfer -L $(ROOT)/ext/parsenv -L $(ROOT)/plugins -L $(ROOT)/elisp -L $(ROOT)/plugins/bookmark+ -L $(ROOT)/plugins/auto-complete -L $(ROOT)/plugins/swiper -L $(ROOT)/plugins/realgud
EL := $(wildcard *.el)
ELC := $(patsubst %.el,%.elc,$(wildcard *.el))
DOC := README.md
rwildcard=$(wildcard $1$2)$(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))
TESTS := $(call rwildcard,test/,test_*.el)
ifdef COMSPEC
	BLANK = echo.
else
	BLANK = echo -e
endif

# $(info $(SHELL))

check: $(TESTS)

$(TESTS):
	@$(BLANK)
	@$(BLANK)
	@echo Running -*- $@ -*-
	@$(BLANK)
	$(EMACS) -Q $(DEPS) -batch -l $@ -f ert-run-tests-batch-and-exit

%.elc: %.el
	$(EMACS) -Q -batch $(DEPS) -f batch-byte-compile $<

compile: $(ELC)

clean:
	rm -f $(ELC)

package: $(PACKAGE)-$(VERSION).tar
$(PACKAGE)-$(VERSION).tar: $(EL) $(DOC)
	rm -rf $(PACKAGE)-$(VERSION)/
	mkdir $(PACKAGE)-$(VERSION)/
	cp $(EL) $(DOC) $(PACKAGE)-$(VERSION)/
	tar cf $@ $(PACKAGE)-$(VERSION)/
	rm -rf $(PACKAGE)-$(VERSION)/

.PHONY: compile clean test $(TESTS) check
