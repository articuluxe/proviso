EMACS=$(VISUAL) -nw
ROOT=$(HOME)/.emacs.d
DEPS=-L . -L $(ROOT)/ext/xfer -L $(ROOT)/ext/parsenv -L $(ROOT)/plugins -L $(ROOT)/elisp -L $(ROOT)/plugins/bookmark+ -L $(ROOT)/plugins/auto-complete -L $(ROOT)/plugins/swiper -L $(ROOT)/plugins/realgud
ELC := $(patsubst %.el,%.elc,$(wildcard *.el))
rwildcard=$(wildcard $1$2)$(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))
TESTS := $(call rwildcard,test/,test_*.el)

check: $(TESTS)

$(TESTS):
	@echo "\n\nRunning -*- $@ -*-\n"
	$(EMACS) $(DEPS) -batch -l $@ -f ert-run-tests-batch-and-exit

%.elc: %.el
	$(EMACS) -Q -batch $(DEPS) -f batch-byte-compile $<

compile: $(ELC)

clean:
	rm -f $(ELC)

.PHONY: compile clean test $(TESTS) check
