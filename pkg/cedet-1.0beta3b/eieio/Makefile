# Automatically Generated Makefile by EDE.
# For use with: make
#
# DO NOT MODIFY THIS FILE OR YOUR CHANGES MAY BE LOST.
# EDE is the Emacs Development Environment.
# http://cedet.sourceforge.net/ede.shtml
# 

top=
ede_FILES=Project.ede Makefile

EMACS=emacs
LOADPATH=../common/ ../speedbar/ 
LOADDEFS=eieio-loaddefs.el
LOADDIRS=.
init_LISP=eieio-load.el
EMACS=emacs
eieio_LISP=eieio.el eieio-comp.el eieio-opt.el eieio-custom.el eieio-doc.el eieio-base.el
examples_LISP=tree.el call-tree.el chart.el eieio-speedbar.el linemark.el lmcompile.el
eieio_info_TEXINFOS=eieio.texi
MAKEINFO=makeinfo
test_LISP=eieio-tests.el
Misc_MISC=INSTALL ChangeLog
VERSION=0.18beta2
DISTDIR=$(top)eieio-$(VERSION)



all: autoloads init eieio examples eieio.info

.PHONY: autoloads
autoloads: 
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(require 'cedet-autogen)" >> $@-compile-script
	$(EMACS) -batch --no-site-file -l $@-compile-script -f cedet-batch-update-autoloads $(LOADDEFS) $(LOADDIRS)

.PHONY: init
init: $(init_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	$(EMACS) -batch --no-site-file -l $@-compile-script -f batch-byte-compile $^

.PHONY: eieio
eieio: $(eieio_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	$(EMACS) -batch --no-site-file -l $@-compile-script -f batch-byte-compile $^

.PHONY: examples
examples: $(examples_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	$(EMACS) -batch --no-site-file -l $@-compile-script -f batch-byte-compile $^

eieio.info: $(eieio_info_TEXINFOS)
	$(MAKEINFO) $<

.PHONY: test
test: $(test_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	$(EMACS) -batch --no-site-file -l $@-compile-script -f batch-byte-compile $^

Misc: 
	@

tags: 


clean:
	rm -f *.elc *.html *.info*

.PHONY: dist

dist: autoloads eieio.info
	mkdir $(DISTDIR)
	cp eieio-loaddefs.el $(init_LISP) $(eieio_LISP) $(examples_LISP) $(eieio_info_TEXINFOS) eieio.info* $(test_LISP) $(Misc_MISC) $(ede_FILES) $(DISTDIR)

Makefile: Project.ede
	@echo Makefile is out of date!  It needs to be regenerated by EDE.
	@echo If you have not modified Project.ede, you canuse `touch' to update the Makefile time stamp.	@false



# End of Makefile
