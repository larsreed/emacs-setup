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
LOADPATH=../common/ ../speedbar/ ../eieio/\
   
LOADDEFS=ede-loaddefs.el
LOADDIRS=.
init_LISP=ede-load.el
EMACS=emacs
ede_LISP=project-am.el ede.el ede-pmake.el ede-pconf.el ede-proj.el ede-dired.el ede-proj-elisp.el ede-proj-misc.el ede-proj-obj.el ede-proj-prog.el ede-proj-archive.el ede-proj-shared.el ede-proj-info.el ede-proj-scheme.el ede-proj-aux.el ede-proj-comp.el ede-source.el ede-util.el ede-system.el ede-speedbar.el
aux-lisp_LISP=autoconf-edit.el autoconf-compat.el
misc_AUX=NEWS README TODO ede-proj-skel.el
ede_info_TEXINFOS=ede.texi ede-projects.texi ede-targets.texi ede-sourcecode.texi ede-compilers.texi
MAKEINFO=makeinfo
test_AUX=
VERSION=1.0beta3
DISTDIR=$(top)EDE-$(VERSION)



all: autoloads init ede aux-lisp ede.info

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

.PHONY: ede
ede: $(ede_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	$(EMACS) -batch --no-site-file -l $@-compile-script -f batch-byte-compile $^

.PHONY: aux-lisp
aux-lisp: $(aux-lisp_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	$(EMACS) -batch --no-site-file -l $@-compile-script -f batch-byte-compile $^

ede.info: $(ede_info_TEXINFOS)
	$(MAKEINFO) $<

tags: 


clean:
	rm -f *.elc *.html *.info*

.PHONY: dist

dist: autoloads ede.info
	mkdir $(DISTDIR)
	cp ede-loaddefs.el $(init_LISP) $(ede_LISP) $(aux-lisp_LISP) $(misc_AUX) $(ede_info_TEXINFOS) ede.info* $(test_AUX) $(ede_FILES) $(DISTDIR)

Makefile: Project.ede
	@echo Makefile is out of date!  It needs to be regenerated by EDE.
	@echo If you have not modified Project.ede, you canuse `touch' to update the Makefile time stamp.	@false



# End of Makefile
