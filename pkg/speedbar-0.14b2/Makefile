# Automatically Generated Makefile by EDE.
# For use with: make
#
# DO NOT MODIFY THIS FILE OR YOUR CHANGES MAY BE LOST.
# EDE is the Emacs Development Environment.
# http://cedet.sourceforge.net/ede.shtml
# 

top=
ede_FILES=Project.ede Makefile

speedbar_LISP=dframe.el speedbar.el
EMACS=emacs
LOADPATH=
info_TEXINFOS=speedbar.texi
MAKEINFO=makeinfo
opt_LISP=rpm.el sb-gud.el sb-info.el sb-rmail.el sb-w3.el sb-texinfo.el bigclock.el sb-html.el sb-ant.el
aux_AUX=INSTALL
images_MISC=sb-dir-plus.xpm sb-dir-minus.xpm sb-dir.xpm sb-pg-plus.xpm sb-pg-minus.xpm sb-pg.xpm sb-mail.xpm sb-tag-plus.xpm sb-tag-minus.xpm sb-tag.xpm sb-tag-gt.xpm sb-tag-v.xpm sb-tag-type.xpm sb-image.el sb-obj.xpm sb-chk.xpm sb-objod.xpm
VERSION=0.14beta2
DISTDIR=$(top)speedbar-$(VERSION)



all: speedbar speedbar.info

speedbar: $(speedbar_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	@if test ! -z "${LOADPATH}" ; then\
	   for loadpath in ${LOADPATH}; do \
	      echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	    done;\
	fi
	@echo "(setq debug-on-error t)" >> $@-compile-script
	$(EMACS) -batch -l $@-compile-script -f batch-byte-compile $^

speedbar.info: $(info_TEXINFOS)
	$(MAKEINFO) -o $@ $<

opt: $(opt_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	@if test ! -z "${LOADPATH}" ; then\
	   for loadpath in ${LOADPATH}; do \
	      echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	    done;\
	fi
	@echo "(setq debug-on-error t)" >> $@-compile-script
	$(EMACS) -batch -l $@-compile-script -f batch-byte-compile $^

images: 
	@

tags: 

clean:
	rm -f *.elc *.html *.info


dist: speedbar.info
	rm -rf $(DISTDIR)
	mkdir $(DISTDIR)
	cp $(speedbar_LISP) $(info_TEXINFOS) speedbar.info* $(opt_LISP) $(aux_AUX) $(images_MISC) $(ede_FILES) $(DISTDIR)
	tar -cvzf $(DISTDIR).tar.gz $(DISTDIR)
	rm -rf $(DISTDIR)

Makefile: Project.ede
	@echo Makefile is out of date!  It needs to be regenerated by EDE.
	@false



# End of Makefile
