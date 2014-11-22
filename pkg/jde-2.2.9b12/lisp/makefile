# $Revision: 1.13 $ $Date: 2002/08/07 06:36:14 $ 
#
# This makefile compiles the JDE lisp files.
#
# Adapted by Paul Kinnucan from a Windows/NT batch script 
# created by Steve Rowley <sgr@alum.mit.edu>
#

EMACS = emacs

# Default locations of the packages required to compile the JDE.
EIEIO = ../../eieio
ELIB  = ../../elib
SEMANTIC = ../../semantic
SPEEDBAR = ../../speedbar

all:
	test -d $(SEMANTIC) -a -d $(SPEEDBAR) -a -d $(ELIB) -a -d $(EIEIO)
	rm -f *.elc jde-compile-script-init
	echo "(add-to-list 'load-path \".\")" > jde-compile-script-init
	echo "(add-to-list 'load-path \"$(SEMANTIC)\")" >> jde-compile-script-init
	echo "(add-to-list 'load-path \"$(SPEEDBAR)\")"  >> jde-compile-script-init
	echo "(add-to-list 'load-path \"$(ELIB)\")" >>jde-compile-script-init
	echo "(add-to-list 'load-path \"$(EIEIO)\")" >>jde-compile-script-init
	echo "(setq debug-on-error t)" >> jde-compile-script-init
	echo "(require 'jde-compat)" >> jde-compile-script-init
	echo "(require 'jde)" >>jde-compile-script-init
	$(EMACS) -batch -l jde-compile-script-init -f batch-byte-compile *.el
	rm -f jde-compile-script-init

# 
# $Log: makefile,v $
# Revision 1.13  2002/08/07 06:36:14  paulk
# Removed code intended to eliminate spurious warnings when byte-compiling the JDEE. The
# removed code now resides in a separate package, jde-compat.el. Thanks to Andy Piper
# for suggesting this restructuring. Also fixed a number of compiler warnings caused
# by genuine bugs.
#
# Revision 1.12  2001/11/14 06:26:13  paulk
# Now requires jde to make sure the JDE is loaded before compiling.
#
# Revision 1.11  2001/11/14 05:29:26  paulk
# Remove yet another mysterious added #.
#
# Revision 1.10  2001/11/14 05:27:41  paulk
# Remove comment characters.
#
# Revision 1.9  2001/11/14 05:24:31  paulk
# Use variables to specify location of required packages. Check for existence of required packages.
# 
# 

# End of makefile