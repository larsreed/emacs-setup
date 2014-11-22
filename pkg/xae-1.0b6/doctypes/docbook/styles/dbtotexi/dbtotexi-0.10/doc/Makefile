
all: dbtotexi.texinfo dbtotexi.info dbtotexi.html dbtotexi.dvi dbtotexi.ps

dbtotexi.texinfo: dbtotexi.xml version.xml

%.info : %.texinfo
	makeinfo $<

%.html : %.texinfo
	makeinfo --html --no-split $<

%.dvi : %.texinfo
	texi2dvi $<

%.texinfo : %.xml
	DBTOTEXIHOME=.. ../dbtotexi $< $@

%.ps : %.dvi
	dvips -o $@ $<
