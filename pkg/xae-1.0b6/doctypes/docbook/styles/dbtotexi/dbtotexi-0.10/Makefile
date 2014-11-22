
###########################################################################
# things you may wish to change

# shell used to execute dbtotexi script
DBTOTEXISHELL = /bin/bash

# installation prefix
prefix = /usr/local

# where to put dbtotexinfo
DBTOTEXIBIN = $(prefix)/bin

# where to put the support files
DBTOTEXIHOME = $(prefix)/share/dbtotexi

# where to put the info file
DBTOTEXIINFO = $(prefix)/info

# which Java VM to use
#JAVAVM = kaffe -addclasspath \$$appclasspath
#JAVAVM = java -classpath \$$appclasspath:/usr/local/java/lib/classes.zip
#JAVAVM = jre -cp \$$appclasspath
JAVAVM = java -cp \$$appclasspath

# which Java compiler to use (only needed if you change DBToTexiSupport.java)
JAVAC = jikes -classpath /usr/local/java/jre/lib/rt.jar
#JAVAC = jikes -classpath /usr/local/java/lib/classes.zip
#JAVAC = jikes -classpath /usr/local/kaffe/share/kaffe/Klasses.jar
#JAVAC = javac

# the name of the SGML to XML convertor
SX = sx

###########################################################################
# the rest

VERSION = 0.10

JARS = xp.jar xt.jar sax.jar

SHAREFILES = dbtotexi.xsl DBToTexiSupport.class $(JARS)

TARFILES = \
	$(SHAREFILES) \
	dbtotexi.sh \
	DBToTexiSupport.java \
	Makefile \
	README \
	NEWS \
	COPYING \
	ChangeLog \
	sax-copying.txt \
	xt-copying.txt \
	xp-copying.txt

DOCTARFILES = \
	doc/Makefile \
	doc/dbtotexi.xml \
	doc/dbtotexi.texinfo \
	doc/dbtotexi.info \
	doc/dbtotexi.dvi \
	doc/dbtotexi.html

all : DBToTexiSupport.class dbtotexi docversion docoutput

docversion :
	echo "<!ENTITY version \"$(VERSION)\">" > doc/version.xml.tmp
	if cmp -s doc/version.xml doc/version.xml.tmp ; \
	then rm doc/version.xml.tmp; \
        else mv doc/version.xml.tmp doc/version.xml; fi

dbtotexi : dbtotexi.sh Makefile
	cat dbtotexi.sh | \
	sed \
	-e "s!DEFAULTDBTOTEXIHOME!$(DBTOTEXIHOME)!g" \
        -e "s!JAVAVM!$(JAVAVM)!g" \
	-e "s!VERSION!$(VERSION)!g" \
        -e "s!DBTOTEXISHELL!$(DBTOTEXISHELL)!g" > $@
	chmod +x $@

install : $(SHAREFILES) dbtotexi
	-mkdir $(prefix)
	mkdir -p $(DBTOTEXIHOME)
	for f in $(SHAREFILES); do \
	cp $$f $(DBTOTEXIHOME); \
	done
	-mkdir $(DBTOTEXIBIN)
	cp dbtotexi $(DBTOTEXIBIN)

install-doc : docoutput
	cp doc/dbtotexi.info $(wildcard doc/dbtotexi.info-*) $(DBTOTEXIINFO)
	install-info --info-dir=$(DBTOTEXIINFO) $(DBTOTEXIINFO)/dbtotexi.info

clean :
	rm -f DBToTexiSupport.class dbtotexi

%.texinfo : %.xml dbtotexi.xsl DBToTexiSupport.class
	DBTOTEXIHOME=. ./dbtotexi $*.xml $@

%.xml : %.sgm
	$(SX) -xlower -xndata $*.sgm > $*.xml

%.class : %.java
	$(JAVAC) $*.java

docoutput :
	cd doc; make

dist : dbtotexi-$(VERSION).tar.gz

dbtotexi-$(VERSION).tar.gz : docoutput $(TARFILES)
	-mkdir tartmp
	rm -rf tartmp/dbtotexi-$(VERSION)
	mkdir tartmp/dbtotexi-$(VERSION)
	mkdir tartmp/dbtotexi-$(VERSION)/doc
	cp -p $(TARFILES) tartmp/dbtotexi-$(VERSION)
	cp -p $(DOCTARFILES) tartmp/dbtotexi-$(VERSION)/doc
	cd tartmp; tar -vczf ../$@ dbtotexi-$(VERSION)
