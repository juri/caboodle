PREFIX = /usr/local
DESKTOPLOCATION = $(PREFIX)/share/applications

INSTALL = install
RM = rm
MKDIR = mkdir
SED = sed
MAKE = make

.PHONY : install build clean

build:
	cd src && $(MAKE)	

install: build
	$(MKDIR) -p build
	$(MKDIR) -m 755 -p $(PREFIX)/bin
	$(MKDIR) -m 755 -p $(PREFIX)/share/caboodle
	$(MKDIR) -m 755 -p $(DESKTOPLOCATION)
	$(SED) "s|PREFIX|$(PREFIX)|" caboodle.desktop > build/caboodle.desktop
	$(INSTALL) -m 644 share/caboodle/caboodle.glade share/caboodle/caboodle.svg $(PREFIX)/share/caboodle
	$(INSTALL) -m 755 src/caboodle $(PREFIX)/bin
	$(INSTALL) -m 644 build/caboodle.desktop $(DESKTOPLOCATION)
	update-desktop-database $(DESKTOPLOCATION)
	
clean:
	$(RM) -rf build
	$(RM) -f src/*.cmi src/*.cmx src/*.cmo src/*.o src/caboodle
