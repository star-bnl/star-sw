#
.PHONY: all clean
#
SHELL := csh
#
# CREATE SYMBOLIC LINKS NEEDED FOR HTML DOCUMENTATION
#
all:
	make_links.csh `cat known_asps.txt`
#
clean:
	find . -type l -print -exec rm -f {} \;
#
