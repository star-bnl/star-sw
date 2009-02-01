# $Id: Makefile,v 1.1.1.2 2009/02/01 17:10:15 fisyak Exp $
#
# Makefile for building Geant3 examples
# 
# The following environment variables have to be set:
# CERNLIB  - path to packlib CERNLIB library
# PYTHIA6  - path to PYTHIA6 library
# ROOTSYS  - path to Root

MAKEFLAGS= --no-print-directory

TARGETS  = gexam1 gexam3 gexam4 model

.PHONY: default

default:
	@for TARGET in $(TARGETS); do (export TARGET; $(MAKE) -f ../config/MakeExe); done;:

clean:
	@$(MAKE) -f ../config/MakeExe clean
