*
* $Id: agchead.inc,v 1.1 1998/04/10 15:30:44 fisyak Exp $
*
* $Log: agchead.inc,v $
* Revision 1.1  1998/04/10 15:30:44  fisyak
* Move gstar into CVS
*
*
*
* agchead.inc
*
*CMZ :  1.30/00 24/03/97  21.25.13  by  Pavel Nevski
*-- Author :    Pavel Nevski   24/03/97
*
*** contains defintions for run header user buffer
*
      COMMON /AgCHEAD/ author,machine,date,time,
     >       geant_version,ge_run,ge_rndm_run(2)
*
      character*80 author,machine
      integer      date,time
      real         geant_version
      integer      ge_run,ge_rndm_run(2)
*
