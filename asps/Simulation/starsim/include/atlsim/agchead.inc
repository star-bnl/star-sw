*
* $Id: agchead.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
*
* $Name:  $
*
*
* agchead.inc
*
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
