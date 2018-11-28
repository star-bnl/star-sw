*
* $Id: csrec.inc,v 1.1 2018/11/19 20:43:51 perev Exp $
*
* $Log: csrec.inc,v $
* Revision 1.1  2018/11/19 20:43:51  perev
* 64bits add files
*
* Revision 1.1.1.1  1996/02/26 17:16:34  mclareni
* Comis
*
*
#ifndef CERNLIB_COMIS_CSREC_INC
#define CERNLIB_COMIS_CSREC_INC
*
*
* csrec.inc
*
      CHARACTER*80 REC,REC1,MAINPR*8,PROMPT(5)*4
      COMMON/CSCBFR/REC,REC1,MAINPR,PROMPT

#endif
