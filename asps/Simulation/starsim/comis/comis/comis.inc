*
* $Id: comis.inc,v 1.2 2020/06/04 23:05:02 perev Exp $
*
* $Log: comis.inc,v $
* Revision 1.2  2020/06/04 23:05:02  perev
* add csguard call
*
* Revision 1.1  2018/11/19 20:43:51  perev
* 64bits add files
*
* Revision 1.1.1.1  1996/02/26 17:16:34  mclareni
* Comis
*
*
#ifndef CERNLIB_COMIS_COMIS_INC
#define CERNLIB_COMIS_COMIS_INC
*
*
* comis.inc
*
      COMMON/COMIS/IA(LASTK)
      INTEGER KOD(LASTK)
      REAL RA(LASTK)
      EQUIVALENCE(IA,RA),(IA,KOD)

      integer IAGUARD
      COMMON/COMIS/IAGUARD(10)
      

#endif
