*
* $Id: s_event.inc,v 1.1 1998/04/10 15:32:01 fisyak Exp $
*
* $Log: s_event.inc,v $
* Revision 1.1  1998/04/10 15:32:01  fisyak
* Move gstar into CVS
*
*
*
* s_event.inc
*
#if defined(CERNLIB_HADR)
*CMZ :          16/12/97  14.12.59  by  Pavel Nevski
*CMZ :  3.21/02 29/03/94  15.41.37  by  S.Giani
*-- Author :    *** NVE 09-FEB-1988 ***
      PARAMETER (MXEVEN=12*MXGKGH)
      COMMON/EVENT / NSIZE,NCUR,NEXT,NTOT,EVE(MXEVEN)
C
#endif
