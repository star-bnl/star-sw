*
* $Id: s_blankp.inc,v 1.2 1998/04/16 16:58:51 fisyak Exp $
*
* $Log: s_blankp.inc,v $
* Revision 1.2  1998/04/16 16:58:51  fisyak
* 2nd pass with gstar
*
*
*       *** NVE 09-FEB-1988 ***
*
* s_blankp.inc
*
#if defined(CERNLIB_HADR)
*CMZ :          16/12/97  14.12.59  by  Pavel Nevski
*CMZ :  3.21/02 29/03/94  15.41.37  by  S.Giani
*-- Author :    *** NVE 09-FEB-1988 ***
      PARAMETER (MXGKPV=MXGKGH)
      COMMON /VECUTY/ PV(10,MXGKPV)
#endif
