*
* $Id: agcdig.inc,v 1.1 1998/04/10 15:30:43 fisyak Exp $
*
* $Log: agcdig.inc,v $
* Revision 1.1  1998/04/10 15:30:43  fisyak
* Move gstar into CVS
*
*
*
* agcdig.inc
*
*CMZ :  1.00/00 07/02/95  23.16.25  by  Pavel Nevski
*-- Author :    Pavel Nevski   20/12/94
*   AGCDIGA is for internal AGSDIG0/1 communication only
      INTEGER        NLOCAL,   JS,JD,JDX,JX,JXD,JDU,LOCAL
      COMMON/GCLOCA/ NLOCAL(2),JS,JD,JDX,JX,JXD,JDU,LOCAL(14)
      Integer        Iset,Idet,NW,NV,ND,NVU,NDU
      COMMON/AGCDIGA/Iset,Idet,NW,NV,ND,NVU,NDU
*     - - - - - - - - - - - - - - -
*
