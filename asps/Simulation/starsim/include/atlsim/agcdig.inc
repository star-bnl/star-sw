*
* $Id: agcdig.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
*
* $Name:  $
*
*
* agcdig.inc
*
*   AGCDIGA is for internal AGSDIG0/1 communication only
      INTEGER        NLOCAL,   JS,JD,JDX,JX,JXD,JDU,LOCAL
      COMMON/GCLOCA/ NLOCAL(2),JS,JD,JDX,JX,JXD,JDU,LOCAL(14)
      Integer        Iset,Idet,NW,NV,ND,NVU,NDU
      COMMON/AGCDIGA/Iset,Idet,NW,NV,ND,NVU,NDU
*     - - - - - - - - - - - - - - -
*
