* $Id: trroad.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
* $Log: trroad.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.1  2001/02/27 10:15:57  nevski
*  first working release
*
*   - Track road parameters
      REAL            ESHE,FSHE,ZSHE,RSHE,SSHE,ZVRT,DZSIT,
     +                RRSTV,RRSIT,RRMSG,RRGAA,RRTRD,
     +                RFISTV,RFISIT,RFIMSG,RFIGAA,RFITRD
      INTEGER         IWRK
      COMMON /TRROAD/ IWRK,ESHE,FSHE,ZSHE,RSHE,SSHE,ZVRT,DZSIT,
     +                RRSTV(3),RRSIT(3),RRMSG(3),RRGAA(3),RRTRD(3),
     +                RFISTV(3),RFISIT(3),RFIMSG(3),RFIGAA(3),RFITRD(3)
      INTEGER         NTZ
      REAL            RTZ,ZTZ
      COMMON /TZROAD/ NTZ, RTZ(100),ZTZ(100)
*     --------------------------------

