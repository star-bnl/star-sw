* $Id: irepar.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
* $Log: irepar.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.1  2001/02/27 10:15:56  nevski
*  first working release
*
C  -   Fumfit control parameters
      INTEGER         IFSHOW,IFTIME,ICLEAN,IFBREM,
     >                IRC,NUV,NZM,JPR,NDF,NTR,IKINE,ISHOW
      REAL            BCONST,DBREM,CHI2,PTPR,SSHE0,UKINE,
     >                DA0,DZ0,DELCHI,CHIBAD,SIG0,DT0,XTR
      COMMON /IREDAT/ JPR,IFBREM,UKINE,IKINE,ISHOW,PTPR,SSHE0,
     >                DA0,DZ0,SIG0,DT0,DELCHI,CHIBAD
      COMMON /IREPAR/ BCONST,DBREM,IRC,CHI2,NDF,NUV,NZM,NTR,XTR,
     >                IFSHOW,IFTIME,ICLEAN
C---------------------------------------------------------------------
