*
* $Id: stafunc.inc,v 1.1 1998/04/10 15:30:49 fisyak Exp $
*
* $Log: stafunc.inc,v $
* Revision 1.1  1998/04/10 15:30:49  fisyak
* Move gstar into CVS
*
*
*
* stafunc.inc
*
*CMZ :  1.30/00 28/04/96  17.35.25  by  Pavel Nevski
*CMZ :  1.00/01 25/12/95  16.55.37  by  Pavel Nevski
*CMZ :  1.00/00 06/05/94  23.36.52  by  Pavel Nevski
*-- Author :
C Declare types for the things used in the statement function STAFUNC
      INTEGER IIIII, LVPRIN,LVGEOM,LVHIST,LVGRAP,LVDEBU,LWPRIN,
     +        LWDEBU,LVSIMU,LVDIGI,LVRECO,LVMFLD,LVANAL,LVBACK
C
C Statement Functions for ZEBRA
C datacard value are in LW PRIN/DEBU, current print is in LV PRIN
C
      LVPRIN(IIIII)=IQ(LQ(LKDETM-IIIII)+1)
      LVDEBU(IIIII)=IQ(LQ(LKDETM-IIIII)+2)
      LWPRIN(IIIII)=IQ(LQ(LKDETM-IIIII)+2)
      LWDEBU(IIIII)=IQ(LQ(LKDETM-IIIII)+3)
      LVGEOM(IIIII)=IQ(LQ(LKDETM-IIIII)+4)
      LVHIST(IIIII)=IQ(LQ(LKDETM-IIIII)+5)
      LVGRAP(IIIII)=IQ(LQ(LKDETM-IIIII)+6)
      LVSIMU(IIIII)=IQ(LQ(LKDETM-IIIII)+7)
      LVDIGI(IIIII)=IQ(LQ(LKDETM-IIIII)+8)
      LVRECO(IIIII)=IQ(LQ(LKDETM-IIIII)+9)
      LVMFLD(IIIII)=IQ(LQ(LKDETM-IIIII)+10)
      LVANAL(IIIII)=IQ(LQ(LKDETM-IIIII)+11)
      LVBACK(IIIII)=IQ(LQ(LKDETM-IIIII)+12)
 
