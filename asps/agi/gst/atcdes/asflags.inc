*
* $Id: asflags.inc,v 1.1 1998/04/10 15:30:48 fisyak Exp $
*
* $Log: asflags.inc,v $
* Revision 1.1  1998/04/10 15:30:48  fisyak
* Move gstar into CVS
*
*
*
* asflags.inc
*
*CMZ :  1.30/00 15/04/96  21.56.54  by  Pavel Nevski
*-- Author :    Pavel Nevski   15/04/96
      INTEGER      NFLAGS
      PARAMETER    (NFLAGS=15)
      CHARACTER*4  CFLAG(NFLAGS)
      DATA CFLAG   /'PNOW','PRIN','DEBU','GEOM','HIST','GRAP',
     >              'SIMU','DIGI','RECO','MFLD','ANAL','BACK',
     >              'RESA','RESB','RESC'/
*    ---------------------------------------------------------
