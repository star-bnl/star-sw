*
* $Id: asflags.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
*
* $Name:  $
*
*
* asflags.inc
*
      INTEGER      NFLAGS
      PARAMETER    (NFLAGS=15)
      CHARACTER*4  CFLAG(NFLAGS)
      DATA CFLAG   /'PNOW','PRIN','DEBU','GEOM','HIST','GRAP',
     >              'SIMU','DIGI','RECO','MFLD','ANAL','BACK',
     >              'RESA','RESB','RESC'/
*    ---------------------------------------------------------
