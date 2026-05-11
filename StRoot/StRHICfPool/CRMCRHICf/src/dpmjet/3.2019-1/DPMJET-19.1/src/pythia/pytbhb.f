cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
      SUBROUTINE PYTBHB(MT,MB,MHP,BR,GAMT)
C  WIDTH AND BRANCHING RATIO FOR (ON-SHELL) T-> B W+, T->B H+
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      DOUBLE PRECISION MT,MB,MHP,MW,KFUN
      include 'inc/pyctbh'

C   TOP WIDTH CALCULATION
C       VTB  = 0.99
      MW=SQRT(MW2)
      XB=(MB/MT)**2
      XW=(MW/MT)**2
C unvar      XH =(MHP/MT)**2
      GAMTBH = 0D0
      IF (MT .LT. (MHP+MB)) THEN
C  T ->B W ONLY
         BETW = SQRT(1.D0-2*(XB+XW)+(XW-XB)**2)
         GAMTBW = VTB**2*ALPHA/(16*SW2)*MT/XW*BETW*
     &        (2*(1.D0-XB-XW)-(1.D0+XB-XW)*(1.D0-XB -2*XW) )
         GAMT  = GAMTBW
      ELSE
C T ->BW +T ->B H^+
         BETW = SQRT(1.D0-2*(XB+XW)+(XW-XB)**2)
         GAMTBW = VTB**2*ALPHA/(16*SW2)*MT/XW*BETW*
     &        (2*(1.D0-XB-XW)-(1.D0+XB-XW)*(1.D0-XB -2*XW) )
C
         KFUN = SQRT( (1.D0-(MHP/MT)**2-(MB/MT)**2)**2
     &        -4.D0*(MHP*MB/MT**2)**2 )
         GAMTBH= ALPHA/SW2/8.D0*VTB**2*KFUN/MT *
     &        (V**2*((MT+MB)**2-MHP**2)+A**2*((MT-MB)**2-MHP**2))
         GAMT  = GAMTBW+GAMTBH
      ENDIF
C THUS BR IS
      BR=GAMTBH/GAMT
      RETURN
      END
