cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYRVI2
C...Function to integrate L-R interference contributions
 
      DOUBLE PRECISION FUNCTION PYRVI2(ID1,ID2,ID3)
 
      IMPLICIT NONE
      DOUBLE PRECISION LO,HI,PYRVG2,PYGAUS
      DOUBLE PRECISION RES, AB, RMS
      INTEGER ID1,ID2,ID3, IDR, IDR2, KFR, INTRES
      LOGICAL DCMASS
      EXTERNAL PYRVG2,PYGAUS
      include 'inc/pyrvnv'
      include 'inc/pyrvpm'
C...Initialize mass and width information
      PYRVI2 = 0D0
      RM(0)  = RMS(0)
      RM(1)  = RMS(ID1)
      RM(2)  = RMS(ID2)
      RM(3)  = RMS(ID3)
      RESM(1)= RES(IDR,1)
      RESW(1)= RES(IDR,2)
      RESM(2)= RES(IDR+1,1)
      RESW(2)= RES(IDR+1,2)
C...A->B and B->A for antisparticles
      A(1)   = AB(1+INTRES(IDR,3),INTRES(IDR,1),INTRES(IDR,2))
      B(1)   = AB(2-INTRES(IDR,3),INTRES(IDR,1),INTRES(IDR,2))
      A(2)   = AB(1+INTRES(IDR+1,3),INTRES(IDR+1,1),INTRES(IDR+1,2))
      B(2)   = AB(2-INTRES(IDR+1,3),INTRES(IDR+1,1),INTRES(IDR+1,2))
C...Boundaries and mass flag
      LO     = (RM(1)+RM(2))**2
      HI     = (RM(0)-RM(3))**2
      MFLAG  = DCMASS
      PYRVI2 = PYGAUS(PYRVG2,LO,HI,1D-3)
      RETURN
      END
