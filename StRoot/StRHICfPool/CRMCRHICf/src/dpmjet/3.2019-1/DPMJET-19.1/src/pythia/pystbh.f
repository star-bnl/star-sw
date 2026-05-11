cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYSTBH (and auxiliaries)
C.. Evaluates the matrix elements for t + b + H production.
 
      SUBROUTINE PYSTBH(WTTBH)
 
C...DOUBLE PRECISION AND INTEGER DECLARATIONS
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
 
C...COMMONBLOCKS
      include 'inc/pydat1'
      include 'inc/pydat2'
      include 'inc/pypars'
      include 'inc/pyint1'
      include 'inc/pyint2'
      include 'inc/pyint3'
      include 'inc/pyint4'
      include 'inc/pysubs'
      include 'inc/pymssm'
      include 'inc/pysgcm'
      include 'inc/pyctbh'
 
C...LOCAL ARRAYS AND COMPLEX VARIABLES
      DIMENSION QQ(4,2),PP(4,3)
      DATA QQ/8*0D0/
 
      WTTBH=0D0
 
C...KINEMATIC PARAMETERS.
      SHPR=SQRT(VINT(26))*VINT(1)
      PH=SQRT(VINT(21))*VINT(1)
      SPH=PH**2
 
C...SET UP OUTGOING KINEMATICS: 1=T, 2=TBAR, 3=H.
      DO 100 I=1,2
        PT=SQRT(MAX(0D0,VINT(197+5*I)))
        PP(1,I)=PT*COS(VINT(198+5*I))
        PP(2,I)=PT*SIN(VINT(198+5*I))
  100 CONTINUE
      PP(1,3)=-PP(1,1)-PP(1,2)
      PP(2,3)=-PP(2,1)-PP(2,2)
      PMS1=VINT(201)**2+PP(1,1)**2+PP(2,1)**2
      PMS2=VINT(206)**2+PP(1,2)**2+PP(2,2)**2
      PMS3=SPH+PP(1,3)**2+PP(2,3)**2
      PMT3=SQRT(PMS3)
      PP(3,3)=PMT3*SINH(VINT(211))
      PP(4,3)=PMT3*COSH(VINT(211))
      PMS12=(SHPR-PP(4,3))**2-PP(3,3)**2
      PP(3,1)=(-PP(3,3)*(PMS12+PMS1-PMS2)+
     &VINT(213)*(SHPR-PP(4,3))*VINT(220))/(2D0*PMS12)
      PP(3,2)=-PP(3,1)-PP(3,3)
      PP(4,1)=SQRT(PMS1+PP(3,1)**2)
      PP(4,2)=SQRT(PMS2+PP(3,2)**2)
 
C...CM SYSTEM, INGOING QUARKS/GLUONS
      QQ(3,1) = SHPR/2.D0
      QQ(4,1) = QQ(3,1)
      QQ(3,2) = -QQ(3,1)
      QQ(4,2) = QQ(4,1)
 
C...PARAMETERS FOR AMPLITUDE METHOD
      ALPHA = AEM
      ALPHAS = AS
      SW2 = PARU(102)
      MW2 = PMAS(24,1)**2
      TANB = PARU(141)
      VTB = VCKM(3,3)
      RMB=PYMRUN(5,VINT(52))
 
      ISUB=MINT(1)
 
      IF (ISUB.EQ.401) THEN
        CALL PYTBHG(QQ(1,1),QQ(1,2),PP(1,1),PP(1,2),PP(1,3),
     &  VINT(201),VINT(206),RMB,VINT(43),WTTBH)
      ELSE IF (ISUB.EQ.402) THEN
        CALL PYTBHQ(QQ(1,1),QQ(1,2),PP(1,1),PP(1,2),PP(1,3),
     &  VINT(201),VINT(206),RMB,VINT(43),WTTBH)
      END IF
 
      RETURN
      END
