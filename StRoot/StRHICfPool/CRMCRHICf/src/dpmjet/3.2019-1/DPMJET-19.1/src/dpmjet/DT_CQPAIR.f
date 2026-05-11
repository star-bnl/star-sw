
      SUBROUTINE DT_CQPAIR(Xqmax,Xaqmax,Xq,Xaq,Iflv,Irej)
 
C***********************************************************************
C This subroutine Creates a Quark-antiquark PAIR from the sea.         *
C                                                                      *
C   XQMAX   maxium energy fraction of quark (input)                    *
C   XAQMAX  maxium energy fraction of antiquark (input)                *
C   XQ      energy fraction of quark (output)                          *
C   XAQ     energy fraction of antiquark (output)                      *
C   IFLV    quark flavour (- antiquark flavor) (output)                *
C                                                                      *
C This version dated 14.5.00  is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , DT_SAMSQX , egluon , rq , rz , seasq , 
     &                 Xaq , Xaqmax , xgluon , xgmax , xgmaxi , xgmin , 
     &                 xhlp , Xq , Xqmax , xthr1 , xthr2 , zmax , zmin
      INTEGER Iflv , Irej , nloop
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
 
C
      Irej = 0
      Xq = 0.0D0
      Xaq = 0.0D0
C
C sample quark flavour
C
C  set seasq here (the one from DTCHAI should be used in the future)
      seasq = 0.5D0
      Iflv = INT(1.0D0+DT_RNDM(Xqmax)*(2.0D0+seasq))
C
C sample energy fractions of sea pair
C we first sample the energy fraction of a gluon and then split the gluon
C
C  maximum energy fraction of the gluon forced via input
      xgmaxi = Xqmax + Xaqmax
C  minimum energy fraction of the gluon
      xthr1 = 4.0D0/UMO**2
      xthr2 = 0.54D0/UMO**1.5D0
      xgmin = MAX(xthr1,xthr2)
C  maximum energy fraction of the gluon
      xgmax = 0.3D0
      xgmax = MIN(xgmaxi,xgmax)
      IF ( xgmin.GE.xgmax ) THEN
         Irej = 1
         RETURN
      END IF
C
C  sample energy fraction of the gluon
      nloop = 0
 100  nloop = nloop + 1
      IF ( nloop.GE.50 ) THEN
         Irej = 1
         RETURN
      END IF
      xgluon = DT_SAMSQX(xgmin,xgmax)
      egluon = xgluon*UMO/2.0D0
C
C  split gluon into q-aq pair (we follow PHOJET's subroutine PHO_GLU2QU)
      zmin = MIN(0.1D0,0.5D0/egluon)
      zmax = 1.0D0 - zmin
      rz = DT_RNDM(zmax)
      xhlp = ((1.0D0-rz)*zmin**3+rz*zmax**3)**0.33333
      rq = DT_RNDM(zmax)
      IF ( rq.LT.0.5D0 ) THEN
         Xq = xgluon*xhlp
         Xaq = xgluon - Xq
      ELSE
         Xaq = xgluon*xhlp
         Xq = xgluon - Xaq
      END IF
      IF ( (Xq.GT.Xqmax) .OR. (Xaq.GT.Xaqmax) ) GOTO 100
 
      END SUBROUTINE
