cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYGRAW
C...Universal Extra Dimensions Model (UED)
C...
C...See Macesanu etal. hep-ph/0201300 eqns.31 and 34.
C...
C...Integrand for the KK boson -> SM boson + graviton
C...graviton mass distribution (and gravity mediated total width),
C...which contains (see 0201300 and below for the full product)
C...the gravity mediated partial decay width Gamma(xx, yy)
C... i.e. GRADEN(YY)*PYWDKK(XXA)
C...  where xx is exclusive to gravity
C...  yy=m_Graviton/m_bosonKK denotes the Universal extra dimension
C...  and xxa=sqrt(xx**2+yy**2) refers to all of the extra dimensions.

      DOUBLE PRECISION FUNCTION PYGRAW(YIN)

C...Double precision and integer declarations
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT INTEGER (I-N)

C...Pythia commonblocks
      include 'inc/pydat1'

C...Local UED commonblocks and variables
      include 'inc/uedgra'
      include 'inc/intsav'

C...External: Pythia's Gamma function
      EXTERNAL PYGAMM

C...Pi
      PI=PARU(1)
      PI2=PI*PI

      YMIN=1.D-9/RINV
      YY=YSAV
      XX=SQRT(1.-YY**2)*YIN
      DJAC=(1.-YMIN)*SQRT(1.-YY**2)
      FAC=2.*PI**((NDIM-1.)/2.)*XMPLNK**2*RINV**NDIM/XMD**(NDIM+2)
      XND=(NDIM-1.)/2.
      GAMMN=PYGAMM(XND)
      FAC=FAC/GAMMN
      XXA=SQRT(XX**2+YY**2)
      GRADEN=4./PI2 * (YY**2/(1.-YY**2)**2)*(1.+DCOS(PI*YY))

      PYGRAW=DJAC*
     +     FAC*XX**(NDIM-2)*GRADEN*PYWDKK(XXA)

      RETURN
      END
