cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------

C...PYWDKK
C...Universal Extra Dimensions Model (UED)
C...
C...Multiplied by the square modulus of a form factor
C...(see GRADEN in function PYGRAW)
C...PYWDKK is the KK boson -> SM boson + graviton
C...gravity mediated partial decay width Gamma(xx, yy)
C...  where xx is exclusive to gravity
C...  yy=m_Graviton/m_bosonKK denotes the Universal extra dimension
C...  and xxa=sqrt(xx**2+yy**2) refers to all of the extra dimensions
C...
C...N.B. The Feynman rules for the couplings of the graviton fields
C...to the UED fields are related to the corresponding couplings of
C...the graviton fields to the SM fields by the form factor.

      DOUBLE PRECISION FUNCTION PYWDKK(X)

C...Double precision and integer declarations
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT INTEGER (I-N)

C...Pythia commonblocks
      include 'inc/pydat1'
      include 'inc/pydat2'

C...Local UED commonblocks and variables
      include 'inc/uedgra'
      include 'inc/kappa'

      PI=PARU(1)

C...gamma* mass 473
      KCQKK=473
      XMNKK=PMAS(KCQKK,1)

C...Bosons partial width Macesanu hep-ph/0201300
      PYWDKK=XKAPPA**2/(96.*PI)*XMNKK**3/X**4*
     +          ((1.-X**2)**2*(1.+3.*X**2+6.*X**4))

      RETURN
      END
