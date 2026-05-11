
      SUBROUTINE PHO_SASDIR(X,Q2,P2,Q02,Xpga)
      IMPLICIT NONE
      REAL aem2pi , cgam , P2 , pmb , pmc , Q02 , Q2 , X , Xpga , xtmp
      INTEGER kf , kfl
C...Purpose: to evaluate the direct contribution, i.e. the C^gamma term,
C...as needed in MSbar parametrizations.
      SAVE 
      DIMENSION Xpga(-6:6)
      DATA pmc/1.3/ , pmb/4.6/ , aem2pi/0.0011614/
 
C...Reset output.
      DO kfl = -6 , 6
         Xpga(kfl) = 0.
      END DO
 
C...Evaluate common x-dependent expression.
      xtmp = (X**2+(1.-X)**2)*(-LOG(X)) - 1.
      cgam = 3.*aem2pi*X*(xtmp*(1.+P2/(P2+Q02))+6.*X*(1.-X))
 
C...d, u, s part by simple charge factor.
      Xpga(1) = (1./9.)*cgam
      Xpga(2) = (4./9.)*cgam
      Xpga(3) = (1./9.)*cgam
 
C...Also fill for antiquarks.
      DO kf = 1 , 5
         Xpga(-kf) = Xpga(kf)
      END DO
 
      END SUBROUTINE
