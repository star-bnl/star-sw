
      DOUBLE PRECISION FUNCTION PHO_CT14MASS(I)
C     Returns the value of the quark mass for the i-th flavor
C     The flavors are:
C     1  2  3  4  5  6
C     u  d  s  c  b  t
      IMPLICIT NONE
      DOUBLE PRECISION AMAss
      INTEGER ISEtch , IPDsset , I , IPDsformat
      COMMON /SETCHANGE/ ISEtch , IPDsset , IPDsformat/MASSTBL/ AMAss(6)
 
 
      IF ( IPDsset.NE.1 )
     &      STOP 'pho_CT14Mass: the PDF table was not initialized'
 
      PHO_CT14MASS = AMAss(I)
 
      END FUNCTION
