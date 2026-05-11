
      SUBROUTINE PHO_SETCT14(Tablefile, Lenfname)
      
#ifdef FOR_FLUKA
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE '(IOUNIT)'
#else
      IMPLICIT NONE
#endif

      INTEGER ierr , IPDsformat , IPDsset , ISEtch
 
C  input/output channels
      INCLUDE 'inc/poinou'
      INTEGER Lenfname
      CHARACTER Tablefile*1024
      COMMON /SETCHANGE/ ISEtch , IPDsset , IPDsformat
      DATA IPDsset , IPDsformat/0 , 0/
      SAVE 
 
#ifdef FOR_FLUKA
      CALL OAUXFI(Tablefile(1:Lenfname, LUNRDB,'OLD',IERR)
      IF (IERR.GT.0) GOTO 100
      CALL PHO_READPDS0 (LUNRDB)
      CLOSE (LUNRDB)
      
      Isetch=1; ipdsset=1
      RETURN
#else
      Open(1, File=Tablefile(1:Lenfname), Status='OLD', Err=100)
      IF (IERR.GT.0) GOTO 100
      CALL PHO_READPDS0 (1)
      CLOSE (1)
      
      Isetch=1; ipdsset=1
      RETURN
#endif
 
100   WRITE (LO,*) ' Data file ' , Tablefile(1:Lenfname) , 
     & ' cannot be opened in pho_SetCT14!!'
      STOP
      END SUBROUTINE
