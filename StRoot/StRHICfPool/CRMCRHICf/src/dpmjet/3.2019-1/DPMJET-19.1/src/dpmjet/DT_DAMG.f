
      DOUBLE PRECISION FUNCTION DT_DAMG(It)
 
      IMPLICIT NONE
      DOUBLE PRECISION aam , dam , dgauni , DT_RNDM , gasuni , gauno , 
     &                 gaunon , uniga , v1 , vo , vv
      INTEGER i , io , It , nstab
      SAVE 
 
C particle properties (BAMJET index convention),
C (dublicate of DTPART for HADRIN)
      INCLUDE 'inc/hnablt'
 
      DIMENSION gasuni(14)
      DATA gasuni/ - 1.D0 , -.98D0 , -.95D0 , -.87D0 , -.72D0 , -.48D0 , 
     &     -.17D0 , .17D0 , .48D0 , .72D0 , .87D0 , .95D0 , .98D0 , 
     &     1.D0/
      DATA gauno/2.352D0/
      DATA gaunon/2.4D0/
      DATA io/14/
      DATA nstab/23/
 
      i = 1
      IF ( It.LE.0 ) THEN
         DT_DAMG = 0.0D0
         GOTO 99999
      ELSE IF ( It.GT.nstab ) THEN
         dgauni = gauno*gaunon/DBLE(io-1)
         vv = DT_RNDM(dgauni)
         vv = vv*2.0D0 - 1.0D0 + 1.D-16
 50      vo = gasuni(i)
         i = i + 1
         v1 = gasuni(i)
         IF ( vv.GT.v1 ) GOTO 50
         uniga = dgauni*(DBLE(i)-2.0D0+(vv-vo+1.D-16)/(v1-vo)
     &           -(DBLE(io)-1.0D0)*0.5D0)
         dam = GAH(It)*uniga/gauno
         aam = AMH(It) + dam
         DT_DAMG = aam
         RETURN
      END IF
      DT_DAMG = AMH(It)
99999 END FUNCTION
