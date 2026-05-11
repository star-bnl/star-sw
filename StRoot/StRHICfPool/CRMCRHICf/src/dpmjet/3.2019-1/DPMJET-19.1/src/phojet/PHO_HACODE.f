
      SUBROUTINE PHO_HACODE(Id1,Id2,Idcpc1,Idcpc2)
C*********************************************************************
C
C     determination of hadron index from quarks
C
C     input:   ID1,ID2   parton code according to PDG conventions
C
C     output:  IDcpc1,2  CPC particle codes
C
C*********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER Id1 , Id2 , Idcpc1 , Idcpc2
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  general particle data
      INCLUDE 'inc/popar2'
 
C  local variables
      INTEGER ii , jj , kk , i1 , i2
 
      Idcpc1 = 0
      Idcpc2 = 0
 
      IF ( Id1*Id2.LT.0 ) THEN
C  meson
         IF ( Id1.GT.0 ) THEN
            ii = Id1
            jj = -Id2
         ELSE
            ii = Id2
            jj = -Id1
         END IF
         Idcpc1 = ID_psm_list(ii,jj)
         Idcpc2 = ID_vem_list(ii,jj)
 
      ELSE
C  baryon
         i1 = ABS(Id1)
         i2 = ABS(Id2)
         IF ( i1.GT.6 ) THEN
            ii = i1/1000
            jj = (i1-ii*1000)/100
            kk = i2
         ELSE
            ii = i1
            jj = i2/1000
            kk = (i2-jj*1000)/100
         END IF
         Idcpc1 = SIGN(ID_b8_list(ii,jj,kk),Id1)
         Idcpc2 = SIGN(ID_b10_list(ii,jj,kk),Id1)
 
      END IF
 
      END SUBROUTINE
