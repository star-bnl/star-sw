
      SUBROUTINE DT_SHFAST(Mode,Ppn,Iback)
 
      IMPLICIT NONE
      INTEGER i , Iback , jbproj , jbtarg , jjproj , jjtarg , jp , jpz , 
     &        jt , jtz , Mode
      DOUBLE PRECISION ONE , pp , Ppn , TINY1 , TINY10 , TWO , ZERO
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY10=1.0D-10,TINY1=1.0D-1,ONE=1.0D0,
     &           TWO=2.0D0)
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C Glauber formalism: parameters
      INCLUDE 'inc/dtglam'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
 
      Iback = 0
 
      IF ( Mode.EQ.2 ) THEN
         OPEN (47,FILE='outdata0/shmakov.out',STATUS='UNKNOWN')
         WRITE (47,99010) IT , ITZ , IP , IPZ , IJProj , IBProj , 
     &                    IJTarg , IBTarg , Ppn
         WRITE (47,99020) RASh(1) , RBSh(1) , BMAx(1) , BSTep(1)
         WRITE (47,99030) SIGsh , ROSh , GSH
         DO i = 1 , 100
            WRITE (47,'(1X,E15.5)') BSIte(1,1,1,i)
         END DO
         WRITE (47,99040) NSIteb , NSTatb , ECMnn(1) , XSPro(1,1,1) , 
     &                    BSLope
         CLOSE (47)
      ELSE
         OPEN (47,FILE='outdata0/shmakov.out',STATUS='UNKNOWN')
         READ (47,99010) jt , jtz , jp , jpz , jjproj , jbproj , 
     &                   jjtarg , jbtarg , pp
         IF ( (jt.EQ.IT) .AND. (jtz.EQ.ITZ) .AND. (jp.EQ.IP) .AND. 
     &        (jpz.EQ.IPZ) .AND. (jjproj.EQ.IJProj) .AND. 
     &        (jbproj.EQ.IBProj) .AND. (jjtarg.EQ.IJTarg) .AND. 
     &        (jbtarg.EQ.IBTarg) .AND. (ABS(pp-Ppn).LT.(Ppn*0.01D0)) )
     &        THEN
            READ (47,99020) RASh(1) , RBSh(1) , BMAx(1) , BSTep(1)
            READ (47,99030) SIGsh , ROSh , GSH
            DO i = 1 , 100
               READ (47,'(1X,E15.5)') BSIte(1,1,1,i)
            END DO
            READ (47,99040) NSIteb , NSTatb , ECMnn(1) , XSPro(1,1,1) , 
     &                      BSLope
         ELSE
            Iback = 1
         END IF
         CLOSE (47)
      END IF
99010 FORMAT (1X,8I5,E15.5)
99020 FORMAT (1X,4E15.5)
99030 FORMAT (1X,3E15.5)
99040 FORMAT (1X,2I10,3E15.5)
 
      END SUBROUTINE
