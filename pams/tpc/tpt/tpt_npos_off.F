      SUBROUTINE n_max_zero(trk,nmax)
      IMPLICIT NONE
      
      REAL    trk(6)
      INTEGER nmax,i,l,ipad1(8),ipad2(5),ipad3(31)
      REAL    x0, y0, xhit, yhit !straight line parameters
      REAL    zpl,sav2
      REAL    ypad(45),xwidth(45)
C     
   
      DATA ipad1/0,8,8,8,6,8,8,8/
      DATA ipad2/8,6,10,8,10/
      DATA ipad3/4*2,0,3*2,0,5*2,0,3*2,0,4*2,0,4*2,3*0/
      
C//   first job is to initialize ypad, xwidth ( lengths in cm )
      ypad(1) = 60.0            !radial position of innermost padrow
      xwidth(1) = 29.48         !full width 88pads x .335 cm spacing 
      DO i =2,8
         ypad(i) = ypad(i -1) + 4.8
         xwidth(i) = xwidth(i -1) +  float(ipad1(i))*.335
      ENDDO
      DO i =9,13
         ypad(i) = ypad(i-1) + 5.20
	 xwidth(i) = xwidth(i-1) +  float(ipad2(i-8))*.335
      ENDDO
C     // 14th row is first padrow in the outer sector
      ypad(14) = ypad(13) + 7.6 !radial position of first outer sector padrow
      xwidth(14) = 65.77        !98 pads x .67 cm spacing
      DO i = 15,45
         ypad(i) = ypad(i-1) +2.00
         xwidth(i) = xwidth(i-1) + float(ipad3(i-14))*.67
      ENDDO
      
C***  z position of padplane
      zpl = 210.
      
C***  Rotate track so its sector pads are normal to y-axis
C***  Watch out. This is not as trivial as it seems.
      sav2 = trk(2)
      IF(amod(trk(2),0.5236).gt.0.2618) THEN !0.2618 = 15 degrees
         trk(2)=amod(trk(2),0.5236)+1.0472 !only 60 degrees
      ELSE
         trk(2)=amod(trk(2),0.5236)+1.5708 ! 0.5236 = 30 degrees
      ENDIF
      
      trk(4)=trk(4) +  trk(2) - sav2 ! psi has const. relation to phi
      
C***  Loop over three adjacent sectors
      
      nmax = 0
      trk(2) = trk(2) - 2.*0.5236
      trk(4) = trk(4) - 2.*0.5236
      
      DO 75 l=1,3
         trk(2) = trk(2) + 0.5236
         trk(4) = trk(4) + 0.5236
         
         x0 = trk(1) * cos(trk(2))
         y0 = trk(1) * sin(trk(2))
         
         DO 20 i=1,45
            yhit = ypad(i) -y0
            IF( (ypad(i).le.(abs(trk(1)*sin(trk(2)))+
     +           (zpl-abs(trk(3)))/abs(trk(5)))) ) THEN
               IF(abs(trk(4)).le. 0.0001) THEN
                  IF(y0.ge.ypad(1) )  nmax = nmax +1 ! superhit
                  GOTO 75       ! no more possible hits in this sector
               ELSE 
                  xhit = x0 +  yhit/(tan(trk(4)))
               ENDIF
               
               IF(abs(xhit).le.0.5*xwidth(i) ) nmax = nmax +1
            ENDIF
            
 20      CONTINUE          
         
         
 75   CONTINUE                  ! over three sectors
      
 90   IF(nmax.eq.0) nmax = 1    ! protect divisions

      RETURN
      END
