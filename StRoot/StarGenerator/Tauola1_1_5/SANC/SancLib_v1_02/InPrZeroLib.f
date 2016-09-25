        DOUBLE COMPLEX FUNCTION InProd_zero(p1,l1,p2,l2)
c////////////////////////////////////////////////////////////////
c         small s_{+,-}(p1,p2) for massless case:              //
c                 p1^2 = p2^2 = 0                              // 
c                                                              //
c     k0(0) = 1.d0                                             //
c     k0(1) = 1.d0                                             //
c     k0(2) = 0.d0  Kleisse_Stirling k0 points to X-axis       // 
c     k0(3) = 0.d0                                             //
c                                                              //
c////////////////////////////////////////////////////////////////
           IMPLICIT NONE
           INTEGER           l1,l2,i    
           DOUBLE PRECISION  p1(0:3),p2(0:3)
           DOUBLE PRECISION  forSqrt1,forSqrt2,sqrt1,sqrt2
           DOUBLE COMPLEX    i_,Dcmplx
           LOGICAL           equal
           INTRINSIC         Dcmplx
           PARAMETER         ( i_=(0.d0,1.d0) )
           INCLUDE 'MC_Declare.h'               

           equal = .TRUE.    
           DO i=0,3
             IF (p1(i).NE.p2(i))  equal = equal.AND..FALSE.                
           ENDDO

           IF ( (l1.EQ.l2) .OR. equal ) THEN

              InProd_zero = Dcmplx(0d0,0d0)

           ELSEIF ( (l1.EQ.+1) .AND. (l2.EQ.-1) ) THEN

              forSqrt1 = (p1(0)-p1(1))/(p2(0)-p2(1))
              forSqrt2 = 1.0d0/forSqrt1
              sqrt1    = Dsqrt(forSqrt2)
              sqrt2    = Dsqrt(forSqrt1)

              InProd_zero = (p1(2)+i_*p1(3))*sqrt1 -
     &                      (p2(2)+i_*p2(3))*sqrt2 

           ELSEIF ( (l1.EQ.-1) .AND. (l2.EQ.+1) ) THEN

              forSqrt1 = (p1(0)-p1(1))/(p2(0)-p2(1))
              forSqrt2 = 1.0d0/forSqrt1
              sqrt1    = Dsqrt(forSqrt2)
              sqrt2    = Dsqrt(forSqrt1)

              InProd_zero = (p2(2)-i_*p2(3))*sqrt2 -
     &                      (p1(2)-i_*p1(3))*sqrt1 
                                    
           ELSE
                 
              WRITE(MCLUN,*) " "             
              WRITE(MCLUN,*) " ERROR IN InProd_zero:"
              WRITE(MCLUN,*) "       WRONG VALUES FOR l1,l2: l1,l2 = -1,+1"
              WRITE(MCLUN,*) " "             
              STOP

           ENDIF   

        RETURN
        END
