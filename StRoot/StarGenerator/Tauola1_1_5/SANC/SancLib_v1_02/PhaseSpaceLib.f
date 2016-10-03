       SUBROUTINE MakePhaseSpace_2body(p1,m1,p2,m2,k,phaseSpace2)     
 
         IMPLICIT NONE
         INTEGER          i
         DOUBLE PRECISION m1,m2,p1Mod,phaseSpace2,beta
         DOUBLE PRECISION k(0:3),p1(0:3),p2(0:3),LamSqr
         EXTERNAL         LamSqr 
         INCLUDE 'MC_Declare.h'   
                                            
         phaseSpace2 = 1d0/4d0/pi**2
         
         k(0)  = 0d0                       
         p1(0) = (mb**2+m1**2-m2**2)/2d0/mb
         p2(0) = (mb**2+m2**2-m1**2)/2d0/mb
         p1Mod = Dsqrt(p1(0)**2-m1**2)           
         Call RandVectOnSp3b(p1Mod,p1)     
         DO i=1,3
           p2(i) =-p1(i) 
           k(i)  = 0d0
         ENDDO           
         beta = LamSqr(mb**2,m1**2,m2**2)
c                                                    cosTh    ph
         phaseSpace2 = phaseSpace2*beta/8d0/mb**2 * 2d0*pi * 2d0  

       RETURN
       END

       SUBROUTINE MakePhaseSpace_3body(p1,m1,p2,m2,k,phaseSpace)
c################################################################################
c                                                                               #
c  This procedure makes transformation of p1 and p2 momentas from `R' to `Lab'  #
c  frame and returns p1,p2 and k momentas in LAB frame, which obey momenta      #
c  conservation law: p1+p2+k=Q (in `Lab' frame: Q(3-vector)=0, Q(0)=mb )        #
c  (rest frame of Z) and magnitude of the 3-particle `phaseSpace' volume ...    #
c  Collinearity and IR-divergence are taken into account...                     #
c                                                                               #
c  p1,p2      --- fermion momentas                                              #
c  k          --- photon momenta                                                #
c  phaseSpace --- 3-Particle Phase Space Volume                                 #
c                                                                               #
c################################################################################
         IMPLICIT NONE        
         DOUBLE PRECISION s,sMin,sMax,theta3,ph3,LamSqr,spMin
         DOUBLE PRECISION FRanGen,kMod,p1Mod,expH,spMax,r,sp,p2Mod
         DOUBLE PRECISION p1(0:3),p2(0:3),k(0:3),m1,m2
         DOUBLE PRECISION phaseFactor,phaseSpace,engConserv,momConserv
         EXTERNAL         FRanGen,LamSqr
         INCLUDE 'MC_Declare.h'   

         PhaseSpace = 1d0/2d0**11/pi**5     

c.. Simulation of  ivnariant mass of F anti_F system => s, with the density 1/(mb**2-s)
c.. NOTATIONS: s=(p1+p2)^2 , sp=mb^2-s
         spMax    = 2d0*mb*k0Max   ! k0Max - upper limit for photon nergy, less then kinematicaly allowed!

         spMin    = 2d0*mb*omega  
         r  = FRanGen()     
         sp = spMax*(spMin/spMax)**r  ! below: Jackobian factor
         phaseSpace = phaseSpace * 4d0*Dlog(spMax/spMin)

c.. Photon momenta simulation  in LAB frame...          
         k(0) = sp/2d0/mb
         kMod = k(0)
         CALL RandVectOnSp3b(kMod,k)
         CALL GetAngles(k,theta3,ph3) 
         phaseSpace = phaseSpace * 2d0 * 2d0*pi

c.. F-anti_F momenta simulation in R frame <==> p1+p2==0...
c.. We have changed flat distribution of (p1+p2) to 1/(1-(beta*cos(theta))**2),
c.. taking into account a collinearity !!!
         s = mb**2-sp
         p1(0) = (s+m1**2-m2**2)/2d0/Dsqrt(s)
         p2(0) = (s+m2**2-m1**2)/2d0/Dsqrt(s)
         p1Mod = Dsqrt(p1(0)**2-m1**2) 
         CALL RandVectOnSp3_coll(p1Mod,p1,phaseFactor)         
         p2(1) =-p1(1) 
         p2(2) =-p1(2) 
         p2(3) =-p1(3)                                ! below: Jackobian factor
         phaseSpace = phaseSpace*LamSqr(s,m1**2,m2**2)/s * phaseFactor

c.. Boost from R to LAB frame.....
c.. (NOTE: Z-axis of R frame points to k 3-vector and gamma_euler==0, 
c..  we have a freedom of rotation about z-axis of R frame !!! )
         expH = mb/Dsqrt(s)
         CALL BoostZ(expH,p1,p1)
         CALL BoostZ(expH,p2,p2)
c.. after this Boost, (p1+p2) 3-vector points opposite to k 3-vector...  

c.. One have to make transformation from the BOOSTED coordinate frame to the LAB frame...
c.. Rotate about X-axis...
         CALL RotateX(theta3,p1,p1)
         CALL RotateX(theta3,p2,p2)
  
c.. Rotate about Z-axis...
         CALL RotateZ(pi/2d0-ph3,p1,p1)
         CALL RotateZ(pi/2d0-ph3,p2,p2)

c.. Check, that all is right: k+p1+p2==0 as a 3-vector in `LAB' frame...
         momConserv = +(p1(1)+p2(1)+k(1))**2
     &                +(p1(2)+p2(2)+k(2))**2
     &                +(p1(3)+p2(3)+k(3))**2
         engConserv = (p1(0)+p2(0)+k(0)-mb)**2  ! mb = p1(0)+p2(0)+k(0) in LAB frame !!
         IF (momConserv.GT.1d-20.OR.engConserv.GT.1d-20) THEN
            PRINT*, ""
            PRINT*, "ERROR IN MakePhaseSpace_3body : "
            PRINT*, "      TRANSFORMATION FROM `R' TO `LAB': ----> ERROR"
            PRINT*, ""
            STOP
         ENDIF
   
       RETURN
       END  


       DOUBLE PRECISION FUNCTION LamSqr(x,y,z)

         IMPLICIT NONE
         DOUBLE PRECISION x,y,z

         LamSqr = Dsqrt( (x+y-z)**2-4d0*x*y )
         
       RETURN
       END  


       DOUBLE PRECISION FUNCTION ScalProd4_pm(p,q)
c##########################################################
c          Calculates Scalar product of 4-vectors         #
c                    in Pauli metric                      #
c##########################################################
         IMPLICIT NONE
         DOUBLE PRECISION p(0:3),q(0:3)
         
         ScalProd4_pm = p(1)*q(1)+p(2)*q(2)+p(3)*q(3)-p(0)*q(0)

       RETURN
       END


       DOUBLE PRECISION FUNCTION ScalProd4(p,q)
c#####################################################
c      Calculates Scalar product of 4-vectors        #
c            in Bjorken-Drell metric...              #
c#####################################################
         IMPLICIT NONE
         DOUBLE PRECISION p(0:3),q(0:3)
         
         ScalProd4 = p(0)*q(0)-p(1)*q(1)-p(2)*q(2)-p(3)*q(3)

       RETURN
       END
