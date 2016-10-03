       SUBROUTINE BoostX(expH,A,Ap)
c########################################################################
c                                                                       #
c  Makes Lorentz boost to X-axis direction!!                            #
c         'bet' is relative velocity                                    #
c       z            z'                                                 #
c        | (K)        |   (K')                                          #
c        |            |                                                 #
c        |            |                                                 #
c        |        x   |   --> bet                                       #
c         --------->   ------------> x'                                 #
c       /            /                                                  #
c      /            /                                                   #
c    y/          y'/                                                    #
c                                                                       # 
c  expH = EXP(u) = Sqrt((1-bet)/(1+bet))                                #
c                                                                       # 
c    x0'= x0*ch(u)-x*sh(u)                                              #
c    x' = x*ch(u) -x0*sh(u)                                             #
c    y' = y                                                             #
c    z' = z                                                             #
c                                                                       #
c  For a reversed transformation: expH <--> 1/expH (bet <--> -bet !)    #
c                                                                       #
c########################################################################
         IMPLICIT NONE
         INTEGER          i
         DOUBLE PRECISION A(0:3),Ap(0:3),At(0:3)
         DOUBLE PRECISION expH,A1pA0,A1mA0
                  
         DO i=0,3
           At(i) = A(i) 
         ENDDO

         A1pA0 = (At(1) + At(0))/2d0
         A1mA0 = (At(1) - At(0))/2d0

         Ap(0) = A1pA0/expH - A1mA0*expH
         Ap(1) = A1pA0/expH + A1mA0*expH
         Ap(2) = At(2)
         Ap(3) = At(3)
 
       RETURN
       END


       SUBROUTINE BoostZ(expH,A,Ap)
c#######################################################################
c                                                                      #
c  Makes Lorentz boost to Z-axis direction!!                           #
c         'bet' is relative velocity                                   #
c       y            y'                                                #
c        | (K)        |   (K')                                         #
c        |            |                                                #
c        |            |                                                #
c        |        z   |   --> bet  z'                                  #
c         --------->   ------------>                                   #
c       /            /                                                 #
c      /            /                                                  #
c    x/          x'/                                                   #
c                                                                      #     
c  expH = EXP(u) = Sqrt((1-bet)/(1+bet))                               #
c                                                                      #
c    x0'= x0*ch(u)-x*sh(u)                                             # 
c    x' = x                                                            #
c    y' = y                                                            # 
c    z' = z*ch(u)-x0*sh(u)                                             #
c                                                                      #
c  For a reversed transformation: expH <--> 1/expH (bet <--> -bet !!)  # 
c                                                                      #
c#######################################################################
         IMPLICIT NONE
         INTEGER          i
         DOUBLE PRECISION A(0:3),Ap(0:3),At(0:3)
         DOUBLE PRECISION expH,A3pA0,A3mA0
                   
         DO i=0,3
           At(i) = A(i) 
         ENDDO

         A3pA0 = (At(3) + At(0))/2d0
         A3mA0 = (At(3) - At(0))/2d0

         Ap(0) = A3pA0/expH - A3mA0*expH
         Ap(1) = At(1)
         Ap(2) = At(2)
         Ap(3) = A3pA0/expH + A3mA0*expH
 
       RETURN
       END


       SUBROUTINE RotateY(theta,A,Ap)
c###################################################
c  Makes rotation about Y-axis with theta angle!!  #
c         (  Clockwise direction..  )              #
c###################################################
         IMPLICIT NONE
         INTEGER          i
         DOUBLE PRECISION A(0:3),Ap(0:3),At(0:3)
         DOUBLE PRECISION cosTh,sinTh,theta

         DO i=0,3
           At(i) = A(i) 
         ENDDO
       
         cosTh = Dcos(theta)   
         sinTh = Dsin(theta)   

         Ap(1) = At(1)*cosTh - At(3)*sinTh
         Ap(2) = At(2)
         Ap(3) = At(3)*cosTh + At(1)*sinTh 

       RETURN
       END


       SUBROUTINE RotateX(theta,A,Ap)
c##################################################
c  Makes rotation about X-axis with theta angle!! # 
c         (  Clockwise direction..  )             #
c##################################################
         IMPLICIT NONE
         INTEGER          i
         DOUBLE PRECISION A(0:3),Ap(0:3),At(0:3)
         DOUBLE PRECISION cosTh,sinTh,theta
       
         DO i=0,3
           At(i) = A(i) 
         ENDDO

         cosTh = Dcos(theta)   
         sinTh = Dsin(theta)   

         Ap(1) = At(1)
         Ap(2) = At(2)*cosTh + At(3)*sinTh
         Ap(3) = At(3)*cosTh - At(2)*sinTh

       RETURN
       END


       SUBROUTINE RotateZ(theta,A,Ap)
c##################################################
c  Makes rotation about Z-axis with theta angle!! # 
c         (  Clockwise direction..  )             #
c##################################################
         IMPLICIT NONE
         INTEGER          i
         DOUBLE PRECISION A(0:3),Ap(0:3),At(0:3)
         DOUBLE PRECISION cosTh,sinTh,theta
       
         DO i=0,3
           At(i) = A(i) 
         ENDDO

         cosTh = Dcos(theta)   
         sinTh = Dsin(theta)   

         Ap(1) = At(1)*cosTh + At(2)*sinTh
         Ap(2) = At(2)*cosTh - At(1)*sinTh
         Ap(3) = At(3)

       RETURN
       END


       SUBROUTINE GetAngles(X,theta,ph)
c#############################################
c      Calculates the Polar(theta) and       #
c        Azimuthal(ph) angles of X           #
c#############################################
         IMPLICIT NONE 
         DOUBLE PRECISION X(0:3),theta,ph          
         DOUBLE PRECISION XyXx,Xmod          
         INCLUDE 'MC_Declare.h'              

         Xmod = Dsqrt(X(1)**2 + X(2)**2 + X(3)**2)

         IF (Xmod.EQ.0d0) THEN
            PRINT*,"ERROR IN GetAngles : ZERO VECTOR!!"
            STOP
         ELSE
            theta = Dacos(X(3)/Xmod)
            IF (X(1).EQ.0d0.AND.X(2).GT.0d0) THEN
               ph = pi/2d0           
            ELSEIF (X(1).EQ.0d0.AND.X(2).LT.0d0) THEN
               ph = 3d0*pi/2d0           
            ELSE
               XyXx = Datan(Dabs(X(2))/Dabs(X(1)))
               IF (X(1).GT.0d0.AND.X(2).GT.0d0) THEN
                  ph = XyXx
               ELSEIF (X(1).LT.0d0.AND.X(2).GT.0d0) THEN
                  ph = pi - XyXx
               ELSEIF (X(1).LT.0d0.AND.X(2).LT.0d0) THEN
                  ph = pi + XyXx
               ELSEIF (X(1).GT.0d0.AND.X(2).LT.0d0) THEN
                  ph = 2d0*pi - XyXx
               ELSE
                  PRINT*," "
                  PRINT*,"ERROR In GetAngles : INCOMPLETE LOGIC!!"
                  PRINT*," "
                  STOP 
               ENDIF
            ENDIF 
         ENDIF 

       RETURN
       END 


       DOUBLE PRECISION FUNCTION getAngleXY(x,y)
c###################################################
c            calculates the angle                  #
c         between x and y 3-vectors..              #
c###################################################
         IMPLICIT NONE
         DOUBLE PRECISION x(0:3),y(0:3),cosTh
         DOUBLE PRECISION xMod,yMod,scalProd1
          
         xMod = Dsqrt(x(1)**2+x(2)**2+x(3)**2)
         yMod = Dsqrt(y(1)**2+y(2)**2+y(3)**2)
         IF ((xMod.NE.0d0).AND.(yMod.NE.0d0)) THEN
            scalProd1  = x(1)*y(1)+x(2)*y(2)+x(3)*y(3)
            cosTh      = scalProd1/xMod/yMod
            getAngleXY = Dacos(cosTh)
         ELSE
            PRINT*,""
            PRINT*,"ERROR IN getAnglesXY() :" 
            PRINT*,"      ONE OF THE 3-VECTOR HAS ZERO LENGTH !!"
            PRINT*,""
            STOP
         ENDIF
  
       RETURN
       END  
