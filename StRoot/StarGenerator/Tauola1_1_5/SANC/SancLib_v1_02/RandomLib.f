        SUBROUTINE RandVectOnSp3a(Mod,X,thetaX,phX)
c########################################################
c   generates uniformly distributed random             ##
c   vector on 3-sphere,with length "Mod",              ##
c   returns thetaX - polar and phX-azimuthal Angles    ##
c########################################################
          IMPLICIT NONE
          REAL             XX(2)
          DOUBLE PRECISION Mod,X(0:3),thetaX,phX
          DOUBLE PRECISION cosTh,sinTh,ph
          INCLUDE 'MC_Declare.h'         
 
          CALL RanGen(XX,2)   

          cosTh = 2d0*XX(1)-1d0
          sinTh = Dsqrt(1d0-cosTh**2)        
          ph    = 2d0*pi*XX(2)

          thetaX = Dacos(cosTh)
          phX    = ph

          X(1) = Mod*sinTh*Dcos(ph)
          X(2) = Mod*sinTh*Dsin(ph)
          X(3) = Mod*cosTh

        RETURN
        END


        SUBROUTINE RandVectOnSp3b(Mod,X)
c########################################################
c       generates uniformly distributed random         ##
c        vector on 3-sphere,with length "Mod"          ##
c########################################################

          IMPLICIT NONE
          REAL             XX(2)
          DOUBLE PRECISION Mod,X(0:3)
          DOUBLE PRECISION cosTh,sinTh,ph
          INCLUDE 'MC_Declare.h'         
 
          CALL RanGen(XX,2)   

          cosTh = 2d0*XX(1)-1d0
          sinTh = Dsqrt(1d0-cosTh**2)        
          ph    = 2d0*pi*XX(2)

          X(1) = Mod*sinTh*Dcos(ph)
          X(2) = Mod*sinTh*Dsin(ph)
          X(3) = Mod*cosTh

        RETURN
        END


        SUBROUTINE RandVectOnSp3_coll(Xmod,X,phaseFactor)
          
          IMPLICIT NONE
          REAL             XX(3)
          DOUBLE PRECISION Xmod,X(0:3),sumCharge,phaseFactor
          DOUBLE PRECISION cosTh,sinTh,ph,coeff1,coeff2,coeffS
          DOUBLE PRECISION beta1I,beta2I,beta1C,beta2C,frac2,frac1
          INCLUDE 'MC_Declare.h'         

          CALL RanGen(XX,3)  

          sumCharge = abs(qf1)+abs(qf2)
          beta1I = Dsqrt(1d0+(mf1/Xmod)**2)
          beta2I = Dsqrt(1d0+(mf2/Xmod)**2)
          frac1  = beta1I*abs(qf1)/sumCharge
          frac2  = beta2I*abs(qf2)/sumCharge
          coeff1 = -Dlog(((beta1I-1d0)/(beta1I+1d0))**frac1)
          coeff2 = -Dlog(((beta2I-1d0)/(beta2I+1d0))**frac2)
          coeffS = coeff1+coeff2

c... first branch -- |qf1|/|q|/(1-beta1*cosTH)                         
          IF (XX(1).LT.coeff1/coeffS) THEN                 
             beta1C = (beta1I-1d0)/(beta1I+1d0)   
             cosTh  = beta1I-(beta1I+1d0)*beta1C**XX(2)
c... second  branch -- |qf2|/|q|/(1+beta2*cosTH)
          ELSE                                             
             beta2C = (beta2I+1d0)/(beta2I-1d0)   
             cosTh  = (beta2I-1d0)*beta2C**XX(2)-beta2I
          ENDIF    
          
          sinTh= Dsqrt(1d0-cosTh**2)        
          ph   = 2d0*pi*XX(3)
                   
          X(1) = Xmod*sinTh*Dcos(ph)
          X(2) = Xmod*sinTh*Dsin(ph)
          X(3) = Xmod*cosTh
                   
          phaseFactor=coeffS*(beta1I-cosTh)*(beta2I+cosTh)
     &                      /(beta1I*beta2I+(frac1-frac2)*cosTh) * 2d0*pi
        RETURN     
        END        


        SUBROUTINE RandVectOnSp3_coll_old(Xmod,X,phaseFactor)
          
          IMPLICIT NONE
          REAL             XX(2)
          DOUBLE PRECISION Xmod,X(0:3),phaseFactor
          DOUBLE PRECISION cosTh,sinTh,ph,r,d,betC,logBetC
          INCLUDE 'MC_Declare.h'         
          CALL RanGen(XX,2)   

          betC    = (X(0)+Xmod)/(X(0)-Xmod)     ! (1+bet)/(1-bet)
          logBetC = Dlog(betC)
          r       = 2d0*XX(1)-1d0
          d       = (betC)**r
          cosTh   = X(0)*(d-1d0)/(d+1d0)/Xmod
          sinTh   = Dsqrt(1d0-cosTh**2)        
          ph      = 2d0*pi*XX(2)

          X(1) = Xmod*sinTh*Dcos(ph)
          X(2) = Xmod*sinTh*Dsin(ph)
          X(3) = Xmod*cosTh
c                  Jackobian corresponding to d(cosTh)  and   to d(ph)
          phaseFactor = 4d0*X(0)*d*logBetc/(d+1d0)**2/Xmod * 2d0*pi     

        RETURN
        END


        DOUBLE PRECISION FUNCTION FRanGen()            

          IMPLICIT NONE
          REAL rvec(1) 
         
          CALL RanGen(rvec,1)
          FRanGen = Dble(rvec(1))

        RETURN
        END


        SUBROUTINE RanGen(RVEC,LENV)                                      

          INTEGER          j
          DIMENSION        RVEC(LENV)
          DOUBLE PRECISION grnd
          EXTERNAL         grnd
          INCLUDE 'MC_Declare.h'        

          IF (ranGenType.EQ.1) THEN
             CALL RANLUX(RVEC,LENV)              ! RANLUX                           
          ELSEIF (ranGenType.EQ.2) THEN 
             CALL RANMAR(RVEC,LENV)              ! RANMAR                                    
          ELSEIF (ranGenType.EQ.3) THEN
             DO j=1,LENV                  
                RVEC(j) = Real(grnd())           ! MERSENNE TWISTER 
             ENDDO                               
          ELSE
             PRINT*, ""
             PRINT*, " ERROR IN RanGen():"
             PRINT*, "       THIS RANDOM GENERATOR DOES NOT EXISTS..."
             PRINT*, "          FOR INITIALIZATION SEE MC_Init():"
             PRINT*, "            RanGenType = 1 for 'ranlux'"      
             PRINT*, "            RanGenType = 2 for 'ranmar'"       
             PRINT*, "            RanGenType = 3 for 'mersenne twister'"       
             PRINT*, ""
             STOP
          ENDIF
 
        RETURN
        END 


      SUBROUTINE RANMAR(RVEC,LENV)                                      
C =====================================================================
C Universal random number generator proposed by Marsaglia and Zaman     
C in report FSU-SCRI-87-50                                              
C        modified by F. James, 1988 and 1989, to generate a vector      
C        of pseudorandom numbers RVEC of length LENV, and to put in     
C        the COMMON block everything needed to specify currrent state,  
C        and to add input and output entry points RMARIN, RMARUT.       
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C!!!  Calling sequences for RANMAR:                                  ++ 
C!!!      CALL RANMAR (RVEC, LEN)   returns a vector RVEC of LEN     ++ 
C!!!                   32-bit random floating point numbers between  ++ 
C!!!                   zero and one.                                 ++ 
C!!!      CALL RMARIN(I1,N1,N2)   initializes the generator from one ++ 
C!!!                   32-bit integer I1, and number counts N1,N2    ++ 
C!!!                  (for initializing, set N1=N2=0, but to restart ++ 
C!!!                    a previously generated sequence, use values  ++ 
C!!!                    output by RMARUT)                            ++ 
C!!!      CALL RMARUT(I1,N1,N2)   outputs the value of the original  ++ 
C!!!                  seed and the two number counts, to be used     ++ 
C!!!                  for restarting by initializing to I1 and       ++ 
C!!!                  skipping N1*100000000+N2 numbers.              ++ 
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

      DIMENSION RVEC(*)                                                 
      COMMON/RASET1/U(97),C,I97,J97                                     
      PARAMETER (MODCNS=1000000000)                                     
      SAVE CD, CM, TWOM24, NTOT, NTOT2, IJKL                            
      DATA NTOT,NTOT2,IJKL/-1,0,0/                                      
C                                                                       
      IF (NTOT .GE. 0)  GO TO 50                                        
C                                                                       
C        Default initialization. User has called RANMAR without RMARIN. 
      IJKL = 54217137                                                   
      NTOT = 0                                                          
      NTOT2 = 0                                                         
      KALLED = 0                                                        
      GO TO 1                                                           
C                                                                       
      ENTRY      RMARIN(IJKLIN, NTOTIN,NTOT2N)                          
C         Initializing routine for RANMAR, may be called before         
C         generating pseudorandom numbers with RANMAR. The input        
C         values should be in the ranges:  0<=IJKLIN<=900 OOO OOO       
C                                          0<=NTOTIN<=999 999 999       
C                                          0<=NTOT2N<<999 999 999!      
C To get the standard values in Marsaglia's paper, IJKLIN=54217137      
C                                            NTOTIN,NTOT2N=0            
      IJKL = IJKLIN                                                     
      NTOT = MAX(NTOTIN,0)                                              
      NTOT2= MAX(NTOT2N,0)                                              
      KALLED = 1                                                        
C          always come here to initialize                               
    1 CONTINUE                                                          
      IJ = IJKL/30082                                                   
      KL = IJKL - 30082*IJ                                              
      I = MOD(IJ/177, 177) + 2                                          
      J = MOD(IJ, 177)     + 2                                          
      K = MOD(KL/169, 178) + 1                                          
      L = MOD(KL, 169)                                                  
      WRITE(6,'(A,I10,2X,2I10)') ' RANMAR INITIALIZED:',IJKL,NTOT,NTOT2 
CCC      PRINT '(A,4I10)', '   I,J,K,L= ',I,J,K,L                       
      DO 2 II= 1, 97                                                    
      S = 0.                                                            
      T = .5                                                            
      DO 3 JJ= 1, 24                                                    
         M = MOD(MOD(I*J,179)*K, 179)                                   
         I = J                                                          
         J = K                                                          
         K = M                                                          
         L = MOD(53*L+1, 169)                                           
         IF (MOD(L*M,64) .GE. 32)  S = S+T                              
    3    T = 0.5*T                                                      
    2 U(II) = S                                                         
      TWOM24 = 1.0                                                      
      DO 4 I24= 1, 24                                                   
    4 TWOM24 = 0.5*TWOM24                                               
      C  =   362436.*TWOM24                                             
      CD =  7654321.*TWOM24                                             
      CM = 16777213.*TWOM24                                             
      I97 = 97                                                          
      J97 = 33                                                          
C       Complete initialization by skipping                             
C            (NTOT2*MODCNS + NTOT) random numbers                       
      DO 45 LOOP2= 1, NTOT2+1                                           
      NOW = MODCNS                                                      
      IF (LOOP2 .EQ. NTOT2+1)  NOW=NTOT                                 
      IF (NOW .GT. 0)  THEN                                             
        WRITE(6,'(A,I15)') ' RMARIN SKIPPING OVER ',NOW                 
       DO 40 IDUM = 1, NTOT                                             
       UNI = U(I97)-U(J97)                                              
       IF (UNI .LT. 0.)  UNI=UNI+1.                                     
       U(I97) = UNI                                                     
       I97 = I97-1                                                      
       IF (I97 .EQ. 0)  I97=97                                          
       J97 = J97-1                                                      
       IF (J97 .EQ. 0)  J97=97                                          
       C = C - CD                                                       
       IF (C .LT. 0.)  C=C+CM                                           
   40  CONTINUE                                                         
      ENDIF                                                             
   45 CONTINUE                                                          
      IF (KALLED .EQ. 1)  RETURN                                        
C                                                                       
C          Normal entry to generate LENV random numbers                 
   50 CONTINUE                                                          
      DO 100 IVEC= 1, LENV                                              
      UNI = U(I97)-U(J97)                                               
      IF (UNI .LT. 0.)  UNI=UNI+1.                                      
      U(I97) = UNI                                                      
      I97 = I97-1                                                       
      IF (I97 .EQ. 0)  I97=97                                           
      J97 = J97-1                                                       
      IF (J97 .EQ. 0)  J97=97                                           
      C = C - CD                                                        
      IF (C .LT. 0.)  C=C+CM                                            
      UNI = UNI-C                                                       
      IF (UNI .LT. 0.) UNI=UNI+1.                                       
      RVEC(IVEC) = UNI                                                  
C             Replace exact zeros by uniform distr. *2**-24             
         IF (UNI .EQ. 0.)  THEN                                         
         ZUNI = TWOM24*U(2)                                             
C             An exact zero here is very unlikely, but let's be safe.   
         IF (ZUNI .EQ. 0.) ZUNI= TWOM24*TWOM24                          
         RVEC(IVEC) = ZUNI                                              
         ENDIF                                                          
  100 CONTINUE                                                          
      NTOT = NTOT + LENV                                                
         IF (NTOT .GE. MODCNS)  THEN                                    
         NTOT2 = NTOT2 + 1                                              
         NTOT = NTOT - MODCNS                                           
         ENDIF                                                          
      RETURN                                                            
C           Entry to output current status                              
      ENTRY RMARUT(IJKLUT,NTOTUT,NTOT2T)                                
      IJKLUT = IJKL                                                     
      NTOTUT = NTOT                                                     
      NTOT2T = NTOT2                                                    
      RETURN                                                            
      END                                                               


      SUBROUTINE RANLUX(RVEC,LENV)
C         Subtract-and-borrow random number generator proposed by
C         Marsaglia and Zaman, implemented by F. James with the name
C         RCARRY in 1991, and later improved by Martin Luescher
C         in 1993 to produce "Luxury Pseudorandom Numbers".
C     Fortran 77 coded by F. James, 1993
C          
C       references: 
C  M. Luscher, Computer Physics Communications  79 (1994) 100
C  F. James, Computer Physics Communications 79 (1994) 111
C
C   LUXURY LEVELS.
C   ------ ------      The available luxury levels are:
C
C  level 0  (p=24): equivalent to the original RCARRY of Marsaglia
C           and Zaman, very long period, but fails many tests.
C  level 1  (p=48): considerable improvement in quality over level 0,
C           now passes the gap test, but still fails spectral test.
C  level 2  (p=97): passes all known tests, but theoretically still
C           defective.
C  level 3  (p=223): DEFAULT VALUE.  Any theoretically possible
C           correlations have very small chance of being observed.
C  level 4  (p=389): highest possible luxury, all 24 bits chaotic.
C
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for RANLUX:                                  ++
C!!!      CALL RANLUX (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero (not included) and one (also not incl.). ++
C!!!      CALL RLUXGO(LUX,INT,K1,K2) initializes the generator from  ++
C!!!               one 32-bit integer INT and sets Luxury Level LUX  ++
C!!!               which is integer between zero and MAXLEV, or if   ++
C!!!               LUX .GT. 24, it sets p=LUX directly.  K1 and K2   ++
C!!!               should be set to zero unless restarting at a break++ 
C!!!               point given by output of RLUXAT (see RLUXAT).     ++
C!!!      CALL RLUXAT(LUX,INT,K1,K2) gets the values of four integers++
C!!!               which can be used to restart the RANLUX generator ++
C!!!               at the current point by calling RLUXGO.  K1 and K2++
C!!!               specify how many numbers were generated since the ++
C!!!               initialization with LUX and INT.  The restarting  ++
C!!!               skips over  K1+K2*E9   numbers, so it can be long.++
C!!!   A more efficient but less convenient way of restarting is by: ++
C!!!      CALL RLUXIN(ISVEC)    restarts the generator from vector   ++
C!!!                   ISVEC of 25 32-bit integers (see RLUXUT)      ++
C!!!      CALL RLUXUT(ISVEC)    outputs the current values of the 25 ++
C!!!                 32-bit integer seeds, to be used for restarting ++
C!!!      ISVEC must be dimensioned 25 in the calling program        ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION RVEC(LENV)
      DIMENSION SEEDS(24), ISEEDS(24), ISDEXT(25)
      PARAMETER (MAXLEV=4, LXDFLT=3)
      DIMENSION NDSKIP(0:MAXLEV)
      DIMENSION NEXT(24)
      PARAMETER (TWOP12=4096., IGIGA=1000000000,JSDFLT=314159265)
      PARAMETER (ITWO24=2**24, ICONS=2147483563)
      SAVE NOTYET, I24, J24, CARRY, SEEDS, TWOM24, TWOM12, LUXLEV
      SAVE NSKIP, NDSKIP, IN24, NEXT, KOUNT, MKOUNT, INSEED
      INTEGER LUXLEV
      LOGICAL NOTYET
      DATA NOTYET, LUXLEV, IN24, KOUNT, MKOUNT /.TRUE., LXDFLT, 0,0,0/
      DATA I24,J24,CARRY/24,10,0./
C                               default
C  Luxury Level   0     1     2   *3*    4
      DATA NDSKIP/0,   24,   73,  199,  365 /
Corresponds to p=24    48    97   223   389
C     time factor 1     2     3     6    10   on slow workstation
C                 1    1.5    2     3     5   on fast mainframe
C
C  NOTYET is .TRUE. if no initialization has been performed yet.
C              Default Initialization by Multiplicative Congruential
      IF (NOTYET) THEN
         NOTYET = .FALSE.
         JSEED = JSDFLT  
         INSEED = JSEED
c         WRITE(6,'(A,I12)') ' RANLUX DEFAULT INITIALIZATION: ',JSEED
         LUXLEV = LXDFLT
         NSKIP = NDSKIP(LUXLEV)
         LP = NSKIP + 24
         IN24 = 0
         KOUNT = 0
         MKOUNT = 0
c         WRITE(6,'(A,I2,A,I4)')  ' RANLUX DEFAULT LUXURY LEVEL =  ',
c     +        LUXLEV,'      p =',LP
            TWOM24 = 1.
         DO 25 I= 1, 24
            TWOM24 = TWOM24 * 0.5
         K = JSEED/53668
         JSEED = 40014*(JSEED-K*53668) -K*12211
         IF (JSEED .LT. 0)  JSEED = JSEED+ICONS
         ISEEDS(I) = MOD(JSEED,ITWO24)
   25    CONTINUE
         TWOM12 = TWOM24 * 4096.
         DO 50 I= 1,24
         SEEDS(I) = REAL(ISEEDS(I))*TWOM24
         NEXT(I) = I-1
   50    CONTINUE
         NEXT(1) = 24
         I24 = 24
         J24 = 10
         CARRY = 0.
         IF (SEEDS(24) .EQ. 0.) CARRY = TWOM24
      ENDIF
C
C          The Generator proper: "Subtract-with-borrow",
C          as proposed by Marsaglia and Zaman,
C          Florida State University, March, 1989
C
      DO 100 IVEC= 1, LENV
      UNI = SEEDS(J24) - SEEDS(I24) - CARRY 
      IF (UNI .LT. 0.)  THEN
         UNI = UNI + 1.0
         CARRY = TWOM24
      ELSE
         CARRY = 0.
      ENDIF
      SEEDS(I24) = UNI
      I24 = NEXT(I24)
      J24 = NEXT(J24)
      RVEC(IVEC) = UNI
C  small numbers (with less than 12 "significant" bits) are "padded".
      IF (UNI .LT. TWOM12)  THEN
         RVEC(IVEC) = RVEC(IVEC) + TWOM24*SEEDS(J24)
C        and zero is forbidden in case someone takes a logarithm
         IF (RVEC(IVEC) .EQ. 0.)  RVEC(IVEC) = TWOM24*TWOM24
      ENDIF
C        Skipping to luxury.  As proposed by Martin Luscher.
      IN24 = IN24 + 1
      IF (IN24 .EQ. 24)  THEN
         IN24 = 0
         KOUNT = KOUNT + NSKIP
         DO 90 ISK= 1, NSKIP
         UNI = SEEDS(J24) - SEEDS(I24) - CARRY
         IF (UNI .LT. 0.)  THEN
            UNI = UNI + 1.0
            CARRY = TWOM24
         ELSE
            CARRY = 0.
         ENDIF
         SEEDS(I24) = UNI
         I24 = NEXT(I24)
         J24 = NEXT(J24)
   90    CONTINUE
      ENDIF
  100 CONTINUE
      KOUNT = KOUNT + LENV
      IF (KOUNT .GE. IGIGA)  THEN
         MKOUNT = MKOUNT + 1
         KOUNT = KOUNT - IGIGA
      ENDIF
      RETURN
C
C           Entry to input and float integer seeds from previous run
      ENTRY RLUXIN(ISDEXT)
*     IF block added by Phillip Helbig after correpondence with James
      IF (NOTYET) THEN
         WRITE(6,'(A)')  ' PROPER RESULTS ONLY WITH INITIALISATION FROM 
     $25 INTEGERS OBTAINED WITH RLUXUT'
         NOTYET = .FALSE.
      ENDIF
         TWOM24 = 1.
         DO 195 I= 1, 24
         NEXT(I) = I-1
  195    TWOM24 = TWOM24 * 0.5
         NEXT(1) = 24
         TWOM12 = TWOM24 * 4096.
      WRITE(6,'(A)') ' FULL INITIALIZATION OF RANLUX WITH 25 INTEGERS:'
      WRITE(6,'(5X,5I12)') ISDEXT
      DO 200 I= 1, 24
      SEEDS(I) = REAL(ISDEXT(I))*TWOM24
  200 CONTINUE
      CARRY = 0.
      IF (ISDEXT(25) .LT. 0)  CARRY = TWOM24
      ISD = IABS(ISDEXT(25))
      I24 = MOD(ISD,100)
      ISD = ISD/100
      J24 = MOD(ISD,100)
      ISD = ISD/100
      IN24 = MOD(ISD,100)
      ISD = ISD/100
      LUXLEV = ISD
        IF (LUXLEV .LE. MAXLEV) THEN
          NSKIP = NDSKIP(LUXLEV)
          WRITE (6,'(A,I2)') ' RANLUX LUXURY LEVEL SET BY RLUXIN TO: ',
     +                         LUXLEV
        ELSE  IF (LUXLEV .GE. 24) THEN
          NSKIP = LUXLEV - 24
          WRITE (6,'(A,I5)') ' RANLUX P-VALUE SET BY RLUXIN TO:',LUXLEV
        ELSE
          NSKIP = NDSKIP(MAXLEV)
          WRITE (6,'(A,I5)') ' RANLUX ILLEGAL LUXURY RLUXIN: ',LUXLEV
          LUXLEV = MAXLEV
        ENDIF
      INSEED = -1
      RETURN
C
C                    Entry to ouput seeds as integers
      ENTRY RLUXUT(ISDEXT)
      DO 300 I= 1, 24
         ISDEXT(I) = INT(SEEDS(I)*TWOP12*TWOP12)
  300 CONTINUE
      ISDEXT(25) = I24 + 100*J24 + 10000*IN24 + 1000000*LUXLEV
      IF (CARRY .GT. 0.)  ISDEXT(25) = -ISDEXT(25)
      RETURN
C
C                    Entry to output the "convenient" restart point
      ENTRY RLUXAT(LOUT,INOUT,K1,K2)
      LOUT = LUXLEV
      INOUT = INSEED
      K1 = KOUNT
      K2 = MKOUNT
      RETURN
C
C                    Entry to initialize from one or three integers
      ENTRY RLUXGO(LUX,INS,K1,K2)
         IF (LUX .LT. 0) THEN
            LUXLEV = LXDFLT
         ELSE IF (LUX .LE. MAXLEV) THEN
            LUXLEV = LUX
         ELSE IF (LUX .LT. 24 .OR. LUX .GT. 2000) THEN
            LUXLEV = MAXLEV
            WRITE (6,'(A,I7)') ' RANLUX ILLEGAL LUXURY RLUXGO: ',LUX
         ELSE
            LUXLEV = LUX
            DO 310 ILX= 0, MAXLEV
              IF (LUX .EQ. NDSKIP(ILX)+24)  LUXLEV = ILX
  310       CONTINUE
         ENDIF
      IF (LUXLEV .LE. MAXLEV)  THEN
         NSKIP = NDSKIP(LUXLEV)
         WRITE(6,'(A,I2,A,I4)') ' RANLUX LUXURY LEVEL SET BY RLUXGO :',
     +        LUXLEV,'     P=', NSKIP+24
      ELSE
          NSKIP = LUXLEV - 24
          WRITE (6,'(A,I5)') ' RANLUX P-VALUE SET BY RLUXGO TO:',LUXLEV
      ENDIF
      IN24 = 0
      IF (INS .LT. 0)  WRITE (6,'(A)')   
     +   ' Illegal initialization by RLUXGO, negative input seed'
      IF (INS .GT. 0)  THEN
        JSEED = INS
        WRITE(6,'(A,3I12)') ' RANLUX INITIALIZED BY RLUXGO FROM SEEDS',
     +      JSEED, K1,K2
      ELSE
        JSEED = JSDFLT
        WRITE(6,'(A)')' RANLUX INITIALIZED BY RLUXGO FROM DEFAULT SEED'
      ENDIF
      INSEED = JSEED
      NOTYET = .FALSE.
      TWOM24 = 1.
         DO 325 I= 1, 24
           TWOM24 = TWOM24 * 0.5
         K = JSEED/53668
         JSEED = 40014*(JSEED-K*53668) -K*12211
         IF (JSEED .LT. 0)  JSEED = JSEED+ICONS
         ISEEDS(I) = MOD(JSEED,ITWO24)
  325    CONTINUE
      TWOM12 = TWOM24 * 4096.
         DO 350 I= 1,24
         SEEDS(I) = REAL(ISEEDS(I))*TWOM24
         NEXT(I) = I-1
  350    CONTINUE
      NEXT(1) = 24
      I24 = 24
      J24 = 10
      CARRY = 0.
      IF (SEEDS(24) .EQ. 0.) CARRY = TWOM24
C        If restarting at a break point, skip K1 + IGIGA*K2
C        Note that this is the number of numbers delivered to
C        the user PLUS the number skipped (if luxury .GT. 0).
      KOUNT = K1
      MKOUNT = K2
      IF (K1+K2 .NE. 0)  THEN
        DO 500 IOUTER= 1, K2+1
          INNER = IGIGA
          IF (IOUTER .EQ. K2+1)  INNER = K1
          DO 450 ISK= 1, INNER
            UNI = SEEDS(J24) - SEEDS(I24) - CARRY 
            IF (UNI .LT. 0.)  THEN
               UNI = UNI + 1.0
               CARRY = TWOM24
            ELSE
               CARRY = 0.
            ENDIF
            SEEDS(I24) = UNI
            I24 = NEXT(I24)
            J24 = NEXT(J24)
  450     CONTINUE
  500   CONTINUE
C         Get the right value of IN24 by direct calculation
        IN24 = MOD(KOUNT, NSKIP+24)
        IF (MKOUNT .GT. 0)  THEN
           IZIP = MOD(IGIGA, NSKIP+24)
           IZIP2 = MKOUNT*IZIP + IN24
           IN24 = MOD(IZIP2, NSKIP+24)
        ENDIF
C       Now IN24 had better be between zero and 23 inclusive
        IF (IN24 .GT. 23) THEN
           WRITE (6,'(A/A,3I11,A,I5)')  
     +    '  Error in RESTARTING with RLUXGO:','  The values', INS,
     +     K1, K2, ' cannot occur at luxury level', LUXLEV
           IN24 = 0
        ENDIF
      ENDIF
      RETURN
      END

*##############################################################
*                MERSENNE TWISTER
*==============================================================
* A C-program for MT19937: Real number version
*   genrand() generates one pseudorandom real number (double)
* which is uniformly distributed on [0,1]-interval, for each
* call. sgenrand(seed) set initial values to the working area
* of 624 words. Before genrand(), sgenrand(seed) must be
* called once. (seed is any 32-bit integer except for 0).
* Integer generator is obtained by modifying two lines.
*   Coded by Takuji Nishimura, considering the suggestions by
* Topher Cooper and Marc Rieffel in July-Aug. 1997.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Library General Public
* License as published by the Free Software Foundation; either
* version 2 of the License, or (at your option) any later
* version.
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
* See the GNU Library General Public License for more details.
* You should have received a copy of the GNU Library General
* Public License along with this library; if not, write to the
* Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
* 02111-1307  USA
*
* Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.
* When you use this, send an email to: matumoto@math.keio.ac.jp
* with an appropriate reference to your work.
*
************************************************************************
* Fortran translation by Hiroshi Takano.  Jan. 13, 1999.
*
*   genrand()      -> double precision function grnd()
*   sgenrand(seed) -> subroutine sgrnd(seed)
*                     integer seed
*
* This program uses the following non-standard intrinsics.
*   ishft(i,n): If n>0, shifts bits in i by n positions to left.
*               If n<0, shifts bits in i by n positions to right.
*   iand (i,j): Performs logical AND on corresponding bits of i and j.
*   ior  (i,j): Performs inclusive OR on corresponding bits of i and j.
*   ieor (i,j): Performs exclusive OR on corresponding bits of i and j.
*
************************************************************************
      subroutine sgrnd(seed)
*
      implicit integer(a-z)
*
* Period parameters
      parameter(N     =  624)
*
      dimension mt(0:N-1)
*                     the array for the state vector
      common /block/mti,mt
      save   /block/
*
*      setting initial seeds to mt[N] using
*      the generator Line 25 of Table 1 in
*      [KNUTH 1981, The Art of Computer Programming
*         Vol. 2 (2nd Ed.), pp102]
*
      mt(0)= iand(seed,-1)
      do 1000 mti=1,N-1
        mt(mti) = iand(69069 * mt(mti-1),-1)
 1000 continue
*
      return
      end
************************************************************************
      double precision function grnd()
*
      implicit integer(a-z)
*
* Period parameters
      parameter(N     =  624)
      parameter(N1    =  N+1)
      parameter(M     =  397)
      parameter(MATA  = -1727483681)
*                                    constant vector a
      parameter(UMASK = -2147483647)
*                                    most significant w-r bits
      parameter(LMASK =  2147483647)
*                                    least significant r bits
* Tempering parameters
      parameter(TMASKB= -1658038656)
      parameter(TMASKC= -272236544)
*
      dimension mt(0:N-1)
*                     the array for the state vector
      common /block/mti,mt
      save   /block/
      data   mti/N1/
*                     mti==N+1 means mt[N] is not initialized
*
      dimension mag01(0:1)
      data mag01/0, MATA/
      save mag01
*                        mag01(x) = x * MATA for x=0,1
*
      TSHFTU(y)=ishft(y,-11)
      TSHFTS(y)=ishft(y,7)
      TSHFTT(y)=ishft(y,15)
      TSHFTL(y)=ishft(y,-18)
*
      if(mti.ge.N) then
*                       generate N words at one time
        if(mti.eq.N+1) then
*                            if sgrnd() has not been called,
          call sgrnd(4357)
*                              a default initial seed is used
        endif
*
        do 1000 kk=0,N-M-1
            y=ior(iand(mt(kk),UMASK),iand(mt(kk+1),LMASK))
            mt(kk)=ieor(ieor(mt(kk+M),ishft(y,-1)),mag01(iand(y,1)))
 1000   continue
        do 1100 kk=N-M,N-2
            y=ior(iand(mt(kk),UMASK),iand(mt(kk+1),LMASK))
            mt(kk)=ieor(ieor(mt(kk+(M-N)),ishft(y,-1)),mag01(iand(y,1)))
 1100   continue
        y=ior(iand(mt(N-1),UMASK),iand(mt(0),LMASK))
        mt(N-1)=ieor(ieor(mt(M-1),ishft(y,-1)),mag01(iand(y,1)))
        mti = 0
      endif
*
      y=mt(mti)
      mti=mti+1
      y=ieor(y,TSHFTU(y))
      y=ieor(y,iand(TSHFTS(y),TMASKB))
      y=ieor(y,iand(TSHFTT(y),TMASKC))
      y=ieor(y,TSHFTL(y))
*
      if(y.lt.0) then
        grnd=(dble(y)+2.0d0**32)/(2.0d0**32-1.0d0)
      else
        grnd=dble(y)/(2.0d0**32-1.0d0)
      endif
*
      return
      end
