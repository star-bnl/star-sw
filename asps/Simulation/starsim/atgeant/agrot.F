#ifdef AGROTF_TEST
      program test
      real axi(3)/0,0,0/,RMX(3,3)
      real gang(6)
      
      
      call GVGAMX(-999.,RMX)
     
      axi(1) = 1;
      axi(2) = 1;
      axi(3) = 1;
      call GVROT (axi, 10., RMX)
      call GVROT (axi,-10., RMX)
      do n=1,3
      write(*,*) RMX(n,1),RMX(n,2),RMX(n,3)
      enddo

      gang(1)=-999
      call AGROT(gang,axi(1),axi(2),axi(3), 33.)
      call AGROT(gang,axi(1),axi(2),axi(3),-33.)
      write(*,*)'GANG=',gang


      end

#endif
      SUBROUTINE GVGAMX(gang,RMX)
************************************************************************
*                                                                      *
*   GVGAMX (gang,RMX)                             VP Oct2008           *
*       Routine converts GEANT 6 angles                                *
*       to Rotation Transformation Matrix                              *
*                                                                      *
*                                                                      *
*   ARGUMENTS:                                                         *
*       gang    array with geant angles:			       *
*               THETAx,PHIx,THETAy,PHIy,THETAz,PHIz (in  degrees )     *
*       RMX     rotation matrix                           *
*                                                                      *
************************************************************************
      implicit       none
      real           gang(6),xax,yax,zax,angp
      REAL           RMX(3,3),PI,RADS
      REAL           THERAD,PHIRAD,SINTHE,COSTHE,SINPHI,COSPHI
      integer        N
      PARAMETER      (PI = 3.141592653589793 , RADS = PI/180.)

      if (gang(1) .le.-999) then
        call vzero(RMX,9)
	RMX(1,1) = 1
	RMX(2,2) = 1
	RMX(3,3) = 1
      else
	DO N = 1,3
            THERAD = gang(2*N-1)*RADS
            PHIRAD = gang(2*N-0)*RADS
            SINTHE = SIN(THERAD)
            COSTHE = COS(THERAD)
            SINPHI = SIN(PHIRAD)
            COSPHI = COS(PHIRAD)
            RMX(N,1) = SINTHE * COSPHI
            RMX(N,2) = SINTHE * SINPHI
            RMX(N,3) = COSTHE
	enddo
      endif
      end
      SUBROUTINE GVMXGA(RMX,gang)
************************************************************************
*                                                                      *
*   GVMXGA (RMX,gang)                             VP Oct2008           *
*       converts Rotation Transformation Matrix                        *
*       into converts GEANT 6 angles                                   *
*                                                                      *
*                                                                      *
*   ARGUMENTS:                                                         *
*       RMX     rotation matrix                                        *
*       gang    array with geant angles:			       *
*               THETAx,PHIx,THETAy,PHIy,THETAz,PHIz (in  degrees )     *
*                                                                      *
************************************************************************
      implicit       none
      real           gang(6),xax,yax,zax,angp
      REAL           RMX(3,3),PI,DEGR
      REAL           THE,PHI,SINTHE,COSTHE,SINPHI,COSPHI
      integer        N
      PARAMETER      (PI = 3.141592653589793 , DEGR = 180./PI)
      DO N = 1,3
        COSTHE = RMX(N,3)
	PHI = 0
        if      (COSTHE-1.ge.-1e-6) then
	  THE = 0
        else if (COSTHE+1.le. 1e-6) then
	  THE = 180
        else
          SINPHI = RMX(N,2)
	  COSPHI = RMX(N,1)
	  PHI = atan2(SINPHI,COSPHI)*DEGR
          THE = atan2(sqrt(SINPHI**2+COSPHI**2),COSTHE)*DEGR
        endif
        if (abs(the-ANINT(the)).le.1e-5) the = ANINT(the)
        if (abs(phi-ANINT(phi)).le.1e-5) phi = ANINT(phi)
	gang(2*N-1)=the
	gang(2*N-0)=phi
      enddo
      end

      SUBROUTINE GVROT (DIRC, ANGP, RMX)
************************************************************************
*                                                                      *
*   CALL GYROT(DIRC,ANGP,TRANS)                              VP 880722 *
*                                       REVISED              VP 921009 *
*       Routine for filling Rotation Transformation Matrix             *
*       from axis and rotation angle around                            *
*                                                                      *
*   ARGUMENTS:                                                         *
*       DIRC    direct cosinuses (may be not normalised)               *
*       ANGP    rotation angle in degrees                              *
*       TRANZ   rotation & shift matrix 4*4  (INPUT/OUTPUT)            *
*                                                                      *
************************************************************************
      REAL           DIRC(3),TRANS(3,3),AX(3),RMX(3,3),RMXt(3,3)
      PARAMETER      (PI = 3.141592653589793 , RADS = PI/180.)
 
      ANG = -ANGP*RADS
 
      CALL VUNIT (DIRC, AX, 3)
 
      CA  = COS(ANG)
      SA  = SIN(ANG)
      IF (CA.LT.0.5)            THEN
        CA1 = 1. - CA
      ELSE
        CA1 = SA**2/(1.+CA)
      ENDIF
 
      DO 120 J1 = 1,3
        DO 100 J2 = 1,3
  100     TRANS(J1,J2) = CA1*AX(J1)*AX(J2)
        TRANS(J1,J1)   = TRANS(J1,J1) + CA
  120   CONTINUE
 
      TRANS(1,2) = TRANS(1,2) - SA*AX(3)
      TRANS(2,1) = TRANS(2,1) + SA*AX(3)
      TRANS(1,3) = TRANS(1,3) + SA*AX(2)
      TRANS(3,1) = TRANS(3,1) - SA*AX(2)
      TRANS(2,3) = TRANS(2,3) - SA*AX(1)
      TRANS(3,2) = TRANS(3,2) + SA*AX(1)
*

      do J1 = 1,3
      do J2 = 1,3
        S=0
        do J3=1,3
	  S = S + TRANS(J1,J3)*RMX(j3,J2)
        enddo
	RMXt(J1,J2) = S
      enddo
      enddo
      call ucopy (RMXt,RMX,9)

      RETURN
      END

      SUBROUTINE AGROT (gang,xax,yax,zax,angp)
************************************************************************
*                                                                      *
*   AGROT (gang,xax,yax,zax,ang)                             VP Oct2008*
*       Routine for Rotation Transformation Matrix                     *
*       fin form GEANT 6 angles array                                  *
*                                                                      *
*   ARGUMENTS:                                                         *
*       gang    array with geant angles:			       *
*               THETAx,PHIx,THETAy,PHIy,THETAz,PHIz (in  degrees )     *
*       Xax,Yax,Zax direct cosinuses (may be not normalised) of axis   *
*       DIRC    direct cosinuses (may be not normalised)               *
*       ANGP    rotation angle in degrees around axis                            *
*                                                                      *
************************************************************************
      implicit       none
      real           gang(6),xax,yax,zax,angp
      REAL           AX(3),RotM(3,3)
      
      call gvgamx(gang,RotM)
      AX(1)= Xax
      AX(2)= Yax
      AX(1)= Zax
      call gvrot(AX,angp,RotM)
      call gvmxga(RotM,gang)
     
      end  

     
