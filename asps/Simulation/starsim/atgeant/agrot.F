!//#define AGROTF_TEST
#ifdef AGROTF_TEST
      program test
      real gang(6)
      real gaTst(6)/159, 180, 93.9209, 79.7145, 110.596, 348.239/    
      real gaUni(6)/90.0,    0.0,   90.0,   90.0,    0.0,    0.0/    
      real xax(3) /1,0,0/
      real RMX(3,3)
      integer iax(3)/1,2,3/      
      real    ang(3)/11,-222,333/

#if 1
      write(*,*)'gaTst=',gaTst
      call GVGAMX(gaTst,RMX)
      write(*,*) 'RMX ',rmx(1,1),rmx(2,1),rmx(3,1)
      write(*,*) 'RMX ',rmx(1,2),rmx(2,2),rmx(3,2)
      write(*,*) 'RMX ',rmx(1,3),rmx(2,3),rmx(3,3)
      write(*,*) ' '

      call GVMXGA(RMX,gang)
*      write(*,*)'GANG=',gang
      call GYROT (xax, 111., RMX)
      write(*,*) 'RMX2',rmx(1,1),rmx(2,1),rmx(3,1)
      write(*,*) 'RMX2',rmx(1,2),rmx(2,2),rmx(3,2)
      write(*,*) 'RMX2',rmx(1,3),rmx(2,3),rmx(3,3)
      write(*,*) ' '


#endif

#if 1
      call ucopy(gaUni,gang,6)
**      gang(1) = -999
      call AGROTS(iax,ang,3,gang)
      write(*,*) 
     +'CORRECT angles:132 153 98.1519 55.5894 136.844 316.802'     
      write(*,*)'GANG=',gang
#endif

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
*       into GEANT 6 angles                                            *
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
        if (abs(the-ANINT(the)).le.1e-4) the = ANINT(the)
        if (abs(phi-ANINT(phi)).le.1e-4) phi = ANINT(phi)
        if (the .lt.0) the = the +360
        if (the .gt.180) then
	  the = 360-the
	  phi = phi +180
	endif
        if (phi .lt.  0) phi = phi + 360
        if (phi .gt.360) phi = phi - 360
	gang(2*N-1)=the
	gang(2*N-0)=phi
      enddo
      end

      SUBROUTINE GYROT (DIRC, ANGP, RMX)
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
*       TRANZ   rotation & shift matrix 3*3  (INPUT/OUTPUT)            *
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
	RMXt(J2,J1) = TRANS(1,J1)*RMX(j2,1)
     +	            + TRANS(2,J1)*RMX(j2,2)
     +	            + TRANS(3,J1)*RMX(j2,3)
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
      AX(3)= Zax
      call GYROT(AX,angp,RotM)
      call gvmxga(RotM,gang)
     
      end  

      SUBROUTINE AGROTS(iax,ang,nang,gang)
************************************************************************
*                                                                      *
*   AGROTS (iax,ang,nang,gang)                               VP Nov2008*
*       Routine for 3 Rotation around X,Y,Z                            *
*       output in form GEANT 6 angles array                            *
*                                                                      *
*   ARGUMENTS:                                                         *
*       iAx     array with axis numbers. 1,2,3 = X,Y,Z                 *
*       ang     array with rotation angles in degrees around this axis *
*       nang    number of rotations             		       *
*       gang    array with geant angles:			       *
*               THETAx,PHIx,THETAy,PHIy,THETAz,PHIz (in  degrees )     *
*                                                                      *
************************************************************************
      integer iAx(*)
      real    gang(6),gangI(6),ang(*)
      real    ax(3),RMX(3,3)
      integer i,k

      call GVGAMX(gang,RMX)
      do 100 i=1,nang
        k = iAx(i)
        if (k.eq.0) goto 100
        if (k.le.3) then
          call vzero(ax,3)
	  ax(iAx(i)) = 1
 	  call GYROT (ax, ang(i), RMX)
        else 
          call GVMXGA(RMX,gangI)
          call OLEG(k,gangI,gang)
          call GVGAMX(gang,RMX)
        endif
 100  continue
      call GVMXGA(RMX,gang)
      end

      integer function IGOR(ort)
************************************************************************
*                                                                      *
*   IGOR (ort)                                               VP Nov2008*
*       function to encode orts                                        *
*       orts could be XYZ,YZX,-Z-YX etc                                *
*                                                                      *
*   ARGUMENTS:                                                         *
*       ort     character string:			               *
*                                                                      *
************************************************************************
      character *(*) ort     
      integer pw2(3) /1,2,4/
      integer pw10(3) /1,10,100/
      integer i,j,minus,ix,msk
      
      igor = 0
      j = 0
      msk = 0
      do i=1,3
        j = j+1
        minus = 0
	if (ort(j:j) .eq. '-') then
	  j = j+1
	  minus = 3
	endif
        ix = index('XYZxyz',ort(j:j))
        if (ix.gt.3) ix = ix-3
        if (ix<1 .or. ix>3) then
	  write(*,*) '*** WRONG ORT = ',ort,  '***'
          igor = 0
          return
        endif
        igor = igor + pw10(i)*(minus+ix)
        msk = msk + pw2(ix)
      enddo
      if (msk .ne.7) then
	write(*,*) '*** WRONG ORT = ',ort,  '***'
        igor = 0
      endif
      end
      subroutine OLEG(orti,gangI,gang)
************************************************************************
*                                                                      *
*   OLEG (orti,gang)                                         VP Nov2008*
*                                                                      *
*       calculate geant angles vector from encoded ort information     *
*       returnned by IGOR(ort)                                         *
*                                                                      *
*   ARGUMENTS:                                                         *
*       orti     encoded by IGOR number:  (input)		       *
*       gangI    initial  6 geant angles: (input)	               *
*       gang     resuling 6 geant angles: (output)                     *
*                                                                      *
************************************************************************
      integer 	orti,iort,i,j
      real   	gang(2,3),gangI(2,3)

      iort = orti
      if (iort.eq.0) iort = 321
      do i=1,3
        j = mod(iort,10)
        if (j<1 .or. j>6) then
	  write(*,*) '*** WRONG ORTI = ',orti,  '***'
          return
	endif
        if (j.le.3) then
	  gang(1,i) = gangI(1,j)
          gang(2,i) = gangI(2,j)
        else
	  gang(1,i) = 180 - gangI(1,j-3)
          gang(2,i) = 180 + gangI(2,j-3)
          if (gang(2,i).lt.-180) gang(2,i)=gang(2,i)+360
          if (gang(2,i).gt. 180) gang(2,i)=gang(2,i)-360
        endif
        iort = iort/10
      enddo
      end
