
      DOUBLE PRECISION FUNCTION PHO_PARTONX12(Iprtn,Xx,Qq)
 
C  Given the parton distribution function in the array U in
C  COMMON / PEVLDT / , this routine interpolates to find
C  the parton distribution at an arbitray point in x and q.
C
      IMPLICIT NONE
      DOUBLE PRECISION ALScteq , const1 , const2 , const3 , const4 , 
     &                 const5 , const6 , ff , fij , fvec , fx , g1 , 
     &                 g4 , h00 , onep , q , QBAse , QINi , QMAx , Qq
      DOUBLE PRECISION s12 , s1213 , s13 , s23 , s24 , s2434 , s34 , 
     &                 sdet , sf2 , sf3 , ss , svec1 , svec2 , svec3 , 
     &                 svec4 , sy2 , sy3 , t12 , t13 , t23
      DOUBLE PRECISION t24 , t34 , tdet , tf2 , tf3 , tmp , tmp1 , 
     &                 tmp2 , tt , TV , tvec1 , tvec2 , tvec3 , tvec4 , 
     &                 ty2 , ty3 , UPD , x , XMIn , xpow
      DOUBLE PRECISION XV , xvpow , Xx
      INTEGER i , ientry , ip , IPDsformat , IPDsset , Iprtn , ISEtch , 
     &        it , j1 , jlq , jlx , jm , jq , jtmp , ju , jx , lout , 
     &        MAXVAL , MXF , MXPQX
      INTEGER MXQ , MXVal , MXX , NFMx , nqvec , NT , NX
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      PARAMETER (MXX=201,MXQ=40,MXF=6,MAXVAL=4)
      PARAMETER (MXPQX=(MXF+1+MAXVAL)*MXQ*MXX)
 
      COMMON /CTQPAR1/ QBAse , XV(0:MXX) , TV(0:MXQ) , UPD(MXPQX) , 
     &                 ALScteq(0:MXQ)/CTQPAR2/ NX , NT , NFMx , 
     &                 MXVal/XQRANGE/ QINi , QMAx , 
     &                 XMIn/SETCHANGE/ ISEtch , IPDsset , IPDsformat
 
      DIMENSION fvec(4) , fij(4)
      DIMENSION xvpow(0:MXX)
      DATA onep/1.00001/
      DATA xpow/0.3D0/          !**** choice of interpolation variable
      DATA nqvec/4/
      DATA ientry/0/
      DATA x , q , jx , jq/ - 1D0 , -1D0 , 0 , 0/
      SAVE xvpow
      SAVE x , q , jx , jq , jlx , jlq
      SAVE ss , const1 , const2 , const3 , const4 , const5 , const6
      SAVE sy2 , sy3 , s23 , tt , t12 , t13 , t23 , t24 , t34 , ty2 , 
     &   ty3
      SAVE tmp1 , tmp2 , tdet
 
C store the powers used for interpolation on first call...
      IF ( ISEtch.EQ.1 ) THEN
         ISEtch = 0
 
         xvpow(0) = 0D0
         DO i = 1 , NX
            xvpow(i) = XV(i)**xpow
         END DO
      ELSE IF ( (Xx.EQ.x) .AND. (Qq.EQ.q) ) THEN
         GOTO 300
      END IF
 
      x = Xx
      q = Qq
      tt = LOG(LOG(q/QBAse))
 
C      -------------    find lower end of interval containing x, i.e.,
C                       get jx such that xv(jx) .le. x .le. xv(jx+1)...
      jlx = -1
      ju = NX + 1
 100  IF ( ju-jlx.GT.1 ) THEN
         jm = (ju+jlx)/2
         IF ( x.GE.XV(jm) ) THEN
            jlx = jm
         ELSE
            ju = jm
         END IF
         GOTO 100
      END IF
C                     Ix    0   1   2      Jx  JLx         Nx-2     Nx
C                           |---|---|---|...|---|-x-|---|...|---|---|
C                     x     0  Xmin               x                 1
C
      IF ( jlx.LE.-1 ) THEN
         IF ( LPRi.GT.4 ) WRITE (lout,'(A,1pE12.4)')
     &         'Severe error: x <= 0 in pho_PartonX12! x = ' , x
         STOP
      ELSE IF ( jlx.EQ.0 ) THEN
         jx = 0
      ELSE IF ( jlx.LE.NX-2 ) THEN
 
C                For interrior points, keep x in the middle, as shown above
         jx = jlx - 1
      ELSE IF ( jlx.EQ.NX-1 .OR. x.LT.onep ) THEN
 
C                  We tolerate a slight over-shoot of one (OneP=1.00001),
C              perhaps due to roundoff or whatever, but not more than that.
C                                      Keep at least 4 points >= Jx
         jx = jlx - 2
      ELSE
         IF ( LPRi.GT.4 ) WRITE (lout,'(A,1pE12.4)')
     &         'Severe error: x > 1 in pho_PartonX12! x = ' , x
         STOP
      END IF
C          ---------- Note: JLx uniquely identifies the x-bin; Jx does not.
 
C                       This is the variable to be interpolated in
      ss = x**xpow
 
      IF ( jlx.GE.2 .AND. jlx.LE.NX-2 ) THEN
 
C     initiation work for "interior bins": store the lattice points in s...
         svec1 = xvpow(jx)
         svec2 = xvpow(jx+1)
         svec3 = xvpow(jx+2)
         svec4 = xvpow(jx+3)
 
         s12 = svec1 - svec2
         s13 = svec1 - svec3
         s23 = svec2 - svec3
         s24 = svec2 - svec4
         s34 = svec3 - svec4
 
         sy2 = ss - svec2
         sy3 = ss - svec3
 
C constants needed for interpolating in s at fixed t lattice points...
         const1 = s13/s23
         const2 = s12/s23
         const3 = s34/s23
         const4 = s24/s23
         s1213 = s12 + s13
         s2434 = s24 + s34
         sdet = s12*s34 - s1213*s2434
         tmp = sy2*sy3/sdet
         const5 = (s34*sy2-s2434*sy3)*tmp/s12
         const6 = (s1213*sy2-s12*sy3)*tmp/s34
 
      END IF
 
C         --------------Now find lower end of interval containing Q, i.e.,
C                          get jq such that qv(jq) .le. q .le. qv(jq+1)...
      jlq = -1
      ju = NT + 1
 200  IF ( ju-jlq.GT.1 ) THEN
         jm = (ju+jlq)/2
         IF ( tt.GE.TV(jm) ) THEN
            jlq = jm
         ELSE
            ju = jm
         END IF
         GOTO 200
      END IF
 
      IF ( jlq.LE.0 ) THEN
         jq = 0
      ELSE IF ( jlq.LE.NT-2 ) THEN
C                                  keep q in the middle, as shown above
         jq = jlq - 1
      ELSE
C                         JLq .GE. Nt-1 case:  Keep at least 4 points >= Jq.
         jq = NT - 3
 
      END IF
C                                   This is the interpolation variable in Q
 
      IF ( jlq.GE.1 .AND. jlq.LE.NT-2 ) THEN
C                                        store the lattice points in t...
         tvec1 = TV(jq)
         tvec2 = TV(jq+1)
         tvec3 = TV(jq+2)
         tvec4 = TV(jq+3)
 
         t12 = tvec1 - tvec2
         t13 = tvec1 - tvec3
         t23 = tvec2 - tvec3
         t24 = tvec2 - tvec4
         t34 = tvec3 - tvec4
 
         ty2 = tt - tvec2
         ty3 = tt - tvec3
 
         tmp1 = t12 + t13
         tmp2 = t24 + t34
 
         tdet = t12*t34 - tmp1*tmp2
 
      END IF
 
 
C get the pdf function values at the lattice points...
 
 300  IF ( Iprtn.GT.MXVal ) THEN
         ip = -Iprtn
      ELSE
         ip = Iprtn
      END IF
      jtmp = ((ip+NFMx)*(NT+1)+(jq-1))*(NX+1) + jx + 1
 
      DO it = 1 , nqvec
         j1 = jtmp + it*(NX+1)
 
         IF ( jx.EQ.0 ) THEN
C                      For the first 4 x points, interpolate x^2*f(x,Q)
C                      This applies to the two lowest bins JLx = 0, 1
C            We can not put the JLx.eq.1 bin into the "interrior" section
C                           (as we do for q), since Upd(J1) is undefined.
            fij(1) = 0
            fij(2) = UPD(j1+1)*XV(1)**2
            fij(3) = UPD(j1+2)*XV(2)**2
            fij(4) = UPD(j1+3)*XV(3)**2
C
C                 Use Polint which allows x to be anywhere w.r.t. the grid
 
            CALL PHO_POLINT4F(xvpow(0),fij(1),ss,fx)
 
            IF ( x.GT.0D0 ) fvec(it) = fx/x**2
C                                              Pdf is undefined for x.eq.0
         ELSE IF ( jlx.EQ.NX-1 ) THEN
C                                                This is the highest x bin:
 
            CALL PHO_POLINT4F(xvpow(NX-3),UPD(j1),ss,fx)
 
            fvec(it) = fx
 
         ELSE
C                       for all interior points, use Jon's in-line function
C                              This applied to (JLx.Ge.2 .and. JLx.Le.Nx-2)
            sf2 = UPD(j1+1)
            sf3 = UPD(j1+2)
 
            g1 = sf2*const1 - sf3*const2
            g4 = -sf2*const3 + sf3*const4
 
            fvec(it) = (const5*(UPD(j1)-g1)+const6*(UPD(j1+3)-g4)
     &                 +sf2*sy3-sf3*sy2)/s23
 
         END IF
 
      END DO
C                                   We now have the four values Fvec(1:4)
C     interpolate in t...
 
      IF ( jlq.LE.0 ) THEN
C                         1st Q-bin, as well as extrapolation to lower Q
         CALL PHO_POLINT4F(TV(0),fvec(1),tt,ff)
 
      ELSE IF ( jlq.GE.NT-1 ) THEN
C                         Last Q-bin, as well as extrapolation to higher Q
         CALL PHO_POLINT4F(TV(NT-3),fvec(1),tt,ff)
      ELSE
C                         Interrior bins : (JLq.GE.1 .and. JLq.LE.Nt-2)
C       which include JLq.Eq.1 and JLq.Eq.Nt-2, since Upd is defined for
C                         the full range QV(0:Nt)  (in contrast to XV)
         tf2 = fvec(2)
         tf3 = fvec(3)
 
         g1 = (tf2*t13-tf3*t12)/t23
         g4 = (-tf2*t34+tf3*t24)/t23
 
         h00 = ((t34*ty2-tmp2*ty3)*(fvec(1)-g1)/t12+(tmp1*ty2-t12*ty3)
     &         *(fvec(4)-g4)/t34)
 
         ff = (h00*ty2*ty3/tdet+tf2*ty3-tf3*ty2)/t23
      END IF
 
      PHO_PARTONX12 = ff
 
C                                       ********************
      END FUNCTION
