
      SUBROUTINE PHO_GRSCALC(X,Q2,P2,Ugam,Dgam,Sgam,Ggam)
      IMPLICIT NONE
      DOUBLE PRECISION a , alp , b , bet , Dgam , ds1 , dsp0 , dspart1 , 
     &                 g1 , g2 , ga , gb , gc , gd , ge , gep , Ggam , 
     &                 gp0 , gpart1 , gpart2
      DOUBLE PRECISION lam2 , lp1 , lp2 , mu2 , P2 , PHO_GRSF1 , 
     &                 PHO_GRSF2 , Q2 , s , s2 , Sgam , spart2 , suppr , 
     &                 u1 , ud2 , udpart2 , Ugam , up0 , upart1 , X
      SAVE 
 
      DIMENSION u1(40) , ds1(40) , g1(40)
      DIMENSION ud2(20) , s2(20) , g2(20)
      DIMENSION up0(20) , dsp0(20) , gp0(20)
C
      DATA u1/ - 0.139D0 , 0.783D0 , 0.132D0 , 0.087D0 , 0.003D0 , 
     &     -0.0134D0 , 0.009D0 , -0.017D0 , 0.092D0 , -0.516D0 , 
     &     -0.085D0 , 0.439D0 , 0.013D0 , 0.108D0 , -0.019D0 , 
     &     -0.272D0 , -0.167D0 , 0.138D0 , 0.076D0 , 0.026D0 , 
     &     -0.013D0 , 0.27D0 , 0.107D0 , -0.097D0 , 0.04D0 , 0.064D0 , 
     &     0.011D0 , 0.002D0 , 0.057D0 , -0.057D0 , 0.162D0 , -0.172D0 , 
     &     0.124D0 , -0.016D0 , -0.065D0 , 0.044D0 , -1.009D0 , 
     &     0.622D0 , 0.227D0 , -0.184D0/
      DATA ds1/0.033D0 , 0.007D0 , -0.0516D0 , 0.12D0 , 0.001D0 , 
     &     -0.013D0 , 0.018D0 , -0.028D0 , 0.102D0 , -0.595D0 , 
     &     -0.114D0 , 0.669D0 , 0.022D0 , 0.001D0 , -0.003D0 , 
     &     -0.0583D0 , -0.041D0 , 0.035D0 , 0.009D0 , 0.009D0 , 
     &     0.004D0 , 0.054D0 , 0.025D0 , -0.02D0 , 0.007D0 , 0.021D0 , 
     &     0.01D0 , 0.004D0 , -0.067D0 , 0.06D0 , -0.148D0 , 0.13D0 , 
     &     0.032D0 , -0.009D0 , -0.06D0 , 0.036D0 , -0.39D0 , 0.033D0 , 
     &     0.245D0 , -0.171D0/
      DATA g1/0.025D0 , 0.D0 , -0.018D0 , 0.112D0 , -0.025D0 , 0.177D0 , 
     &     -0.022D0 , 0.024D0 , 0.001D0 , -0.0104D0 , 0.D0 , 0.D0 , 
     &     -1.082D0 , -1.666D0 , 0.D0 , 0.086D0 , 0.D0 , 0.053D0 , 
     &     0.005D0 , -0.058D0 , 0.034D0 , 0.073D0 , 1.08D0 , 1.63D0 , 
     &     -0.0256D0 , -0.088D0 , 0.D0 , 0.D0 , -0.004D0 , 0.016D0 , 
     &     0.007D0 , -0.012D0 , 0.01D0 , -0.673D0 , 0.126D0 , -0.167D0 , 
     &     0.032D0 , -0.227D0 , 0.086D0 , -0.159D0/
      DATA ud2/0.756D0 , 0.187D0 , 0.109D0 , -0.163D0 , 0.002D0 , 
     &     0.004D0 , 0.054D0 , -0.039D0 , 22.53D0 , -21.02D0 , 5.608D0 , 
     &     0.332D0 , -0.008D0 , -0.021D0 , 0.381D0 , 0.572D0 , 4.774D0 , 
     &     1.436D0 , -0.614D0 , 3.548D0/
      DATA s2/0.902D0 , 0.182D0 , 0.271D0 , -0.346D0 , 0.017D0 , 
     &     -0.01D0 , -0.011D0 , 0.0065D0 , 17.1D0 , -13.29D0 , 6.519D0 , 
     &     0.031D0 , -0.0176D0 , 0.003D0 , 1.243D0 , 0.804D0 , 4.709D0 , 
     &     1.499D0 , -0.48D0 , 3.401D0/
      DATA g2/0.364D0 , 1.31D0 , 0.86D0 , -0.254D0 , 0.611D0 , 0.008D0 , 
     &     -0.097D0 , -2.412D0 , -0.843D0 , 2.248D0 , -0.201D0 , 
     &     1.33D0 , 0.572D0 , 0.44D0 , 1.233D0 , 0.009D0 , 0.954D0 , 
     &     1.862D0 , 3.791D0 , -0.079D0/
      DATA up0/1.551D0 , 0.105D0 , 1.089D0 , -0.172D0 , 3.822D0 , 
     &     -2.162D0 , 0.533D0 , -0.467D0 , -0.412D0 , 0.2D0 , 0.377D0 , 
     &     0.299D0 , 0.487D0 , 0.0766D0 , 0.119D0 , 0.063D0 , 7.605D0 , 
     &     0.234D0 , -0.567D0 , 2.294D0/
      DATA dsp0/2.484D0 , 1.214D0 , 1.088D0 , -0.1735D0 , 4.293D0 , 
     &     -2.802D0 , 0.5975D0 , -0.1193D0 , -0.0872D0 , 0.0418D0 , 
     &     0.128D0 , 0.0337D0 , 0.127D0 , 0.0135D0 , 0.14D0 , 0.0423D0 , 
     &     6.946D0 , 0.814D0 , 1.531D0 , 0.124D0/
      DATA gp0/1.682D0 , 1.1D0 , 0.5888D0 , -0.4714D0 , 0.5362D0 , 
     &     0.0127D0 , -2.438D0 , 0.03399D0 , 0.07825D0 , 0.05842D0 , 
     &     0.08393D0 , 2.348D0 , -0.07182D0 , 1.084D0 , 0.3098D0 , 
     &     -0.07514D0 , 3.327D0 , 1.1D0 , 2.264D0 , 0.2675D0/
C
      mu2 = 0.25D0
      lam2 = 0.232D0*0.232D0
C
      IF ( P2.LE.0.25D0 ) THEN
         s = LOG(LOG(Q2/lam2)/LOG(mu2/lam2))
         lp1 = 0.D0
         lp2 = 0.D0
      ELSE
         s = LOG(LOG(Q2/lam2)/LOG(P2/lam2))
         lp1 = LOG(P2/mu2)*LOG(P2/mu2)
         lp2 = LOG(P2/mu2+LOG(P2/mu2))
      END IF
C
      alp = up0(1) + lp1*u1(1) + lp2*u1(2)
      bet = up0(2) + lp1*u1(3) + lp2*u1(4)
      a = up0(3) + lp1*u1(5) + lp2*u1(6) + (up0(4)+lp1*u1(7)+lp2*u1(8))
     &    *s
      b = up0(5) + lp1*u1(9) + lp2*u1(10)
     &    + (up0(6)+lp1*u1(11)+lp2*u1(12))
     &    *s**0.5 + (up0(7)+lp1*u1(13)+lp2*u1(14))*s**2
      gb = up0(8) + lp1*u1(15) + lp2*u1(16)
     &     + (up0(9)+lp1*u1(17)+lp2*u1(18))
     &     *s + (up0(10)+lp1*u1(19)+lp2*u1(20))*s**2
      ga = up0(11) + lp1*u1(21) + lp2*u1(22)
     &     + (up0(12)+lp1*u1(23)+lp2*u1(24))*s**0.5
      gc = up0(13) + lp1*u1(25) + lp2*u1(33)
     &     + (up0(14)+lp1*u1(26)+lp2*u1(34))*s
      gd = up0(15) + lp1*u1(27) + lp2*u1(35)
     &     + (up0(16)+lp1*u1(28)+lp2*u1(36))*s
      ge = up0(17) + lp1*u1(29) + lp2*u1(37)
     &     + (up0(18)+lp1*u1(30)+lp2*u1(38))*s
      gep = up0(19) + lp1*u1(31) + lp2*u1(39)
     &      + (up0(20)+lp1*u1(32)+lp2*u1(40))*s
      upart1 = PHO_GRSF2(X,s,alp,bet,a,b,ga,gb,gc,gd,ge,gep)
C
      alp = dsp0(1) + lp1*ds1(1) + lp2*ds1(2)
      bet = dsp0(2) + lp1*ds1(3) + lp2*ds1(4)
      a = dsp0(3) + lp1*ds1(5) + lp2*ds1(6)
     &    + (dsp0(4)+lp1*ds1(7)+lp2*ds1(8))*s
      b = dsp0(5) + lp1*ds1(9) + lp2*ds1(10)
     &    + (dsp0(6)+lp1*ds1(11)+lp2*ds1(12))
     &    *s**0.5 + (dsp0(7)+lp1*ds1(13)+lp2*ds1(14))*s**2
      gb = dsp0(8) + lp1*ds1(15) + lp2*ds1(16)
     &     + (dsp0(9)+lp1*ds1(17)+lp2*ds1(18))
     &     *s + (dsp0(10)+lp1*ds1(19)+lp2*ds1(20))*s**2
      ga = dsp0(11) + lp1*ds1(21) + lp2*ds1(22)
     &     + (dsp0(12)+lp1*ds1(23)+lp2*ds1(24))*s
      gc = dsp0(13) + lp1*ds1(25) + lp2*ds1(33)
     &     + (dsp0(14)+lp1*ds1(26)+lp2*ds1(34))*s
      gd = dsp0(15) + lp1*ds1(27) + lp2*ds1(35)
     &     + (dsp0(16)+lp1*ds1(28)+lp2*ds1(36))*s
      ge = dsp0(17) + lp1*ds1(29) + lp2*ds1(37)
     &     + (dsp0(18)+lp1*ds1(30)+lp2*ds1(38))*s
      gep = dsp0(19) + lp1*ds1(31) + lp2*ds1(39)
     &      + (dsp0(20)+lp1*ds1(32)+lp2*ds1(40))*s
      dspart1 = PHO_GRSF2(X,s,alp,bet,a,b,ga,gb,gc,gd,ge,gep)
C
      alp = gp0(1) + lp1*g1(1) + lp2*g1(2)
      bet = gp0(2) + lp1*g1(3) + lp2*g1(4)
      a = gp0(3) + lp1*g1(5) + lp2*g1(6) + (gp0(4)+lp1*g1(7)+lp2*g1(8))
     &    *s**0.5
      b = gp0(5) + lp1*g1(9) + lp2*g1(10)
     &    + (gp0(6)+lp1*g1(11)+lp2*g1(12))*s**2
      gb = gp0(7) + lp1*g1(13) + lp2*g1(14)
     &     + (gp0(8)+lp1*g1(15)+lp2*g1(16))*s
      ga = gp0(9) + lp1*g1(17) + lp2*g1(18)
     &     + (gp0(10)+lp1*g1(19)+lp2*g1(20))
     &     *s**0.5 + (gp0(11)+lp1*g1(21)+lp2*g1(22))*s**2
      gc = gp0(12) + lp1*g1(23) + lp2*g1(24)
     &     + (gp0(13)+lp1*g1(25)+lp2*g1(26))*s**2
      gd = gp0(14) + lp1*g1(27) + lp2*g1(28)
     &     + (gp0(15)+lp1*g1(29)+lp2*g1(30))
     &     *s + (gp0(16)+lp1*g1(31)+lp2*g1(32))*s**2
      ge = gp0(17) + lp1*g1(33) + lp2*g1(34)
     &     + (gp0(18)+lp1*g1(35)+lp2*g1(36))*s
      gep = gp0(19) + lp1*g1(37) + lp2*g1(38)
     &      + (gp0(20)+lp1*g1(39)+lp2*g1(40))*s
      gpart1 = PHO_GRSF2(X,s,alp,bet,a,b,ga,gb,gc,gd,ge,gep)
C
      s = LOG(LOG(Q2/lam2)/LOG(mu2/lam2))
      suppr = 1.D0/(1.D0+P2/0.59D0)**2
C
      alp = ud2(1)
      bet = ud2(2)
      a = ud2(3) + ud2(4)*s
      ga = ud2(5) + ud2(6)*s**0.5
      gc = ud2(7) + ud2(8)*s
      b = ud2(9) + ud2(10)*s + ud2(11)*s**2
      gb = ud2(12) + ud2(13)*s + ud2(14)*s**2
      gd = ud2(15) + ud2(16)*s
      ge = ud2(17) + ud2(18)*s
      gep = ud2(19) + ud2(20)*s
      udpart2 = suppr*PHO_GRSF1(X,s,alp,bet,a,b,ga,gb,gc,gd,ge,gep)
C
      alp = s2(1)
      bet = s2(2)
      a = s2(3) + s2(4)*s
      ga = s2(5) + s2(6)*s**0.5
      gc = s2(7) + s2(8)*s
      b = s2(9) + s2(10)*s + s2(11)*s**2
      gb = s2(12) + s2(13)*s + s2(14)*s**2
      gd = s2(15) + s2(16)*s
      ge = s2(17) + s2(18)*s
      gep = s2(19) + s2(20)*s
      spart2 = suppr*PHO_GRSF2(X,s,alp,bet,a,b,ga,gb,gc,gd,ge,gep)
C
      alp = g2(1)
      bet = g2(2)
      a = g2(3) + g2(4)*s**0.5
      b = g2(5) + g2(6)*s**2
      gb = g2(7) + g2(8)*s
      ga = g2(9) + g2(10)*s**0.5 + g2(11)*s**2
      gc = g2(12) + g2(13)*s**2
      gd = g2(14) + g2(15)*s + g2(16)*s**2
      ge = g2(17) + g2(18)*s
      gep = g2(19) + g2(20)*s
      gpart2 = suppr*PHO_GRSF1(X,s,alp,bet,a,b,ga,gb,gc,gd,ge,gep)
C
      Ugam = upart1 + udpart2
      Dgam = dspart1 + udpart2
      Sgam = dspart1 + spart2
      Ggam = gpart1 + gpart2
C
      END SUBROUTINE
