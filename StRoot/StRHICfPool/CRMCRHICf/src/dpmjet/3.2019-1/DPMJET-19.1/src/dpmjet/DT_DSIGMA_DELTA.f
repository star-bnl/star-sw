
      DOUBLE PRECISION FUNCTION DT_DSIGMA_DELTA(Lnu,Qq,S,Aml,Md)
 
      IMPLICIT NONE
      DOUBLE PRECISION Aml , aml2 , aml4 , ans , ans1 , ans2 , ans3 , 
     &                 c3 , c32 , c3v , c4 , c42 , c5a , c5a2 , gf , 
     &                 gf2 , p1cm , pi , pik , piq
      DOUBLE PRECISION q , qk , Qq , S , vk , vpi , vq
      INTEGER Lnu
      SAVE 
 
C...Reaction nu + N -> lepton + Delta
C.  returns the  cross section
C.  dsigma/dt
C.  INPUT  LNU = 1, 2  (neutrino-antineutrino)
C.         QQ = t (always negative)  GeV**2
C.         S  = (c.m energy)**2      GeV**2
C.  OUTPUT =  10**-38 cm+2/GeV**2
C-----------------------------------------------------
      REAL*8 mn , mn2 , mn4 , Md , md2 , md4
      DATA mn/0.938/
      DATA pi/3.1415926/
 
      gf = (1.1664*1.97)
      gf2 = gf*gf
      mn2 = mn*mn
      mn4 = mn2*mn2
      md2 = Md*Md
      md4 = md2*md2
      aml2 = Aml*Aml
      aml4 = aml2*aml2
      vq = (mn2-md2-Qq)/2.
      vpi = (mn2+md2-Qq)/2.
      vk = (S+Qq-mn2-aml2)/2.
      pik = (S-mn2)/2.
      qk = (aml2-Qq)/2.
      piq = (Qq+mn2-md2)/2.
      q = SQRT(-Qq)
      c3v = 2.07*SQRT(EXP(-6.3*q)*(1.+9*q))
      c3 = SQRT(3.)*c3v/mn
      c4 = -c3/Md             ! attenzione al segno
      c5a = 1.18/(1.-Qq/0.4225)**2
      c32 = c3**2
      c42 = c4**2
      c5a2 = c5a**2
 
      IF ( Lnu.EQ.1 ) THEN
         ans3 = -md2*vpi*qk*Qq*c32 + md2*vpi*qk*c5a2 + 
     &          2.*md2*vq*pik*qk*c32 + 2.*md2*vq*qk*piq*c32 + 
     &          md4*vpi*qk*Qq*c42 - 2.*vk**2*vpi*Qq*c32 + 
     &          2.*vk**2*vpi*c5a2 + 4.*vk*vpi*vq*qk*c32 + 
     &          2.*vk*vpi*vq*c5a2 + 2.*vpi*vq**2*qk*c32
         ans2 = 2.*mn*Md*md2*vk**2*Qq*c42 - 4.*mn*Md*md2*vk*vq*qk*c42 - 
     &          2.*mn*Md*md2*vq**2*qk*c42 - 2.*mn*Md*md2*qk**2*c32 - 
     &          3.*mn*Md*md2*qk*Qq*c32 + mn*Md*md2*qk*c5a2 - 
     &          mn*Md*md4*qk*Qq*c42 + 2.*mn*Md*vk**2*c5a2 + 
     &          2.*mn*Md*vk*vq*c5a2 + 4.*mn*c3*c4*md2*vk**2*Qq - 
     &          8.*mn*c3*c4*md2*vk*vq*qk - 4.*mn*c3*c4*md2*vq**2*qk - 
     &          2.*mn*c3*c4*md4*qk*Qq - 4.*mn*c3*c5a*md2*vk*Qq + 
     &          4.*mn*c3*c5a*md2*vq*qk - 2.*Md*c3*c4*md2*vk*pik*Qq + 
     &          2.*Md*c3*c4*md2*vk*qk*piq + 2.*Md*c3*c4*md2*vpi*qk*Qq + 
     &          2.*Md*c3*c4*md2*vq*pik*qk + 2.*Md*c3*c4*md2*vq*qk*piq - 
     &          2.*Md*c3*c4*vk**2*vpi*Qq + 4.*Md*c3*c4*vk*vpi*vq*qk + 
     &          2.*Md*c3*c4*vpi*vq**2*qk - Md*c3*c5a*md2*pik*Qq + 
     &          Md*c3*c5a*md2*qk*piq - 3.*Md*c3*c5a*vk*vpi*Qq + 
     &          Md*c3*c5a*vk*vq*piq + 3.*Md*c3*c5a*vpi*vq*qk - 
     &          Md*c3*c5a*vq**2*pik + c4*c5a*md2*vk*vpi*Qq + 
     &          c4*c5a*md2*vk*vq*piq - c4*c5a*md2*vpi*vq*qk - 
     &          c4*c5a*md2*vq**2*pik - c4*c5a*md4*pik*Qq + 
     &          c4*c5a*md4*qk*piq - 2.*md2*vk**2*vpi*Qq*c42 + 
     &          4.*md2*vk*vpi*vq*qk*c42 - 2.*md2*vk*pik*Qq*c32 + 
     &          2.*md2*vk*qk*piq*c32 + 2.*md2*vpi*vq**2*qk*c42 - 
     &          2.*md2*vpi*qk**2*c32 + ans3
      ELSE
         ans3 = -md2*vpi*qk*Qq*c32 + md2*vpi*qk*c5a2 + 
     &          2.*md2*vq*pik*qk*c32 + 2.*md2*vq*qk*piq*c32 + 
     &          md4*vpi*qk*Qq*c42 - 2.*vk**2*vpi*Qq*c32 + 
     &          2.*vk**2*vpi*c5a2 + 4.*vk*vpi*vq*qk*c32 + 
     &          2.*vk*vpi*vq*c5a2 + 2.*vpi*vq**2*qk*c32
         ans2 = 2.*mn*Md*md2*vk**2*Qq*c42 - 4.*mn*Md*md2*vk*vq*qk*c42 - 
     &          2.*mn*Md*md2*vq**2*qk*c42 - 2.*mn*Md*md2*qk**2*c32 - 
     &          3.*mn*Md*md2*qk*Qq*c32 + mn*Md*md2*qk*c5a2 - 
     &          mn*Md*md4*qk*Qq*c42 + 2.*mn*Md*vk**2*c5a2 + 
     &          2.*mn*Md*vk*vq*c5a2 + 4.*mn*c3*c4*md2*vk**2*Qq - 
     &          8.*mn*c3*c4*md2*vk*vq*qk - 4.*mn*c3*c4*md2*vq**2*qk - 
     &          2.*mn*c3*c4*md4*qk*Qq + 4.*mn*c3*c5a*md2*vk*Qq - 
     &          4.*mn*c3*c5a*md2*vq*qk - 2.*Md*c3*c4*md2*vk*pik*Qq + 
     &          2.*Md*c3*c4*md2*vk*qk*piq + 2.*Md*c3*c4*md2*vpi*qk*Qq + 
     &          2.*Md*c3*c4*md2*vq*pik*qk + 2.*Md*c3*c4*md2*vq*qk*piq - 
     &          2.*Md*c3*c4*vk**2*vpi*Qq + 4.*Md*c3*c4*vk*vpi*vq*qk + 
     &          2.*Md*c3*c4*vpi*vq**2*qk + Md*c3*c5a*md2*pik*Qq - 
     &          Md*c3*c5a*md2*qk*piq + 3.*Md*c3*c5a*vk*vpi*Qq - 
     &          Md*c3*c5a*vk*vq*piq - 3.*Md*c3*c5a*vpi*vq*qk + 
     &          Md*c3*c5a*vq**2*pik - c4*c5a*md2*vk*vpi*Qq - 
     &          c4*c5a*md2*vk*vq*piq + c4*c5a*md2*vpi*vq*qk + 
     &          c4*c5a*md2*vq**2*pik + c4*c5a*md4*pik*Qq - 
     &          c4*c5a*md4*qk*piq - 2.*md2*vk**2*vpi*Qq*c42 + 
     &          4.*md2*vk*vpi*vq*qk*c42 - 2.*md2*vk*pik*Qq*c32 + 
     &          2.*md2*vk*qk*piq*c32 + 2.*md2*vpi*vq**2*qk*c42 - 
     &          2.*md2*vpi*qk**2*c32 + ans3
      END IF
      ans1 = 32.*ans2
      ans = ans1/(3.*md2)
      p1cm = (S-mn2)/(2.*SQRT(S))
      DT_DSIGMA_DELTA = gf2/2.*ans/(64.*pi*S*p1cm**2)
 
      END FUNCTION
