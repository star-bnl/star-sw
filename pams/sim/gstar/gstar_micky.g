**********************************************************************
module  agukine is the pseudo physics event generator
author  Pavel Nevski
Created August,14 1997
**********************************************************************
+CDE,GCONST,GCUNIT,GCFLAG.
*
    structure MIKY { version, np, code(10), mult(10), slope(10), dy(10) }
    Integer   LENOCC,Ip,Ivert,I,Ipart,Mult,Ier,Nu,ItrTyp,Itr,Iprin
    Real      RNDM,GAMMA2,Zero(4)/0,0,0,0/,UB(100),PLAB(3),
              Amass,Charge,Tlife,a,b,pt,y,phi,dummy
    Character Cname*20
*
    gamma2(dummy)=-alog(Rndm(1)*Rndm(2))
*
    Begin
    If (first) then
       fill MIKY(1)                  ! generator input
          version = 1                  ! versioning schema
          np    = 5                    ! number of particle types
          code  = {8,  9,  11, 12, 14} ! list of particle codes
          mult  = {3., 3., 2., 2., 1.} ! particle multiplicities
          slope = {6., 6., 6., 6., 6.} ! PT spectra slopes
          dy    = {2., 2., 2., 2., 2.} ! width of rapidity distribution
       endfill
       first = .false.
    endif
*
    Use   MIKY
*
    prin1; (' *** generating an event with micky-mouse generator ***')
    Call AgSVERT(Zero,0,0,Ub,0,Ivert)
    do Ip=1,nint(miky_NP) 
       Ipart=miky_code(Ip)
       Call  GFPART (Ipart,Cname,ItrTyp,Amass,Charge,Tlife,Ub,Nu)
       check Ipart>0 & Itrtyp>0
*                                 negative multiplicity means no fluctuations
       mult=nint(abs(miky_MULT(Ip)))
       If (miky_MULT(Ip)>0)  Call POISSN (miky_MULT(Ip),Mult,Ier)
*                                 generate
       do I=1,Mult
          call RANNOR(a,b)
          pt      = Gamma2(0)/miky_SLOPE(Ip)
          y       = miky_DY(Ip)*a
          phi     = TwoPi*Rndm(1)
          pLab(1) = pt*cos(phi)
          pLab(2) = pt*sin(phi)
          pLab(3) = sqrt(Amass**2+pt**2)*sinh(y)
          CALL AGSKINE(Plab,Ipart,Ivert,Ub,0,Itr)
       enddo
    enddo
* 
END
