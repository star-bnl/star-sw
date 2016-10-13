    subroutine gstar_micky
      print *,' *  ***  GSTAR mickey-mouse event generator loaded.  ***  *'
      print *,' *  **   Use   "USER/INPUT U mickey.mouse"  command   **  *'
      print *,' *  ***  to generate a pseudo-physics event with it  ***  *'
      call micky
    end
*
**********************************************************************
module  micky is the pseudo physics event generator
author  Pavel Nevski
Created August,14 1997
**********************************************************************
+CDE,GCONST,GCUNIT,GCFLAG.
    Integer   Iprin,i
    character warm*50,warn*50
    real      s
*
    structure MIKY { version, np, code(10), mult(10), slope(10), dy(10) }
*
    Begin
    If (first) then
       fill MIKY(1)                  ! generator input
          version = 1                  ! versioning schema
          np    = 5                    ! number of particle types
          code  = {8,  9,  11, 12, 14} ! list of particle codes
          mult  = {3., 3., 2., 2., 1.} ! particle multiplicities(+ve Poisson)
          slope = {5., 5., 4., 4.,3.3} ! PT spectra slopes (c/GeV)
          dy    = {2., 2., 2., 2., 2.} ! width of rapidity Gaussian(-ve uniform)
       endfill
       first = .false.
    endif
*
    Use   MIKY
          warm = ' a negative multiplicity means no fluctuations'
    s=0;  do i=1,miky_np { s+=miky_slope(i); }
          warn = '  Input data contain spectrum slopes [c/GeV]  '
    if (s<miky_np) then
          warn = '   Input data contain inverse slopes [GeV/c]  '
          do i=1,miky_np { if(miky_slope(i)>0) miky_slope(i)=1./miky_slope(i);}
    endif
*
#if 0
    Check Iprin>1

    <w> warm,warn;
    (' === mickey-mouse events will be generated with parameters ==='/,
     5x,' (',a,') '/ 5x,' (',a,') '/,
     5x,' geant_code   multiplicity  inv_pt_slope(GeV/c)  rapidity_width ')
    do i=1,miky_np
       <w> miky_code(i),miky_mult(i),1./miky_slope(i),miky_dy(i)
           (F15.0,3F15.3)
    enddo 
#endif
    end
*
*
    subroutine Mickine
+CDE,GCONST,GCUNIT,GCFLAG.
*
    structure MIKY { version, np, code(10), mult(10), slope(10), dy(10) }
    Integer   LENOCC,Ip,Ivert,I,Ipart,Mult,Ier,Nu,ItrTyp,Itr,Iprin
    Real      RNDM,GAMMA2,Zero(4)/0,0,0,0/,UB(100),PLAB(3),
              Amass,Charge,Tlife,a,b,pt,y,phi,dummy
#if 1
    Real      cosl, pmom, ekin 
#endif
    Character Cname*20
    gamma2(dummy)=-alog(Rndm(1)*Rndm(2))
*
    Iprin=ISLFLAG('MICK','PRIN')
#if 0
    Check Iprin>=0

    prin1; (' *** generating an event with mickey-mouse generator ***')
#endif
    USE /DETM/MICK/MIKY
#if 0
    Call AgSVERT(Zero,0,0,Ub,0,Ivert)

    do Ip=1,nint(miky_NP)
       Ipart=miky_code(Ip)
       Call  GFPART (Ipart,Cname,ItrTyp,Amass,Charge,Tlife,Ub,Nu)
       check Ipart>0 & Itrtyp>0

*                                 negative multiplicity means no fluctuations
       mult=nint(abs(miky_MULT(Ip)))
       If (miky_MULT(Ip)>0)  Call POISSN (miky_MULT(Ip),Mult,Ier)

*                                 generate
       prin2 mult,%L(Cname); (2x,'*** generating ',i5,1x,a,'''s')
       do I=1,Mult
         Itr=0;
         while(Itr.eq.0) {
           call RANNOR(a,b)
           pt      = Gamma2(0.)/miky_SLOPE(Ip)
           if (miky_DY(Ip)>0) {  y = miky_DY(Ip)*a; }
           else               {  y = miky_DY(Ip)*(RNDM(3)-0.5)*2;}
           phi     = TwoPi*Rndm(1)
           pLab(1) = pt*cos(phi)
           pLab(2) = pt*sin(phi)
           pLab(3) = sqrt(Amass**2+pt**2)*sinh(y)
           prin4 Ipart,Plab; (' *** next particle ',i3,4F9.3)
           CALL AGSKINE(Plab,Ipart,Ivert,Ub,0,Itr)
         }
       enddo
    enddo
#else 
    ip = 0
    ipart = 3 
    ekin = 5.89e-6 
    Call  GFPART (Ipart,Cname,ItrTyp,Amass,Charge,Tlife,Ub,Nu)	
    pmom = sqrt((ekin + Amass)**2 - Amass**2)	
    do i = 1, 10000
      if (ip > 10) goto :ok:	
      Zero(1) = -200 + 400*Rndm(1)	
      Zero(2) = -200 + 400*Rndm(1)	
      Zero(3) = -200 + 400*Rndm(1)	
      dummy = sqrt(Zero(1)**2 + Zero(2)**2)
      if (dummy < 60 .or. dummy > 180) goto :err:
      phi     = TwoPi*Rndm(1)
      cosl    = -1. + 2*Rndm(1)
      pLab(3) = pmom*cosl
      pT      = sqrt(pmom**2 - pLab(3)**2)
      pLab(1) = pt*cos(phi)
      pLab(2) = pt*sin(phi)
      ip = ip + 1
      Call gSVERT(Zero,0,0,Ub,0,Ivert)     	
      CALL GSKINE(Plab,Ipart,Ivert,Ub,0,Itr)
:err:
    enddo	
:ok:
* 
#endif
* 
END
*

