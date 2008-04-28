replace [+CDE,XTRGAS] with [
*     - commons for dE/dx simulation
      Real       Ener,Fosc,EpsR,EpsI,Fint,Emin,Emax,Eps,
                 Beta2,Gamma2,Z,A,Ro,pi,Wp2,S2,MeeV
      Integer    Ltab,Ntab,Lu,ie
      Parameter (Ltab=500)
      COMMON /XTRGTAB/ Ntab,Ener(Ltab),Fosc(Ltab),EpsR(Ltab),
                            EpsI(Ltab),Fint(Ltab)
      common /XTRGVAR/ Lu,Emin,Emax,Eps,Z,A,Ro,Beta2,Gamma2,
                            ie,pi,Wp2,S2,MeeV
*     - - - - - - - - - - - - - - - - - - - - - - - - - -
]
Replace[<#(#)>] With [exp(max( XFintera(max(#2,Emin),Ener,#1,Ntab), -99.))]
#if 0
********************************************************************
                subroutine     ArCH4
*    GASTEST -   Ionization losses test in 4 mm of XE+CO2+CF4 mix. *
*   a) primary ionization centers along a track                    *
*   b) their energy distribution according to Ermilova et al,      *
*          Internal energy is in eV, time in ns                    *
********************************************************************
Replace [HIST(#)]  With  [CALL xtrfill(#1,0);]
+CDE,TYPING.
Character*8    cgam,cpat
Integer        LL,Lgam, Nt,ig,jg,nc,Lu,Nev,is
Real           Energy,gam,dNdx,FDN, XFINTERA,RNDM,g,x,s,E,Ed,gammax,Dgam
Real           bgl, bg
Integer        id
Integer        ns, js, ks
Parameter      (ns=11)
Real           dEdx,step(ns)
Parameter      (LL=120, Lgam=101)
Common /DEDX/  Nt,Energy(LL),gam(Lgam),dNdx(Lgam),FDN(LL,Lgam)
Data           Dgam/0/, gammax/10000/, Lu/6/
*              initialise  dE/dx tables
  Call XGASINI(Lu); 

  Call Hbook1 (10,'dN/dx vs lg(E/m)',Lgam,0.,Alog10(Gammax),0)
  do is = 1, ns
{
  step(is) = 2.**((is - 2.)/3.)	
}	
  do jg = 1, Lgam
{
    bgl = -1. + 0.05*(jg - 1)
    bg  = 10.**bgl	
    Write (Cgam,'(F8.2)') bg; 	
    g = sqrt(bg**2 + 1)
    Call XGASTAB(g,dNdx(jg),Nt,Energy,FDN(1,jg))
    gam(jg)=g;           
    if (mod(jg,10) .eq. 1) print *,' gam, dN/dx= ',g,DNdx(jg)
    If Nt>LL { Output; (' XTRDDIG init fatal err: LL too short'); stop;}
    Call HFILL(10,(jg-.1)*Dgam,1.,dNdx(jg))
    do ks = 1, ns 
  {
     s = step(ks)	
     Write (Cpat,'(F8.2)') step(ks); 
     id = 100*jg + ks
     print *,'book id: ',id,' dE/dx(keV/cm), beta*gamma='//Cgam//' path='//Cpat
     Call HBOOK1(id,'log10(dE/dx(keV/cm)), beta*gamma='//Cgam//' path='//Cpat,500,-2.,3.,0)
  }	
     Do Nev=1,200 000
  {  {E,x,nc}=0; js = 1
        Loop " to simulate primary collisions for ionisation losses "
        {  x+=-ALOG(RNDM(-1))/dNdx(jg);        
	   do ks = js, ns
           {
             if (abs(x)>step(ks)) then
               js = js + 1
               dEdx = 1.e-3*E/step(ks)
               id = 100*jg + ks
               if (dEdx>0) Call HFILL(id, log10(dEdx),   1.,1.)
             endif
           }
           if (js > ns) break;
           Nc+=1;
           Ed=exp(XFINTERA(RNDM(-1)*dNdx(jg),Fdn(1,jg),Energy,Nt));  
           E+=Ed;
        }
   }
   print *,'bin gam=',jg,'/',g,' and s=',is,'/',s,' done'
}	
End
#endif

**************************************************************************
                function     XSIGMA (lgE)
**************************************************************************
+CDE,TYPING,XTRGAS.
  Real XFINTERA,XSIGMA,lgE
  Xsigma = <Fosc(lgE)>*exp(lgE)
End

**************************************************************************
                function     XFREAL (d)
* Re part of complex refractive index - the proper value of the integral *
**************************************************************************
+CDE,TYPING,XTRGAS.
Real XFINTERA,XFREAL,lgE,Fp,Fm,d,x
    lgE=Ener(ie);  Fp=0;  If (lgE+d>Emin) Fp=<Fosc(lgE+d)>
                   Fm=0;  If (lgE-d>Emin) Fm=<Fosc(lgE-d)>
    x=exp(d);      XFreal = x/(x**2-1)*(Fp-Fm)
End

********************************************************************
       subroutine     XGASTAB (gamma,dNdx,Nt,En,FN)
*             dN2/dEdx table for a fixed gamma factor              *
********************************************************************
+CDE,TYPING,XTRGAS.
Integer   Nt,i
Real      XGINT,gamma,dNdx,En(*),FN(*), Rener/0.05/,S,Es,Ei,Eo
External  XDNDEDX
*---  final tables for dN/dx integral vs Energy for a gamma   ------
  Gamma2=gamma**2; Beta2=1-1/Gamma2;
  {S,Nt,Es}=0;  Ei=Emax;
  do i=Ntab,1,-1
  { Eo=Ei; Ei=Ener(i); S+=XGint(XDNDEDX,Ei,Eo,eps);
    Check abs(Es-Ei)>Rener | i==1;
    Es=Ei; nt+=1; En(nt)=Ei; FN(nt)=S;
  }
  If (Lu>6)  Output gamma,S,nt
  (' gamma=',E10.3,3x,'dN/dx=',F8.2,2x,'Nt=',i5)
  dNdx=S
End

***********************************************************************
       function     XDNDEDX  (LgE)
* Grishin,Ermilova,Kotelnikov,NIM A307(1991),273 - the only right way *
***********************************************************************
+CDE,TYPING,XTRGAS.
Real XDNDEDX,XFINTERA,lgE,E
Complex i,Ceps1,C1,C2
  i=Cmplx(0.,1.);  E=exp(LgE);
  Ceps1 = CMPLX( Xfintera(LgE,Ener,EpsR,Ntab)/E**2, <EpsI(LgE)> ) " Epsilon-1"
  C1 = 1/Gamma2-Ceps1*Beta2;   C2 = C1/(1+Ceps1)*Clog(2*Beta2*MeeV/E/C1)
  XDNDEDX = S2/Beta2 * (-2*Aimag(C2)/Wp2/pi + (1-<Fint(lgE)>)/E**2)*E
End
#if 0
***********************************************************************
                function     XFINTERA (X,A,F,N)
* Descriprion:  simple linear interpolation                           *
***********************************************************************
 Integer N,K,K1,K2,NO;
 Real XFINTERA,X,A(N),F(N),X1,X2;
 Data K1,K2,NO/0,0,0/
* print *, 'x=', x, ' a(1)=',a(1),' a(N)= ',a(n),' f(1)= ',f(1),' f(N)=', f(N), 'N=',N
* Unless (N==NO) & (A(K1)<x&x<A(K2))
 { XFINTERA=F(1);  Check N>1 & x>A(1);     K1=1;  K2=N;  NO=N;
   While K2-K1>1  { K=(K1+K2)/2; IF A(K)<X {K1=K} else {K2=K} 
*	print *,'k1=',k1,' k2=',k2,' k=',k,' A(k1)=', A(k1), ' x=',x,' A(k2)=',A(k2)	
}
 }
 X1=A(K1); X2=A(K1+1); XFINTERA=(F(K1)*(X-X2)+F(K1+1)*(X1-X))/(X1-X2)
 if (	XFINTERA > 13) {
	print *, 'x=', x, ' a(1)=',a(1),' a(N)= ',a(n)
	print *,' f(1)= ',f(1),' f(N)=', f(N), ' N=',N		
	print *,'============================================'
}
END
#endif
***********************************************************************
                function     XGINT   (EXT,A,B,EPS)
* description:  simplest integration procedure with fixed accuracy    *
***********************************************************************
  Integer M/4/,N,I,K
  Real XGINT,A,B,Eps,OTB,Y,D,Ext,
       U(4)/-.8611363,-.3399810, .3399810 ,.8611363/,
       W(4)/ .3478548, .6521452, .6521452, .3478548/
  EXTERNAL EXT
 N=10;  OTB=0;  Loop
 {  Y=OTB; OTB=0; D=(B-A)*.5/N;
    DO I=1,N  { DO K=1,M  { OTB+=W(K)*EXT(A+D*(2*I-1+U(K)))*D; } }
    XGINT=OTB;   N=2*N
    IF N>100 000 { Print *,'Divergence !!!',a,b,otb;   Return }
 } While EPS>0 & ABS(OTB-Y)>ABS(EPS*OTB)
END

**********************************************************************
                subroutine     XGASINI (Lout)
*  Description:                                                      *
*          dE/dx  simulation for the TRD Xe/Co2/CF4 mixture          *
*        Uses Grishin,Ermilova,Kotelnikov,NIM A307(1991),273         *
*                  PLN 18.04.92, data tables of ILG                  *
*                                                                    *
**********************************************************************
+CDE,TYPING,XTRGAS.
Integer  Lout
Real     XGINT,c,Nav,erg,mb,Me,QE,r0,h,he,ne,S1,Eo,Ei,E,S
External XSIGMA,XFREAL
Replace[;'#'#=#;] With _
       [;"#1"#2=#3; IF (Lu>=6) OUTPUT #2; (' XGASINI: #1 : #2 = ',E13.6);]
*
"-----    physic constants and normalisation stuff            -------"
 Lu=Lout;   eps=0.01;  Emin=12.0;  Emax=1.e7;
 Call XGASDAT
'Light speed         'c  =2.99979e+10;    pi =3.1415926;      Nav= 6.02214e23;
'1 ev to erg {erg}   'erg=1.60218e-12;  '1mb to cm2          'mb = 1.e-18;
'Electron mass {g}   'Me =9.10943e-28;  'same in ev          'MeeV=Me*c**2/erg;
'Electron charge{ESU}'QE =4.80321e-10;  'electron radius{cm} 'r0=Qe**2/Me/c**2;
'Plank constant{erg} 'h  =1.05457e-27;  'same in ev          'he = h*c/erg;
'Gas density         'Ro = Ro;          'Electron density    'ne = Nav*Z/A*Ro;
'plasma freq**2 {ev} 'Wp2= 4*pi*r0*ne*he**2
'x-section to F.osc  'S1 = mb/(2*pi**2*r0*he*Z)
'dN/dx scale         'S2 = 2*pi*r0*ne*Qe**2/erg
*
"----  Sum over cross-section and prepare tables of ref.index   -----"
                                       S=0;           Ei=Emax
 do ie=Ntab,1,-1 { Eo=Ei; Ei=Ener(ie); S+=XGint(XSIGMA,Ei,Eo,eps); Fint(ie)=S;}
 do ie=1,Ntab    { Fint(ie)=Alog(Fint(ie)/S);   Fosc(ie)-=Alog(S); }
 do ie=1,Ntab    { E=exp(Ener(ie));  EpsI(ie)=Alog(Wp2/E*pi/2)+Fosc(ie)
                   EpsR(ie)=Wp2/E*XGint(XFREAL,0.,Emax,eps) "scaled by" * E**2
                 }
OUTPUT S1*S; (' MGASINI: Cross-section sum  ',F13.6)
End


+DECK,xgasdat,T=geant.
*CMZ :          03/10/95  09.41.45  by  Pavel Nevski
*-- Author :    Baranov Serguei   22/09/95
********************************************************************
                    subroutine    XGASDAT
*             Parameters of Xe/CO2/CF4/Ar(3 atm) gas mixture       *
*             carefully tabulated by Igor Gavrilenko               *
********************************************************************
+CDE,TYPING,XTRGAS.
Integer      Ngas,Nmat, Lxenon,Lcarbon,Lftor,Loxigen,Largon,Lhydrogen,
             i1,i2,i3,i4,i5,i6,i7,ilast, Ncom,Index,Natom,Iadr
Real         Exenon,Sxenon, Ecarbon,Scarbon, Eftor,Sftor, Eoxigen,Soxigen,
             Eargon,Sargon, Ehydrogen,Shydrogen,
             Press,Dens,Aw,Zw,Weight, EE,SS
Character*4  Medium
Parameter   (Lxenon=189, Lcarbon=98, Lftor=98, Loxigen=97, 
             Largon=138, Lhydrogen=55,
             i1=1, i2=i1+Lxenon, i3=i2+Lcarbon, i4=i3+Lftor,
             i5=i4+Loxigen, i6=i5+Largon, i7=i6+Lhydrogen, 
             Ngas=5, Nmat=8, Ilast=i7)
Dimension    Iadr(7),EE(Ilast),SS(Ilast),
             Ncom(Ngas),Press(Ngas),Dens(Ngas),
             Medium(Nmat),Index(Nmat),Natom(Nmat),
             Aw(Nmat),Zw(Nmat),Weight(Nmat)
Dimension    Exenon(Lxenon), Ecarbon(Lcarbon), Eftor(Lftor), Eoxigen(Loxigen),
             Eargon(Largon), Ehydrogen(Lhydrogen)
Equivalence (Exenon,EE(i1)),(Ecarbon,EE(i2)),(Eftor,EE(i3)),(Eoxigen,EE(i4)),
            (Eargon,EE(i5)),(Ehydrogen,EE(i6))
Dimension    Sxenon(Lxenon), Scarbon(Lcarbon), Sftor(Lftor), Soxigen(Loxigen),
             Sargon(Largon), Shydrogen(Lhydrogen)
Equivalence (Sxenon,SS(i1)),(Scarbon,SS(i2)), (Sftor,SS(i3)),(Soxigen,SS(i4)),
            (Sargon,SS(i5)),(Shydrogen,SS(i6))
*
*            Dens   /.005485, .001842,  .0039,  .00178,   .00067/, original
*"Ar CH10 at 25 C and 1.003 atm"
Data         Ncom   /1,2,2,1,2/,  Press/.00, .00, .00, .90, 0.10 /,
             Dens   /.005485, .001842,  .0039,  .00164,   .00066/,  
             Medium /'Xe',   'C','O',  'C','F',   'Ar',  'C','H'/,
             Index  /  1 ,    2,  4,    2,  3,      5,    2,  6 /,
             Natom  /  1 ,    1,  2,    1,  4,      1,    1,  4 /,
             Aw     /131.3,  12, 16,   12, 19,  39.95,   12,  1 /,
             Zw     / 54,     6,  8,    6,  9,     18,    6,  1 /,
             Iadr   /  i1,   i2,   i3,   i4,   i5,   i6,   i7   /

Data Exenon / _
 12.08, 13.45, 13.48, 13.62, 13.78, 13.93, 14.09, 14.25, 14.42, 14.59,
 14.76, 14.94, 15.12, 15.31, 15.50, 15.69, 15.89, 16.10, 16.31, 16.53,
 16.75, 16.98, 17.22, 17.46, 17.71, 17.97, 18.23, 18.50, 18.78, 19.07,
 19.37, 19.68, 20.00, 20.32, 20.66, 21.01, 21.38, 21.75, 22.14, 22.54,
 22.96, 23.39, 23.84, 24.31, 24.80, 25.30, 25.83, 26.38, 26.95, 27.55,
 28.18, 28.83, 29.52, 30.24, 30.99, 31.79, 32.63, 33.51, 34.44, 35.42,
 36.46, 37.57, 38.74, 39.99, 41.33, 42.75, 44.28, 45.92, 47.68, 49.59,
 51.66, 53.90, 56.35, 59.04, 61.99, 65.25, 68.88, 72.93, 77.49, 79.99,
 82.65, 85.50, 88.56, 91.84, 95.37, 99.18, 103.3, 107.8, 112.7, 118.1,
 124.0, 126.5, 129.1, 131.9, 134.8, 137.8, 140.9, 144.2, 147.6, 151.2,
 155.0, 158.9, 163.1, 167.5, 172.2, 177.1, 182.3, 187.8, 193.7, 200.0,
 206.6, 213.8, 221.4, 229.6, 238.4, 248.0, 258.3, 269.5, 281.8, 295.2,
 309.9, 326.3, 344.4, 364.6, 387.4, 413.3, 442.8, 476.8, 516.6, 563.5,
 619.9, 670.0, 689.0, 708.0, 729.0, 751.0, 775.0, 800.0, 827.0, 855.0,
 886.0, 918.0, 928.7, 953.7, 991.8, 1033., 1078., 1127., 1181., 1240.,
 1305., 1378., 1459., 1550., 1653., 1771., 1907., 2066., 2254., 2480.,
 2755., 3100., 3542., 4133., 4780., 4781., 5000., 5099., 5100., 5451.,
 5452., 5635., 6199., 6888., 7749., 8856.,10330.,12400.,15500.,20660.,
31000.,34560.,35420.,41330.,49590.,61990.,82650.,124000.,145300. /
Data Sxenon / _
 66.30, 66.30, 66.20, 65.72, 65.22, 64.69, 64.13, 63.53, 62.88, 62.19,
 61.45, 60.67, 59.83, 58.94, 58.00, 57.01, 55.96, 54.87, 53.74, 52.55,
 51.32, 50.05, 48.75, 47.40, 46.03, 44.62, 43.19, 41.73, 40.25, 38.76,
 37.26, 35.75, 34.23, 32.71, 31.20, 29.69, 28.19, 26.71, 25.24, 23.79,
 22.37, 20.98, 19.61, 18.28, 16.98, 15.72, 14.51, 13.33, 12.21, 11.13,
 10.10, 9.124, 8.201, 7.332, 6.519, 5.762, 5.064, 4.425, 3.845, 3.406,
 3.038, 2.727, 2.444, 2.205, 2.004, 1.855, 1.731, 1.656, 1.591, 1.560,
 1.524, 1.519, 1.551, 1.673, 1.995, 2.658, 3.888, 6.009, 9.266, 11.97,
 15.27, 18.75, 22.04, 24.71, 26.49, 27.07, 26.28, 24.11, 20.68, 16.31,
 11.38, 9.637, 8.111, 6.783, 5.637, 4.656, 3.823, 3.126, 2.550, 2.082,
 1.710, 1.422, 1.207, 1.056, .9588, .9066, .8914, .9058, .9429, .9964,
 1.061, 1.131, 1.203, 1.271, 1.334, 1.388, 1.432, 1.462, 1.478, 1.479,
 1.465, 1.436, 1.392, 1.336, 1.267, 1.190, 1.105, 1.017, .9275, .8416,
 .7633, .7200, 4.300, 4.750, 3.650, 3.150, 2.800, 2.500, 2.250, 2.000,
 1.800, 1.600, 1.849, 1.805, 1.732, 1.649, 1.558, 1.461, 1.358, 1.251,
 1.141, 1.031, .9199, .8108, .7044, .6019, .5047, .4140, .3310, .2568,
 .1927, .1397,.09911,.07193,.05560, .1578, .1408, .1338, .1882, .1584,
 .1835, .1678, .1296,.09741,.07080,.04931,.03248,.01982,.01083,.00497,
.00166,.00666,.00625,.00419,.00261,.00146,.00069,.00024,.00016/
Data Ecarbon / _
    10,    20,    30,    35,    40,    45,    50,    55,    60,    65,
    70,    75,    80,    85,    90,    95,   100,   105,   110,   115,
   120,   125,   130,   135,   140,   145,   150,   155,   160,   165,
   170,   175,   180,   185,   190,   195,   200,   210,   220,   230,
   240,   250,   260,   270,   280,   290,   300,   310,   320,   330,
   340,   350,   360,   370,   380,   390,   400,   420,   440,   460,
   480,   500,   520,   540,   560,   580,   600,   650,   700,   750,
   800,   850,   900,   950,  1000,  1100,  1200,  1300,  1400,  1500,
  1600,  1700,  1800,  1900,  2000,  2500,  3000,  3500,  4000,  4500,
  5000,  5500,  6000,  8000, 10000, 15000, 20000, 30000 /
Data Scarbon / _
  14.0,  10.0,  5.78,  4.42,  3.44,  2.73,  2.21,  1.81,  1.51,  1.28,
  1.09,  .940,  .820,  .720,  .630,  .560,  .500,  .451,  .407,  .369,
  .336,  .307,  .281,  .259,  .238,  .220,  .204,  .190,  .176,  .164,
  .154,  .144,  .135,  .126,  .119,  .112,  .105,  .094,  .084,  .075,
  .068,  .061,  .056,  .051,  .046,  1.02,  .950,  .880,  .820,  .760,
  .710,  .660,  .620,  .580,  .550,  .510,  .484,  .430,  .384,  .344,
  .309,  .279,  .253,  .230,  .210,  .191,  .175,  .142,  .117,  .097,
  .082,  .069,  .059,  .051, .0443, .0339, .0265, .0211, .0171, .0140,
 .0116, .0097, .0082,  .007,  .006, .0031,.00179,.00112,.00075,.00052,
.00038,.000284,.000218,.88e-4,.46e-4,.16e-4,.86e-5,.506e-5/
Data Eftor /  _
    13,    30,    35,    40,    45,    50,    55,    60,    65,    70,
    75,    80,    85,    90,    95,   100,   105,   110,   115,   120,
   125,   130,   135,   140,   145,   150,   155,   160,   165,   170,
   175,   180,   185,   190,   195,   200,   210,   220,   230,   240,
   250,   260,   270,   280,   290,   300,   310,   320,   330,   340,
   350,   360,   370,   380,   390,   400,   420,   440,   460,   480,
   500,   520,   540,   560,   580,   600,   650,   700,   750,   800,
   850,   900,   950,  1000,  1100,  1200,  1300,  1400,  1500,  1600,
  1700,  1800,  1900,  2000,  2500,  3000,  3500,  4000,  4500,  5000,
  5500,  6000,  8000, 10000, 15000, 20000, 30000, 40000/
Data Sftor / _
  9.44,  9.44,  10.22, 9.37,  8.49,  7.64,  6.86,  6.15,  5.51,  4.95,
  4.45,  4.01,  3.63,  3.28,  2.98,  2.71,  2.47,  2.26,  2.07,  1.90,
  1.75,  1.61,  1.49,  1.37,  1.27,  1.18,  1.10,  1.02,  .960,  .890,
  .840,  .780,  .740,  .690,  .650,  .610,  .540,  .487,  .437,  .394,
  .356,  .323,  .294,  .268,  .246,  .226,  .208,  .191,  .177,  .164,
  .152,  .142,  .132,  .123,  .115,  .108,  .095,  .084,  .075,  .067,
  .060,  .055, .0494, .0450, .0411, .0376, .0306,  .444,  .375,  .319,
  .274,  .238,  .207,  .182,  .142,  .113,  .092,  .075,  .063,  .053,
 .0447, .0383, .033,  .0286, .0154, .0092, .0059,.00401,.00284,.00209,
.00158,.00122,.000468,.00024,.729e-4,.331e-4,.131e-4,.836e-5/
Data Eoxigen / _
    15,    30,    35,    40,    45,    50,    55,    60,    65,    70,
    75,    80,    85,    90,    95,   100,   105,   110,   115,   120,
   125,   130,   135,   140,   145,   150,   155,   160,   165,   170,
   175,   180,   185,   190,   195,   200,   210,   220,   230,   240,
   250,   260,   270,   280,   290,   300,   310,   320,   330,   340,
   350,   360,   370,   380,   390,   400,   420,   440,   460,   480,
   500,   520,   540,   560,   580,   600,   650,   700,   750,   800,
   850,   900,   950,  1000,  1100,  1200,  1300,  1400,  1500,  1600,
  1700,  1800,  1900,  2000,  2500,  3000,  3500,  4000,  4500,  5000,
  5500,  6000,  8000, 10000, 15000, 20000, 30000/
Data Soxigen / _
 10.49, 10.49,  9.25,  8.04,  6.95,  6.00,  5.20,  4.52,  3.94,  3.45,
  3.04,  2.69,  2.39,  2.13,  1.91,  1.71,  1.55,  1.40,  1.27,  1.16,
  1.06, .9700, .8900, .8200, .7600, .7000, .6500, .6000, .5600, .5200,
 .4880, .4570, .4280, .4020, .3770, .3550, .3160, .2820, .2530, .3940,
 .2060, .1870, .1710, .1560, .1430, .1310, .1210, .1120, .1040, .0960,
 .0890, .0830, .0780, .0730, .0680, .0640, .0560, .0500, .0446, .0400,
 .0360, .0326, .5700, .5300, .4830, .4450, .3660, .3050, .2570, .2180,
 .1870, .1610, .1400, .1230, .0950, .0760, .0610, .0499, .0413, .0346,
 .0292, .0249, .0214, .0185, .0098, .0058, .0037, .0025,.00177,.00129,
.00097,.00075,.000302,.000154,.476e-4,.226e-4,.99e-5/

Data Eargon /      _
             15.70 ,    15.89 ,    16.10 ,    16.31 ,    16.53 ,
             16.75 ,    16.98 ,    17.22 ,    17.46 ,    17.71 ,
             17.97 ,    18.23 ,    18.50 ,    18.78 ,    19.07 ,
             19.37 ,    19.68 ,    20.00 ,    20.32 ,    20.66 ,
             21.01 ,    21.38 ,    21.75 ,    22.14 ,    22.54 ,
             22.96 ,    23.39 ,    23.84 ,    24.31 ,    24.80 ,
             25.30 ,    25.83 ,    26.38 ,    26.95 ,    27.55 ,
             28.18 ,    28.83 ,    29.52 ,    30.24 ,    30.99 ,
             31.79 ,    32.63 ,    33.51 ,    34.44 ,    35.42 ,
             36.46 ,    37.57 ,    38.74 ,    39.99 ,    41.33 ,
             42.75 ,    44.28 ,    45.92 ,    47.68 ,    49.59 ,
             51.66 ,    53.90 ,    56.35 ,    59.04 ,    61.99 ,
             65.25 ,    68.88 ,    72.93 ,    77.49 ,    82.65 ,
             88.56 ,    95.37 ,   103.30 ,   112.70 ,   124.00 ,
            130.50 ,   137.80 ,   145.90 ,   155.00 ,   165.30 ,
            177.10 ,   190.70 ,   206.60 ,   225.40 ,   245.00 ,
            248.00 ,   258.30 ,   269.50 ,   281.80 ,   295.20 ,
            310.00 ,   326.30 ,   344.40 ,   364.70 ,   387.40 ,
            413.30 ,   442.80 ,   476.90 ,   516.60 ,   563.60 ,
            619.90 ,   652.50 ,   688.80 ,   729.30 ,   774.90 ,
            826.50 ,   885.60 ,   953.70 ,  1033.00 ,  1127.00 ,
           1240.00 ,  1305.00 ,  1378.00 ,  1459.00 ,  1550.00 ,
           1653.00 ,  1771.00 ,  1907.00 ,  2066.00 ,  2254.00 ,
           2480.00 ,  2755.00 ,  3100.00 ,  3204.00 ,  3263.00 ,
           3444.00 ,  3646.00 ,  3874.00 ,  4133.00 ,  4428.00 ,
           4768.00 ,  5166.00 ,  5635.00 ,  6199.00 ,  6888.00 ,
           7749.00 ,  8856.00 , 10330.00 , 12400.00 , 15500.00 ,
          20660.00 , 31000.00 , 61990.00                         /
Data Sargon /      _
          29.2     , 29.5     , 30.3     , 31.1     , 31.8     ,
          32.5     , 33.1     , 33.7     , 34.2     , 34.7     ,
          35.1     , 35.5     , 35.8     , 36.1     , 36.3     ,
          36.5     , 36.6     , 36.7     , 36.8     , 36.7     ,
          36.7     , 36.5     , 36.3     , 36.1     , 35.7     ,
          35.4     , 34.9     , 34.4     , 33.8     , 33.1     ,
          32.3     , 31.4     , 30.5     , 29.5     , 28.3     ,
          27.1     , 25.7     , 24.3     , 22.7     , 21.0     ,
          19.1     , 17.1     , 15.0     , 12.8     , 10.3     ,
          7.77     , 6.10     , 4.62     , 3.41     , 2.47     ,
          1.77     , 1.30     , 1.03     , .914     , .916     ,
          1.00     , 1.13     , 1.28     , 1.36     , 1.42     ,
          1.45     , 1.48     , 1.48     , 1.47     , 1.45     ,
          1.41     , 1.36     , 1.29     , 1.20     , 1.10     ,
          1.05     , .987     , .923     , .856     , .785     ,
          .709     , .630     , .547     , .461     , .381     ,
          4.66     , 4.23     , 3.83     , 3.45     , 3.10     ,
          2.76     , 2.45     , 2.16     , 1.89     , 1.64     ,
          1.41     , 1.20     , 1.01     , .836     , .682     ,
          .546     , .484     , .426     , .373     , .324     ,
          .278     , .237     , .199     , .165     , .135     ,
          .108     , .0955    , .0842    , .0736    , .0639    ,
          .0549    , .0467    , .0393    , .0326    , .0266    ,
          .0213    , .0166    , .0126    , .0117    , .0959    ,
          .0827    , .0706    , .0598    , .0501    , .0414    ,
          .0338    , .0271    , .0213    , .0164    , .0123    ,
          .00889   , .00616   , .00403   , .00244   , .00132   ,
          .000599  , .000196  , .000029                          /

data Ehydrogen /  _
            13.60 ,    16.70 ,    21.20 ,    26.80 ,    30.50 ,
            40.80 ,    49.30 ,    72.40 ,    80.00 ,    91.50 ,
           100.00 ,   120.00 ,   130.00 ,   150.00 ,   170.00 ,
           200.00 ,   250.00 ,   300.00 ,   350.00 ,   400.00 ,
           450.00 ,   500.00 ,   550.00 ,   600.00 ,   650.00 ,
           700.00 ,   750.00 ,   800.00 ,   850.00 ,   900.00 ,
           950.00 ,  1000.00 ,  1500.00 ,  2000.00 ,  3000.00 ,
          4000.00 ,  5000.00 ,  6000.00 ,  8000.00 , 10000.00 ,
         15000.00 , 20000.00 , 30000.00 , 40000.00 , 50000.00 ,
         60000.00 , 80000.00 ,100000.00 ,150000.00 ,200000.00 ,
        300000.00 ,400000.00 ,500000.00 ,600000.00 ,800000.00  /
data Shydrogen /  _
          6.40     , 3.632    , 1.888    , .9707    , .6760    ,
          .2892    , .1620    , .0519    , .0380    , .02540   ,
          .0193    , .0110    , .00853   , .00545   , .00369   ,
          .00220   , .00107   , .000604  , .000363  , .000232  ,
          .000161  , .000113  , .0000812 , .0000615 , .0000489 ,
          .0000383 , .0000297 , .0000236 , .0000197 , .0000165 ,
          .0000134 , .0000109 , .00000293, .00000111, .280E-6  ,
          .105E-6  , .490E-7  , .263E-7  , .979E-8  , .453E-8  ,
          .112E-8  , .416E-9  , .102E-9  , .376E-10 , .173E-10 ,
          .919E-11 , .337E-11 , .155E-11 , .377E-12 , .138E-12 ,
          .336E-13 , .123E-13 , .565E-14 , .299E-14 , .110E-14  /
*
 Integer Ini,LG,LC,i,j,k,l,m
 Real    XFINTERA,Em,Eo,E,S,W,SP
 Data    Ini/0/
" protect against double usage "   Check Ini==0;  Ini=1;
*
"---   prepare E,x-section tables, mix gases and sum x-sections   ----"
Emin=Alog(Emin);  Emax=Alog(Emax);         "translate everything to logarithms"
Do i=1,ilast-1 {  E=SS(i); SS(i)=Alog(SS(i)); EE(i)=Alog(EE(i)); 
}
Em=Emin;  m=0;
While Em<Emax                                         "merge all energies     "
{  M+=1; {Ener(M),Eo}=Em; Em=Emax; k=0; 
   Do LG=1,Ngas 
   {  Do LC=1,Ncom(LG)
      { K+=1;  Check Press(LG)*Natom(k)>0  
        j=iadr(index(k));  L=iadr(index(k)+1)-j;
        do i=1,L  { E=EE(j+i-1); If(Eo<E<Em) Em=E; }
   }  }
}
 Ntab=m;   If m>=Ltab { Stop ' XGASDAT Fatal data error - table too short ' };
 {k,S,Z,A,Ro,Sp}=0
 Do LG=1,Ngas                                         "get component weights  "
 {  Ro+=Dens(LG)*Press(LG);  SP+=Press(LG);  Do LC=1,Ncom(LG)
    {  k+=1;  w=Press(LG)*Natom(k);  Weight(k)=w;  Check w>0  
       S+=w;  Z+=Zw(k)*w;            A+=Aw(k)*w;
 }  }                
* Do j=1,k { weight(j)/=S; }  
 Do ie=1,Ntab
 {  E=Ener(ie);  {k,Fosc(ie)}=0;                      "get weighted x-section "
    do LG=1,Ngas { do LC=1,Ncom(LG)
    {  k+=1;       Check weight(k)>0; 
       m=index(k);  j=iadr(m);  L=iadr(m+1)-j+1;  Check E>=EE(j)
       Fosc(ie) += exp(max(XFINTERA (E,EE(j),SS(j),L),-50.))*Weight(k)*A/Z
    }} 
    IF fosc(ie)>0 { Fosc(ie) = Alog(Fosc(ie)) } else { Fosc(ie)=-99 }
 }
Output (Medium(i),i=1,k); (' XGASINI: materials ',10A8  )
Output (Weight(i),i=1,k); (' XGASINI: weights - ',10f8.5)
print *,' A =',A,'  Z =',Z
end
#if 0

**********************************************************************
   subroutine    XTRFILL (a,b,c)
**********************************************************************
   character  a*(*),aa*40
   integer    b,c
   logical    First/.true./,On/.false./
   L=Lenocc(a);    aa=a(1:L);      Call MYFILL(aa,b,c);
   End


******************************************************************************
                subroutine     XGASPLT
Replace[GRAF(#,#,#,#)] With _
[ ;K+=1;  Call HBOOK1(k,'#1 vs Energy',5,1.,5.,0);
   Call HMINIM(K,#2+0.);     Call HMAXIM(K,#3+0.);
   Call HPLOT(K,' ',' ',0);  Call HPLAX('lgE','#1');
   Call HPLFUN(Elog,#4,Ntab,' ');
]
******************************************************************************
+CDE,TYPING,XTRGAS.
 Character*4 OPT(6)/'NBOX','NSTA','FIT','LOGY','    ','    '/
 Real        Elog(Ltab)
 Integer     k
"---------------           plot some results          -----------------------"
 Call HTITLE('Ionization losses theory')
 K=0;   Do ie=1,Ntab  { Elog(ie)=Ener(ie)/alog(10.) }
 Graf ( (Real(Epsilon)-1) * E**2     , -2,+1, EpsR);
 Graf ( lg(Imag (Epsilon) )          , -30,0, EpsI);
 Graf ( lg(oscillator strength)      , -20,0, Fosc);
 Graf ( lg(integrated cross-section) , -5, 0, Fint);
End
#endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

               subroutine    aaa

+CDE,TYPING,XTRGAS.
 Character*4 OPT(6)/'NBOX','NSTA','FIT','LOGY','    ','    '/
 Real        Elog(Ltab),DN(Ltab),XDNDEDX,D 
 Integer     k
"---------------           plot some results          -----------------------"
 Call HTITLE('Ionization losses theory')
 K=0;   Do ie=1,Ntab  { Elog(ie)=Ener(ie)/alog(10.); D=XDNDEDX(ener(ie));
                        if (D>0) DN(ie)=alog10(D);
                      }
 Graf ( dN/dE*dx  , -3, 2, dn);

End
























