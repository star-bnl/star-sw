C: definitions from /afs/rhic.bnl.gov/star/packages/.DEV2/.rh80_icc80/bin/../../.rh80_icc80/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
*     - commons for dE/dx simulation
*     - - - - - - - - - - - - - - - - - - - - - - - - - -
#if 1
********************************************************************
      SUBROUTINE ARCH4                                                    
*    GASTEST -   Ionization losses test in 4 mm of XE+CO2+CF4 mix. *
*   a) primary ionization centers along a track                    *
*   b) their energy distribution according to Ermilova et al,      *
*          Internal energy is in eV, time in ns                    *
********************************************************************
      IMPLICIT NONE                                                       
      CHARACTER*8 CGAM,CPAT                                               
      INTEGER LL,LGAM, NT,IG,JG,NC,LU,NEV,IS                              
      REAL ENERGY,GAM,DNDX,FDN, XFINTERA,RNDM,G,X,S,E,ED,GAMMAX,DGAM      
      REAL BGL, BG                                                        
      INTEGER ID                                                          
      INTEGER NS, JS, KS                                                  
      PARAMETER (NS=11)                                                   
      REAL DEDX,STEP(NS)                                                  
      PARAMETER (LL=120)                                                  
      PARAMETER (LGAM=101)                                                
      COMMON /DEDX/ NT,ENERGY(LL),GAM(LGAM),DNDX(LGAM),FDN(LL,LGAM)       
      DATA DGAM/0/, GAMMAX/10000/, LU/6/                                  
*              initialise  dE/dx tables
      CALL XGASINI(LU)                                                    
      CALL HBOOK1 (10,'dN/dx vs lg(E/m)',LGAM,0.,ALOG10(GAMMAX),0)        
C *                                                                       
C    Loop here                                                            
         DO 5011 IS=1,NS                                                  
         STEP(IS) = 2.**((IS - 2.)/3.)                                    
5011  CONTINUE                                                            
5012  CONTINUE                                                            
C *                                                                       
C    Loop here                                                            
         DO 5021 JG=1,LGAM                                                
         BGL = -1. + 0.05*(JG - 1)                                        
         BG = 10.**BGL                                                    
         WRITE (CGAM,'(F8.2)') BG                                         
         G = SQRT(BG**2 + 1)                                              
         CALL XGASTAB(G,DNDX(JG),NT,ENERGY,FDN(1,JG))                     
         GAM(JG)=G                                                        
C    Check MOD(JG,10) .EQ. 1                                              
         IF (MOD(JG,10) .EQ. 1) PRINT *,' gam, dN/dx= ',G,DNDX(JG)        
C    *                                                                    
C       Check NT>LL                                                       
            IF (NT.GT.LL) THEN                                            
            WRITE(LU,5030)                                                
5030        FORMAT(' XTRDDIG init fatal err: LL too short')               
            STOP                                                          
         END IF                                                           
         CALL HFILL(10,(JG-.1)*DGAM,1.,DNDX(JG))                          
C    *                                                                    
C       Loop here                                                         
            DO 5041 KS=1,NS                                               
            S = STEP(KS)                                                  
            WRITE (CPAT,'(F8.2)') STEP(KS)                                
            ID = 100*JG + KS                                              
            PRINT *,'book id: ',ID,' dE/dx(keV/cm), beta*gamma='//CGAM//  
     *      ' path='//CPAT                                                
            CALL HBOOK1(ID,'log10(dE/dx(keV/cm)), beta*gamma='//CGAM//'   
     *path='//CPAT,500,-2.,3.,0)                                          
5041     CONTINUE                                                         
5042     CONTINUE                                                         
C    *                                                                    
C       Loop here                                                         
            DO 5051 NEV=1,200 000                                         
            E=0                                                           
            X=0                                                           
            NC=0                                                          
            JS = 1                                                        
C       *                                                                 
C          Loop here                                                      
5061           CONTINUE                                                   
               X=X-ALOG(RNDM(-1))/DNDX(JG)                               
C          *                                                              
C             Loop here                                                   
                  DO 5071 KS=JS,NS                                        
C             Check ABS(X)>STEP(KS)                                       
                  IF (ABS(X).GT.STEP(KS)) THEN                            
                  JS = JS + 1                                             
                  DEDX = 1.E-3*E/STEP(KS)                                 
                  ID = 100*JG + KS                                        
C             Check DEDX>0                                                
                  IF (DEDX.GT.0) CALL HFILL(ID, LOG10(DEDX), 1.,1.)       
                  ENDIF                                                   
5071           CONTINUE                                                   
5072           CONTINUE                                                   
C          Check JS > NS                                                  
               IF (JS .GT. NS)GO TO 5062                                  
               NC=NC+1                                                    
               ED=EXP(XFINTERA(RNDM(-1)*DNDX(JG),FDN(1,JG),ENERGY,NT))    
               E=E+ED                                                     
            GO TO 5061                                                    
5062        CONTINUE                                                      
5051     CONTINUE                                                         
5052     CONTINUE                                                         
         PRINT *,'bin gam=',JG,'/',G,' and s=',IS,'/',S,' done'           
5021  CONTINUE                                                            
5022  CONTINUE                                                            
      END                                                                 
#endif
**************************************************************************
      FUNCTION XSIGMA (LGE)                                               
**************************************************************************
      IMPLICIT NONE                                                       
      REAL ENER,FOSC,EPSR,EPSI,FINT,EMIN,EMAX,EPS, BETA2,GAMMA2,Z,A,RO,   
     *PI,WP2,S2,MEEV                                                      
      INTEGER LTAB,NTAB,LU,IE                                             
      PARAMETER (LTAB=500)                                                
      COMMON /XTRGTAB/ NTAB,ENER(LTAB),FOSC(LTAB),EPSR(LTAB),             
     *EPSI(LTAB),FINT(LTAB)                                               
      COMMON /XTRGVAR/ LU,EMIN,EMAX,EPS,Z,A,RO,BETA2,GAMMA2, IE,PI,WP2,   
     *S2,MEEV                                                             
      REAL XFINTERA,XSIGMA,LGE                                            
      XSIGMA = EXP(MAX( XFINTERA(MAX(LGE,EMIN),ENER,FOSC,NTAB),           
     *-99.))*EXP(LGE)                                                     
      END                                                                 
**************************************************************************
      FUNCTION XFREAL (D)                                                 
* Re part of complex refractive index - the proper value of the integral *
**************************************************************************
      IMPLICIT NONE                                                       
      REAL ENER,FOSC,EPSR,EPSI,FINT,EMIN,EMAX,EPS, BETA2,GAMMA2,Z,A,RO,   
     *PI,WP2,S2,MEEV                                                      
      INTEGER LTAB,NTAB,LU,IE                                             
      PARAMETER (LTAB=500)                                                
      COMMON /XTRGTAB/ NTAB,ENER(LTAB),FOSC(LTAB),EPSR(LTAB),             
     *EPSI(LTAB),FINT(LTAB)                                               
      COMMON /XTRGVAR/ LU,EMIN,EMAX,EPS,Z,A,RO,BETA2,GAMMA2, IE,PI,WP2,   
     *S2,MEEV                                                             
      REAL XFINTERA,XFREAL,LGE,FP,FM,D,X                                  
      LGE=ENER(IE)                                                        
      FP=0                                                                
C Check LGE+D>EMIN                                                        
      IF (LGE+D.GT.EMIN) FP=EXP(MAX( XFINTERA(MAX(LGE+D,EMIN),ENER,FOSC,  
     *NTAB), -99.))                                                       
      FM=0                                                                
C Check LGE-D>EMIN                                                        
      IF (LGE-D.GT.EMIN) FM=EXP(MAX( XFINTERA(MAX(LGE-D,EMIN),ENER,FOSC,  
     *NTAB), -99.))                                                       
      X=EXP(D)                                                            
      XFREAL = X/(X**2-1)*(FP-FM)                                         
      END                                                                 
********************************************************************
      SUBROUTINE XGASTAB (GAMMA,DNDX,NT,EN,FN)                            
*             dN2/dEdx table for a fixed gamma factor              *
********************************************************************
      IMPLICIT NONE                                                       
      REAL ENER,FOSC,EPSR,EPSI,FINT,EMIN,EMAX,EPS, BETA2,GAMMA2,Z,A,RO,   
     *PI,WP2,S2,MEEV                                                      
      INTEGER LTAB,NTAB,LU,IE                                             
      PARAMETER (LTAB=500)                                                
      COMMON /XTRGTAB/ NTAB,ENER(LTAB),FOSC(LTAB),EPSR(LTAB),             
     *EPSI(LTAB),FINT(LTAB)                                               
      COMMON /XTRGVAR/ LU,EMIN,EMAX,EPS,Z,A,RO,BETA2,GAMMA2, IE,PI,WP2,   
     *S2,MEEV                                                             
      INTEGER NT,I                                                        
      REAL XGINT,GAMMA,DNDX,EN(*),FN(*), RENER/0.05/,S,ES,EI,EO           
      EXTERNAL XDNDEDX                                                    
*---  final tables for dN/dx integral vs Energy for a gamma   ------
      GAMMA2=GAMMA**2                                                     
      BETA2=1-1/GAMMA2                                                    
      S=0                                                                 
      NT=0                                                                
      ES=0                                                                
      EI=EMAX                                                             
C *                                                                       
C    Loop here                                                            
         DO 5011 I=NTAB,1,-1                                              
         EO=EI                                                            
         EI=ENER(I)                                                       
         S=S+XGINT(XDNDEDX,EI,EO,EPS)                                     
C    Skip Unless ABS(ES-EI)>RENER | I==1                                  
         IF ((ABS(ES-EI).LE.RENER).AND.(I.NE.1))GO TO 5011                
         ES=EI                                                            
         NT=NT+1                                                          
         EN(NT)=EI                                                        
         FN(NT)=S                                                         
5011  CONTINUE                                                            
5012  CONTINUE                                                            
C Check LU>6                                                              
      IF (LU.GT.6) WRITE(LU,5020)GAMMA,S,NT                               
5020  FORMAT(' gamma=',E10.3,3X,'dN/dx=',F8.2,2X,'Nt=',I5)                
      DNDX=S                                                              
1     CONTINUE                                                            
      END                                                                 
***********************************************************************
      FUNCTION XDNDEDX (LGE)                                              
* Grishin,Ermilova,Kotelnikov,NIM A307(1991),273 - the only right way *
***********************************************************************
      IMPLICIT NONE                                                       
      REAL ENER,FOSC,EPSR,EPSI,FINT,EMIN,EMAX,EPS, BETA2,GAMMA2,Z,A,RO,   
     *PI,WP2,S2,MEEV                                                      
      INTEGER LTAB,NTAB,LU,IE                                             
      PARAMETER (LTAB=500)                                                
      COMMON /XTRGTAB/ NTAB,ENER(LTAB),FOSC(LTAB),EPSR(LTAB),             
     *EPSI(LTAB),FINT(LTAB)                                               
      COMMON /XTRGVAR/ LU,EMIN,EMAX,EPS,Z,A,RO,BETA2,GAMMA2, IE,PI,WP2,   
     *S2,MEEV                                                             
      REAL XDNDEDX,XFINTERA,LGE,E                                         
      COMPLEX I,CEPS1,C1,C2                                               
      I=CMPLX(0.,1.)                                                      
      E=EXP(LGE)                                                          
C  Epsilon-1                                                              
      CEPS1 = CMPLX( XFINTERA(LGE,ENER,EPSR,NTAB)/E**2, EXP(MAX(          
     *XFINTERA(MAX(LGE,EMIN),ENER,EPSI,NTAB), -99.)) )                    
      C1 = 1/GAMMA2-CEPS1*BETA2                                           
      C2 = C1/(1+CEPS1)*CLOG(2*BETA2*MEEV/E/C1)                           
      XDNDEDX = S2/BETA2 * (-2*AIMAG(C2)/WP2/PI + (1-EXP(MAX(             
     *XFINTERA(MAX(LGE,EMIN),ENER,FINT,NTAB), -99.)))/E**2)*E             
      END                                                                 
#if 0
***********************************************************************
      FUNCTION XFINTERA (X,A,F,N)                                         
* Descriprion:  simple linear interpolation                           *
***********************************************************************
      INTEGER N,K,K1,K2,NO                                                
      REAL XFINTERA,X,A(N),F(N),X1,X2                                     
      DATA K1,K2,NO/0,0,0/                                                
* print *, 'x=', x, ' a(1)=',a(1),' a(N)= ',a(n),' f(1)= ',f(1),' f(N)=', ), 'N=',N
* Unless (N==NO) & (A(K1)<x&x<A(K2))
      XFINTERA=F(1)                                                       
C    Skip Unless N>1 & X>A(1)                                             
         IF (N.LE.1.OR.X.LE.A(1))GO TO 1                                  
         K1=1                                                             
         K2=N                                                             
         NO=N                                                             
C    *                                                                    
C       Loop here                                                         
5011        IF(K2-K1.LE.1) GO TO 5012                                     
            K=(K1+K2)/2                                                   
C       *                                                                 
C          Check A(K)<X                                                   
               IF (A(K).LT.X) THEN                                        
               K1=K                                                       
            ELSE                                                          
               K2=K                                                       
            END IF                                                        
*      print *,'k1=',k1,' k2=',k2,' k=',k,' A(k1)=', A(k1), ' x=',x,' A(k2,A(k2)
         GO TO 5011                                                       
5012     CONTINUE                                                         
      X1=A(K1)                                                            
      X2=A(K1+1)                                                          
      XFINTERA=(F(K1)*(X-X2)+F(K1+1)*(X1-X))/(X1-X2)                      
C *                                                                       
C    Check ( XFINTERA > 13)                                               
         IF (( XFINTERA .GT. 13)) THEN                                    
         PRINT *, 'x=', X, ' a(1)=',A(1),' a(N)= ',A(N)                   
         PRINT *,' f(1)= ',F(1),' f(N)=', F(N), ' N=',N                   
         PRINT *,'============================================'           
      END IF                                                              
1     CONTINUE                                                            
      END                                                                 
#endif
***********************************************************************
      FUNCTION XGINT (EXT,A,B,EPS)                                        
* description:  simplest integration procedure with fixed accuracy    *
***********************************************************************
      INTEGER M/4/,N,I,K                                                  
      REAL XGINT,A,B,EPS,OTB,Y,D,EXT, U(4)/-.8611363,-.3399810,           
     *.3399810 ,.8611363/, W(4)/ .3478548, .6521452, .6521452, .3478548/  
     *                                                                    
      EXTERNAL EXT                                                        
      N=10                                                                
      OTB=0                                                               
C *                                                                       
C    Loop here                                                            
5011     CONTINUE                                                         
         Y=OTB                                                            
         OTB=0                                                            
         D=(B-A)*.5/N                                                     
C    *                                                                    
C       Loop here                                                         
            DO 5021 I=1,N                                                 
C       *                                                                 
C          Loop here                                                      
               DO 5031 K=1,M                                              
               OTB=OTB+W(K)*EXT(A+D*(2*I-1+U(K)))*D                       
5031        CONTINUE                                                      
5032        CONTINUE                                                      
5021     CONTINUE                                                         
5022     CONTINUE                                                         
         XGINT=OTB                                                        
         N=2*N                                                            
C    *                                                                    
C       Check N>100 000                                                   
            IF (N.GT.100 000) THEN                                        
            PRINT *,'Divergence !!!',A,B,OTB                              
            RETURN                                                        
         END IF                                                           
         IF (EPS.LE.0.OR.ABS(OTB-Y).LE.ABS(EPS*OTB))GO TO 5012            
      GO TO 5011                                                          
5012  CONTINUE                                                            
      END                                                                 
**********************************************************************
      SUBROUTINE XGASINI (LOUT)                                           
*  Description:                                                      *
*          dE/dx  simulation for the TRD Xe/Co2/CF4 mixture          *
*        Uses Grishin,Ermilova,Kotelnikov,NIM A307(1991),273         *
*                  PLN 18.04.92, data tables of ILG                  *
*                                                                    *
**********************************************************************
      IMPLICIT NONE                                                       
      REAL ENER,FOSC,EPSR,EPSI,FINT,EMIN,EMAX,EPS, BETA2,GAMMA2,Z,A,RO,   
     *PI,WP2,S2,MEEV                                                      
      INTEGER LTAB,NTAB,LU,IE                                             
      PARAMETER (LTAB=500)                                                
      COMMON /XTRGTAB/ NTAB,ENER(LTAB),FOSC(LTAB),EPSR(LTAB),             
     *EPSI(LTAB),FINT(LTAB)                                               
      COMMON /XTRGVAR/ LU,EMIN,EMAX,EPS,Z,A,RO,BETA2,GAMMA2, IE,PI,WP2,   
     *S2,MEEV                                                             
      INTEGER LOUT                                                        
      REAL XGINT,C,NAV,ERG,MB,ME,QE,R0,H,HE,NE,S1,EO,EI,E,S               
      EXTERNAL XSIGMA,XFREAL                                              
*
C -----    physic constants and normalisation stuff            -------    
      LU=LOUT                                                             
      EPS=0.01                                                            
      EMIN=12.0                                                           
      EMAX=1.E7                                                           
      CALL XGASDAT                                                        
C Light speed                                                             
      C=2.99979E+10                                                       
C Check LU>=6                                                             
      IF (LU.GE.6) WRITE(LU,5010)C                                        
5010  FORMAT(' XGASINI: Light speed          : C = ',E13.6)               
      PI =3.1415926                                                       
      NAV= 6.02214E23                                                     
C 1 ev to erg {erg}                                                       
      ERG=1.60218E-12                                                     
C Check LU>=6                                                             
      IF (LU.GE.6) WRITE(LU,5020)ERG                                      
5020  FORMAT(' XGASINI: 1 ev to erg {erg}    : ERG = ',E13.6)             
C 1mb to cm2                                                              
      MB=1.E-18                                                           
C Check LU>=6                                                             
      IF (LU.GE.6) WRITE(LU,5030)MB                                       
5030  FORMAT(' XGASINI: 1mb to cm2           : MB = ',E13.6)              
C Electron mass {g}                                                       
      ME=9.10943E-28                                                      
C Check LU>=6                                                             
      IF (LU.GE.6) WRITE(LU,5040)ME                                       
5040  FORMAT(' XGASINI: Electron mass {g}    : ME = ',E13.6)              
C same in ev                                                              
      MEEV=ME*C**2/ERG                                                    
C Check LU>=6                                                             
      IF (LU.GE.6) WRITE(LU,5050)MEEV                                     
5050  FORMAT(' XGASINI: same in ev           : MEEV = ',E13.6)            
C Electron charge{ESU}                                                    
      QE=4.80321E-10                                                      
C Check LU>=6                                                             
      IF (LU.GE.6) WRITE(LU,5060)QE                                       
5060  FORMAT(' XGASINI: Electron charge{ESU} : QE = ',E13.6)              
C electron radius{cm}                                                     
      R0=QE**2/ME/C**2                                                    
C Check LU>=6                                                             
      IF (LU.GE.6) WRITE(LU,5070)R0                                       
5070  FORMAT(' XGASINI: electron radius{cm}  : R0 = ',E13.6)              
C Plank constant{erg}                                                     
      H=1.05457E-27                                                       
C Check LU>=6                                                             
      IF (LU.GE.6) WRITE(LU,5080)H                                        
5080  FORMAT(' XGASINI: Plank constant{erg}  : H = ',E13.6)               
C same in ev                                                              
      HE=H*C/ERG                                                          
C Check LU>=6                                                             
      IF (LU.GE.6) WRITE(LU,5090)HE                                       
5090  FORMAT(' XGASINI: same in ev           : HE = ',E13.6)              
C Gas density                                                             
      RO=RO                                                               
C Check LU>=6                                                             
      IF (LU.GE.6) WRITE(LU,5100)RO                                       
5100  FORMAT(' XGASINI: Gas density          : RO = ',E13.6)              
C Electron density                                                        
      NE=NAV*Z/A*RO                                                       
C Check LU>=6                                                             
      IF (LU.GE.6) WRITE(LU,5110)NE                                       
5110  FORMAT(' XGASINI: Electron density     : NE = ',E13.6)              
C plasma freq**2 {ev}                                                     
      WP2=4*PI*R0*NE*HE**2                                                
C Check LU>=6                                                             
      IF (LU.GE.6) WRITE(LU,5120)WP2                                      
5120  FORMAT(' XGASINI: plasma freq**2 {ev}  : WP2 = ',E13.6)             
C x-section to F.osc                                                      
      S1=MB/(2*PI**2*R0*HE*Z)                                             
C Check LU>=6                                                             
      IF (LU.GE.6) WRITE(LU,5130)S1                                       
5130  FORMAT(' XGASINI: x-section to F.osc   : S1 = ',E13.6)              
C dN/dx scale                                                             
      S2=2*PI*R0*NE*QE**2/ERG                                             
C Check LU>=6                                                             
      IF (LU.GE.6) WRITE(LU,5140)S2                                       
5140  FORMAT(' XGASINI: dN/dx scale          : S2 = ',E13.6)              
*
C ----  Sum over cross-section and prepare tables of ref.index   -----    
      S=0                                                                 
      EI=EMAX                                                             
C *                                                                       
C    Loop here                                                            
         DO 5151 IE=NTAB,1,-1                                             
         EO=EI                                                            
         EI=ENER(IE)                                                      
         S=S+XGINT(XSIGMA,EI,EO,EPS)                                      
         FINT(IE)=S                                                       
5151  CONTINUE                                                            
5152  CONTINUE                                                            
C *                                                                       
C    Loop here                                                            
         DO 5161 IE=1,NTAB                                                
         FINT(IE)=ALOG(FINT(IE)/S)                                        
         FOSC(IE)=FOSC(IE)-ALOG(S)                                        
5161  CONTINUE                                                            
5162  CONTINUE                                                            
C *                                                                       
C    Loop here                                                            
         DO 5171 IE=1,NTAB                                                
         E=EXP(ENER(IE))                                                  
         EPSI(IE)=ALOG(WP2/E*PI/2)+FOSC(IE)                               
C    scaled by                                                            
         EPSR(IE)=WP2/E*XGINT(XFREAL,0.,EMAX,EPS)  * E**2                 
5171  CONTINUE                                                            
5172  CONTINUE                                                            
      WRITE(LU,5180)S1*S                                                  
5180  FORMAT(' MGASINI: Cross-section sum  ',F13.6)                       
      END                                                                 
C * DECK,XGASDAT,T=GEANT.                                                 
*CMZ :          03/10/95  09.41.45  by  Pavel Nevski
*-- Author :    Baranov Serguei   22/09/95
********************************************************************
      SUBROUTINE XGASDAT                                                  
*             Parameters of Xe/CO2/CF4/Ar(3 atm) gas mixture       *
*             carefully tabulated by Igor Gavrilenko               *
********************************************************************
      IMPLICIT NONE                                                       
      REAL ENER,FOSC,EPSR,EPSI,FINT,EMIN,EMAX,EPS, BETA2,GAMMA2,Z,A,RO,   
     *PI,WP2,S2,MEEV                                                      
      INTEGER LTAB,NTAB,LU,IE                                             
      PARAMETER (LTAB=500)                                                
      COMMON /XTRGTAB/ NTAB,ENER(LTAB),FOSC(LTAB),EPSR(LTAB),             
     *EPSI(LTAB),FINT(LTAB)                                               
      COMMON /XTRGVAR/ LU,EMIN,EMAX,EPS,Z,A,RO,BETA2,GAMMA2, IE,PI,WP2,   
     *S2,MEEV                                                             
      INTEGER NGAS,NMAT, LXENON,LCARBON,LFTOR,LOXIGEN,LARGON,LHYDROGEN,   
     *I1,I2,I3,I4,I5,I6,I7,ILAST, NCOM,INDEX,NATOM,IADR                   
      REAL EXENON,SXENON, ECARBON,SCARBON, EFTOR,SFTOR, EOXIGEN,SOXIGEN,  
     * EARGON,SARGON, EHYDROGEN,SHYDROGEN, PRESS,DENS,AW,ZW,WEIGHT, EE,   
     *SS                                                                  
      CHARACTER*4 MEDIUM                                                  
      PARAMETER (LXENON=189)                                              
      PARAMETER (LCARBON=98)                                              
      PARAMETER (LFTOR=98)                                                
      PARAMETER (LOXIGEN=97)                                              
      PARAMETER (LARGON=138)                                              
      PARAMETER (LHYDROGEN=55)                                            
      PARAMETER (I1=1)                                                    
      PARAMETER(I2=I1+LXENON, I3=I2+LCARBON, I4=I3+LFTOR, I5=I4+LOXIGEN,  
     * I6=I5+LARGON, I7=I6+LHYDROGEN, NGAS=5, NMAT=8, ILAST=I7)           
      DIMENSION IADR(7),EE(ILAST),SS(ILAST), NCOM(NGAS),PRESS(NGAS),      
     *DENS(NGAS), MEDIUM(NMAT),INDEX(NMAT),NATOM(NMAT), AW(NMAT),         
     *ZW(NMAT),WEIGHT(NMAT)                                               
      DIMENSION EXENON(LXENON), ECARBON(LCARBON), EFTOR(LFTOR),           
     *EOXIGEN(LOXIGEN), EARGON(LARGON), EHYDROGEN(LHYDROGEN)              
      EQUIVALENCE (EXENON,EE(I1)),(ECARBON,EE(I2)),(EFTOR,EE(I3)),        
     *(EOXIGEN,EE(I4)), (EARGON,EE(I5)),(EHYDROGEN,EE(I6))                
      DIMENSION SXENON(LXENON), SCARBON(LCARBON), SFTOR(LFTOR),           
     *SOXIGEN(LOXIGEN), SARGON(LARGON), SHYDROGEN(LHYDROGEN)              
      EQUIVALENCE (SXENON,SS(I1)),(SCARBON,SS(I2)), (SFTOR,SS(I3)),       
     *(SOXIGEN,SS(I4)), (SARGON,SS(I5)),(SHYDROGEN,SS(I6))                
*
*            Dens   /.005485, .001842,  .0039,  .00178,   .00067/, origina
*"Ar CH10 at 25 C and 1.003 atm"
      DATA NCOM /1,2,2,1,2/, PRESS/.00, .00, .00, .90, 0.10 /, DENS /     
     *.005485, .001842, .0039, .00164, .00066/, MEDIUM /'Xe', 'C','O', 
     *'C','F', 'Ar', 'C','H'/, INDEX / 1 , 2, 4, 2, 3, 5, 2, 6 /, NATOM/  
     * 1 , 1, 2, 1, 4, 1, 1, 4 /, AW /131.3, 12, 16, 12, 19, 39.95, 12,   
     *1 /, ZW / 54, 6, 8, 6, 9, 18, 6, 1 /, IADR / I1, I2, I3, I4, I5,    
     *I6, I7 /                                                            
      DATA EXENON / 12.08, 13.45, 13.48, 13.62, 13.78, 13.93, 14.09,      
     *14.25, 14.42, 14.59, 14.76, 14.94, 15.12, 15.31, 15.50, 15.69,      
     *15.89, 16.10, 16.31, 16.53, 16.75, 16.98, 17.22, 17.46, 17.71,      
     *17.97, 18.23, 18.50, 18.78, 19.07, 19.37, 19.68, 20.00, 20.32,      
     *20.66, 21.01, 21.38, 21.75, 22.14, 22.54, 22.96, 23.39, 23.84,      
     *24.31, 24.80, 25.30, 25.83, 26.38, 26.95, 27.55, 28.18, 28.83,      
     *29.52, 30.24, 30.99, 31.79, 32.63, 33.51, 34.44, 35.42, 36.46,      
     *37.57, 38.74, 39.99, 41.33, 42.75, 44.28, 45.92, 47.68, 49.59,      
     *51.66, 53.90, 56.35, 59.04, 61.99, 65.25, 68.88, 72.93, 77.49,      
     *79.99, 82.65, 85.50, 88.56, 91.84, 95.37, 99.18, 103.3, 107.8,      
     *112.7, 118.1, 124.0, 126.5, 129.1, 131.9, 134.8, 137.8, 140.9,      
     *144.2, 147.6, 151.2, 155.0, 158.9, 163.1, 167.5, 172.2, 177.1,      
     *182.3, 187.8, 193.7, 200.0, 206.6, 213.8, 221.4, 229.6, 238.4,      
     *248.0, 258.3, 269.5, 281.8, 295.2, 309.9, 326.3, 344.4, 364.6,      
     *387.4, 413.3, 442.8, 476.8, 516.6, 563.5, 619.9, 670.0, 689.0,      
     *708.0, 729.0, 751.0, 775.0, 800.0, 827.0, 855.0, 886.0, 918.0,      
     *928.7, 953.7, 991.8, 1033., 1078., 1127., 1181., 1240., 1305.,      
     *1378., 1459., 1550., 1653., 1771., 1907., 2066., 2254., 2480.,      
     *2755., 3100., 3542., 4133., 4780., 4781., 5000., 5099., 5100.,      
     *5451., 5452., 5635., 6199., 6888., 7749., 8856.,10330.,12400.,      
     *15500.,20660., 31000.,34560.,35420.,41330.,49590.,61990.,82650.,    
     *124000.,145300. /                                                   
      DATA SXENON / 66.30, 66.30, 66.20, 65.72, 65.22, 64.69, 64.13,      
     *63.53, 62.88, 62.19, 61.45, 60.67, 59.83, 58.94, 58.00, 57.01,      
     *55.96, 54.87, 53.74, 52.55, 51.32, 50.05, 48.75, 47.40, 46.03,      
     *44.62, 43.19, 41.73, 40.25, 38.76, 37.26, 35.75, 34.23, 32.71,      
     *31.20, 29.69, 28.19, 26.71, 25.24, 23.79, 22.37, 20.98, 19.61,      
     *18.28, 16.98, 15.72, 14.51, 13.33, 12.21, 11.13, 10.10, 9.124,      
     *8.201, 7.332, 6.519, 5.762, 5.064, 4.425, 3.845, 3.406, 3.038,      
     *2.727, 2.444, 2.205, 2.004, 1.855, 1.731, 1.656, 1.591, 1.560,      
     *1.524, 1.519, 1.551, 1.673, 1.995, 2.658, 3.888, 6.009, 9.266,      
     *11.97, 15.27, 18.75, 22.04, 24.71, 26.49, 27.07, 26.28, 24.11,      
     *20.68, 16.31, 11.38, 9.637, 8.111, 6.783, 5.637, 4.656, 3.823,      
     *3.126, 2.550, 2.082, 1.710, 1.422, 1.207, 1.056, .9588, .9066,      
     *.8914, .9058, .9429, .9964, 1.061, 1.131, 1.203, 1.271, 1.334,      
     *1.388, 1.432, 1.462, 1.478, 1.479, 1.465, 1.436, 1.392, 1.336,      
     *1.267, 1.190, 1.105, 1.017, .9275, .8416, .7633, .7200, 4.300,      
     *4.750, 3.650, 3.150, 2.800, 2.500, 2.250, 2.000, 1.800, 1.600,      
     *1.849, 1.805, 1.732, 1.649, 1.558, 1.461, 1.358, 1.251, 1.141,      
     *1.031, .9199, .8108, .7044, .6019, .5047, .4140, .3310, .2568,      
     *.1927, .1397,.09911,.07193,.05560, .1578, .1408, .1338, .1882,      
     *.1584, .1835, .1678, .1296,.09741,.07080,.04931,.03248,.01982,      
     *.01083,.00497, .00166,.00666,.00625,.00419,.00261,.00146,.00069,    
     *.00024,.00016/                                                      
      DATA ECARBON / 10, 20, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80,  
     * 85, 90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145,      
     *150, 155, 160, 165, 170, 175, 180, 185, 190, 195, 200, 210, 220,    
     *230, 240, 250, 260, 270, 280, 290, 300, 310, 320, 330, 340, 350,    
     *360, 370, 380, 390, 400, 420, 440, 460, 480, 500, 520, 540, 560,    
     *580, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1100, 1200,      
     *1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2500, 3000, 3500,   
     *4000, 4500, 5000, 5500, 6000, 8000, 10000, 15000, 20000, 30000 /    
      DATA SCARBON / 14.0, 10.0, 5.78, 4.42, 3.44, 2.73, 2.21, 1.81,      
     *1.51, 1.28, 1.09, .940, .820, .720, .630, .560, .500, .451, .407,   
     *.369, .336, .307, .281, .259, .238, .220, .204, .190, .176, .164,   
     *.154, .144, .135, .126, .119, .112, .105, .094, .084, .075, .068,   
     *.061, .056, .051, .046, 1.02, .950, .880, .820, .760, .710, .660,   
     *.620, .580, .550, .510, .484, .430, .384, .344, .309, .279, .253,   
     *.230, .210, .191, .175, .142, .117, .097, .082, .069, .059, .051,   
     *.0443, .0339, .0265, .0211, .0171, .0140, .0116, .0097, .0082,      
     *.007, .006, .0031,.00179,.00112,.00075,.00052, .00038,.000284,      
     *.000218,.88E-4,.46E-4,.16E-4,.86E-5,.506E-5/                        
      DATA EFTOR / 13, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85,    
     *90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150,      
     *155, 160, 165, 170, 175, 180, 185, 190, 195, 200, 210, 220, 230,    
     *240, 250, 260, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360,    
     *370, 380, 390, 400, 420, 440, 460, 480, 500, 520, 540, 560, 580,    
     *600, 650, 700, 750, 800, 850, 900, 950, 1000, 1100, 1200, 1300,     
     *1400, 1500, 1600, 1700, 1800, 1900, 2000, 2500, 3000, 3500, 4000,   
     *4500, 5000, 5500, 6000, 8000, 10000, 15000, 20000, 30000, 40000/    
      DATA SFTOR / 9.44, 9.44, 10.22, 9.37, 8.49, 7.64, 6.86, 6.15,       
     *5.51, 4.95, 4.45, 4.01, 3.63, 3.28, 2.98, 2.71, 2.47, 2.26, 2.07,   
     *1.90, 1.75, 1.61, 1.49, 1.37, 1.27, 1.18, 1.10, 1.02, .960, .890,   
     *.840, .780, .740, .690, .650, .610, .540, .487, .437, .394, .356,   
     *.323, .294, .268, .246, .226, .208, .191, .177, .164, .152, .142,   
     *.132, .123, .115, .108, .095, .084, .075, .067, .060, .055, .0494,  
     * .0450, .0411, .0376, .0306, .444, .375, .319, .274, .238, .207,    
     *.182, .142, .113, .092, .075, .063, .053, .0447, .0383, .033,       
     *.0286, .0154, .0092, .0059,.00401,.00284,.00209, .00158,.00122,     
     *.000468,.00024,.729E-4,.331E-4,.131E-4,.836E-5/                     
      DATA EOXIGEN / 15, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85,  
     * 90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150,     
     *155, 160, 165, 170, 175, 180, 185, 190, 195, 200, 210, 220, 230,    
     *240, 250, 260, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360,    
     *370, 380, 390, 400, 420, 440, 460, 480, 500, 520, 540, 560, 580,    
     *600, 650, 700, 750, 800, 850, 900, 950, 1000, 1100, 1200, 1300,     
     *1400, 1500, 1600, 1700, 1800, 1900, 2000, 2500, 3000, 3500, 4000,   
     *4500, 5000, 5500, 6000, 8000, 10000, 15000, 20000, 30000/           
      DATA SOXIGEN / 10.49, 10.49, 9.25, 8.04, 6.95, 6.00, 5.20, 4.52,    
     *3.94, 3.45, 3.04, 2.69, 2.39, 2.13, 1.91, 1.71, 1.55, 1.40, 1.27,   
     *1.16, 1.06, .9700, .8900, .8200, .7600, .7000, .6500, .6000,        
     *.5600, .5200, .4880, .4570, .4280, .4020, .3770, .3550, .3160,      
     *.2820, .2530, .3940, .2060, .1870, .1710, .1560, .1430, .1310,      
     *.1210, .1120, .1040, .0960, .0890, .0830, .0780, .0730, .0680,      
     *.0640, .0560, .0500, .0446, .0400, .0360, .0326, .5700, .5300,      
     *.4830, .4450, .3660, .3050, .2570, .2180, .1870, .1610, .1400,      
     *.1230, .0950, .0760, .0610, .0499, .0413, .0346, .0292, .0249,      
     *.0214, .0185, .0098, .0058, .0037, .0025,.00177,.00129, .00097,     
     *.00075,.000302,.000154,.476E-4,.226E-4,.99E-5/                      
      DATA EARGON / 15.70 , 15.89 , 16.10 , 16.31 , 16.53 , 16.75 ,       
     *16.98 , 17.22 , 17.46 , 17.71 , 17.97 , 18.23 , 18.50 , 18.78 ,     
     *19.07 , 19.37 , 19.68 , 20.00 , 20.32 , 20.66 , 21.01 , 21.38 ,     
     *21.75 , 22.14 , 22.54 , 22.96 , 23.39 , 23.84 , 24.31 , 24.80 ,     
     *25.30 , 25.83 , 26.38 , 26.95 , 27.55 , 28.18 , 28.83 , 29.52 ,     
     *30.24 , 30.99 , 31.79 , 32.63 , 33.51 , 34.44 , 35.42 , 36.46 ,     
     *37.57 , 38.74 , 39.99 , 41.33 , 42.75 , 44.28 , 45.92 , 47.68 ,     
     *49.59 , 51.66 , 53.90 , 56.35 , 59.04 , 61.99 , 65.25 , 68.88 ,     
     *72.93 , 77.49 , 82.65 , 88.56 , 95.37 , 103.30 , 112.70 , 124.00 ,  
     * 130.50 , 137.80 , 145.90 , 155.00 , 165.30 , 177.10 , 190.70 ,     
     *206.60 , 225.40 , 245.00 , 248.00 , 258.30 , 269.50 , 281.80 ,      
     *295.20 , 310.00 , 326.30 , 344.40 , 364.70 , 387.40 , 413.30 ,      
     *442.80 , 476.90 , 516.60 , 563.60 , 619.90 , 652.50 , 688.80 ,      
     *729.30 , 774.90 , 826.50 , 885.60 , 953.70 , 1033.00 , 1127.00 ,    
     *1240.00 , 1305.00 , 1378.00 , 1459.00 , 1550.00 , 1653.00 ,         
     *1771.00 , 1907.00 , 2066.00 , 2254.00 , 2480.00 , 2755.00 ,         
     *3100.00 , 3204.00 , 3263.00 , 3444.00 , 3646.00 , 3874.00 ,         
     *4133.00 , 4428.00 , 4768.00 , 5166.00 , 5635.00 , 6199.00 ,         
     *6888.00 , 7749.00 , 8856.00 , 10330.00 , 12400.00 , 15500.00 ,      
     *20660.00 , 31000.00 , 61990.00 /                                    
      DATA SARGON / 29.2 , 29.5 , 30.3 , 31.1 , 31.8 , 32.5 , 33.1 ,      
     *33.7 , 34.2 , 34.7 , 35.1 , 35.5 , 35.8 , 36.1 , 36.3 , 36.5 ,      
     *36.6 , 36.7 , 36.8 , 36.7 , 36.7 , 36.5 , 36.3 , 36.1 , 35.7 ,      
     *35.4 , 34.9 , 34.4 , 33.8 , 33.1 , 32.3 , 31.4 , 30.5 , 29.5 ,      
     *28.3 , 27.1 , 25.7 , 24.3 , 22.7 , 21.0 , 19.1 , 17.1 , 15.0 ,      
     *12.8 , 10.3 , 7.77 , 6.10 , 4.62 , 3.41 , 2.47 , 1.77 , 1.30 ,      
     *1.03 , .914 , .916 , 1.00 , 1.13 , 1.28 , 1.36 , 1.42 , 1.45 ,      
     *1.48 , 1.48 , 1.47 , 1.45 , 1.41 , 1.36 , 1.29 , 1.20 , 1.10 ,      
     *1.05 , .987 , .923 , .856 , .785 , .709 , .630 , .547 , .461 ,      
     *.381 , 4.66 , 4.23 , 3.83 , 3.45 , 3.10 , 2.76 , 2.45 , 2.16 ,      
     *1.89 , 1.64 , 1.41 , 1.20 , 1.01 , .836 , .682 , .546 , .484 ,      
     *.426 , .373 , .324 , .278 , .237 , .199 , .165 , .135 , .108 ,      
     *.0955 , .0842 , .0736 , .0639 , .0549 , .0467 , .0393 , .0326 ,     
     *.0266 , .0213 , .0166 , .0126 , .0117 , .0959 , .0827 , .0706 ,     
     *.0598 , .0501 , .0414 , .0338 , .0271 , .0213 , .0164 , .0123 ,     
     *.00889 , .00616 , .00403 , .00244 , .00132 , .000599 , .000196 ,    
     *.000029 /                                                           
      DATA EHYDROGEN / 13.60 , 16.70 , 21.20 , 26.80 , 30.50 , 40.80 ,    
     *49.30 , 72.40 , 80.00 , 91.50 , 100.00 , 120.00 , 130.00 , 150.00   
     *, 170.00 , 200.00 , 250.00 , 300.00 , 350.00 , 400.00 , 450.00 ,    
     *500.00 , 550.00 , 600.00 , 650.00 , 700.00 , 750.00 , 800.00 ,      
     *850.00 , 900.00 , 950.00 , 1000.00 , 1500.00 , 2000.00 , 3000.00 ,  
     * 4000.00 , 5000.00 , 6000.00 , 8000.00 , 10000.00 , 15000.00 ,      
     *20000.00 , 30000.00 , 40000.00 , 50000.00 , 60000.00 , 80000.00 ,   
     *100000.00 ,150000.00 ,200000.00 , 300000.00 ,400000.00 ,500000.00   
     *,600000.00 ,800000.00 /                                             
      DATA SHYDROGEN / 6.40 , 3.632 , 1.888 , .9707 , .6760 , .2892 ,     
     *.1620 , .0519 , .0380 , .02540 , .0193 , .0110 , .00853 , .00545 ,  
     * .00369 , .00220 , .00107 , .000604 , .000363 , .000232 , .000161   
     *, .000113 , .0000812 , .0000615 , .0000489 , .0000383 , .0000297 ,  
     * .0000236 , .0000197 , .0000165 , .0000134 , .0000109 , .00000293,  
     * .00000111, .280E-6 , .105E-6 , .490E-7 , .263E-7 , .979E-8 ,       
     *.453E-8 , .112E-8 , .416E-9 , .102E-9 , .376E-10 , .173E-10 ,       
     *.919E-11 , .337E-11 , .155E-11 , .377E-12 , .138E-12 , .336E-13 ,   
     *.123E-13 , .565E-14 , .299E-14 , .110E-14 /                         
*
      INTEGER INI,LG,LC,I,J,K,L,M                                         
      REAL XFINTERA,EM,EO,E,S,W,SP                                        
      DATA INI/0/                                                         
C Skip Unless INI==0                                                      
      IF (INI.NE.0)GO TO 1                                                
      INI=1                                                               
*
C ---   prepare E,x-section tables, mix gases and sum x-sections   ----   
      EMIN=ALOG(EMIN)                                                     
      EMAX=ALOG(EMAX)                                                     
C translate everything to logarithms                                      
C *                                                                       
C    Loop here                                                            
         DO 5011 I=1,ILAST-1                                              
         E=SS(I)                                                          
         SS(I)=ALOG(SS(I))                                                
         EE(I)=ALOG(EE(I))                                                
5011  CONTINUE                                                            
5012  CONTINUE                                                            
      EM=EMIN                                                             
      M=0                                                                 
C *                                                                       
C    Loop here                                                            
5021     IF(EM.GE.EMAX) GO TO 5022                                        
         M=M+1                                                            
         ENER(M)=EM                                                       
         EO=EM                                                            
         EM=EMAX                                                          
         K=0                                                              
C    *                                                                    
C       Loop here                                                         
            DO 5031 LG=1,NGAS                                             
C       *                                                                 
C          Loop here                                                      
               DO 5041 LC=1,NCOM(LG)                                      
               K=K+1                                                      
C          Skip Unless PRESS(LG)*NATOM(K)>0                               
               IF (PRESS(LG)*NATOM(K).LE.0)GO TO 5041                     
               J=IADR(INDEX(K))                                           
               L=IADR(INDEX(K)+1)-J                                       
C          *                                                              
C             Loop here                                                   
                  DO 5051 I=1,L                                           
                  E=EE(J+I-1)                                             
                  IF (EO.LT.E.AND.E.LT.EM) EM=E                           
5051           CONTINUE                                                   
5052           CONTINUE                                                   
5041        CONTINUE                                                      
5042        CONTINUE                                                      
5031     CONTINUE                                                         
5032     CONTINUE                                                         
      GO TO 5021                                                          
5022  CONTINUE                                                            
      NTAB=M                                                              
C *                                                                       
C    Check M>=LTAB                                                        
         IF (M.GE.LTAB) THEN                                              
         STOP ' XGASDAT Fatal data error - table too short '              
      END IF                                                              
      K=0                                                                 
      S=0                                                                 
      Z=0                                                                 
      A=0                                                                 
      RO=0                                                                
      SP=0                                                                
C *                                                                       
C    Loop here                                                            
         DO 5061 LG=1,NGAS                                                
         RO=RO+DENS(LG)*PRESS(LG)                                         
         SP=SP+PRESS(LG)                                                  
C    *                                                                    
C       Loop here                                                         
            DO 5071 LC=1,NCOM(LG)                                         
            K=K+1                                                         
            W=PRESS(LG)*NATOM(K)                                          
            WEIGHT(K)=W                                                   
C       Skip Unless W>0                                                   
            IF (W.LE.0)GO TO 5071                                         
            S=S+W                                                         
            Z=Z+ZW(K)*W                                                   
            A=A+AW(K)*W                                                   
5071     CONTINUE                                                         
5072     CONTINUE                                                         
5061  CONTINUE                                                            
5062  CONTINUE                                                            
* Do j=1,k { weight(j)/=S; }
C *                                                                       
C    Loop here                                                            
         DO 5081 IE=1,NTAB                                                
         E=ENER(IE)                                                       
         K=0                                                              
         FOSC(IE)=0                                                       
C    get weighted x-section                                               
C    *                                                                    
C       Loop here                                                         
            DO 5091 LG=1,NGAS                                             
C       *                                                                 
C          Loop here                                                      
               DO 5101 LC=1,NCOM(LG)                                      
               K=K+1                                                      
C          Skip Unless WEIGHT(K)>0                                        
               IF (WEIGHT(K).LE.0)GO TO 5101                              
               M=INDEX(K)                                                 
               J=IADR(M)                                                  
               L=IADR(M+1)-J+1                                            
C          Skip Unless E>=EE(J)                                           
               IF (E.LT.EE(J))GO TO 5101                                  
               FOSC(IE)=FOSC(IE)+EXP(MAX(XFINTERA (E,EE(J),SS(J),L),      
     *         -50.))*WEIGHT(K)*A/Z                                       
5101        CONTINUE                                                      
5102        CONTINUE                                                      
5091     CONTINUE                                                         
5092     CONTINUE                                                         
C    *                                                                    
C       Check FOSC(IE)>0                                                  
            IF (FOSC(IE).GT.0) THEN                                       
            FOSC(IE) = ALOG(FOSC(IE))                                     
         ELSE                                                             
            FOSC(IE)=-99                                                  
         END IF                                                           
5081  CONTINUE                                                            
5082  CONTINUE                                                            
      WRITE(LU,5110)(MEDIUM(I),I=1,K)                                     
5110  FORMAT(' XGASINI: materials ',10A8)                                 
      WRITE(LU,5120)(WEIGHT(I),I=1,K)                                     
5120  FORMAT(' XGASINI: weights - ',10F8.5)                               
      PRINT *,' A =',A,'  Z =',Z                                          
1     CONTINUE                                                            
      END                                                                 
#if 0
**********************************************************************
      SUBROUTINE XTRFILL (A,B,C)                                          
**********************************************************************
      CHARACTER A*(*),AA*40                                               
      INTEGER B,C                                                         
      LOGICAL FIRST/.TRUE./,ON/.FALSE./                                   
      L=LENOCC(A)                                                         
      AA=A(1:L)                                                           
      CALL MYFILL(AA,B,C)                                                 
      END                                                                 
***************************************************************************
      SUBROUTINE XGASPLT                                                  
***************************************************************************
      IMPLICIT NONE                                                       
      REAL ENER,FOSC,EPSR,EPSI,FINT,EMIN,EMAX,EPS, BETA2,GAMMA2,Z,A,RO,   
     *PI,WP2,S2,MEEV                                                      
      INTEGER LTAB,NTAB,LU,IE                                             
      PARAMETER (LTAB=500)                                                
      COMMON /XTRGTAB/ NTAB,ENER(LTAB),FOSC(LTAB),EPSR(LTAB),             
     *EPSI(LTAB),FINT(LTAB)                                               
      COMMON /XTRGVAR/ LU,EMIN,EMAX,EPS,Z,A,RO,BETA2,GAMMA2, IE,PI,WP2,   
     *S2,MEEV                                                             
      CHARACTER*4 OPT(6)/'NBOX','NSTA','FIT','LOGY','    ','    '/        
      REAL ELOG(LTAB)                                                     
      INTEGER K                                                           
C ---------------           plot some results          -----------------  
C ------                                                                  
      CALL HTITLE('Ionization losses theory')                             
      K=0                                                                 
C *                                                                       
C    Loop here                                                            
         DO 5011 IE=1,NTAB                                                
         ELOG(IE)=ENER(IE)/ALOG(10.)                                      
5011  CONTINUE                                                            
5012  CONTINUE                                                            
      K=K+1                                                               
      CALL HBOOK1(K,'(REAL(EPSILON)-1) * E**2 VS ENERGY',5,1.,5.,0)       
      CALL HMINIM(K,-2+0.)                                                
      CALL HMAXIM(K,+1+0.)                                                
      CALL HPLOT(K,' ',' ',0)                                             
      CALL HPLAX('LGE','(REAL(EPSILON)-1) * E**2')                        
      CALL HPLFUN(ELOG,EPSR,NTAB,' ')                                     
      K=K+1                                                               
      CALL HBOOK1(K,'LG(IMAG (EPSILON) ) VS ENERGY',5,1.,5.,0)            
      CALL HMINIM(K,-30+0.)                                               
      CALL HMAXIM(K,0+0.)                                                 
      CALL HPLOT(K,' ',' ',0)                                             
      CALL HPLAX('LGE','LG(IMAG (EPSILON) )')                             
      CALL HPLFUN(ELOG,EPSI,NTAB,' ')                                     
      K=K+1                                                               
      CALL HBOOK1(K,'LG(OSCILLATOR STRENGTH) VS ENERGY',5,1.,5.,0)        
      CALL HMINIM(K,-20+0.)                                               
      CALL HMAXIM(K,0+0.)                                                 
      CALL HPLOT(K,' ',' ',0)                                             
      CALL HPLAX('LGE','LG(OSCILLATOR STRENGTH)')                         
      CALL HPLFUN(ELOG,FOSC,NTAB,' ')                                     
      K=K+1                                                               
      CALL HBOOK1(K,'LG(INTEGRATED CROSS-SECTION) VS ENERGY',5,1.,5.,0)   
      CALL HMINIM(K,-5+0.)                                                
      CALL HMAXIM(K,0+0.)                                                 
      CALL HPLOT(K,' ',' ',0)                                             
      CALL HPLAX('LGE','LG(INTEGRATED CROSS-SECTION)')                    
      CALL HPLFUN(ELOG,FINT,NTAB,' ')                                     
      END                                                                 
#endif
