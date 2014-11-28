C: definitions from /afs/cern.ch/user/n/nevski/bin/geant3
************************************************************************
      SUBROUTINE HEPTUP                                                 
      CALL HERMES (0)                                                   
      CALL HEPHELP                                                      
      END                                                               
************************************************************************
      SUBROUTINE HEPEXAMPLE                                             
      INTEGER MM(2)/0,0/,DD(2)/0,0/,IW(2)/90,91/,P/0/                   
      REAL PP(3),VV(3)                                                  
*   call HEPfat
*   call HEPdense
      INTEGER N/1/,K                                                    
C *                                                                     
      DO 5011 K=1,N                                                     
         NP=12000                                                       
C    *                                                                  
         DO 5021 J=1,4300                                               
*                                      B,F,  Et,At,    A1,Z1,A2,Z2
            CALL HEPEVENT ('hijing',0,NP, 3.,1.5,100.,0.1, 197.,97.,    
     *      197.,97.)                                                   
*      more information like number of participants etc
*      call hepinfo  ( 101,102,104,105)
C       *                                                               
            DO 5031 I=1,NP                                              
               PP(1)=1                                                  
               PP(2)=2                                                  
               PP(3)=3*RNDM(1)                                          
               VV(1)=0                                                  
               VV(2)=0                                                  
               VV(3)=.01*RNDM(1)                                        
               CALL HEPPART (I,1,421,MM,DD,PP,10.,1.,VV,0.)             
5031        CONTINUE                                                    
5032        CONTINUE                                                    
5021     CONTINUE                                                       
5022     CONTINUE                                                       
5011  CONTINUE                                                          
5012  CONTINUE                                                          
      N=2*N                                                             
*   call hepend('z')
      END                                                               
************************************************************************
      SUBROUTINE HEPHELP                                                
      PRINT *,'*********************************************************
     ******************'                                                
      PRINT *,'* A utility set to write a standard HEPEVNT n-tuple 999 i
     *n evgen.run.nt  *'                                                
      PRINT *,'*********************************************************
     ******************'                                                
      PRINT *,'*          mandatory Calles:                             
     *                *'                                                
      PRINT *,'* HEPEvent (generator, run, Npart, B,F,Et,At, A1,Z1,A2,Z2
     *) - new event   *'                                                
      PRINT *,'* HEPPart  (ipa,ist,pdg, moth,idau,pp, Ep,Am,vv,vt) - wri
     *te new particle *'                                                
      PRINT *,'* HEPEnd   (option) - close ntuple and compress it on "z"
     * option         *'                                                
      PRINT *,'*          optional Calls:                               
     *                *'                                                
      PRINT *,'* HEPinfo  (i1,i2,i3,i4) - add more information to the ne
     *w event header  *'                                                
      PRINT *,'* HEPdens  - dense packing: no mother-daughter relations,
     * no vertex info *'                                                
      PRINT *,'* HEPfat   - fat packing: precise vertex info            
     *                *'                                                
      PRINT *,'* HEPnormal- return to default packing: vertex limited wi
     *thin 1 mk       *'                                                
      PRINT *,'*          experts Call:                                 
     *                *'                                                
      PRINT *,'* HEPmax (IPdg, IRef, NPart, Vxyzt, Nbit) - set limits on
     * HEP variables  *'                                                
      PRINT *,'*********************************************************
     ******************'                                                
      END                                                               
************************************************************************
      SUBROUTINE HEPRUN (RUN)                                           
      IMPLICIT NONE                                                     
      INTEGER SYSTEMF,GETPID,LENOCC,HEPNUMB,RUN,RID,IS,LUN/98/,LUX/2/   
      INTEGER NP,IDRUN,IEVT,IDAT,ITIM,IGEN,PID,IRNDM                    
      COMMON /HEP_HEAD/ NP,IDRUN,IEVT,IDAT,ITIM,IGEN,PID,IRNDM          
      LOGICAL BTEST                                                     
      CHARACTER*8 CP,CR                                                 
      CHARACTER LINE*80/' '/                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C Check RUN>0                                                           
      IF (RUN.GT.0) RETURN                                              
      CR = '1'
      call getopt('-run',CR)
      read (CR,*) RUN
      write(6,*) "  Run number =",RUN
      IDRUN=RUN                                                         
      PID = GETPID()                                                   
*
      IRNDM = PID                                                       
C *                                                                     
C    Loop here                                                          
         DO 5061 IS=0,31                                                
C    Check BTEST(IDRUN,IS)                                              
         IF (BTEST(IDRUN,IS)) IRNDM=IBSET(IRNDM,30-IS)                  
5061  CONTINUE                                                          
5062  CONTINUE                                                          
      CALL RLUXGO (LUX,IRNDM,0,0)                                       
      PRINT *,'CALL RANLUX(RVEC,LEN) to generate random numbers (V115)' 
*
      END                                                               
************************************************************************
      SUBROUTINE HEPEVENT (GENERATOR, RUN, NPART, B,F,ET,AT, A1,Z1,A2,  
     *Z2)                                                               
      IMPLICIT NONE                                                     
      INTEGER GETPID,IVER/11/,IPMX/1000000/,MXRF/1/,MXPA/65000/,NV/16/  
      INTEGER MAXIP,MAXRF,MAXPA,MAXNV,K,IC/0/,ID/999/,LUX/2/,I1,I2,I3,  
     *I4                                                                
      REAL VXMAX,VXMX/0.001/,VXMM                                       
* Input parameters:
      CHARACTER GENERATOR*(*)                                           
      INTEGER RUN,NPART,IPA,IST,PDG,MOTH(2),IDAU(2)                     
      REAL B,F,ET,AT,A1,Z1,A2,Z2,PP(3),EP,AM,VV(3),VT                   
* Cernlib related:
      INTEGER NWPAW,IPAW, LENOCC, SYSTEMF, LREC,BSIZE, IQUEST           
      PARAMETER (NWPAW=1 000 000, LREC=8100, BSIZE=LREC)                
      COMMON /PAWC/ IPAW(NWPAW)                                         
      COMMON /QUEST/ IQUEST(100)                                        
* Hepevnt related:
      INTEGER IS,L,MREF,LUH/99/                                         
      CHARACTER CR*8, CP*8, OPTION*1, GENER*20, FILE*20/' '/            
      INTEGER NP,IDRUN,IEVT,IDAT,ITIM,IGEN,PID,IRNDM                    
      COMMON /HEP_HEAD/ NP,IDRUN,IEVT,IDAT,ITIM,IGEN,PID,IRNDM          
      INTEGER IP,ISTAT,IPDG,MOT1,MOT2,IDA1,IDA2                         
      REAL PXYZ,ENER,MASS,VXYZ,VTIME                                    
      COMMON /HEP_PART/ IP,ISTAT,IPDG,MOT1,MOT2,IDA1,IDA2,PXYZ(3),ENER, 
     *MASS, VXYZ(3),VTIME                                               
* Local:
      PARAMETER (K=9)                                                   
      INTEGER I,CC(K)                                                   
      CHARACTER*20 GG(K),FF(K)                                          
      DATA (GG(I),FF(I),CC(I),I=1,K) / 'nexus' , 'nexus.optns' , 9, 'sta
     *rpom' , 'starpom.inp' , 8, 'starlight', 'starlight.in', 7,        
     *'venus' , 'optns.dat' , 6, 'hijing' , 'hijev.inp' , 5, 'mevsim' , 
     *'mult_gen.in' , 4, 'rqmd' , 'rqmd.inp' , 3, 'pythia' , 'pythia.dat
     *a' , 1, 'user' , 'user.input' , 0/                                
      LOGICAL FIRST/.TRUE./                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C Check FIRST                                                           
      IF (FIRST) THEN                                                   
      FIRST=.FALSE.                                                     
      MREF = MXRF*MXPA                                                  
      GENER= GENERATOR                                                  
      CALL CUTOL(GENER)                                                 
C Check RUN > 0                                                         
      IF (RUN .GT. 0) IDRUN = RUN                                       
C Check IDRUN<=0                                                        
      IF (IDRUN.LE.0) CALL HEPRUN(IDRUN)                                
      CALL HEPNUMBER (IDRUN,CR)                                         
      PID = GETPID()                                                    
      CALL HEPNUMBER (PID,CP)                                           
*   Is HBOOK and memory initialised ?
C *                                                                     
C    Check IPAW(1)==0                                                   
         IF (IPAW(1).EQ.0) THEN                                         
         PRINT *,' HBOOK initialised for HEP'                           
         CALL HLIMIT(NWPAW)                                             
      END IF                                                            
*   print *,' hbook initialised with len = ',Ipaw(1)
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      VXMM=0                                                            
C Check VXMX>0                                                          
      IF (VXMX.GT.0) VXMM=-VXMX                                         
      FILE='evgen.'//CR(1:LENOCC(CR))//'.nt'                            
C  anyway truncated in HRFILE to 65000                                  
      IQUEST(10) = 66000                                                
      CALL HROPEN (LUH,'HEPEVNT',FILE,'QN7',LREC,IS)                    
C Check IS!=0                                                           
      IF (IS.NE.0) STOP ' HEPTUPLE: Can not open output file '          
      CALL RZCDIR ('//HEPEVNT', ' ')                                    
      CALL HBSET ('BSIZE',BSIZE,IS)                                     
C Check IS!=0                                                           
      IF (IS.NE.0) STOP ' HEPTUPLE: Can not set buffer size '           
      CALL HBNT (ID,'HEPEVNT',' ')                                      
      CALL HEPBNAME (ID,IP, 'itrac' , 0, -1, MXPA)                      
      CALL HEPBNAME (ID,ISTAT,'istat' , 0, -1, 15)                      
      CALL HEPBNAME (ID,IPDG, 'ipdg' , 0, -IPMX,IPMX)                   
      CALL HEPBNAME (ID,MOT1, 'moth1' , 0, -1, MREF)                    
      CALL HEPBNAME (ID,MOT2, 'moth2' , 0, -MREF, 1)                    
      CALL HEPBNAME (ID,IDA1, 'idau1' , 0, -1, MREF)                    
      CALL HEPBNAME (ID,IDA2, 'idau2' , 0, -1, MREF)                    
      CALL HEPBNAME (ID,PXYZ, 'Pxyz(3)' , 0, 0, 0)                      
      CALL HEPBNAME (ID,ENER, 'ener' , 0, 0, 0)                         
      CALL HEPBNAME (ID,MASS, 'mass:R:' ,16, -1, 10)                    
C  mm                                                                   
      CALL HEPBNAME (ID,VXYZ, 'Vxyz(3):R:' ,NV, VXMM, VXMX)             
C mm/c                                                                  
      CALL HEPBNAME (ID,VTIME,'Vtime:R:' ,NV, 0, VXMX)                  
*   1 mm/c=0.33 ns;   ct=3.e11: tmax=5000 -> 17 ns
C *                                                                     
C    Loop here                                                          
         DO 5011 IGEN=1,K-1                                             
         L=MIN(LENOCC(GENER),LENOCC(GG(IGEN)))                          
         IF (GENER(1:L).EQ.GG(IGEN)(1:L))GO TO 5012                     
5011  CONTINUE                                                          
5012  CONTINUE                                                          
      CALL HEPINPUT(FF(IGEN))                                           
      ENDIF                                                             
*
      CALL VZERO (IP,16)                                                
*  Call RLUXAT(Lux,Irndm,Ida1,Ida2)
* if (MRef>999) { ida1=Pid/Mref; ida2=Mod(Pid,Mref) }
      IEVT=IEVT+1                                                       
      IP=NPART                                                          
      ISTAT=IVER                                                        
      IPDG=IPMX                                                         
      CALL DATIME (IDAT,ITIM)                                           
      IPDG=IPDG-1                                                       
      PXYZ(1)=IDRUN                                                     
      PXYZ(2)=IEVT                                                      
      PXYZ(3)=IDAT                                                      
      PXYZ(4)=ITIM                                                      
      PXYZ(5)=CC(IGEN)                                                  
      CALL HFNT(ID)                                                     
      IPDG=IPDG-1                                                       
      PXYZ(1)=B                                                         
      PXYZ(2)=F                                                         
      PXYZ(3)=ET                                                        
      PXYZ(4)=AT                                                        
      PXYZ(5)=1                                                         
      CALL HFNT(ID)                                                     
      IPDG=IPDG-1                                                       
      PXYZ(1)=A1                                                        
      PXYZ(2)=Z1                                                        
      PXYZ(3)=A2                                                        
      PXYZ(4)=Z2                                                        
      PXYZ(5)=2                                                         
      CALL HFNT(ID)                                                     
      NP=NPART                                                          
      RETURN                                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENTRY HEPINFO (I1, I2, I3, I4)                                    
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IPDG=IPDG-1                                                       
      PXYZ(1)=I1                                                        
      PXYZ(2)=I2                                                        
      PXYZ(3)=I3                                                        
      PXYZ(4)=I4                                                        
      PXYZ(5)=3                                                         
      CALL HFNT(ID)                                                     
      RETURN                                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENTRY HEPPART (IPA,IST,PDG,MOTH,IDAU,PP,EP,AM,VV,VT)              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL RZCDIR('//HEPEVNT',' ')                                      
      IP = IPA                                                          
      ISTAT = MIN(IST,9)                                                
      IPDG = PDG                                                        
      MOT1 = MOTH(1)                                                    
      MOT2 = MOTH(2)                                                    
      IDA1 = IDAU(1)                                                    
      IDA2 = IDAU(2)                                                    
      CALL UCOPY(PP,PXYZ,3)                                             
      CALL UCOPY(VV,VXYZ,3)                                             
      VTIME = VT                                                        
      MASS = AM                                                         
      ENER = EP                                                         
*  if (ipa==Np) Ip=-1
      CALL HFNT(ID)                                                     
      RETURN                                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENTRY HEPEND(OPTION)                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL HROUT(0,IC,'NT')                                             
      CALL HREND('HEPEVNT')                                             
C Check OPTION=='z' | OPTION=='Z'                                       
      IF (OPTION.EQ.'z' .OR. OPTION.EQ.'Z') I=SYSTEMF('gzip -f '//      
     *FILE(1:LENOCC(FILE)))                                             
      RETURN                                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENTRY HEPNORMAL                                                   
      MXRF=1                                                            
      NV=16                                                             
      RETURN                                                            
      ENTRY HEPDENSE                                                    
      MXRF=0                                                            
      NV= 1                                                             
      RETURN                                                            
      ENTRY HEPFAT                                                      
      MXRF=1                                                            
      VXMX=0                                                            
      RETURN                                                            
      ENTRY HEPMAX (MAXIP, MAXRF, MAXPA, VXMAX, MAXNV)                  
      IPMX=MAXIP                                                        
      MREF=MAXRF                                                        
      MXPA=MAXPA                                                        
      VXMX=VXMAX                                                        
      NV=MAXNV                                                          
      RETURN                                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END                                                               
************************************************************************
      FUNCTION HEPNUMB (LINE,VAR)                                       
      IMPLICIT NONE                                                     
      INTEGER HEPNUMB,LENOCC,INDEX,ICHAR,I,J                            
      CHARACTER*(*) LINE,VAR                                            
      HEPNUMB = 0                                                       
      I=INDEX(LINE,VAR(1:LENOCC(VAR)))                                  
C Skip Unless I>0                                                       
      IF (I.LE.0)GO TO 1                                                
C *                                                                     
      DO 5011 I=I+LENOCC(VAR),LENOCC(LINE)                              
C    Skip Unless LINE(I:I)!=' '                                         
         IF (LINE(I:I).EQ.' ')GO TO 5011                                
         J=ICHAR(LINE(I:I))-ICHAR('0')                                  
C    Check J<0 | J>9                                                    
         IF (J.LT.0 .OR. J.GT.9) RETURN                                 
         HEPNUMB=10*HEPNUMB+J                                           
5011  CONTINUE                                                          
5012  CONTINUE                                                          
1     CONTINUE                                                          
      END                                                               
************************************************************************
      SUBROUTINE HEPINPUT (INPUT)                                       
      CHARACTER INPUT*(*),LINE*128                                      
      INTEGER LENOCC,LI/98/,ID/998/                                     
      CLOSE (LI)                                                        
      CALL HBNT (ID,'HEPinput',' ')                                     
      CALL HBNAMC (ID,'HEPinput',LINE, 'line(4):C*32:')                 
      OPEN (LI,FILE=INPUT(1:LENOCC(INPUT)),STATUS='OLD',ERR=  5010)     
C *                                                                     
C    Loop here                                                          
5021     CONTINUE                                                       
         READ (LI,'(a)',ERR=5010,END=5010) LINE                         
         CALL HFNT(ID)                                                  
      GO TO 5021                                                        
5022  CONTINUE                                                          
5010  CLOSE (LI)                                                        
      END                                                               
************************************************************************
      SUBROUTINE HEPBNAME(ID,VAR,FORM,NB,IA,IB)                         
      IMPLICIT NONE                                                     
      INTEGER LENOCC,INDEX,NB,ID,IA,IB,VAR,L                            
      CHARACTER C*8,CC*80,FORM*(*)                                      
      CC=FORM                                                           
      L=INDEX(FORM,':')                                                 
C Check L>0                                                             
      IF (L.GT.0) CC=FORM(1:L-1)                                        
C *                                                                     
C    Check IA!=0 | IB!=0                                                
         IF (IA.NE.0 .OR. IB.NE.0) THEN                                 
         CC = FORM                                                      
         CALL HEPNUMBER(NB,C)                                           
C    Check NB>0                                                         
         IF (NB.GT.0) CC=CC(1:LENOCC(CC))//C(1:LENOCC(C))               
C    Check INDEX(CC,':')>0                                              
         IF (INDEX(CC,':').GT.0) CC=CC(1:LENOCC(CC))//':'               
         CALL HEPNUMBER(IA,C)                                           
         CC=CC(1:LENOCC(CC))//'['//C(1:LENOCC(C))                       
         CALL HEPNUMBER(IB,C)                                           
         CC=CC(1:LENOCC(CC))//','//C(1:LENOCC(C))//']'                  
      END IF                                                            
      CALL HBNAME(ID,'particle',VAR,CC(1:LENOCC(CC)))                   
      END                                                               
************************************************************************
      SUBROUTINE HEPNUMBER(NUM,CNUM)                                    
      IMPLICIT NONE                                                     
      CHARACTER CNUM*(*),S*14                                           
      INTEGER ANUM,NUM,L,I,I1,I2                                        
      REAL RNUM                                                         
      EQUIVALENCE (RNUM,ANUM)                                           
      ANUM=NUM                                                          
C *                                                                     
C    Check ABS(NUM)<=1000000                                            
         IF (ABS(NUM).LE.1000000) THEN                                  
         WRITE (S, * ) ANUM                                             
      ELSE                                                              
         WRITE (S,'(f14.6)') RNUM                                       
      END IF                                                            
      I1=14                                                             
      I2=1                                                              
C *                                                                     
C    Loop here                                                          
         DO 5011 I=1,14                                                 
C    Skip Unless S(I:I)!=' '                                            
         IF (S(I:I).EQ.' ')GO TO 5011                                   
         I1=MIN(I1,I)                                                   
         I2=MAX(I2,I)                                                   
5011  CONTINUE                                                          
5012  CONTINUE                                                          
      CNUM=S(I1:I2)                                                     
      L=I2-I1+1                                                         
1     CONTINUE                                                          
      END                                                               
************************************************************************
      FUNCTION RNDM(SEED)                                               
      REAL RNDM,B                                                       
      INTEGER IRNDM,SEED,IB,LUX,INI,K1,K2,IVEC(25)                      
      EQUIVALENCE (IB,B)                                                
      LOGICAL FIRST/.TRUE./                                             
C *                                                                     
C    Check (FIRST)                                                      
         IF (FIRST) THEN                                                
         FIRST=.FALSE.                                                  
         PRINT *,'*** ','RNDM HAS BEEN REPLACED BY RANLUX',' ***'       
      END IF                                                            
      CALL RANLUX(B,1)                                                  
      RNDM = B                                                          
      RETURN                                                            
      ENTRY IRNDM (SEED)                                                
C *                                                                     
C    Check (FIRST)                                                      
         IF (FIRST) THEN                                                
         FIRST=.FALSE.                                                  
         PRINT *,'*** ','RNDM HAS BEEN REPLACED BY RANLUX',' ***'       
      END IF                                                            
      CALL RANLUX(B,1)                                                  
      IRNDM = IB                                                        
      RETURN                                                            
      ENTRY RDMIN (SEED)                                                
C *                                                                     
C    Check (FIRST)                                                      
         IF (FIRST) THEN                                                
         FIRST=.FALSE.                                                  
         PRINT *,'*** ','RNDM HAS BEEN REPLACED BY RANLUX',' ***'       
      END IF                                                            
      CALL RLUXGO(2,SEED,0,0)                                           
      PRINT *,' RANLUX restarted using ',2,SEED,0,0                     
      PRINT *,' Use RLUXIN(Ivec) to a direct restart'                   
      RETURN                                                            
      ENTRY RDMOUT (SEED)                                               
C *                                                                     
C    Check (FIRST)                                                      
         IF (FIRST) THEN                                                
         FIRST=.FALSE.                                                  
         PRINT *,'*** ','RNDM HAS BEEN REPLACED BY RANLUX',' ***'       
      END IF                                                            
      CALL RLUXAT(LUX,INI,K1,K2)                                        
      CALL RLUXUT(IVEC)                                                 
      PRINT *,' to restart RANLUX use RLUXGO(',LUX,INI,K1,K2,')'        
      PRINT *,' or use RLUXIN with the following vector: '              
      PRINT *,IVEC                                                      
      RETURN                                                            
      END                                                               
************************************************************************
      SUBROUTINE RANNOR(A,B)                                            
      IMPLICIT NONE                                                     
      REAL Y(2),A,B,X,R                                                 
      LOGICAL FIRST/.TRUE./                                             
C *                                                                     
C    Check (FIRST)                                                      
         IF (FIRST) THEN                                                
         FIRST=.FALSE.                                                  
         PRINT *,'*** ','RANNOR HAS BEEN REPLACED BY F(RANLUX)',' ***'  
      END IF                                                            
      CALL RANLUX(Y,2)                                                  
C Check Y(1).EQ.0.                                                      
      IF (Y(1).EQ.0.) CALL RANLUX(Y,2)                                  
      X = 6.283185*Y(2)                                                 
      R = SQRT (-2.0*LOG(Y(1)))                                         
      A = R*SIN (X)                                                     
      B = R*COS (X)                                                     
      RETURN                                                            
      END                                                               
************************************************************************
      subroutine getopt(myId,myOpt)
      
      character myId*4,myOpt*(*),c*100
      integer   iOpt,nOpt,iargc,l,lOpt
      
      nOpt = iargc();
      do 100 iOpt=1,nOpt-1
        call getarg(iOpt,c)
	if (c(1:4) .ne. myId) goto 100
        c = ' '
        call getarg(iOpt+1,c)
	l = lenocc(c)
        lOpt = len(myOpt)
	myopt(1:lOpt) = ' '
	myopt(1:l) = c(1:l)
	return
 100  continue
      end	

	
	
	
      
      
      
      
      
      
      
