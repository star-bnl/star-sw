   subroutine gstar_input
+CDE,typing,gcnum.
   print *,' Gstar User Input for TEXT, TXOLD and FZ formats activated '
   print *,' To activate XDF format readout do " gexec gstar_readxdf " '
   end

*************************************************************************
   subroutine agusopen(file)
*
* Description: open a TXT, EGZ(FZ) or XDF event generator data file.    *
*              Data type should corresponds to the filename extension   *
*              File types are saved in Ccommand - one letter per file   *
*************************************************************************
   Implicit   None   
   Integer    LENOCC,CSADDR,Iadr,i,J,ier,L,N,idot,Igate
   Character  file*(*),C*1,Table*120
   common     /agcuser/ Igate,Table
*
+CDE,AGCKINE.
*
    L=LENOCC(file);  Check L>0;
    C=' '
    Do i=1,L-1  {  check file(i:i)='.'; idot=i; C=file(i+1:i+1);  }
    Call CLTOU(C)
*
    N=LENOCC(CCOMMAND)+1;  Igate=N
    print *,' AgUsOpen: input from ',file,' mode ',C 
*
    if      C=='E'                       " egz format "
    {  Call AgzOPEN('PZ',file,'EK',0,0)
       Call AgZREAD('P',ier)
       if (Ier!=0)  goto :e:
    }
    else if C=='X'                       " xdf format "
    {  J=Csaddr('XDF_OPEN')
       If ( J==0 )  goto :e:
       call CsJCAL(J,2,file)
    }
    else if C=='T'                       "  any text  "
    {  Call AgFOPEN(21-N,file,ier) 
       if (ier!=0)  goto :e:
    } 
    else if C=='S'                       " STAF table "
    {  Table=File(1:idot-1)
    } 
    else if C=='M'                       "mickey-mouse"
    {  Iadr=CSADDR('GSTAR_MICKY');  If (Iadr!=0) Call CSJCAL(Iadr,0,0,0)
    } 
    CCOMMAND(N:N)=C
    return
:e: print *,' gstar_AgUsOPEN error openning file ',file(1:L)
   end

*************************************************************************
   Subroutine  AgUsRead(ier)
*                                                                       *
* Description: loop over all files opened by USER/INPUT                 *
*              and call the corresponding readout routine               *
*************************************************************************
   Implicit   None
+CDE,GCBANK,GCNUM,SCLINK,RBBANK,AgCKINE.
   Integer    LENOCC,CSADDR,AMI_CALL,Igate,Ier,J,I
   Character  C*1, o*1, Table*120
   common     /agcuser/ Igate,Table
*
*
   o=CHAR(0)
   Call HEPEVNT

   Do i=1,LENOCC(Ccommand)
     C=CCOMMAND(i:i); Igate=i
     if     C=='E' { Call AGZREAD('P',ier);  call gstar_ReadEGZ(Igate)     }
     elseif C=='X' { IrbDIV=IxDIV;           LKARP2=LkEvnt
                     J=CsADDR('XDF_READ'); If (J!=0) call CsJCAL(J,1,Igate)}
     elseif C=='T' {                         call gstar_ReadTXT(Igate)     }
     elseif C=='M' { J=CsADDR ('MICKINE'); IF (J!=0) Call CsJCAL(J,1,Igate)}
     elseif C=='S' { J=AMI_CALL ('gstar_readtab'//o,1,%L(Table)//o)        }

     If Igate<=0   { Ier=1; return }
     print *,' AgUsREAD mode ',C,': # particles in GEANT=',Ntrack,
                                 '  # vertices=',Nvertx
   enddo

*  To generate more header information:
   call GHeader
*
   End
 
*************************************************************************
   Subroutine    gstar_ReadTXT(Igate)
*                                                                       *
* Description: read both NEW and OLD gstar event generator text formats *
Replace [READ[DIGIT](#)#;] with [READ(#2,ERR=:E:)#3;IF(Idebug>=#1)<W>#3;] 
*************************************************************************
   implicit      none
+CDE,GCUNIT,GCFLAG.
*  character*8   tit, eg_name, frame 
   character*120 line
   integer       LENOCC,Index,li,Ieven,Ntrac,Nvert,itr,ivt,nv,nt,Igate,
                 LabelTr,LabelVx,ge_pid,eg_pid,StartVx,StopVx,i,
                 eg_proc,parent
   Real          version,east_z,east_a,west_z,west_a,sqrts,b_max,
                 PP(3),vert(4),UBUF(10)
   integer       istat,eg_pid,moth,daut,num(5)
   data          num/1,1,0,0,0/
   character     Cform*8 /'/6I 9F'/
   Real          phep,vhep
   common/GHEPEVT/ istat,eg_pid,moth(2),daut(2),phep(5),vhep(4)
*
*
   Li=21-Igate
 { Ntrac,Nvert } = 999;
 { itr, ivt    } =  0;
*
 WHILE itr<Ntrac | ivt<Nvert
 { line='end-of-file';   read5 (li,'(a)') Line; (1x,a)

   If Line(1:5)=='GENER'
   {  
     read1 (line(11:),*) version,east_z,east_a,west_z,west_a,sqrts,b_max
                         (' gstar_ReadNew: GENER :',F8.3,4F8.0,F8.1,F8.3)
   }
   else If Line(1:5)=='EVENT'
   { 
     read2 (line(7:),*,end=:a:)  Ieven,Ntrac,Nvert
                         (' gstar_ReadNew: EVENT :',3i8,2f8.3)
     :a: if (Ieven<=-999) goto :e: " end of data "
   }
   else If Line(1:5)='TRACK'
   { 
     read5 (line(7:),*)  ge_pid,PP,LabelTr,StartVx,StopVx,eg_pid  
                         (16x,'TRACK :',i6,3F9.3,4i6)

     Call VZERO(Vert,4); Call AgSVERT(vert,-StartVx,-Igate,Ubuf,0,nv)  
     Itr += 1;           call AgSKINE(PP,ge_pid,nv,Ubuf,0,nt)
   }
   else If Line(1:6)=='VERTEX'
   { 
     read5 (line(8:),*)  Vert,LabelVx,eg_proc,parent
                         (16x,'VERTEX:',4F10.6,3i6)
     ivt += 1;           call AgSVERT(vert,-LabelVx,-Igate,Ubuf,0,nv) 
   }
   else If Line(1:6)=='HEPEVT' & itr+ivt==0  
   { *             HEPEVT text format
     read1 (line(8:),*) Ntrac,Ieven; (' gstar_Read HEPEVT:',i8,' event#',i6)

     do itr=1,Ntrac
     {  read5(li,*) istat,eg_pid,moth,daut,phep,vhep; (6i5,5F8.2,4F9.3)
        num(3)=0;   If (itr==1) num(3)=1
        Call RbSTORE ('/EVNT/GENE/GENT*',num,Cform,15,istat)
        check Istat==1;       Call apdg2gea (eg_pid, ge_pid)
	if ge_pid<=0 
        {  if (Idebug>1) <W> eg_pid;(' gstar_read HEPEVT unknown particle',i6);
           ge_pid = 1000000+eg_pid
        }
        Call AgSVERT ( vhep,  0,  -Igate,  0,  0, nv); 
        Call AgSKINE ( phep, ge_pid,  nv, itr, 0, nt); 
     }  Break
   }
   else If Index(Line,'event')>0 & itr+ivt==0     
   { *              OLD text format
     i=Index(Line,'event');  line(i:i+6)='  ';
     read1 (line,*) Ntrac,Ieven; (' gstar_ReadOld: ',i8,' event# ',i6)
     call VZERO(vert,4); call AgSVERT(vert,-1,-Igate,Ubuf,0,nv)
     do itr=1,Ntrac
     {  read5 (li,*) ge_pid,PP; (16x,i6,3F8.3)
        call AgSKINE(PP,ge_pid,nv,Ubuf,0,nt)
     }  break
   }
   else If LENOCC(Line)>0
   { <w> line(1:LENOCC(Line)); (' unknown line : ',a); }
 } return
*
:e:<w> line; (' gstar_readTXT error in line '/1x,a);  Igate=-1 
   end

*************************************************************************
   subroutine gstar_ReadEGZ (Igate)
*                                                                       *
* Description: copy event generator data from ZEBRA banks (CODE/EVEN)   *
*              Banks are read in a standard way on LKRUNT/LKEVNT links  *
*************************************************************************
   Implicit   None
   Character  Cname*4,Gname*8,Gnamo*8/' '/
   Real       evtver,zproj,aproj,ztarg,atarg,sqrts,bmin,bmax,bimevt,
              plab(4),UBUF(100),VERT(4)/4*0/
   Integer    Iprin,L,i,nptls,nptarg,nntarg,npproj,nnproj,ntry,IdPtl,
              IRC,Ipart,Nvtx,Nt,Igate,Part_Size/6/
*
+CDE,GCBANK,GCUNIT,SCLINK,GCFLAG.
*
      Iprin=Idebug
      prin1; (' gstar_EGZ called to decode RUNT/EVNT banks ')
*
*     generator run header
      check   LKRUNT>0;    call UHTOC(IQ(LKRUNT-4),4,CNAME,4)
      check  "bank length" IQ(LKRUNT-1)>=10 & "name" CNAME=='CODE'
*
      call UHTOC(IQ(LKRUNT+1),4,Gname,8)
      evtver  = Q(LKRUNT+3)
      zproj   = Q(LKRUNT+4)
      aproj   = Q(LKRUNT+5)
      ztarg   = Q(LKRUNT+6)
      atarg   = Q(LKRUNT+7)
      sqrts   = Q(LKRUNT+8)
      bmin    = Q(LKRUNT+9)
      bmax    = Q(LKRUNT+10)
*
      If Gname!=Gnamo
      { Prin1 Gname,evtver,zproj,aproj,ztarg,atarg,sqrts,bmin,bmax
        (' Gstar_EGZ: generator ',a8,' version ',f8.3/10x,
        ' beam z/a',2f5.0,' target a/z',2f5.0,' sqrt(s)=',f6.1,' b=',2f8.3)
          Gnamo=Gname
      }
*     Normal event follows
      Check   LKEVNT>0;    Call UHTOC(IQ(LKEVNT-4),4,CNAME,4)
      check  "bank length" IQ(LKEVNT-1)>=7 & "name" CNAME=='EVEN'
*
*     Do leading part of bank
      nptls   = IQ(LKEVNT+1)
      nptarg  = IQ(LKEVNT+2)
      nntarg  = IQ(LKEVNT+3)
      npproj  = IQ(LKEVNT+4)
      nnproj  = IQ(LKEVNT+5)
      ntry    = IQ(LKEVNT+6)
      bimevt  =  q(LKEVNT+7)

      prin3 nptls,npproj,nnproj,nptarg,nntarg,ntry,bimevt
      (' Gstar_EGZ: setting GEANT vertex with Npart=',i6,' (before filter)'/,
        10x,' beam p/n=',2i5,'  targ p/n=',2i5,'  ntry=',i6,'  b=',f8.3)
      Call aGSVERT(VERT,-1,-Igate,Ubuf,0,Nvtx)
*
*     Do trailing part of bank:  ID, 4-momentum, daughter/parent information
      do i=1,nptls
         L=LKEVNT+7+(i-1)*part_size
         Ipart    =  0
         Idptl    = IQ(L+1)
         Call UCOPY (Q(L+2),plab,4)
         If (Gname='VENUS'  | Gname='HIJET')    Call ISA_to_GEANT (IdPtl,Ipart)
         If (Gname='HIJING')                    Call a PDG 2 GEA  (IdPtl,Ipart)
         If (Cname='FRITIOF'| Gname='FRTIOF17') Call LUND_to_GEANT(IdPtl,Ipart)
         if (Ipart<=0) then
           prin1 Idptl,Gname
           (' Gstar_EGZ: unknown particle ',i6,' from ',a)
           next
         endif
         Call AgGZUFLT('EGZ',1,I,Idptl,Vert,Plab,IRC);  Check IRC==0
         CALL AgSKINE (Plab,Ipart,Nvtx,Ubuf,0,Nt)
      enddo
*
   END

*****************************************************************************
 module  HEPEVNT  is the HEPEVNT event header
 author  P.Nevski
 created 01/01/98
*****************************************************************************
+CDE,AGECOM,GCBANK,SCLINK,RBBANK.

 Old Structure PASS {int SYS1, int SYS2, int SYS3, int PJID,int GJID,int EVID}

 Old Structure GENE {int SYS1, int SYS2, int SYS3, int GRUN,int GEVT,
                     Char GNAM, VRTX, VRTY, VRTZ, VRTT, int WTFL, WEIG }

 Old Structure GENT {int IstHEP, int IdHEP, int JmoHEP(2), int JdaHEP(2),
                     PHEP(3), ENER, MASS, VHEP(3), TIME }

     If (LkEvnt==0) Call MZBOOK(IxDIV,LKAR P2,LkEvnt, 1,'EVNT',2,2,7,2,0)

     IrbDIV=IxDIV;         LKARP2=LkEvnt
     Fill /EVNT/PASS(1)  ! Pass Record Bank
        SYS1 = 1         !  Format flag
        SYS2 = 1         !  Member system word = 100000*NG+NM NGEN 
        SYS3 = 300003    !  Modularity system word = 100000*NW+NT
        PJID = 1         !  Pass Job ID (GJID for latest PASS bank)
        GJID = 1         !  Generator Job ID.
        EVID = 1         !  ZEBRA IDN of event read in or generated
     endfill

     IrbDIV=IxDIV;         LKARP2=LkEvnt
     Fill /EVNT/GENE(1)  ! GENZ Event Bank  
       SYS1 =     1      !  Format flag = 1
       SYS2 =     0      !  Member system word = 100000*NG+NM 
       SYS3 =     0      !  Modularity system word = 100000*NW+NT
       GRUN =     0      !  Generator run number
       GEVT =     1      !  Generator event number
       GNAM = 'VENUS'    !  Generator name
       VRTX =   0.0      !  Interaction vertex position in metres
       VRTY =   0.0      !  idem
       VRTZ =   0.0      !  idem
       VRTT =   0.0      !  Interaction vertex time in seconds
       WTFL =     1      !  Interaction weight flag
       WEIG =  1.00      !  Interaction weight  
     endfill
 
     IrbDIV=IxDIV;             LKARP2=LkEvnt
     Fill /EVNT/GENE/GENT(1) ! HEPEVT parton level data 
       IstHEP  =   0     !  Status flag
       IdHEP   =   0     !  PDG particle code
       JmoHEP  = {0,0}   !  First and last mother (-ve gives a range)
       JdaHEP  = {0,0}   !  First and last daughter (<0 reference to GEANT)
       PHEP    = {0,0,0} !  particle momentum in GeV/c
       ENER    =  0.0    !  particle Total Energy 
       MASS    =  0.0    !  Mass in GeV/c. Not necessarily the on-shell m
       VHEP    = {0,0,0} !  particle origin in x (mm)
       TIME    =  0.0    !  Start time of particle (mm/c)
     endfill

 end


*************************************************************************
   module  GHeader defines GSTAR event header
   author  Pavel Nevski
   created sometime ago
*************************************************************************
+CDE,GCBANK,GCTIME,SCLINK.
*
  old STRUCTURE EGRN {               " Event generator run structure  "_
        INT   generator              " event generator identification ",
        CHAR  eg_name(8)             " event generator name           ",
        REAL  eg_versn               " version of event generator     ",
        INT   eg_run                 " generator run number           ",
        INT   eg_rndm(2)             " generator random numbers       ",
        REAL  sqrts                  " center of mass energy          ",
        INT   is_minbias             " minimum bias flag              ",
        REAL  b_min                  " minimum impact parameter       ",
        REAL  b_max                  " maximum impact parameter       ",
        INT   east_a                 " projectile 1 mass number       ",
        INT   east_z                 " projectile 1 charge            ",
        INT   west_a                 " projectile 2 mass number       ",
        INT   west_z                 " projectile 2 charge            ",
        INT   polariza(10)           " to be defined                  "_
      }
*
  Old STRUCTURE EGEV {               " Event generator event structure"_
        INT   n_event                " eg event number                ",
        REAL  b_impact               " actual impact parameter        ",
        REAL  phi_impa               " reaction plane                 ",
        INT   ev_type                " trigger, minbias bkgd, cosmic  ",
        INT   polariza(10)           " to be defined                  ",
        INT   proteast               " number of participant protons  ",
        INT   neuteast               " number of participant neutrons ",
        INT   protwest               " number of participant protons  ",
        INT   neutwest               " number of participant neutrons ",
        INT   n_track                " # tracks                       ",
        INT   n_vertex               " # vertices                     ",
        INT   fstrack                " # final state tracks           ",
        INT   nfstrack               " # non-final state tracks       ",
        INT   prvertex               " # primary vertices             ",
        INT   fsvertex               " # non-final state vertices     ",
        INT   p_prvert               " pointer to ll of primary vertices",
        INT   p_fsvert               " pointer to ll of final state vert"_
      }
*
  Old structure uevn {                      _
        char author(20), char machine(20), 
        int date, int time, version,
        int ge_run,
        int rndmrun(2),
        int ordered,
        int rndmevt(2),
        int n_event,
        int cav(12),
        int shtk_evt,
        int mx_shtk,
        int nw_shtk,
        int firstvrt,
        int firsttrk,  
        int equals  _
    }
*
   integer iprin,Iadr
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*

   fill /EVNT/UEVN(1) ! user event buffer
      author     = { 'mea','mea' }  ! author
      machine    = { 'ibm','bla' }  ! machine
      date       = igdate           ! date
      time       = igtime           ! time
      version    = gversn           ! geant version
      equals     = 99999            ! end label
   endfill
*
   IF (LKRUNT>0) then
    FILL /EVNT/UEVN/EGRN(1)          !  Event generator run structure 
      generator  = 0                ! event generator identification
      eg_name    = { 'a','b','c' }  ! event generator name
      eg_versn   = Q(LKRUNT+3)      ! version of event generator
      eg_run     = 0                ! generator run number
      eg_rndm    = { 0,0 }          ! generator random numbers
      sqrts      = Q(LKRUNT+8)      ! center of mass energy
      is_minbias = 0                ! minimum bias flag
      b_min      = Q(LKRUNT+9)      ! minimum impact parameter
      b_max      = Q(LKRUNT+10)     ! maximum impact parameter
      east_a     = Q(LKRUNT+5)      ! projectile 1 mass number
      east_z     = Q(LKRUNT+4)      ! projectile 1 charge
      west_a     = Q(LKRUNT+7)      ! projectile 2 mass number
      west_z     = Q(LKRUNT+6)      ! projectile 2 charge
      polariza   = { 0,0,0,0,0 }    ! to be defined
    endfill
   endif
*
   If (LKEVNT>0) then
    FILL /EVNT/UEVN/EGEV(1)     !  Event generator event structure 
      n_event    = 0                ! eg event number
      b_impact   = Q (LKEVNT+7)     ! actual impact parameter
      phi_impa   = 0                ! reaction plane
      ev_type    = 0                ! trigger, minbias bkgd, cosmic, etc.
      polariza   = { 0,0,0,0,0 }    ! to be defined
      proteast   = IQ(LKEVNT+4)     ! number of participant protons
      neuteast   = IQ(LKEVNT+5)     ! number of participant neutrons
      protwest   = IQ(LKEVNT+2)     ! number of participant protons
      neutwest   = IQ(LKEVNT+3)     ! number of participant neutrons
      n_track    = IQ(LKEVNT+1)     ! number of tracks
      n_vertex   = 0                ! number of vertices
      fstrack    = 0                ! number of final state tracks
      nfstrack   = 0                ! number of non-final state tracks
      prvertex   = 0                ! number of primary vertices
      fsvertex   = 0                ! number of non-final state vertices
      p_prvert   = 0                ! pointer to ll of primary vertices
      p_fsvert   = 0                ! pointer to ll of final state vert.
    endfill
   endif
*
   Iadr = 0
   call GsHEAD (len_egrn-2, Bank_egrn(3), iadr)
   call GsHEAD (len_egev-2, Bank_egev(3), iadr)
   call GsHEAD (len_uevn-2, Bank_uevn(3), iadr)
   print *,' Header: geant header set with length =',iadr
*
   end



