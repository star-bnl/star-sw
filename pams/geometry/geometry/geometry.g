***************************************************************************
   subroutine geometry
*                                                                         *
*  author  Pavel Nevski                                                   *
*  Created August 1998                                                    *
*                                                                         *
*  Update history:                                                        *
*  08/19/98, PN: tof is not part of year_2a                               *
*  08/20/98, PJ:                                                          *
*                                                                         *
***************************************************************************
   Implicit   none
   Logical    cave,pipe,svtt,tpce,ftpc,btof,vpdd,magp,calb,ecal,
              mfld,mwc,pse,tof,four,ems,on/.true./,off/.false./
   real       Par(1000),field,dcay(5),shift
   Integer    LENOCC,LL,IDEB,Nsi,i,j,l,nmod(2)
   character  Commands*4000
* - - - - - - - - - - - - - - - - -
+CDE,GCBANK,GCUNIT,GCPHYS,GCCUTS,GCFLAG,AGCKINE.
* - - - - - - - - - - - - - - - - -
replace[;ON#{#;] with [
  IF Index(Commands,'#1')>0 
  { j=Index(Commands,'#1');  l=j+Lenocc('#1')-1;  Commands(j:l)=' ';
    <W>; (' #1: #2');
]
*
   call ASLGETBA ('GEOM','DETP',1000,LL,Par)
   If (JVOLUM>0) call AGDROP ('*')
   IDEB = IDEBUG; J=1
*
* -------------------- set GSTAR absolute default ------------------------
* Set only flags for the main configuration (everthing on, except for tof),
* but no actual parameters (CUTS,Processes,MODES) are set or modified here. 
* If an empty or no DETP GEOM was issued, geometry is defined externally.
*
   {cave,pipe,svtt,tpce,ftpc,btof,vpdd,calb,ecal,magp,mfld} = on;
   {mwc,four,pse}=on      "MultiWire Chambers, 4th Si layer, pseudopadrows"   
   {tof,ems}=off                      "TimeOfFlight, EM calorimeter Sector"
   field=5;  Nsi=7;                                    "defaults constants"
   Commands=' '
*
* -------------------- select USERS configuration ------------------------
* On a non-empty DETP GEOM every keyword makes an action and is erased.
* Actions consist here of selecting the appropriate parameteres and flags.
* This flags are used in the next section to create subsystems and 
* to communicate DETP commands with parameters to them.
* 
If LL>1   
{ Call AGSFLAG  ('GEOM',1)
  CALL UHTOC(PAR(2),4,Commands,LL*4-4);  Call CLTOU(Commands);

  * set geant processes and cuts only if any detp geometry was issued:
   
  {CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM,DCUTE,DCUTM,PPCUTM} =.001;
  {IDCAY,IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,ILOSS,IDRAY,IMULS} = 1;
  {IRAYL,ISTRA} = 0;
  TOFMAX        = 1.e-4 
  NtrSubEv      = 1000
*
  do while (j>0)
  j=0
  on HELP       { you may select the following keywords: ;
                  <W>;('---------------:----------------------------- ');
                  <W>;('Configurations : complete,tpc_only,field_only ');
                  <W>;('               : year_1a,b,c;  year_2a        ');
                  <W>;('Geant Physics  : Hadr_on, Hadr_off, Decay_Only');
                  <W>;('Geometry Detail: mwc_off, pse_off, 4th_off    ');
                  <W>;('Magnetic Field : Field_on/off, field=value    ');
                  <W>;('Auxillary keys : Debug_on/off, Split_on/off   ');
                  <W>;('--------------------------------------------- ');
                  <W>;('Default: complete STAR with hadr_on  ');
                  <W>;('--------------------------------------------- ');
                }  
  on COMPLETE   { Complete STAR geometry;      tof=on;                        }
  on YEAR_1A    { poor approximation to year1: TPC+CTB+FTPC;      
                                              {vpdd,calb,ecal}=off;    Nsi=0; }
  on YEAR_1B    { better year1: TPC+CTB+FTPC+calo patch, no svt; 
                                              {vpdd,ecal}=off; ems=on; Nsi=0; }
  on YEAR_1C    { not a year1:  TPC+CTB+FTPC+calo;  {vpdd,ecal}=off;   Nsi=0; }
  on YEAR_2A    { asymptotic STAR;                                   tof=off; }
  on HADR_ON    { all Geant Physics On;                                       }
  on HADR_OFF   { all Geant Physics on, except for hadronic interactions; 
                                                                       IHADR=0}
  on DECAY_ONLY { Some Physics: decays, mult.scat and energy loss;
                  {IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,IDRAY}=0; Iloss=2}
  on TPC_ONLY   { Minimal geometry - only TPC;
                               {pipe,svtt,ftpc,btof,vpdd,magp,calb,ecal}=off; }
  on FIELD_ONLY { No geometry - only magnetic field;           NtrSubEv=0;
                  {cave,pipe,svtt,tpce,ftpc,btof,vpdd,magp,calb,ecal}=off;    }
  on FIELD_OFF  { no magnetic field;                field=0;                  }
  on FIELD_ON   { Standard (5 KGs) field on;        field=5;                  }
  i=Index(Commands,'FIELD=')
  if i>0        { j=i/4+3; field=Par(1+j);  Commands(i:j*4)=' ';
                  <W> field; (' Modified field value =',F6.2,' KGS');         }
  on MWC_OFF    { Trigger Multy-wire readout off;   mwc=off;                  }
  on PSE_OFF    { No TPC pseudo-padrow generated;   pse=off;                  }
  on 4TH_OFF    { SVT fourth layer off;             Nsi=min(Nsi,6);           }
  on SPLIT_OFF  { events will not be split into subevents;     NtrSubEv=0;    }
  on SPLIT_ON   { events will be split into subevents;         NtrSubEv=1000; }
  on DEBUG_ON   { verbose mode, some graphics; Idebug=max(Idebug,1); Itest=1; }
  on DEBUG_OFF  { standard debug mode;         {Idebug,Itest}=0;              }
  enddo
 }

* sanity check - if something left in commands (unknown keyword), we stop!
  l=LENOCC(commands); if l>0
  {  print *,' Unknown command left => ', commands(1:l), ' <= ',l
     if (Ideb<3) stop 'You better stop here to avoid problems'     
  }
*
* -------------------- setup selected configuration ------------------------
* Now when all parameters and flags are ready, make gstar work as usually
* ie put a MODE or/and DETP command and executing them for selected systems.
*

* - to save secondaries AFTER all decays:      DETP TRAC DCAY 210 210 0.1 0.01
   dcay={210,210,0.1,0.01}
   call AgDETP new ('Trac')
   call AgDETP add ('TracDCAY',dcay,4)
*
   if (cave) Call cavegeo
   if (pipe) Call pipegeo

   Call AGSFLAG('SIMU',2)
* - to switch off the fourth svt layer:        DETP SVTT SVTG.nlayer=6 
   call AgDETP new ('svtt')
   if (svtt & Nsi < 7) call AgDETP add ('svtg.nlayer=',Nsi,1)
   if (svtt) Call svttgeo
 
* - MWC or pseudo padrows needed ? DETP TPCE TPCG(1).MWCread=0 TPRS(1).super=1
*  CRAY does not accept construction: IF (mwc==off) ... I do it differntly:

   call AgDETP new ('tpce')
   If (tpce &.not.mwc) call AgDETP add ('tpcg(1).MWCread=',0,1)
   If (tpce &.not.pse) call AgDETP add ('tprs(1).super='  ,1,1) 
   if (tpce) Call tpcegeo
   if (ftpc) Call ftpcgeo

* - tof system should be on (for year 2):      DETP BTOF BTOG.choice=2
   call AgDETP new ('btof')
   if (tof)  call AgDETP add ('btog.choice=',2,1)
   if (btof) Call btofgeo
     
   Call AGSFLAG('SIMU',1)
   if (vpdd) Call vpddgeo

*  - barrel calorimeter may be a patch of 12 modules at the left side
   Call AgDETP new ('calb')
   nmod={0,12};  shift=51
   if (ems)  call AgDETP add ('calg.nmodule=',Nmod,2)
   if (ems)  call AgDETP add ('calg.shift=',shift,1)
   if (calb) Call calbgeo

   if (ecal) Call ecalgeo
   if (magp) Call magpgeo
*
* - reset magnetic field value (default is 5): DETP MFLD MFLG.Bfield=5
   call AgDETP new ('MFLD')
   if (mfld & field!=5) call AgDETP add ('MFLG(1).Bfield=',field,1)
   if (mfld) Call mfldgeo
*
   if JVOLUM>0 
   { Call ggclos
     If IDEBUG>0 { CALL ICLRWK(0,1); Call GDRAWC('CAVE',1,.2,10.,10.,.03,.03)}
   }
   IDEBUG = IDEB
   ITEST  = min(IDEB,1)
   call agphysi
*
   end
*
******************************temporary here***********************************
*
   subroutine  AgDETP add (Cpar,p,N)
+CDE,Typing,GCBANK,SCLINK.
   Character   Cpar*(*),EQ*1/'='/,Cd*4/'none'/
   Integer     LENOCC,Par(1000),p(N),N,L,I,J,LL,Id/0/,Ld
   Real        R
   Equivalence (R,I)
*
    call ASLGETBA (Cd,'DETP',1000,LL,Par)
    L=Lenocc(Cpar)
    Call UCTOH (Cpar,Par(LL+1),4,L);  LL+=(L+3)/4
    do j=1,N  { I=p(j); if (abs(I)<10000) R=p(j);  LL+=1; Par(LL)=I; }   
    call ASLSETBA (Cd,'DETP',LL,Par)
    return
*
    entry AgDETP new (Cpar)
    Cd=Cpar;  Call CLTOU(cd);  call asbdete (Cd,id)
    call ASLDETBA (Cd,'DETP',1,Ld);  If (Ld>0) call MZDROP (IxCons,Ld,' ')
*
   end



