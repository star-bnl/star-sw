* $Id: geometry.g,v 1.47 2001/05/22 17:40:47 nevski Exp $
* $Log: geometry.g,v $
* Revision 1.47  2001/05/22 17:40:47  nevski
* field find tuning
*
* Revision 1.46  2001/05/21 21:07:05  nevski
* Steves field map added
*
* Revision 1.45  2001/04/09 15:31:35  nevski
* second version of cerenkov light properties introduced
*
* Revision 1.44  2001/03/16 22:09:13  nevski
* some clean-up
*
* Revision 1.43  2001/03/16 00:32:06  nevski
* switch on/off cooling water
*
* Revision 1.42  2001/03/15 01:24:47  nevski
* default btof forced to no TOF tray
*
* Revision 1.41  2001/03/15 01:14:20  nevski
* first approach to forward pion detector
*
* Revision 1.40  2001/03/13 20:56:31  nevski
* variable rich position taken from DB
*
* Revision 1.39  2001/03/12 01:01:30  nevski
* mwc x-hits corrected
*
* Revision 1.38  2001/02/13 02:28:52  nevski
* Y2B: extend CALB patch, add VPD
*
* Revision 1.37  2001/02/07 02:09:09  nevski
* 6 silicon layers in y_2b geometry
*
* Revision 1.36  2000/11/22 17:51:41  nevski
* tof geometry versions 1/2 preserved in btofgeo1, version 3 goes in btofgeo2
*
***************************************************************************
   subroutine geometry
*                                                                         *
*  author  Pavel Nevski                                                   *
*  Created August 1998                                                    *
*                                                                         *
*  Update history:                                                        *
*  08/19/98, PN: tof is not part of year_2a                               *
*  12/04/98, PN: rich  + upstream part + zero degree calo                 *
*  09/26/99, E.Cains: 1H geometry added - one svt ladder at layer 3       *
*  01/27/99, PN: rich in 1H geometry is simulated with hits is quartz & fr*
*  22.05.01, PN: starting with tag y2000 field is version 3 (direct map)  *
***************************************************************************
   Implicit   none
   Logical    cave,pipe,svtt,tpce,ftpc,btof,vpdd,magp,calb,ecal,upst,rich,
              zcal,mfld,mwc,pse,tof,t25,t1,four,ems,alpipe,svtw,
              on/.true./,off/.false./
   real       Par(1000),field,dcay(5),shift(2),wdm
   Integer    LENOCC,LL,IPRIN,Nsi,i,j,l,nmod(2),nonf(3),
              Nleft,Mleft,Rv,Rp,Wfr,Itof,mwx,mf
   character  Commands*4000,Geom*8
* - - - - - - - - - - - - - - - - -
+CDE,GCBANK,GCUNIT,GCPHYS,GCCUTS,GCFLAG,AGCKINE,QUEST.
*  temporarely until GCTLIT is not part of GCTMED:
   Integer        Thrind   ,Jmin,ItCkov,ImCkov,NpCkov
   common/GCTLIT/ Thrind(4),Jmin,ItCkov,ImCkov,NpCkov
* - - - - - - - - - - - - - - - - -
replace[;ON#{#;] with [
  IF Index(Commands,'#1')>0 
  { j=Index(Commands,'#1');  l=j+Lenocc('#1')-1; 
    if (Commands(j:j+3)=='YEAR') Geom=Commands(j+4:l); 
    Commands(j:l)=' ';  <W>; (' #1: #2');
]
*
   call ASLGETBA ('GEOM','DETP',1000,LL,Par)
   If (JVOLUM>0) call AGDROP ('*')
   IPRIN    = IDEBUG
   NtrSubEv = 1000     " automatic !"
*
* -------------------- set GSTAR absolute default ------------------------
* Set only flags for the main configuration (everthing on, except for tof),
* but no actual parameters (CUTS,Processes,MODES) are set or modified here. 
* If an empty or no DETP GEOM was issued, geometry is defined externally.
*
   field=5                                             "defaults constants"
   {cave,pipe,svtt,tpce,ftpc,btof,vpdd,calb,ecal,magp,mfld,upst,zcal} = on;
   {mwc,four,pse}=on      "MultiWire Chambers, 4th Si layer, pseudopadrows"   
   {tof,t25,t1,ems,rich,alpipe}=off   "TimeOfFlight, EM calorimeter Sector"
   Nsi=7; Wfr=0;  Wdm=0; " SVT+SSD, wafer number and width as in code     "
   svtw=on               " water+water manifold in svt, off for Y2000 only"
   mwx=2                 " for Geom=_1 mwx=1 limites x in mwc hits (<Y2K) "
   Itof=2                " use btofgeo2 - default starting from Y2000     "
   Rv=2                  " add non-sensetive hits to RICH system          "
   Rp=2                  " real RICH position and spectra inst.of nominal "
   nonf={1,2,2}          " ecal on right side, FPD parts on left side     "
   mf=2                  " default field - simetrical, as fitted by Bill  "
   Commands=' '; Geom=' '
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
*
  for(j=1;j>0;) { j=0;
  on HELP       { you may select the following keywords: ;
                  <W>;('---------------:----------------------------- ');
                  <W>;('Configurations : complete,tpc_only,field_only ');
                  <W>;('               : year_1a,s,b,h,c;  year_2a    ');
                  <W>;('Geant Physics  : Hadr_on, Hadr_off            ');
                  <W>;('Geant Physics  : Phys_off, Decay_Only         ');
                  <W>;('Geometry Detail: mwc_off, pse_off, 4th_off    ');
                  <W>;('Magnetic Field : Field_on/off, field=value    ');
                  <W>;('Auxillary keys : Debug_on/off, Split_on/off   ');
                  <W>;('--------------------------------------------- ');
                  <W>;('Default: complete STAR with hadr_on,auto-split');
                  <W>;('--------------------------------------------- ');
                }  
  on YEAR_1S    { starting in summer: TPC, CONE, AL pipe;          alpipe=on;
                  {ftpc,vpdd,calb,ecal}=off;                           Nsi=0; }
  on YEAR_1A    { poor approximation to year1: TPC+CTB+FTPC;      
                  {vpdd,calb,ecal}=off;      Itof=1;                   Nsi=0; }
  on YEAR_1B    { better year1: TPC+CTB+FTPC+calo patch+RICH, no svt; 
                  {vpdd,ecal}=off;  {rich,ems,t1}=on; 
                  nmod={12,0}; shift={87,0}; Itof=1; {Rv,Rp}=1;        Nsi=0; }
  on YEAR_1C    { not a year1:  TPC+CTB+FTPC+calo;  
                  {vpdd,ecal}=off;           Itof=1;                   Nsi=0; }

  on YEAR_1H    { even better y1:  TPC+CTB+FTPC+RICH+caloPatch+svtLadder;  
                  {vpdd,ecal}=off;  {rich,ems,t1}=on;  Itof=1; 
                  nmod={12,0}; shift={87,0}; Rp=1; Rv=2; Wdm=6;        Nsi=-3;}

  on YEAR_1E    { even better y1:  TPC+CTB+RICH+caloPatch+svtLadder;  
*    HELEN:       one ladder at R=10.16cm with 7 wafers at the 12 O'Clock...
                  {vpdd,ecal,ftpc}=off;  {rich,ems,t1}=on;  Itof=1;
                  nmod={12,0}; shift={87,0}; Rp=1; Rv=2; Wfr=7; Wdm=6; Nsi=-3;}

  on YEAR_2B    { 2001 geometry first guess - TPC+CTB+FTPC+RICH+CaloPatch+SVT;
                  {rich,ems,t1}=on;  nmod={24,0}; shift={21,0};  
                  nonf={0,2,2};  Itof=2;  Rv=2;                        Nsi=6; }

  on YEAR_2A    { old asymptotic STAR;    Itof=1; mwx=1;  nonf={3,0,0}        }
  on COMPLETE   { Complete STAR geometry; Itof=2; tof=on; nonf={1,2,2}        }

  on YEAR2000   { actual 2000:  TPC+CTB+RICH+caloPatch+svtLadder; 
*                 corrected: MWC readout, RICH reconstructed position, no TOF 
                  {vpdd,ecal,ftpc,svtw}=off; {rich,ems}=on; Field=2.5; 
                  nmod={12,0}; shift={87,0}; Rp=2; Rv=2; Wfr=7; Mf=3;  Nsi=-3;}

  on YEAR2001   { 2001 geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD;
                  {rich,ems,t1}=on;  nmod={24,0}; shift={21,0};  
                  nonf={0,2,2};  Itof=2;  Rv=2;  Mf=3;                 Nsi=6; }

  on HADR_ON    { all Geant Physics On;                                       }
  on HADR_OFF   { all Geant Physics on, except for hadronic interactions; 
                                                                       IHADR=0}
  on PHYS_OFF   { No Physics: only energy loss;
      {IDCAY,IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,IDRAY,IMULS}=0; Iloss=2}
  on DECAY_ONLY { Some Physics: decays, mult.scat and energy loss;
                  {IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,IDRAY}=0; Iloss=2}
  on TPC_ONLY   { Minimal geometry - only TPC;
                     {pipe,svtt,ftpc,btof,vpdd,calb,ecal,magp,upst,zcal}=off; }
  on FIELD_ONLY { No geometry - only magnetic field;              NtrSubEv=0;
      {cave,pipe,svtt,tpce,ftpc,btof,vpdd,magp,calb,ecal,rich,upst,zcal}=off; }
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
  }
* sanity check - if something left in commands (unknown keyword), we stop!
  l=LENOCC(commands); if l>0
  {  print *,' Unknown command left => ', commands(1:l), ' <= ',l
     if (IPRIN==0) stop 'You better stop here to avoid problems'     
  }
}

* -------------------- setup selected configuration ------------------------
* Now when all parameters and flags are ready, make gstar work as usually
* ie put a MODE or/and DETP command and executing them for selected systems.
*
* - to save secondaries AFTER all decays:      DETP TRAC DCAY 210 210 0.1 0.01
   dcay={210,210,0.1,0.01}
   If LL>1 { call AgDETP new ('Trac'); call AgDETP add ('TracDCAY',dcay,4) }
*
   if (rich) ItCKOV = 1
   if (cave)        Call cavegeo
   If (LL>1)        call AgDETP new ('PIPE')
   if (alpipe)      call AgDETP add ('pipg.BeLeng=', 0, 1)
   if (alpipe)      call AgDETP add ('pipg.S1Leng=',230,1)
   if (pipe)        Call pipegeo
   if (upst)        Call upstgeo

   Call AGSFLAG('SIMU',2)
* - to switch off the fourth svt layer:        DETP SVTT SVTG.nlayer=6 
   If (LL>1 & svtt) then
     call AgDETP new ('SVTT')
     if (Nsi < 7)   call AgDETP add ('svtg.nlayer=',   Nsi,1)
     if (Wfr > 0)   call AgDETP add ('svtl(3).nwafer=',wfr,1)
     if (wdm > 0)   call AgDETP add ('swca.WaferWid=', wdm,1)
     if (wdm > 0)   call AgDETP add ('swca.WaferLen=', wdm,1)
     if (.not.svtw) call AgDETP add ('swam.Len=',       0, 1)
   endif
   if (svtt) Call svttgeo
 
* - MWC or pseudo padrows needed ? DETP TPCE TPCG(1).MWCread=0 TPRS(1).super=1
*   CRAY does not accept construction: IF (mwc==off) ... I do it differntly:
* - for year_1 X in mwc hits was limited, keep this (mwx=1)
   If (LL>1 & tpce) then
     call AgDETP new ('TPCE')
     If (Geom(1:2)='_1') mwx=1
     If (  .not. mwc   ) mwx=0
     If ( mwx <2 )  call AgDETP add ('tpcg(1).MWCread=',mwx,1)
     If (.not.pse)  call AgDETP add ('tprs(1).super='  , 1, 1) 
   endif 
   if (tpce) Call tpcegeo
   if (ftpc) Call ftpcgeo
   if (ftpc) Call supogeo

* - tof system should be on (for year 2):      DETP BTOF BTOG.choice=2
   If (LL>1 & btof) then
     call AgDETP new ('BTOF')
     if     (tof) { call AgDETP add ('btog.choice=',2,1) }
     elseif (t25) { call AgDETP add ('btog.choice=',3,1) }
     elseif (t1)  { call AgDETP add ('btog.choice=',4,1) }
     else         { call AgDETP add ('btog.choice=',1,1) }
   endif
   if btof { If Itof==1 { call btofgeo1 } else { call btofgeo2 }}
     
   Call AGSFLAG('SIMU',1)
   if (vpdd) Call vpddgeo

*  - barrel calorimeter may be a patch of 12 modules at the left side
   If (LL>1 & calb) then
     call AgDETP new ('CALB')
     if (ems)  call AgDETP add ('calg.nmodule=',Nmod, 2)
     if (ems)  call AgDETP add ('calg.shift=',  shift,2)
   endif
   if (calb) Call calbgeo
*
   if (LL>1 & rich) then
      call AgDETP new ('Rich')
      if (Rv>0) call AgDETP add ('Rich.Version=', Rv,1) 
      if (Rp>0) call AgDETP add ('Rich.Position=',Rp,1)
      if (Rp>0) call AgDETP add ('Rich.Cversion=',Rp,1)
   endif
   if (rich) Call richgeo

*  - endcap calorimeter may be controled here
   If (LL>1 & ecal) then
      call AgDETP new ('ECAL')
      call AgDETP add ('emcg.OnOff=',Nonf,3)
   endif
   if (ecal) Call ecalgeo
   if (zcal) Call zcalgeo
   if (magp) Call magpgeo
*
* - reset magnetic field value (default is 5): DETP MFLD MFLG.Bfield=5
   If (LL>1) call AgDETP new ('MFLD')
   if (mfld & field!=5) call AgDETP add ('MFLG(1).Bfield=',field,1)
   if (mfld & mf!=0)    call AgDETP add ('MFLG(1).version=',mf,1)
   if (mfld) Call mfldgeo
*
   if JVOLUM>0 
   { Call ggclos
     If IDEBUG>0 { CALL ICLRWK(0,1); Call GDRAWC('CAVE',1,.2,10.,10.,.03,.03)}
   }
   IDEBUG = IPRIN
   ITEST  = min(IPRIN,1)
   Call agphysi
*                      automatic subevent size selection
   If NtrSubev > 0
   { Call MZNEED(IXDIV,1000,'G')
     NLEFT    = max(10,IQUEST(11)/1200)
     MLEFT    = 10**Int(Alog10(Float(Nleft))-1)
     NtrSubEv = MLEFT*(NLEFT/MLEFT)
     Prin1 NtrSubEv; (' Ntrack per subevent = ',i6)
   } 
*
   end

