******************************************************a*********************
   subroutine geometry
*  Prototype of Alice Calorimeter
*  author  Aleksei Pavlinov
*  Created 12-October-2001 from geometry.g for STAR
***************************************************************************
   Implicit   none
   Logical    on/.true./,off/.false./ 
   Logical    cave, cala, k25

   real       Par(1000),field,dcay(5),shift(2),wdm
   Integer    LENOCC,LL,IPRIN,Nsi,i,j,l,nmod(2),nonf(3),
              Nleft,Mleft,Rv,Rp,Wfr,Itof,mwx,mf
   character  Commands*4000,Geom*8
   integer    layer
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
*
   field=5                                             "defaults constants"
   {cave, cala} = on;
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
                  <W>;('Configurations : layer21, layer25');
                  <W>;('--------------------------------------------- ');
                  <W>;('Default: layer21 ALICE with hadr_on,auto-split');
                  <W>;('--------------------------------------------- ');
                }  
  on layer21    {Temporary Alice geometry with 21 layers of Sc and 20 of PB;}
  on layer25    {Temporary Alice geometry with 25 layers of Sc and 24 of PB;
                 {cave, cala, k25} = on; layer=25;}

  on HADR_ON    { all Geant Physics On;                                       }
  on HADR_OFF   { all Geant Physics on, except for hadronic interactions; 
                                                                       IHADR=0}
  on PHYS_OFF   { No Physics: only energy loss;
      {IDCAY,IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,IDRAY,IMULS}=0; Iloss=2}
  on DECAY_ONLY { Some Physics: decays, mult.scat and energy loss;
                  {IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,IDRAY}=0; Iloss=2}
  on FIELD_OFF  { no magnetic field;                field=0;                  }
  on FIELD_ON   { Standard (5 KGs) field on;        field=5;                  }

  i=Index(Commands,'FIELD=')
  if i>0        { j=i/4+3; field=Par(1+j);  Commands(i:j*4)=' ';
                  <W> field; (' Modified field value =',F6.2,' KGS');         }
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
   Call AGSFLAG('SIMU',2)

   if (cave) Call cavegeo

   if(k25) {
      print *,' Define 25 layers '
      call AgDETP new ('CALA')
      call AgDETP add ('calg.nlayer=', 25,1)
   }
   if (cala) Call calageo
*

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

