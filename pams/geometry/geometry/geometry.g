* $Id: geometry.g,v 1.80 2004/01/29 20:46:45 potekhin Exp $
* $Log: geometry.g,v $
* Revision 1.80  2004/01/29 20:46:45  potekhin
* Disabled the long since obsoleted version of TOF,
* because this piece of code would need to be rewritten
* to be compiled with a variety of compiler options.
*
* Revision 1.79  2004/01/22 00:21:32  potekhin
* Provide a facility to position the SVT with the MANY option,
* which we'll likely need due to overlap of the PIXL (embedded
* in SVT) and the mother volume of the beampipe
*
* Revision 1.78  2004/01/19 22:53:27  potekhin
* A small additional piece of logic to steer the
* construction of the barrel calorimeter, better
* code layout and comments
*
* Revision 1.77  2003/12/17 22:15:18  potekhin
* a) In accordance with recent modifications, we also
* introduce the configuration variable for the beam
* pipe, instead of keeping its actual parameters here
* b) corrected the COMPLETE geometry with the newest
* version of the svtt code (with ssd separated out)
* c) removed obsolete variables and logic, streamlined
* the code
* d) better comments and formatting in a few places
*
* Revision 1.76  2003/12/03 19:53:04  potekhin
* a) Corrected a small but annoying bug in
* propagating the geo tag to reco:
* one of the characters was copied twice
* because of the indexing error
*
* b) Added the "shift" angle for the second half
* barrel in Y2004
*
* Revision 1.75  2003/11/20 02:58:10  potekhin
* Changed the correction number scheme, such that it
* allows for a new layout of the SVT to be implemented --
* the one without the nested SSD. This is achieved by
* calling the new code, svttgeo3.
*
* Changed the SSD config number in y2004 to "2": 10 ladders,
* the "1" being one ladder installed previosuly and "3"
* being the complete 20 ladder config.
*
* Made smallchanges in the barrel config flag for y2004,
* improved comments and cleaned out unused variables.
*
* Revision 1.74  2003/11/14 22:56:19  potekhin
* We are about to redo a sim run with y2003x,
* and it seems that me might put in some of the
* prior corrections as well. Therefore, I'm changing
* the correctin level to 2.
*
* Revision 1.73  2003/11/13 00:54:50  potekhin
* Create a facility to modify the TPC
* gas density programmatically
*
* Revision 1.72  2003/11/13 00:21:42  potekhin
* The modification flag we introduced earlier
* to reflect variations in the dimensions of
* certain support structured of SVT takes a borader
* meaning than just the shield, hence we rename
* the variable to SupportVer
*
* Revision 1.71  2003/11/12 18:45:09  potekhin
* As per Fabrice, change the number of layer in the
* SVT to 7 to ensure that the ssd in its current
* version is also included
*
* Revision 1.70  2003/10/30 00:15:42  potekhin
* To perfect our already sophisticated taxonomy of
* the geometry tags, we rename Y2003C into Y2004,
* because the run for which it is meant will start
* in early 2004 anyway. Anyone to confuse 2003
* and 2004, from now on, will be jailed and
* possibly deported.
*
* Revision 1.69  2003/10/29 22:07:30  potekhin
* Two changes:
* 1) As agreed, I swap the tags y2003(b,c) to arrange
* them chronologically for better mneumonics
* 2) Introduced variable for steering of the Silicon Strip
* Detector code (which needs to be written)
*
* Revision 1.68  2003/10/28 00:01:59  potekhin
* As agreed with Jerome, we shall prevent
* proliferation of custom geometries to reduce
* the various dependencies between simu and reco
* and databases. Therefore, the experimental
* geometry "ASYMPT1" has been removed and has
* taken place of the "COMPLETE", which is our
* official sandbox. The Pixel Detector is
* defined in it right now.
*
* Revision 1.67  2003/10/15 23:19:35  potekhin
* Due to an apparent need to have the "most precise"
* geometry describing the STAR configuration in the
* spring'03 run, we introduce the tag Y2003C. It
* includes all the corrections first introduced in
* Y2003A, but also has the extra material in the
* SVT that we recently added in the GEANT model
* and which had been missing before.
*
* Revision 1.66  2003/10/10 23:59:18  potekhin
* Per Jerome's suggestion, which I agree with, we should
* semantically separate our current geometry tags from the
* future development ones. Also, it makes sense to have a more
* complete (as compared to 2003) geometry, for the pixel
* studies. Therefore, the pixel testbed tag has been
* renamed and the STAR detector more fully populated.
* It's now ASYMPT1, and the indexing may continue.
*
* Revision 1.65  2003/10/10 23:15:35  potekhin
* In previous check-n: forgot to mention the new CorrNum=3
* which actually programmatically modifies the inner radius of
* the shield in the SVT -- otherwise the pixel detector won't
* fit. Plus, put in extra flags PIXL_ON and PIPE_OFF to facilitate
* experimentation.
*
* Revision 1.64  2003/10/10 23:12:56  potekhin
* The fact is, we will need a suitable specialized geometry
* for the pixel detector development, as it will require
* a different beampipe and other modifications. I hereby
* create the tag y2003c which will serve this purpose.
* Right now it disables the old beampipe w/o offering anything
* in its place -- this is subject to change as we assimilate
* the new pipe design.
*
* Improved comments and structure in a few places
*
* Revision 1.63  2003/10/01 23:44:17  potekhin
* Code modifications related to persisting the vital
* geometry/version data, for now the magnetic field
* scale and the geometry tag"
*
* 1) Change the semantics of the variable Geom, which was hacky
* anyway, and put the mwx=1 in the individual year_1 branches
* themselves (a lot cleaner than a prior obscure "if")
*
* 2) Store the complete geometry tag in the variable Geom,
* which is 8 characters long
*
* 3) Change the subroutine "geometry" into a "module",
* which process instruments it (via Mortran) to access
* and manipulate ZEBRA banks as structures
*
* 4) Introduce the bank GDAT, as a sub-bank of GEOM,
* which for now contains the field scale and the tag.
*
* Revision 1.62  2003/09/29 19:48:41  potekhin
* 1) Fixed typos in comments
*
* 2) Created a few options that allow the user to selectively include
* certain detectors such as svtt, ecal, calb into the minimal geometry,
* thus facilitating the creation of custom geometries on the fly --
* useful for debugging and detector exploration
*
* 3) Improved the phmd logic
*
* 4) last but not least -- the shift variable for CALB was changed from
* {75,75} (incorrect) to {75,105} in geometry y2003x (full).
*
* Revision 1.61  2003/09/18 22:09:34  potekhin
* Corrected a small comment typo and added the full
* endcap wheel to the  new flagship geometry, y2003b.
* This is done with fill mode 3.
*
* Revision 1.60  2003/09/17 23:10:42  potekhin
* Small improvements to the Correction Level
* logic.
*
* Revision 1.59  2003/08/21 20:29:27  potekhin
* As per discussion with Jerome, I'm introducing
* a cleaner versioning of the 2003 geometries:
*
* y2003a is now really the corrected year2003,
* without any additions -- the SVT layer positions
* have been updated and the old supogeo bug fixed.
*
* y2003b has same corrections as y2003a but will also
* include extra material in the SVT, new ECAL configuration
* as well as a new FPD, plus the Photon Multiplicity Detector.
* Other changes will be done as needed. So for practical
* purposes, this will be the actual Fall'03 geometry.
*
* Revision 1.58  2003/08/05 23:37:09  potekhin
* Continued to use the CorrNum "correction level" to
* correct older geometry bugs in a controlled and versioned
* manner. Keep the newer version of the FTPC support pieces,
* and add a call the new SVTT module, which will
* include a number of corrections.
*
* Revision 1.57  2003/07/03 04:45:20  potekhin
* Added the "special" variation of the year 2003 geometry, which has
* a complete set of the barrel and endcap calorimeter elements. This
* is needed primarily for heavy flavor studies and other rare signals.
* The tag is y2003x.
*
* Also, fixed a subtle bug in the configuration of the endcap calorimeter.
* It did not affect previous simulations, but would manifest itself
* in a complete configurations such as this one.
*
* Revision 1.56  2003/05/01 23:00:16  potekhin
* Photon Multiplicity Detector is now part of the
* GEANT geometry version "y2003a", with proper
* versioning of its position
*
* Revision 1.55  2003/04/29 21:04:55  potekhin
* To keep the consistency of current simulation runs,
* the geometry "year2003" is frozen. All the corrections
* will go into "y2003a", and there will be a number of those.
* Such geometry has been added to this present source file.
* In the current cut of y2003a, to be tested, we corrected
* the supogeo and the offset of the ECAL phi position.
* We are also awaiting further corrections from the SVT group.
*
* Revision 1.55  2003/04/29 16:57:00  potekhin
* New geometry y2003a -- corrected
*
* Revision 1.54  2002/12/10 01:48:25  potekhin
* Important: the hadronic interactions are now indeed actuated in GCALOR
*
* Revision 1.53  2002/12/05 23:28:41  potekhin
* Streamlined the btof config logic
*
* Revision 1.52  2002/11/27 21:53:14  potekhin
* code improvement for readability etc -- moved bbcmgeo call
*
* Revision 1.51  2002/11/03 02:16:10  nevski
* geometry up to 2003 introduced
*
* Revision 1.50  2002/10/28 15:49:35  nevski
* fpd as a separate detector added
*
* Revision 1.49  2002/10/28 15:42:29  nevski
* introducing 2002 version
*
* Revision 1.48  2001/09/10 17:39:34  nevski
* do not set MFLD datacards without a DETP GEOM
*
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
   module geometry is the main steering module for the STAR geometry
   author  Pavel Nevski
   Created August 1998
*                                                                         *
*  Update history:                                                        *
*  08/19/98, PN: tof is not part of year_2a                               *
*  12/04/98, PN: rich  + upstream part + zero degree calo                 *
*  09/26/99, E.Cains: 1H geometry added - one svt ladder at layer 3       *
*  01/27/99, PN: rich in 1H geometry is simulated with hits is quartz & fr*
*  05/22/01, PN: starting with tag y2000 field is version 3 (direct map)  *
*  09/30/03, MP: see the many new CVS comments about recent check-ins     *
*  09/30/03, MP: converted the sub into a MODULE to allow for ZEBRA access*
***************************************************************************

   Structure  GDAT {real mfscale, char gtag(2)}

* list of system on/off switches:
   Logical    cave,pipe,svtt,sisd,tpce,ftpc,
              btof,vpdd,magp,calb,ecal,upst,rich,
              zcal,mfld,bbcm,fpdm,phmd,pixl

* Qualifiers:  TPC        TOF         etc
   Logical    mwc,pse,ems,svtw,
              on/.true./,off/.false./




*  Codes:
*  1 - full ctb,         2 - full TOFp based tof,   3 - partial TOFp based tof,
*  4 - single TOFp tray, 5 - one TOFp and one TOFr, 6 - full TOFr based tof.

   real       Par(1000),field,dcay(5),shift(2),wdm

   Integer    LENOCC,LL,IPRIN,Nsi,i,j,l,kgeom,nmod(2),nonf(3),
              ecal_config, ecal_fill,
              Nleft,Mleft,Rv,Rp,Wfr,Itof,mwx,mf,
              btofconfig,
              CorrNum, PhmdVersion, SisdConfig, PipeConfig, CalbConfig

* configuration variables for tuning the geometry:
*            btofconfig  -- tof trays
*            PhmdVersion -- photon multiplicity detector
*            SisdConfig  -- silicon strip
*            PipeConfig  -- beam pipe
*            CalbConfig  -- barrel calorimeter

* CorrNum allows us to control incremental bug fixes in a more
* organized manner

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
    if (Commands(j:j+3)=='YEAR') Geom=Commands(j:l); 
    if (Commands(j:j)  =='Y')    Geom=Commands(j:l); 
    Commands(j:l)=' ';  <W>; (' #1: #2');
]
*
* If geometry was already built, the local DB will be dropped completely now
* but the request for the next geometry should be saved in a temp. par arrray
   call ASLGETBA ('GEOM','DETP',1000,LL,Par)
   If (JVOLUM>0) call AGDROP ('*')

* -------------------- set GSTAR absolute default ------------------------
* before parsing the request, set some default values:
   IPRIN    = IDEBUG
   NtrSubEv = 1000     " automatic !"

* No correction by default
   CorrNum = 0

* No Photon multiplicity detector or Silicon strip by default, hence init the version:
   PhmdVersion = 0
   SisdConfig  = 0
   PipeConfig  = 2 ! Default, Be pipe used in most of the runs =<2003
   btofconfig  = 1 ! ctb only
   CalbConfig  = 0 ! really make use of it starting in y2004

* Set only flags for the main configuration (everthing on, except for tof),
* but no actual parameters (CUTS,Processes,MODES) are set or modified here. 
* If an empty or no DETP GEOM was issued, geometry is defined externally.

   field=5               " default"

* "Canonical" detectors are all ON by default,
   {cave,pipe,svtt,tpce,ftpc,btof,vpdd,calb,ecal,magp,mfld,upst,zcal} = on;
* whereas some newer stuff is considered optional:
   {bbcm,fpdm,phmd,pixl,sisd} = off;

   {mwc,pse}=on          " MultiWire Chambers, pseudopadrows              "
   {ems,rich}=off        " TimeOfFlight, EM calorimeter Sector            "
   Nsi=7; Wfr=0;  Wdm=0; " SVT+SSD, wafer number and width as in code     "
   svtw=on               " water+water manifold in svt, off for Y2000 only"
   mwx=2                 " for Year_1? mwx=1 limites x in mwc hits (<Y2K) "
   Itof=2                " use btofgeo2 - default starting from Y2000     "
   Rv=2                  " add non-sensitive hits to RICH system          "
   Rp=2                  " real RICH position and spectra inst.of nominal "
   nonf={1,2,2}          " ecal on right side, FPD parts on left side     "
   ecal_config=0         " Ecal: east, west or both                       "
   ecal_fill=3           " full wheel by default                          "
   mf=2                  " default field - symmetrical, as fitted by Bill "
   Commands=' ';

   do kgeom=1,8
      Geom(kgeom:kgeom)='\0'; ! Initialize the string with NULLs for strcpy safety
   enddo;

* -------------------- select USERS configuration ------------------------
* On a non-empty DETP GEOM every keyword results in an action and is erased.
*
* Actions consist here of selecting the appropriate parameteres and flags.
* This flags are used in the next section to create subsystems and 
* to communicate DETP commands with parameters to them.
* 

If LL>1   
{ Call AGSFLAG  ('GEOM',1)
* convert input line into a string of upprecase characters
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
                  <W>;('               : year2000, year2001,year2002  ');
                  <W>;('               : year2003, y2003a             ');
                  <W>;('Gcalor         : Gcalor_on, Gcalor_off        ');
                  <W>;('Geant Physics  : Hadr_on, Hadr_off            ');
                  <W>;('Geant Physics  : Phys_off, Decay_Only         ');
                  <W>;('Geometry Detail: mwc_off, pse_off, 4th_off    ');
                  <W>;('Magnetic Field : Field_on/off, field=value    ');
                  <W>;('Auxillary keys : Debug_on/off, Split_on/off   ');
                  <W>;('--------------------------------------------- ');
                  <W>;('Default: complete STAR with hadr_on,auto-split');
                  <W>;('--------------------------------------------- ');
                }  
  on YEAR_1S    { starting in summer: TPC, CONE, AL pipe;
                  pipeConfig=3;  "Aluminum pipe, non standard"
                  {ftpc,vpdd,calb,ecal}=off;                           Nsi=0;
                  mwx=1;}
*   obsoleted pipe config still kept here for reference, improved logic through pipeConfig:
*   if (alpipe)      {call AgDETP add ('pipg.BeLeng=', 0, 1); call AgDETP add ('pipg.S1Leng=',230,1)}


  on YEAR_1A    { poor approximation to year1: TPC+CTB+FTPC;      
                  {vpdd,calb,ecal}=off;      Itof=1;                   Nsi=0;
                  mwx=1;}
  on YEAR_1B    { better year1: TPC+CTB+FTPC+calo patch+RICH, no svt;
                  btofconfig = 4;
                  {vpdd,ecal}=off;  {rich,ems}=on; 
                  nmod={12,0}; shift={87,0}; Itof=1; {Rv,Rp}=1;        Nsi=0;
                  mwx=1;}
  on YEAR_1C    { not a year1:  TPC+CTB+FTPC+calo;  
                  {vpdd,ecal}=off;           Itof=1;                   Nsi=0;
                  mwx=1;}

  on YEAR_1H    { even better y1:  TPC+CTB+FTPC+RICH+caloPatch+svtLadder;  
                  btofconfig=4;
                  {vpdd,ecal}=off;  {rich,ems}=on;  Itof=1; 
                  nmod={12,0}; shift={87,0}; Rp=1; Rv=2; Wdm=6;        Nsi=-3;
                  mwx=1;}

* HELEN:       one ladder at R=10.16cm with 7 wafers at the 12 O'Clock...
  on YEAR_1E    { even better y1:  TPC+CTB+RICH+caloPatch+svtLadder;  
                  btofconfig=4;
                  {vpdd,ecal,ftpc}=off;  {rich,ems}=on;  Itof=1;
                  nmod={12,0}; shift={87,0}; Rp=1; Rv=2; Wfr=7; Wdm=6; Nsi=-3;
                  mwx=1;}

  on YEAR_2B    { old 2001 geometry first guess - TPC+CTB+FTPC+RICH+CaloPatch+SVT;
                  btofconfig=4;
                  {rich,ems}=on;  nmod={24,0}; shift={21,0};  
                  nonf={0,2,2};  Itof=2;  Rv=2;                        Nsi=6; }

  on YEAR_2A    { old asymptotic STAR;    Itof=1; mwx=1;  bbcm=on;            }


* Retiring this version of "complete", and replacing it with a more modern one:
*  on COMPLETE   { Complete STAR geometry; Itof=2; bbcm=on; ecal_fill=3; ecal_config=3;  }

*****               >>>NEW COMPLETE<<<
*
* as complete as Y2003X below but with all corrections AND pixel detector
*
*************************************************************************************************************
  on COMPLETE  { New Complete + correction 3 in 2003 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD;
                  "svt: 3 layers ";
                     nsi=7  " 3 bi-plane layers + ssd ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     btofconfig=5;
                  "calb" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ecal"
                     ecal_config=3   "both wheels"
                     ecal_fill=3     "all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "
                  "geometry correction "
                     CorrNum = 4;
                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdVersion = 1;
                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 1;
* careful! Achtung!
                   pipeConfig=4;   " provisional"
                   pixl=on;    " put the pixel detector in"
                }




* corrected: MWC readout, RICH reconstructed position, no TOF 
  on YEAR2000   { actual 2000:  TPC+CTB+RICH+caloPatch+svtLadder; 
                  {vpdd,ecal,ftpc,svtw}=off; {rich,ems}=on; Field=2.5; 
                  nmod={12,0}; shift={87,0}; Rp=2; Rv=2; Wfr=7; Mf=3;  Nsi=-3;}

  on YEAR2001   { 2001 geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD;
                  btofconfig=4;
                  {rich,ems}=on;

* a newer way to steer ecal:
                  ecal_config=1   " one ecal patch, west "

* this was put here in recent versions (as of 1.50) and I believe this is wrong as
* it destroys compatibility with earlier code: --max--
*    ecal=off;  
                  nmod={24,0}; shift={21,0}; Itof=2; Rv=2; Mf=3;       Nsi=6; }  
                
****************************************************************************************
  on YEAR2002   { january 2002 geometry - TPC+CTB+FTPC+CaloPatch2+Rich+SVT3+BBC+FPD;
                  "svt: 3 layers ";
                     nsi=6        " 3 bi-plane layers, nsi<=7 ";
                     wfr=0        " numbring is in the code   ";
                     wdm=0        " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on       " Wultiwire chambers are read-out ";
                     pse=on       " inner sector has pseudo padrows ";
                  "rich"
                     rich=on      " have rich ";
                     Rv=2;        " save additional (fake) hits "; 
                  "ctb: central trigger barrer ";
                     Itof=2       " vyzyvat' btofgeo2                 ";
                     btofconfig=4;
                  "calb: barrel calorimeter "
                     ems=on       " sector version "
                     nmod={24,0}  " 24 sectors ";
                     shift={21,0} " starting from 21         "; 
                  "ecal "
                     ecal=off
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      " tabulated field, with correction ";
                }
****************************************************************************************
  on YEAR2003   { draft 2003 geometry - TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbring is in the code   ";
                     wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 "  btofgeo2  ";
                     btofconfig=5;
                  "calb" 
                     ems=on   "endcap "
                     nmod={60,0}; shift={0,0}; " 60 sectors "
                  "ecal" 
                     ecal_config=1   "one ecal patch, west "
                     ecal_fill=1     " sectors 2-5 filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "
                }


***********************************************************************
* In y2003a:
*    removed serious bugs from SUPOGEO (incorrect positioning inside the SVT,
*    where is doesn't belong)
*    corrected CALB -- the shift variable (was 0,0 so the barrel wasn't tilted right)
*    corrected SVT  -- the layer radii (from 250 to 150 microns, see the svt code)
****************************************************************************************
  on Y2003A    { correction 1 in 2003 geometry - TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     btofconfig=5;
                  "calb" 
                     ems=on
                     nmod={60,0}; shift={75,0}; " 60 sectors " 
                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=1     " sectors 2-5 filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "
                  "geometry correction "
                     CorrNum = 1;
                }
***********************************************************************
* y2003b is y2003a, but with the extra material in the SVT
* This is actually an important case (i.e. the "most precise" geometry
* approximation for the early 2003 run) which we were lacking so far.
* This is achieved by setting CorrNum to 2.
* The endcap EMC has one third of one wheel, as before
* For more info on the extra material in SVT -- see web page
****************************************************************************************
  on Y2003B    { correction 2 in 2003 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     btofconfig=5;
                  "calb" 
                     ems=on
                     nmod={60,0}; shift={75,0}; " 60 sectors " 
                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=1     " sectors 2-5 filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "
                  "geometry correction "
                     CorrNum = 2;
                }

****************************************************************************************
  on Y2003X    { same as y2003b but with full calorimeters and phmd
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     btofconfig=5;
                  "calb" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 
                  "ecal" 
                     ecal_config=3   "both wheels"
                     ecal_fill=3     "all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "
                  "geometry correction "
                     CorrNum = 2;
                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdVersion = 1;
                }

*
**********************************************************************
* Corrections and enhancements in y2004:
*    added the Photon Multiplicity Detector (PHMD)
*    The endcap EMC has one complete wheel in the west
*    To be done: 3/4 of the second half of the barrel!
*
*                >>>THIS IS THE MASTER GEOMETRY FOR THE SPRING'04<<<
*
****************************************************************************************
  on Y2004     { baseline 2004 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD; 
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     btofconfig=5;
                  "calb" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following vars assume a different meaning
* because we have to (unfortunately) switch from divisions to copies and
* introduce a map
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"
                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "
                  "geometry correction "
                     CorrNum = 3;
                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdVersion = 1;
                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 2;
                }

  on HADR_ON    { all Geant Physics On;                                       }
  on HADR_OFF   { all Geant Physics on, except for hadronic interactions; 
                                                                       IHADR=0}
  on GCALOR_ON { setting hadr 6 to activate hadronic showers;
                              IHADR=6;}

  on PHYS_OFF   { No Physics: only energy loss;
      {IDCAY,IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,IDRAY,IMULS}=0; Iloss=2}
  on DECAY_ONLY { Some Physics: decays, mult.scat and energy loss;
                  {IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,IDRAY}=0; Iloss=2}
  on TPC_ONLY   { Minimal geometry - only TPC;
                  {pipe,svtt,ftpc,btof,vpdd,calb,ecal,magp,upst,zcal,phmd,fpdm,bbcm}=off; }
  on SVTT_ON    { Optional SVTT added on top of the minimal geo;
                     svtt=on; }
*
  on PIPE_ON    { Optional pipe added on top of the minimal geo;
                     pipe=on; }
  on PIPE_OFF   { Pipe optionally removed;
                     pipe=off; }
*
  on FTPC_ON    { Optional FTPC added on top of the minimal geo;
                     ftpc=on; }
  on BTOF_ON    { Optional BTOF added on top of the minimal geo;
                     btof=on; }
  on ECAL_ON    { Optional ECAL added on top of the minimal geo;
                     ecal=on; }
  on CALB_ON    { Optional CALB added on top of the minimal geo;
                     calb=on; }

  on PIXL_ON    { Optional PIXL added on top of the minimal geo;
                     pixl=on; }

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

* -------------------- persist certain global parameters -------------------

   Fill GDAT                     ! GEANT run data
      mfscale=field/5.0          ! magnetic field scale (nominal)
      gtag={geom(1:4),geom(5:8)} ! geometry tag 
   EndFill

* -------------------- setup selected configuration ------------------------
* Now when all parameters and flags are ready, make gstar work as usually
* ie put a MODE or/and DETP command and executing them for selected systems.
*
* - to save secondaries AFTER all decays:      DETP TRAC DCAY 210 210 0.1 0.01
   dcay={210,210,0.1,0.01}
   If LL>1 { call AgDETP new ('Trac'); call AgDETP add ('TracDCAY',dcay,4) }


*  Advertise the geometry correction level used to implement intra-year corrections:
   write(*,*) '************** ATTENTION: Geometry Correction Level:', CorrNum

   if (rich) ItCKOV = 1
   if (cave)        Call cavegeo

* Pipe:
   If (LL>1)        call AgDETP new ('PIPE')
   call AgDETP add ('pipv.pipeConfig=',pipeConfig,2);
   if (pipe)        Call pipegeo

   if (upst)        Call upstgeo

   Call AGSFLAG('SIMU',2)

* - to switch off the fourth svt layer:        DETP SVTT SVTG.nlayer=6 

   If (LL>1 & svtt) then
     call AgDETP new ('SVTT')
     if (Nsi < 7)           call AgDETP add ('svtg.nlayer=',   Nsi,1)
     if (pipeConfig >= 4)   call AgDETP add ('svtg.ifMany=',     1,1)
     if (Wfr > 0)           call AgDETP add ('svtl(3).nwafer=',wfr,1)
     if (wdm > 0)           call AgDETP add ('swca.WaferWid=', wdm,1)
     if (wdm > 0)           call AgDETP add ('swca.WaferLen=', wdm,1)
     if (.not.svtw)         call AgDETP add ('swam.Len=',       0, 1)
   endif

******************************************************************
* Take care of the correction level and call the appropriate constructor:
  if(svtt) then
    if    (CorrNum==0) then
       call svttgeo

    elseif(CorrNum==1) then
       call svttgeo1 ! geometry bug corrected

    elseif(CorrNum==2) then
       call svttgeo2 ! +extra material added

    elseif(CorrNum==3) then
       call svttgeo3 ! +silicon strip detector separated into its own geo file

    elseif(CorrNum==4) then
       call AgDETP add ('svtg.SupportVer=',2 ,1) ! switch to a larger inner shield, AND smaller beampipe support
       call svttgeo3 ! +silicon strip detector separated into its own geo file
    endif
  endif

* Set the proper configuration of the Silicon Strip Detector
  if(sisd) then
       call AgDETP new ('SISD')
       call AgDETP add ('ssdp.Config=',SisdConfig ,1)
       call sisdgeo
  endif


* - MWC or pseudo padrows needed ? DETP TPCE TPCG(1).MWCread=0 TPRS(1).super=1
*   CRAY does not accept construction: IF (mwc==off) ... I do it differntly:
* - for year_1 X in mwc hits was limited, keep this (mwx=1)

   If (LL>1 & tpce) then
     call AgDETP new ('TPCE')
* Attention -- this line below was effectively moved into individual year 1 declarations:
*     If (Geom(1:2)='_1') mwx=1
* Since we don't need the GEOM variable anymore in this context, we use it differently:
* to simply contains the whole geometry tag such as year_1S or y2003a
     If (  .not. mwc   ) mwx=0
     If ( mwx <2 )  call AgDETP add ('tpcg(1).MWCread=',mwx,1)
     If (.not.pse)  call AgDETP add ('tprs(1).super='  , 1, 1) 
   endif 

* Back in July 2003 Yuri has discovered the discrepancy
* in the gas density. The patch for this is activated here:
   if(CorrNum>=3) then
     call AgDETP add ('tpcg.gasCorr=',2 ,1)
   endif

   if (tpce) Call tpcegeo

   if (ftpc) then
	Call ftpcgeo     ! and look at the support pieces:
	if(CorrNum==0) then
           Call supogeo  ! Default, buggy version
	else            
           Call supogeo1 ! New, corrected version
	endif
   endif

* - tof system should be on (for year 2):      DETP BTOF BTOG.choice=2
   If (LL>1 & btof) then
     call AgDETP new ('BTOF')
     call AgDETP add ('btog.choice=',btofconfig,1)
   endif

   if(btof) then
             if(Itof.eq.1) then
                write(*,*) '*************  ATTENTION : OLD VERSION OF BTOF IS NO LONGER IMPLEMENTED ***********'
             else
                call btofgeo2
             endif
   endif
     
   Call AGSFLAG('SIMU',1)
   if (vpdd) Call vpddgeo

********************** BARREL CALORIMETER ************************
*  - Set up the parameters for the barrel calorimeter
   If (LL>1 & calb) then
     call AgDETP new ('CALB')
     if (ems)  call AgDETP add ('calg.nmodule=',Nmod, 2)
     if (ems)  call AgDETP add ('calg.shift=',  shift,2)
   endif


   if (calb) then ! Pick the version:
       if(CalbConfig==0) Call calbgeo
       if(CalbConfig==1) then
           write(*,*) '************** Creating the new version of the Barrel Calorimeter'
           Call calbgeo1
       endif
   endif
******************************************************************
*  - Set up the parameters for the RICH counter
   if (LL>1 & rich) then
      call AgDETP new ('Rich')
      if (Rv>0) call AgDETP add ('Rich.Version=', Rv,1) 
      if (Rp>0) call AgDETP add ('Rich.Position=',Rp,1)
      if (Rp>0) call AgDETP add ('Rich.Cversion=',Rp,1)
   endif
   if (rich) Call richgeo

******************************************************************
*  - Set up the parameters for the endcap calorimeter
   If (LL>1 & ecal) then
      call AgDETP new ('ECAL')
      call AgDETP add ('emcg.OnOff='   ,ecal_config,1)
      call AgDETP add ('emcg.FillMode=',ecal_fill,1)
   endif

******************************************************************
* The rest of steering:

   if (ecal) Call ecalgeo
   if (bbcm) Call bbcmgeo
   if (fpdm) Call fpdmgeo
   if (zcal) Call zcalgeo
   if (magp) Call magpgeo
   if (pixl) Call pixlgeo


******************************************************************
* If PHMD is present and a non-zero version of the Photon Multiplicity Detector
* is defined, pass the version number to its constructor
* and create it:

   if  (phmd.and.PhmdVersion>0) then
      call AgDETP add ('PMDG.version=',PhmdVersion,1)
      call phmdgeo
   endif

****************  Magnetic Field  ********************************
*
* - reset magnetic field value (default is 5): DETP MFLD MFLG.Bfield=5
   If (LL>1) then
      call AgDETP new ('MFLD')
      if (mfld & field!=5) call AgDETP add ('MFLG(1).Bfield=',field,1)
      if (mfld & mf!=0)    call AgDETP add ('MFLG(1).version=',mf,1)
*     if (mfld & mf>=4)    call AgDETP add ('MFLG(1).nrp=',200,1)
*     if (mfld & mf>=4)    call AgDETP add ('MFLG(1).nzp=',800,1)

   endif
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

