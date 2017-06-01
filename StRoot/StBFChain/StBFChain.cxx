//_____________________________________________________________________
// @(#)StRoot/StBFChain:$Name:  $:$Id: StBFChain.cxx,v 1.640 2017/01/06 22:30:45 genevb Exp $
//_____________________________________________________________________
#include "TROOT.h"
#include "TPRegexp.h"
#include "TString.h"
#include "TObjString.h"
#include "TSystem.h"
#include "TInterpreter.h"
#include "TClassTable.h"
#include "StMemStat.h"
#include "StBFChain.h"
#include "StBFChainOpt.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StTreeMaker/StTreeMaker.h"
#include "StIOMaker/StIOMaker.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StMessMgr.h"
#include "StEnumerations.h"
#include "TTree.h"
#include "TEnv.h"
#include "TObjectTable.h"
#include "TDirIter.h"
#include "StdEdxY2Maker/StdEdxY2Maker.h"
#include "BigFullChain.h" /* To check syntax */
#if 0
#include "StarGenerator/BASE/StarPrimaryMaker.h"
#include "StarGenerator/StarGenEventReader/StarGenEventReader.h"
#endif
#define STAR_LOGGER 1
// PLease, preserve the comment after = { . It is used for documentation formatting
//
#if 0
// Keep to be compatible with old documentaion
#define STR_OBSOLETE "WARNING *** Option is OBSOLETE ***"

//#include "BFC.h"
// ITTF Chain will be put here. Option list starting from minimalistic requirements
// and may not initially work.
// Please, preserve the comment after = { . It is used for documentation formatting
//
// ITTF Chains
//#include "BFC2.h"
#endif

// JL - define this once, use two places
#define BFC_DBREGEXP "(dbv|sdt)(\\d+)(_)(.*)(_)(.*)"


// NoChainOptions -> Number of chain options auto-calculated
TableImpl(Bfc);
ClassImp(StBFChain);
static Int_t NoMakersWithInput = 0; // no. of makers which have time stamp 
static TString Dirs[10] = {""};
//_____________________________________________________________________________
// Hack constructor.
/*!
 * This method can be called with mode 1 or 2 to enable chain setup 1 or chain
 * setup 2.
 *
 * Note: This constructor now accepts a second argument for the chain name.
 * Embedding scripts were modified accordingly.
 *
 * This was primarily set to make possible the transition between the regular
 * chain and the ITTF chain options.
 */
void StBFChain::Setup(Int_t mode) {
  static const Char_t *path  = "./StRoot/StBFChain:$STAR/StRoot/StBFChain";
  TString chain("BFC.C");
  Char_t *file = gSystem->Which(path,chain,kReadPermission);
#ifdef STAR_LOGGER
  if (! file) { LOG_FATAL  << Form("StBFChain::Setup\tFile %s has not been found in path %s",chain.Data(),path) << endm; }
  else        { LOG_WARN   << Form("StBFChain::Setup\tFile %s has been found as %s",chain.Data(),file) << endm; }
#else

  if (! file)   Fatal("StBFChain::Setup","File %s has not been found in path %s",chain.Data(),path);
  else        Warning("StBFChain::Setup","File %s has been found as %s",chain.Data(),file);
#endif
  TString cmd(".L ");
  cmd += file;
  gInterpreter->ProcessLine(cmd);
  fchainOpt  = (St_Bfc *) gInterpreter->Calc("CreateTable()");
  cmd = ".U ";
  cmd += file;
  gInterpreter->ProcessLine(cmd);
  assert(fchainOpt);
  delete [] file;
  fNoChainOptions = fchainOpt->GetNRows();
  fBFC = fchainOpt->GetTable();
  // add predifined time stamps and geometry versions
  const DbAlias_t *DbAlias = GetDbAliases();
  Bfc_st row = {"","","","db,detDb","","","",kFALSE};
  for (Int_t i = 0; DbAlias[i].tag; i++) {
    for (Int_t r = 0; r < 2; r++) {
      TString dbTag("");
      if (r) dbTag += "r";
      dbTag += DbAlias[i].tag;
      if (! kOpt(dbTag,kFALSE)) {
	memset (&row.Key, 0, sizeof(row.Key));
	memcpy (&row.Key, dbTag.Data(), dbTag.Length());
	fchainOpt->AddAt(&row);
	fNoChainOptions = fchainOpt->GetNRows();
	fBFC = fchainOpt->GetTable();
      }
    }
  }
  FDate  = FTime  = 0;
  FDateS = FTimeS = 0;
  fFiltTrg   = "";
  fRunG  = -1;
  Gproperty  = ".gopt.";
  Gvalue     = "";
  Gpattern   = "*";


}
//_____________________________________________________________________________
/// Destructor. Call Finish() . See this method for detail on what is cleaned.
StBFChain::~StBFChain(){
  Finish();
}
//_____________________________________________________________________________
/// Routine handling library loading depending on chain options
Int_t StBFChain::Load()
{
  static const Char_t *prefix[3] = {"lib_","lib",""};
  Int_t status = kStOk;
  Int_t i, iok;
  TString lib(gSystem->GetLibraries(0,"D"));
  TObjArray LoadedLibs;
  ParseString(lib,LoadedLibs);
  TIter next(&LoadedLibs);
  TObjString *LoadedLib;
  while ((LoadedLib = (TObjString *) next())){
    TString Base(gSystem->BaseName(LoadedLib->GetName()));
    Base.ReplaceAll(".so","");
    Base.ReplaceAll(".sl","");
    if (Base.BeginsWith("lib_")) Base.ReplaceAll("lib_","");
    if (Base.BeginsWith("lib"))  Base.ReplaceAll("lib","");
    LoadedLib->SetString(Base);
  }
  for (i = 1; i< fNoChainOptions; i++) { // Load Libraries if any
    if (fBFC[i].Flag) {
      if (strlen(fBFC[i].Libs) > 0) {
	TObjArray Libs;
	ParseString(fBFC[i].Libs,Libs);
	TIter nextL(&Libs);
	TObjString *libe = 0;
	Char_t *path = 0;
	while ((libe = (TObjString *) nextL())) {
	  TString libN(libe->GetString());
	  if (libN.BeginsWith("lib_")) libN.ReplaceAll("lib_","");
	  if (libN.BeginsWith("lib"))  libN.ReplaceAll("lib","");
	  if (libN.Contains("St_g2t") && gClassTable->GetID("St_g2t_bbc") >= 0) {
	    continue;
	  }
	  TString libL("");
	  for (Int_t j = 0; j < 3; j++) {
	    libL = prefix[j]; libL += libN;
	    //LOG_QA  << "    Checking " << libL << endm;
	    if ((path = gSystem->DynamicPathName(libL,kTRUE))) break;
	  }
	  iok = -1;
	  if (path) {
	    TIter next(&LoadedLibs);
	    TObjString *LoadedLib;
	    while ((LoadedLib = (TObjString *) next())){
	      TString Base(LoadedLib->GetName());
	      //LOG_QA  << "    Checking " << Base << endm;
	      if (Base == libN) {iok = 1; break;}
	    }
	    if (iok > 0) continue;
	    iok = gSystem->Load(libL);
	    if (iok < 0)  {
	      LOG_FATAL  << "problem with loading of " << libL.Data() << endm;
	      LOG_FATAL  <<  fBFC[i].Key << " is switched off \t!!!!" << endm;
	      //fBFC[i].Flag = kFALSE;
	      //status = kStErr;
	      //assert(iok >= 0); ?? obviously it is so, but not very specific
	      Int_t libraryload = kStErr;
	      assert(libraryload!=kStErr); // this is obvious and will display an explicit
	      break;
	    } else {
	      if (Debug() > 1) {  TString ts("load "); ts += libL; StMemStat::PrintMem(ts.Data());}
	      LOG_QA  << Form("Library %-22s [%15s] (%s)\tis loaded",libL.Data(),fBFC[i].Key,path) << endm;
	      LoadedLibs.Add(new TObjString(libN));
	    }
	  } else {
#if 0
	    if ( ! index(fBFC[i].Libs,',') || Debug() > 1 ) {
	      LOG_WARN << "No path for Key=" << fBFC[i].Key << "-> Searched for [" << libL 
		       << "] from Libs=" << fBFC[i].Libs << " (will proceed)" << endm;
	    }
#endif
	  }
	}
	Libs.Delete();
      }
    }
  }
  LoadedLibs.Delete();
  return status;
}

//_____________________________________________________________________________
/// Maker-instantiation handler.
/*!
  This routine contains it all (make instantiation that is), from calibration
  precedence to parameter setting depending on option etc ... Other thing done
  here which deserves attention
  - The maker's SetMode() mechanism is treated here.
  - Calibration options like NoMySQLDb, NoCintCalDb or NoCintDb and path are set
  - SetFlavor() sim+ofl or sim, and filestreams is made

  If a maker is added along with some flag options, this is the place to
  implement the switches.
*/
Int_t StBFChain::Instantiate()
{
  Int_t status = kStOk;
  Int_t i;
  if (! fNoChainOptions) return status;
  Long64_t maxsize = kMaxLong64;
  TTree::SetMaxTreeSize(maxsize);
  if (GetOption("quiet")) gEnv->SetValue("quiet", 1); 
  St_db_Maker* dbMk = 0;
  for (i = 1; i< fNoChainOptions; i++) {// Instantiate Makers if any
    if (! fBFC[i].Flag) continue;
    TString maker(fBFC[i].Maker);
    if (maker == "") continue;
    TString Key(fBFC[i].Key);
    TString name(fBFC[i].Name);
    Key.ToLower();
    StMaker *myChain = 0;
    StMaker *mk = 0;
    Bool_t isInChain = kFALSE;
    // Require only instance for the following named makers
    if (maker == "St_db_Maker"  || maker == "StTpcDbMaker" ||
	maker == "StSvtDbMaker" || maker == "StSsdDbMaker" || maker == "StSstDbMaker" ||
	maker == "StPxlDbMaker" || maker == "StIstDbMaker" || maker == "StFmsDbMaker" ||
	maker == "StDetectorDbMaker" ||
	maker == "StMagFMaker"    ||
	maker == "StEEmcDbMaker"  ||
	maker == "St_geant_Maker" ||
	maker == "StMcEventMaker" ||
	maker == "StEventMaker" ||
	maker == "StBTofHitMaker" ||
	maker == "StTofMaker" ||
	maker == "StTofHitMaker" ||
	maker == "StEmcRawMaker" ||
	maker == "StPxlRawHitMaker" ||
	maker == "StPxlClusterMaker" ||
	maker == "StPxlHitMaker" ||
	maker == "StIstRawHitMaker" ||
	maker == "StSstDaqMaker" ||
	maker == "StSstPointMaker" ||
	maker == "StIstClusterMaker" ||
	maker == "StIstHitMaker" ||
	maker == "StSsdDaqMaker" ||
	maker == "StTriggerDataMaker" ||
	maker == "StTpcRTSHitMaker" ||
	maker == "StVMCMaker") {
      mk = GetTopChain()->GetMakerInheritsFrom(maker);
      if (mk) {
	isInChain = kTRUE;
	if (maker == "St_geant_Maker" || maker == "StVMCMaker" || maker == "StMcEventMaker") {
	  LOG_INFO << "StBFChain::Instantiate ignore request for instantiation of " << maker
		   << "(\"" << fBFC[i].Name << "\") because chain alreary has one." << endm;
	  continue;
	}
	if (name == "" || name == mk->GetName()) {
	  LOG_INFO << "StBFChain::Instantiate ignore request for instantiation of " << maker
		   << "(\"" << fBFC[i].Name << "\") because chain alreary has one"
		   << " but accumulate options" << endm;
	  //Accumulate option for these makers	  continue;
	}
      }
    }
    if (strlen(fBFC[i].Chain) > 0) myChain = GetMaker(fBFC[i].Chain);
    if (maker == "St_db_Maker"){
      if (Key.CompareTo("db",TString::kIgnoreCase) == 0) {
	dbMk = (St_db_Maker *) mk;
	if (! dbMk) {
	  TString MySQLDb("MySQL:StarDb");
	  TString MainCintDb("$STAR/StarDb");
	  TString Obj("obj");
	  if (TString(gSystem->Getenv("NODEBUG")) != "") Obj = "OBJ";
	  TString MainCintDbObj("$STAR/.$STAR_HOST_SYS/"); MainCintDbObj += Obj; MainCintDbObj += "/StarDb";
	  if (! gSystem->AccessPathName(MainCintDbObj)) MainCintDbObj = "";
	  TString MyCintDb("$PWD/StarDb");
	  TString MyCintDbObj("$PWD/.$STAR_HOST_SYS/"); MyCintDbObj += Obj; MyCintDbObj += "/StarDb";
	  if (! gSystem->AccessPathName(MyCintDbObj)) MyCintDbObj = "";
	  if (GetOption("NoMySQLDb"))   {MySQLDb = "";}
	  // Removed twice already and put back (start to be a bit boring)
	  // DO NOT REMOVE THE NEXT OPTION - Used in AutoCalibration
	  if (GetOption("NoLocalCintDb")) {MyCintDb = "";}
	  if (GetOption("NoStarCintDb") ) {MainCintDb = "";}
	  if (GetOption("NoCintDb")     ) {MainCintDb = ""; MyCintDb = "";}
	  
	  Int_t j;
	  for (j = 0; j < 10; j++) Dirs[j] = "";
	  j = 0;
	  if (MySQLDb       != "") {Dirs[j] = MySQLDb;    j++;}
	  if (MainCintDb    != "") {Dirs[j] = MainCintDb; j++;}
	  if (MyCintDb      != "") {Dirs[j] = MyCintDb;   j++;}
	  if (MainCintDbObj != "") {Dirs[j] = MainCintDbObj; j++;}
	  if (MyCintDbObj   != "") {Dirs[j] = MyCintDbObj;   j++;}
	  dbMk = new St_db_Maker(fBFC[i].Name,Dirs[0],Dirs[1],Dirs[2],Dirs[3],Dirs[4]);
	  if (!dbMk) goto Error;
	  
	  TString namec = dbMk->GetName();
	  int len       = sizeof(fBFC[i].Name);
	  if ( namec.Length() <= len){
	    strncpy (fBFC[i].Name, namec.Data(),len);
	  } else {
	    gMessMgr->Error() << "Maker name [" << namec
			      << "] length is > " << len 
			      << " - increase BFC Name field length" << endm;
	  }

	  // Determine flavors
	  TString flavors = "ofl"; // default flavor for offline

	  // simulation flavors
	  if (GetOption("Simu") && ! GetOption("NoSimuDb")) flavors.Prepend("sim+");

	  // filestream flavors
	  if (fSetFiles) {
	    TString firstFileName = fSetFiles->GetFileName(0);
	    firstFileName = firstFileName(firstFileName.Last('/')+1,firstFileName.Length());
	    if (firstFileName.BeginsWith("st_")) {
	      TString fileStream = firstFileName(3,firstFileName.Index('_',3)-3);
	      if (fileStream.Length()>0) flavors.Prepend(fileStream += '+');
	    }
	  }
 
	  LOG_INFO << "Using DB flavors: " << flavors << endm;
	  dbMk->SetFlavor(flavors.Data());
	  mk = dbMk;
	}
	if (GetOption("dbSnapshot")) dbMk->SetAttr("dbSnapshot","dbSnapshot.root",dbMk->GetName());
      }
      goto Add2Chain;
    }
    if (!mk && maker == "StIOMaker" && fSetFiles) {
      StIOMaker *inpMk=0;
      if (GetOption("InTree")) {
	Char_t line[80] = "bfcTree";
	Int_t k = kOpt("InTree");
	sscanf(fBFC[k].Comment,"%s",line);
	inpMk = new StIOMaker("inputStream","r",fSetFiles,line);
      }
      else inpMk = new StIOMaker("inputStream","r",fSetFiles);
      mk = inpMk;
      if (mk) {
	TString namec = mk->GetName();
	int      len  = sizeof(fBFC[i].Name);
	if ( namec.Length() <= len){
	  strncpy (fBFC[i].Name, namec.Data() , len);
	} else {
	  gMessMgr->Error() << "Maker name [" << namec
			    << "] length is > " << len 
			    << " - increase BFC Name field length" << endm;
	}

	SetInput("StDAQReader",".make/inputStream/.make/inputStream_DAQ/.const/StDAQReader");
	if (GetOption("ReadAll")) {	//activate all branches
	  // inpMk->SetBranch("*",0,"r");
	  const Char_t *allBranches[] = {
	    "emc_rawBranch","eventBranch","ftpc_rawBranch",
	    "geantBranch","globalBranch","McEventBranch","svt_hitsBranch","svt_tracksBranch",
	    "tpc_hitsBranch","trgBranch",0};
	  for (Int_t i = 0; allBranches[i]; i++) inpMk->SetBranch(allBranches[i],0,"r");
	}
        if (GetOption("adcOnly")) mk->SetAttr("adcOnly",1);                        ;
	NoMakersWithInput++;
	goto Add2Chain;
      }
      goto Error;
    }
    if (!mk && maker == "StTreeMaker" && fFileOut != "") {
      StTreeMaker    *treeMk  = 0;
      if (GetOption("OutTree")) {
	Char_t line[80] = "bfcTree";
	Int_t k = kOpt("OutTree");
	sscanf(fBFC[k].Comment,"%s",line);
	treeMk = new StTreeMaker("outputStream",fFileOut.Data(),line);
      }
      else treeMk = new StTreeMaker("outputStream",fFileOut.Data());
      mk = treeMk;
      if (mk) {
	TString namec =  treeMk->GetName();
	int len       = sizeof(fBFC[i].Name);
	if ( namec.Length() <= len ){
	  strncpy (fBFC[i].Name, namec.Data() , len);
	} else {
	  gMessMgr->Error() << "Maker name [" << namec
			    << "] length is > " << len 
			    << " - increase BFC Name field length" << endm;
	}
	treeMk->SetIOMode("w");
	SetTreeOptions();
	goto Add2Chain;
      }
    }
    // Special makers already created or action which
    // need to take place before 'maker' is created.
    if (! mk) {
      if (maker == "StMuDstMaker" && GetOption("RMuDst")) {
	ProcessLine(Form("new StMuDstMaker(0,0,\"\",\"%s\",\"st:MuDst.root\",1e9)",fInFile.Data()));
	mk = GetMaker("MuDst");
	//	if (mk) mk->SetName("RMuDst");
      } else if (maker == "StPicoDstMaker") {
	if (GetOption("RpicoDst")) {
	  ProcessLine(Form("new StPicoDstMaker(0,\"%s\")",fInFile.Data()));
	} else {
	  ProcessLine(Form("new StPicoDstMaker(1,\"%s\")",fFileOut.Data()));
	}
	mk = GetMaker("PicoDst");
      } else {
	if (strlen(fBFC[i].Name) > 0) mk = New(fBFC[i].Maker,fBFC[i].Name);
	else                          mk = New(fBFC[i].Maker);
      }
      if (! mk) {
	LOG_FATAL << Form("StBFChain::Instantiate() problem with instantiation Maker=[%s] Name=[%s]",fBFC[i].Maker,fBFC[i].Name) << endm;
	assert(mk);
      }
    }

    {
      TString namec = mk->GetName();
      int len       = sizeof(fBFC[i].Name);
      if ( namec.Length() <= len){
	strncpy (fBFC[i].Name,namec.Data(),len);
      } else {
	gMessMgr->Error() << "Maker name [" << namec
			  << "] length is > " << len 
			  << " - increase BFC Name field length" << endm;
      }
    }

    if (maker == "StTpcDbMaker" && GetOption("laserIT"))   mk->SetAttr("laserIT"    ,1);
    if (maker == "StDAQMaker") {
      if (GetOption("adcOnly")) mk->SetAttr("adcOnly",1);                        ;
      NoMakersWithInput++;
    }
    if (maker == "StarPrimaryMaker") {
#if 0
      StarPrimaryMaker *primary = (StarPrimaryMaker *) mk;
      if (GetOption("genIn") && fInFile != "") {
	ProcessLine(Form("StarGenEventReader *EvR = new StarGenEventReader();"
			 "EvR->SetInputFile(\"%s\",\"genevents\",\"primaryEvent\");"
			 "((StarPrimaryMaker *) %p)->AddGenerator(EvR);", fInFile.Data(), mk));
	eventreader -> SetInputFile(fInFile.Data(),"genevents","primaryEvent");
	primary->AddGenerator(eventreader);
      }
#endif
      if (GetOption("genIn")) mk->SetAttr("FreezePV", 1);
    }
    if (maker == "StarGenEventReader" &&  fInFile != "") {
      ProcessLine(Form("((StarGenEventReader *) %p)->SetInputFile(\"%s\",\"genevents\",\"primaryEvent\");", mk, fInFile.Data()));
    }
    if (maker == "St_geant_Maker") { // takes only first request for geant, if it is active then it should be the first one
      Int_t NwGeant = 10; // default geant parameters
      if (!GetOption("fzin")  && !GetOption("fzinSDT") &&
	  !GetOption("ntin")  &&
	  !GetOption("gstar") && 
	  !GetOption("pythia"))                      NwGeant =  5;
      if (GetOption("big"))                          NwGeant = 20;
      if (GetOption("bigbig"))                       NwGeant = 40;
      if (GetOption("verybig"))                      NwGeant = 80;
      ProcessLine(Form("((St_geant_Maker *) %p)->SetNwGEANT(%i);",mk,NwGeant));
      if (GetOption("Higz")) ProcessLine(Form("((St_geant_Maker *) %p)->SetIwtype(1);",mk));
      if (GetOption("paw"))  ProcessLine(Form("((St_geant_Maker *) %p)->SetNwPAW(2);",mk));
      TString CintF(SAttr("GeneratorFile"));
      if (GetOption("fzin")        || GetOption("fzinSDT")        ||
	  GetOption("ntin")        || 
	  GetOption("gstar")       || 
	  GetOption("PrepEmbed")   || 
	  GetOption("mtin")        ||
	  GetOption("mickey")      ||
	  CintF != "") {
	mk->SetActive(kTRUE);
	//	if (GetOption("PrepEmbed")) mk->SetMode(10*(mk->GetMode()/10)+1);
	if (GetOption("Embedding") || GetOption("PrepEmbed") || GetOption("pythiaEmbed") || GetOption("fzinSDT")) mk->SetAttr("Don'tTouchTimeStamp",1);
	if (GetOption("flux"))      mk->SetAttr("flux",1);
	if (GetOption("fzout"))     mk->SetAttr("fzout",1);
	if (GetOption("beamLine"))  mk->SetAttr("beamLine",1);
	if (GetOption("Wenu"))      mk->SetAttr("Wenu",1);
	if (GetOption("mickey"))    mk->SetAttr("Mickey",1);
	if (CintF != "")            mk->SetAttr("GeneratorFile",CintF.Data());
      } else if (GetOption("Generators")) {
	// Keep it active
	//	ProcessLine(Form("((St_geant_Maker *) %p)->Do(\"gkine -4 0\")",mk));
      } else mk->SetActive(kFALSE);
      if (! mk) goto Error;
      SetGeantOptions(mk);
      if (GetOption("fzin")        || GetOption("fzinSDT")        ||
	  GetOption("PrepEmbed")   || 
	  GetOption("mtin")) {
	NoMakersWithInput++;
      }
      if (fRunG > 0) {
	mk->SetAttr("RunG",fRunG);
      }
      if (GetOption("AgStar")) {
	ProcessLine("AgStarReader::Instance();");
      }
    }
    // special maker options
    // m_Mode xyz
    //        x = 1 phys_off                    
    //        y = 1 Passive mode (do not call RunMC()
    //        z = 1 Mixer Mode
    if (maker == "StVMCMaker") {
      if (GetOption("VMCPassive")) {// don't use mk->SetActive(kFALSE) because we want to have InitRun
	mk->SetAttr("VMCPassive",1);
      }
      else {
	if (GetOption("phys_off")) mk->SetAttr("phys_off",1);
	if (GetOption("hadr_off")) mk->SetAttr("hadr_off",1);
	if (fInFile != "")  {
	  if (ProcessLine(Form("((StVMCMaker *) %p)->SetInputFile(\"%s\")",mk,fInFile.Data())))
	    goto Error;
	}
	if (GetOption("VMCAlignment")) {
	  mk->SetAttr("VMCAlignment",1);
	}
	if (fRunG > 0) {
	  mk->SetAttr("RunG",fRunG);
	}
      }
      if (GetOption("Embedding")) mk->SetAttr("Embedding",1);
      TString CintF(SAttr("GeneratorFile"));
      if (CintF != "") mk->SetAttr("GeneratorFile",CintF.Data());
    }

    // ---
    //		Sti(ITTF) start
    // ---
    if (maker == "StiMaker" || maker == "StiVMCMaker" || maker == "StvMaker") {
      if ( maker == "StvMaker" &&  GetOption("StvCA")) {
	//      mk->SetAttr("seedFinders","CA","Stv");              // for CA seed finder
	mk->SetAttr("seedFinders","CA,Default","Stv");      // for CA + Default seed finders
      }

      // When StiCA library is requested CA will be used as seed finder in StiMaker
      if ( GetOption("StiCA") ) {
        mk->SetAttr("seedFinders", "CA DEF");
      }

      // Option to re-use hits in other tracks
      if ( GetOption("hitreuseon") ){
	mk->SetAttr("SetMaxTimes", 100); 
      }

      // old logic for svt and ssd
      if (GetOption("NoSvtIT")){
	mk->SetAttr("useSvt"	,0);
      } else {
	if (GetOption("SvtIT")){
	  mk->SetAttr("useSvt"	  ,1);
	  mk->SetAttr("activeSvt" ,1);
	}
      }
      if (   GetOption("NoSsdIT") &&
	    !GetOption("SstIT") ){
	mk->SetAttr("useSsd"	,0);
      } else {
	if (GetOption("SsdIT")){
	  mk->SetAttr("useSsd"	  ,1);
	  mk->SetAttr("activeSsd" ,1);
	}
      }

      // this was an R&D detector never implemented
      // simulations were made nonetheless
      if (GetOption("HpdIT")){
	mk->SetAttr("useHpd"     ,1);
	mk->SetAttr("activeHpd"  ,1);
      }

      // back to the HFT sub-system
      if (GetOption("NoPxlIT")) {
	mk->SetAttr("usePxl"	 ,kTRUE);
	mk->SetAttr("usePixel"	 ,kTRUE);
      } else {
        if (GetOption("PixelIT") || GetOption("PxlIT") ){
	  mk->SetAttr("usePxl"     ,kTRUE);
	  mk->SetAttr("usePixel"	 ,kTRUE);
	  mk->SetAttr("activePxl"  ,kTRUE);
	  mk->SetAttr("activePixel",kTRUE);
        }
      }
      if (GetOption("NoIstIT")) {
	mk->SetAttr("useIst"	 ,kTRUE);
      } else {
        if (GetOption("IstIT")){
	  mk->SetAttr("useIst"     ,kTRUE);
	  mk->SetAttr("activeIst"  ,kTRUE);
        }
      }
      if (GetOption("NoSstIT")) {
        mk->SetAttr("useSst"	 ,kTRUE);
      } else {
        if (GetOption("SstIT")){
	  mk->SetAttr("useSst"	 ,kTRUE);
	  mk->SetAttr("activeSst"  ,kTRUE);
        }
      }

      // other sub-systems
      if (GetOption("BTofIT")){
	mk->SetAttr("useBTof"    ,1);
	mk->SetAttr("activeBTof" ,1);
      }

      if (GetOption("StiPulls") || 
	  GetOption("StvPulls"))  mk->SetAttr("makePulls"  ,1);
      if (GetOption("skip1row"))  mk->SetAttr("skip1row"   ,1);
      if (GetOption("EastOff"))   mk->SetAttr("EastOff"    ,1);
      if (GetOption("WestOff"))   mk->SetAttr("WestOff"    ,1);
      if (GetOption("laserIT"))   mk->SetAttr("laserIT"    ,1);
      if (GetOption("Alignment")) mk->SetAttr("Alignment"  ,1);
      if (GetOption("Cosmics"))   mk->SetAttr("Cosmics"    ,1);
      mk->PrintAttr();
    }
    if (maker== "StGmtClusterMaker") {
      if (GetOption("gmtCosmics"))  mk->SetAttr("gmtCosmics",  1);
    }
    if (maker=="StKFVertexMaker" && GetOption("laserIT"))   mk->SetAttr("laserIT"    ,1);
    //		Sti(ITTF) end
    if (maker=="StGenericVertexMaker") {
      // VertexFinder methods
      if (GetOption("Sti") || GetOption("StiCA") ||
	  GetOption("Stv") ||
	  GetOption("StiVMC"     ) ) mk->SetAttr("ITTF"         , 1);
      if (GetOption("VFMinuit"   ) ) mk->SetAttr("VFMinuit"   	, 1);
      if (GetOption("VFppLMV"    ) ) mk->SetAttr("VFppLMV"    	, 1);
      if (GetOption("VFppLMV5"   ) ) mk->SetAttr("VFppLMV5"   	, 1);
      if (GetOption("VFPPV"      ) ) mk->SetAttr("VFPPV"      	, 1);
      if ((GetOption("VFPPV") && GetOption("Stv")) || GetOption("VFPPVEv") ) {
        gSystem->Load("StBTofUtil.so");
        mk->SetAttr("VFPPVEv"      , 1);
      } else if (GetOption("VFPPV") && GetOption("Sti")) mk->SetAttr(    "VFPPV", kTRUE);
      if (GetOption("VFPPVEvNoBtof")){
        gSystem->Load("StBTofUtil.so"); //Not used but loaded to avoid fail
        mk->SetAttr("VFPPVEvNoBtof", 1);
      }
      if (GetOption("VFPPVnoCTB" ) ) mk->SetAttr("VFPPVnoCTB" 	, kTRUE);
      if (GetOption("VFFV"       ) ) mk->SetAttr("VFFV"       	, kTRUE);
      if (GetOption("VFMCE"      ) ) mk->SetAttr("VFMCE"      	, kTRUE);
      if (GetOption("VFMinuit2"  ) ) mk->SetAttr("VFMinuit2"  	, kTRUE);
      if (GetOption("VFMinuit3"  ) ) mk->SetAttr("VFMinuit3"  	, kTRUE);
      if (GetOption("beamLine"   ) ) mk->SetAttr("BeamLine"   	, kTRUE);
      if (GetOption("beamLine3D" ) ) mk->SetAttr("BeamLine3D"  	, kTRUE);
      if (GetOption("CtbMatchVtx") ) mk->SetAttr("CTB"        	, kTRUE);
      if (GetOption("min2trkVtx" ) ) mk->SetAttr("minTracks" 	, 2);
      if (GetOption("VtxSeedCalG") ) mk->SetAttr("calibBeamline", 1);
      if (GetOption("usePct4Vtx" ) ) mk->SetAttr("PCT"          , 1);
      if (GetOption("useBTOF4Vtx") ) mk->SetAttr("BTOF"         , 1);
      mk->PrintAttr();
    }
    if (maker=="StKFVertexMaker") {
      if (GetOption("beamLine"   ) ) mk->SetAttr("BeamLine"   	, 1);
      if (GetOption("VFFV"       ) ) mk->SetAttr("VFFV"       	, 1);
      if (GetOption("VFMCE"      ) ) mk->SetAttr("VFMCE"      	, 1);
    }
    if (maker=="StAssociationMaker") {

      LOG_QA << "StBFChain::Instantiate Setting the Parameters for the Association Maker" << endm;

      TString cmd("");
      if (GetOption("ITTF") || GetOption("StiVMC") || GetOption("useInTracker"))
	cmd = Form ("((StAssociationMaker *) %p)->useInTracker();",mk);
      cmd += "StMcParameterDB* parameterDB = StMcParameterDB::instance();";
      // TPC
#if 0
      cmd += "parameterDB->setXCutTpc(.5);"; // 5 mm
      cmd += "parameterDB->setYCutTpc(.5);"; // 5 mm
      cmd += "parameterDB->setZCutTpc(.5);"; // 5 mm
#endif
      cmd += "parameterDB->setReqCommonHitsTpc(3);"; // Require 3 hits in common for tracks to be associated
      // FTPC
#if 0
      cmd += "parameterDB->setRCutFtpc(.3);"; // 3 mm
      cmd += "parameterDB->setPhiCutFtpc(5*(3.1415927/180.0));"; // 5 degrees
#endif
      cmd += "parameterDB->setReqCommonHitsFtpc(3);"; // Require 3 hits in common for tracks to be associated
      // SVT
#if 0
      cmd += "parameterDB->setXCutSvt(.08);"; // 800 um
      cmd += "parameterDB->setYCutSvt(.08);"; // 800 um
      cmd += "parameterDB->setZCutSvt(.08);"; // 800 um
#endif
      cmd += "parameterDB->setReqCommonHitsSvt(1);"; // Require 1 hits in common for tracks to be associated
      if (GetOption("IdTruth")) cmd += Form("((StAssociationMaker *) %p)->useIdAssoc();",mk);
      ProcessLine(cmd);
    }
    if (maker == "StMiniMcMaker" && GetOption("IdTruth") && ! GetOption("OldMiniMc")) {
      mk->SetMode(1);
      gMessMgr->QAInfo() << "StBFChain::Instantiate create simplified MiniMcTree in " << maker  << endm;
    }
    // usually, we do maker first and option second but the
    // logic is more readable with option first here (as it
    // got a bit out of hand)
    if (GetOption("ppOpt") ) {                         // pp specific stuff
      if (maker == "StTrsMaker")
	mk->SetMode(1);       // Pile-up correction
    }

    if (maker == "StStrangeMuDstMaker" && GetOption("CMuDST")&& GetOption("StrngMuDST") ) {
      TString cmd(Form("StStrangeMuDstMaker *pSMMk = (StStrangeMuDstMaker*) %p;",mk));
      cmd += "pSMMk->DoV0();";                                 // Set StrangeMuDstMaker parameters
      cmd += "pSMMk->DoXi();";
      cmd += "pSMMk->DoKink();";
      cmd += "pSMMk->SetNoKeep();";                            // Set flag for output OFF
      ProcessLine(cmd);
    }

    // Alex requested an option (not turned by default) to disable all
    // hit reco cuts. This will make allm hits saved to MuDST /ezTree.
    if ( maker == "StEmcRawMaker" && GetOption("BEmcDebug"))
      mk->SetMode(1); // only one option now, bit a bitmask

    // Use status tables for raw BEMC data (helpful for QA)
    if ( maker == "StEmcRawMaker" && GetOption("BEmcChkStat"))
      mk->SetAttr("BEmcCheckStatus",1);

    // MuDST and ezTree. Combinations are
    //  ezTree         -> ezTree only
    //  CMuDST         -> regular MuDST only
    //  ezTree,CMuDST  -> both
    if (maker == "StMuDstMaker" && GetOption("ezTree") ){
      TString cmd(Form("StMuDstMaker *pMuMk = (StMuDstMaker *) %p;",mk));
      if ( ! GetOption("CMuDST")) cmd += "pMuMk->SetStatus(\"*\",0);";
      cmd += "pMuMk->SetStatus(\"EztAll\",1);";
      ProcessLine(cmd);
    }

    if ( maker == "StPicoDstMaker"){
      if ( GetOption("picoWrite") )  mk->SetMode(1);
      if ( GetOption("picoRead")  )  mk->SetMode(2);   // possibly more magic
      if ( GetOption("PicoVtxVpd"))           mk->SetAttr("picoVtxMode", "PicoVtxVpd");
      else if ( GetOption("PicoVtxDefault"))  mk->SetAttr("picoVtxMode", "PicoVtxDefault");
      
    }


    if (maker == "StLaserEventMaker"){
      // Bill stuff - Empty place-holder
    }
    if (maker == "StDetectorDbMaker") {
      if ( GetOption("DbRichSca") ) mk->SetMode(1);
    }
    if (maker == "StTpcRSMaker") {
      if (! GetOption("TrsToF")) {
	Int_t mode = mk->GetMode();
	mode |= (1 << 10); // kNoToflight   //10 don't account for particle time of flight
	mk->SetMode(mode);
      }
    }
    if (maker == "StTrsMaker") {
      Int_t mode = 0;
      if (GetOption("TrsPileUp")) mode += 1; // Pile-up correction
      if (GetOption("TrsToF"))    mode += 2; // account for particle time of flight
      if (mode) mk->SetMode(mode);
    }
    // Place-holder. Would possibly be a bitmask
    if (maker == "StTofrMatchMaker"){
      mk->SetMode(0);
    }

    if (maker == "StSpaceChargeEbyEMaker") {
      if ( GetOption("SpcChgCal") ||
	   GetOption("SpcChgCalG"))   mk->SetMode(2);
      if ( GetOption("SCScalerCal") ) mk->SetMode(4);
      if ( GetOption("EastOff"))      mk->SetAttr("EastOff",1);
      if ( GetOption("WestOff"))      mk->SetAttr("WestOff",1);
    }
    if (maker == "StEventQAMaker" && GetOption("QAalltrigs"))
      ProcessLine(Form("((StEventQAMaker *) %p)->AllTriggers();",mk));
    //Special options for V0s and Xis using estGlobal tracks
    if(maker=="StV0FinderMaker" && Key=="v0svt"){
      TString cmd(Form("StV0FinderMaker *V0mk=(StV0FinderMaker*) %p;",mk));
      cmd += "V0mk->SetLanguageUsage(0);";
      cmd += "V0mk->SetSVTUsage(1);";
      cmd += "V0mk->SetV0LanguageUsage(3);";
      ProcessLine(cmd);
    }
    if(maker=="StXiFinderMaker" && Key=="xisvt"){
      TString cmd(Form("StXiFinderMaker *Ximk=(StXiFinderMaker*) %p;",mk));
      cmd += "Ximk->SetLanguageUsage(0);";
      cmd += "Ximk->SetSVTUsage(1);";
      cmd += "Ximk->SetV0LanguageUsage(3);";
      cmd += "Ximk->SetXiLanguageUsage(5);";
      ProcessLine(cmd);
    }
    if (maker == "StTpcRTSHitMaker") {
      if ( GetOption("TpxClu2D")) mk->SetAttr("TpxClu2D", 1);
    }
    if (GetOption("NoTpxAfterBurner") && (maker == "StTpcHitMaker" || maker == "StTpcRTSHitMaker")) {
      mk->SetAttr("NoTpxAfterBurner", 1);
    }
    if (maker == "StTpcHitMaker"  && GetOption("NoAnnotateCL")) {
      mk->SetAttr("UseTonkoClusterAnnotation", 0);
    }
    if (maker == "StTpcDbMaker"){
      //      if ( GetOption("Simu") && ! GetOption("NoSimuDb")) mk->SetAttr("Simu",1);
      if ( GetOption("useLDV")    ) mk->SetAttr("useLDV",1) ;// uses laserDV database
      if ( GetOption("useCDV")    ) mk->SetAttr("useCDV",1) ;// uses ofl database
      if ( GetOption("useNewLDV") ) mk->SetAttr("useNewLDV",1);// uses new laserDV
      if (GetOption("ExB")){
	mk->SetAttr("ExB", 1);	// bit 0 is ExB ON or OFF
	if      ( GetOption("EB1") ) mk->SetAttr("EB1", 1);
	else if ( GetOption("EB2") ) mk->SetAttr("EB2", 1);
	else {
	  // depend on RY option i.e. take default for that RealYear data
	  // expectations.
	  if(GetOption("RY1H")    ||
	     GetOption("RY2000")  ||
	     GetOption("RY2001")  ||
	     GetOption("RY2001N") ||
	     GetOption("RY2003")  ||
	     GetOption("RY2003X"))   mk->SetAttr("OldRuns", 1);
	}
	// Other options introduced in October 2001 for distortion corrections
	// studies and year1 re-production. Those are OR additive to the mask.
	//(void) printf("StBFChain:: Options list : %d %d %d %d %d %d %d %d\n",
	//		  kPadrow13,kTwist,kClock,kMembrane,kEndcap,
	//            kIFCShift,kSpaceCharge,kSpaceChargeR2);
	if( GetOption("OBmap")      ) mk->SetAttr("OBmap"      , 1);
	if( GetOption("OPr13")      ) mk->SetAttr("OPr13"      , 1);
	if( GetOption("OTwist")     ) mk->SetAttr("OTwist"     , 1);
	if( GetOption("OClock")     ) mk->SetAttr("OClock"     , 1);
	if( GetOption("OCentm")     ) mk->SetAttr("OCentm"     , 1);
	if( GetOption("OECap")      ) mk->SetAttr("OECap"      , 1);
	if( GetOption("OIFC")       ) mk->SetAttr("OIFC"       , 1);
	if( GetOption("OSpaceZ")    ) mk->SetAttr("OSpaceZ"    , 1);
	if( GetOption("OSpaceZ2")   ) mk->SetAttr("OSpaceZ2"   , 1);
	if( GetOption("OShortR")    ) mk->SetAttr("OShortR"    , 1);
	if( GetOption("OBMap2d")    ) mk->SetAttr("OBMap2d"    , 1);
	if( GetOption("OGridLeak")  ) mk->SetAttr("OGridLeak"  , 1);
	if( GetOption("OGridLeak3D")) mk->SetAttr("OGridLeak3D", 1);
	if( GetOption("OGridLeakFull")) mk->SetAttr("OGridLeakFull", 1);
	if( GetOption("OGGVoltErr") ) mk->SetAttr("OGGVoltErr" , 1);
	if( GetOption("OSectorAlign"))mk->SetAttr("OSectorAlign",1);
      }
      mk->PrintAttr();
    }
    if ((maker == "StdEdxY2Maker"  || maker == "StTpcHitMover") &&
	GetOption("EmbeddingShortCut"))  {
      mk->SetAttr("EmbeddingShortCut", 1);
      mk->PrintAttr();
    }
    if (maker == "StdEdxY2Maker" && GetOption("dEdxCalib")) {
      Int_t Mode = 0;
      SETBIT(Mode,StdEdxY2Maker::kCalibration);
      SETBIT(Mode,StdEdxY2Maker::kGASHISTOGRAMS);
      SETBIT(Mode,StdEdxY2Maker::kPadSelection); 
      SETBIT(Mode,StdEdxY2Maker::kPadSelection);
      SETBIT(Mode,StdEdxY2Maker::kAlignment);
      LOG_INFO << " set dEdxY2 Mode " << Mode << " =======================================" << endm;
      mk->SetMode(Mode); 
    }
#if 0
    if (maker == "StSvtDbMaker" || maker == "StSsdDbMaker"){
      mk->SetMode(0);
      // If simulation running make sure pick up simu stuff from db
      if (GetOption("Simu") && ! GetOption("NoSimuDb")) mk->SetMode(1);
    }
#endif
    // FTPC
    if ((maker == "StFtpcClusterMaker" ||
	 maker == "StFtpcTrackMaker"    )  &&
	GetOption("fdbg"))                     mk->SetMode(mk->GetMode()+2);
    if ( ( maker == "StFtpcClusterMaker" || // ?
	   maker == "StFtpcTrackMaker" ) &&
	 GetOption("flaser"))                  mk->SetMode(mk->GetMode()+1);

    if ((maker == "StFtpcClusterMaker" ||
	 maker == "StFtpcTrackMaker"    )  &&
	GetOption("fgain"))                    mk->SetMode(mk->GetMode()+4);

    // FTPC

    // PMD
    if ( maker == "StPmdReadMaker"         &&
         GetOption("pmdRaw"))                  mk->SetAttr("pmdRaw",1);
    // PMD

    // Hit filtering will be made from a single maker in
    // future with flexible filtering method
    if (maker == "StHitFilterMaker") {
      if (GetOption("SvtHitFilt")){
	// Filter out SVT bad hits, TPC hits not on tracks and all hits if fabs(ZVert)>30
	LOG_QA << "SVT hit filter is ON" << endm;
	TString cmd(Form("StHitFilterMaker *Filtmk=(StHitFilterMaker*) %p;",mk));
	cmd += "Filtmk->setPtLowerCut(-99.);";
	cmd += "Filtmk->setPtUpperCut(-99.);";
	cmd += "Filtmk->setAbsEtaCut(-99);";
	cmd += "Filtmk->setAbsZVertCut(30);";
	ProcessLine(cmd);
      } else if (GetOption("TpcHitFilt")){
	// Filter out TPC hits not on tracks
	LOG_QA << "TPC hit filter is ON" << endm;
	TString cmd(Form("StHitFilterMaker *Filtmk=(StHitFilterMaker*) %p;",mk));
	cmd += "Filtmk->setPtLowerCut(-99.);";
	cmd += "Filtmk->setPtUpperCut(-99.);";
	cmd += "Filtmk->setAbsEtaCut(-99);";
	cmd += "Filtmk->setAbsZVertCut(999);";
	ProcessLine(cmd);
      } else if (GetOption("HftHitFilt")){
	// Filter out TPC hits not on tracks
	LOG_QA << "HFT hit filter is ON" << endm;
	TString cmd(Form("StHitFilterMaker *Filtmk=(StHitFilterMaker*) %p;",mk));
	cmd += "Filtmk->setAbsZVertCut(-1);";
	cmd += "Filtmk->setKeepWestHighEtaHitsForFgt(0);";
	ProcessLine(cmd);
      } else {
	LOG_QA << "Default hit filtering is ON" << endm;
      }
      Int_t    mode = 0;
      if (GetOption("KeepTpcHit")) mode |= (1 << kTpcId);
      if (GetOption("KeepSvtHit")) mode |= (1 << kSvtId);
      mk->SetMode(mode);
      // the m_Mode (Int_t is signed integer 4 bytes) mask is too short for the FGT
      if (GetOption("KeepFgtHit")){
	TString cmd(Form("StHitFilterMaker *Filtmk=(StHitFilterMaker*) %p;",mk));
	cmd += "Filtmk->setKeepWestHighEtaHitsForFgt(1.0);";
	ProcessLine(cmd);
      }
    }
    if (maker == "StMiniMcMaker" && fFileOut != "") {
      ProcessLine(Form("((StMiniMcMaker *) %p)->setFileName(\"%s\");", mk, fFileOut.Data()));
    }
    if (maker == "StMcAnalysisMaker") {
      Int_t mode = 0;
      if (GetOption("McAnaTpc")) mode += 0x1;
      if (GetOption("McAnaSvt")) mode += 0x2;
      if (GetOption("McAnaSsd")) mode += 0x4;
      if (mode)
	ProcessLine(Form("((StMaker *) %p)->SetMode(%i);", mk, mode));
    }
    if (maker == "StBTofCalibMaker") {
      if  (GetOption("UseProjectedVertex")) mk->SetAttr("UseProjectedVertex",1);
      if  (GetOption("UseMCTstart"))        mk->SetAttr("UseMCTstart",1);
    }
    if (maker == "StEventMaker" && GetOption("laserIT"))   mk->SetAttr("laserIT",1);
    if (maker == "StEventMaker" && fFiltTrg.Length()) {
      mk->SetAttr("FiltTrg",(Int_t) (fFiltTrg.BeginsWith('+') ? 1 : -1));
      TString FiltTrgFlavor = fFiltTrg(1,128);
      if (FiltTrgFlavor.Length())
        SetFlavor((FiltTrgFlavor += "+ofl").Data(),"trgOfflineFilter");
    }
  Add2Chain:
    if (! mk) continue;
    if (isInChain) continue;
    if (name == "") strncpy (fBFC[i].Name,(Char_t *) mk->GetName() , sizeof(fBFC[i].Name));
    if (myChain) myChain->AddMaker(mk);
    continue;
  Error:
    status = kStErr;
    LOG_QA	<< " ======================================"          << endm;
    LOG_QA	<< " problem with Instantiation of " << fBFC[i].Maker << endm;
    LOG_QA	<< " ======================================"          << endm;
  }
  //  PrintQAInfo();
  PrintInfo();
  // START the chain (may the force be with you)
  // Create HTML docs of all Maker's inv
#if 0
  if (GetOption("MakeDoc"))  MakeDoc();
#endif
  if (GetOption("Debug"))    SetDEBUG(1);
  if (GetOption("Debug1"))   SetDEBUG(1);
  if (GetOption("Debug2"))   SetDEBUG(2);
  if (GetOption("nohistos")) SetAttr(".histos",0,"*");
  else                       SetAttr(".histos",1,"*");
  if (GetOption("NoRepeat")) gMessMgr->IgnoreRepeats();

  if (GetOption("svt1hit"))  SetAttr("minPrecHits",1,"Sti");
  if (GetOption("svt1hit"))  SetAttr("minPrecHits",1,"StiCA");
  if (GetOption("svt1hit"))  SetAttr("minPrecHits",1,"Stv");
  if (GetOption("svt1hit"))  SetAttr("minPrecHits",1,"StiVMC");
  SetDbOptions(dbMk);

  gMessMgr->QAInfo() << "+++ Setting attribute " << Gproperty.Data() << " = " << Gvalue.Data() << endm;
  SetAttr(Gproperty.Data(),Gvalue.Data(),Gpattern.Data());

  return status;
}
//_____________________________________________________________________
Int_t StBFChain::Init() {

  TDatime td;
  Info("Init","Time=%s Cwd=%s",td.AsString(),gSystem->pwd());

  SetChainOpt(new StBFChainOpt(this));
  //  SetDbOptions(); moved to Instantiation
  if (fNoChainOptions) {
    //  SetGeantOptions(); move back to Init
    if (GetOption("Simu") && ! (GetOption("mtin") || GetOption("PrepEmbed") || GetOption("pythiaEmbed"))) { //  ! Simu
      StEvtHddr *fEvtHddr = (StEvtHddr*)GetDataSet("EvtHddr");
      if (!fEvtHddr) {
	fEvtHddr = new StEvtHddr(m_ConstSet);
	SetOutput(fEvtHddr);	              //Declare this "EvtHddr" for output
      }
      if (fEvtHddr->GetRunNumber() < 0 || fEvtHddr->GetRunNumber() >= 1000000) {
	fEvtHddr->SetRunNumber(1); // to have run positive and < 1000000 (to avoid mess with RunLog)
      }
    }
  }
  if (fNoChainOptions) {
    if (GetOption("NoOutput") || GetOption("EvOutOnly")) {
      if (! GetOption("RMuDst"))
	SetAttr(".call","SetActive(0)","MuDst");		//NO MuDst
      if (! GetOption("EvOutOnly")) {
	SetAttr(".call","SetActive(0)","outputStream");	//NO Out
      }
      SetAttr(".call","SetActive(0)","kink2");
      SetAttr(".call","SetActive(0)","StTagsMaker::");
      SetAttr(".call","SetActive(0)","StStrangeMuDstMaker::");
    }
#if 0
    // force load of geometry for VMC and Sti
    
    if (GetOption("Sti") || GetOption("StiCA") || 
	GetOption("Stv") || 
	GetOption("StiVMC") ||GetOption("VMC") || 
	GetOption("VMCPassive")) {
      const DbAlias_t *DbAlias = GetDbAliases();
      for (Int_t i = 0; DbAlias[i].tag; i++) {
	TString dbTag("r");
	dbTag += DbAlias[i].tag;
	if (GetOption(dbTag)) {
	  TString path("./StarDb/AgiGeometry:$STAR/StarDb/AgiGeometry");
	  if (GetOption("AgML")) {
	    path  = "./StarDb/AgMLGeometry:$STAR/StarDb/AgMLGeometry";
	  }
	  if (GetOption("VmcGeo")) {
	    path  = "./StarDb/VmcGeo:$STAR/StarDb/VmcGeo";
	  }
	  TString geom("Geometry.");
	  geom +=  DbAlias[i].geometry;
	  geom += ".C";
	  Char_t *file = gSystem->Which(path.Data(),geom,kReadPermission);
	  if (file) {
	    LOG_INFO << "StBFChain::Init force load of " << file << endm;
	    TString command = ".L "; command += file;
	    gInterpreter->ProcessLine(command);
	    gInterpreter->Calc("CreateTable()");
	    command.ReplaceAll(".L ",".U ");
	    gInterpreter->ProcessLine(command);
	    delete [] file;
	  } else {
	    LOG_INFO << "StBFChain::Init file for geometry tag  " << geom << " has not been found in path" << path << endm;
	  }
	  break;
	}
      }
    }
#endif
  }
  return StChain::Init();
}
//_____________________________________________________________________
/// Really the destructor (close files, delete pointers etc ...)
Int_t StBFChain::Finish()
{
  if (!fBFC) return kStOK;
  Int_t ians = StChain::Finish();
  TFile *tf = GetTFile();
  if (tf) {
    if (tf->IsWritable()) {
      tf->Write(); tf->Flush(); 
    }
    tf->Close(); delete tf; SetTFile(0);
  }
  SafeDelete(fchainOpt);
  fBFC = 0;
//  delete gMessMgr; gMessMgr = 0;
  return ians;
}


//_____________________________________________________________________
Int_t StBFChain::AddAB (const Char_t *mkname,const StMaker *maker,const Int_t Opt) {
  if (! maker || strlen(mkname) == 0) return kStErr;
  StMaker *parent = maker->GetParentMaker();
  if (parent) {
    TList   *list    = parent->GetMakeList();
    list->Remove((StMaker *)maker);
  }
  StMaker *mk      = GetMaker(mkname);      if (!mk)     return kStErr;
  parent  = mk->GetParentMaker();  if (!parent) return kStErr;
  TList   *list    = parent->GetMakeList(); if (!list)   return kStErr;
  if (Opt > 0) list->AddAfter (mk,(StMaker*)maker);
  else         list->AddBefore(mk,(StMaker*)maker);
  return kStOk;
}

//_____________________________________________________________________
Int_t StBFChain::ParseString (const TString &tChain, TObjArray &Opt, Bool_t Sort) {
  Opt.Clear();
  TObjArray *obj = tChain.Tokenize("[^ ;,]");
  Int_t nParsed = obj->GetEntries();
  Int_t k, N = 0;
  if (GetChain() && GetChain()->Debug() > 2) {
    gMessMgr->QAInfo() << "ParseString " << tChain.Data() << endm;
    for (k = 0; k < nParsed; k++) {
      if (obj->At(k)) {
	gMessMgr->QAInfo() << k << "\t" << ((TObjString *) obj->At(k))->GetName() << endm;
      }
    }
  }
  for (k = 0; k < nParsed; k++) {if (obj->At(k)) {if (k != N) obj->AddAt(obj->At(k),N); N++;}}
  nParsed = N;
  // sort options
  StBFChain *chain = (StBFChain *) StMaker::GetChain();
  if (chain && Sort) {// sort options
    TArrayI idT(nParsed); Int_t *idx = idT.GetArray();
    TArrayI kdT(nParsed); Int_t *kdx = kdT.GetArray();
    for (k = 0; k < nParsed; k++) {
      TString string = ((TObjString *) obj->At(k))->GetString();
      kdx[k] = TMath::Abs(chain->kOpt(string,kFALSE));
    }
    TMath::Sort(nParsed,kdx,idx,0);
    TString sChain;
    for (k = 0; k < nParsed; k++) {
      Opt.AddAtAndExpand(obj->At(idx[k]),k);
      if (k == 0) sChain = ((TObjString *)Opt[k])->GetString();
      else {sChain += ","; sChain += ((TObjString *)Opt[k])->GetString();}
    }
    if (N > 1 && chain->Debug() > 2) {
      gMessMgr->QAInfo() << "Requested chain is :\t" << tChain.Data() << endm;
      gMessMgr->QAInfo() << "Sorted    chain is :\t" << sChain.Data() << endm;
    }
  } else for (k = 0; k < nParsed; k++) Opt.AddAtAndExpand(obj->At(k),k);
  obj->SetOwner(kFALSE);
  delete obj;
  return nParsed;
}

//_____________________________________________________________________
/// Check option if defined (Char_t argument interface)
Int_t StBFChain::kOpt (const Char_t *tag, Bool_t Check) const {
  TString Tag(tag);
  Int_t kO = kOpt(&Tag, Check);
  return kO;
}
//_____________________________________________________________________
/// Check option if defined.
/*!
  This method checks if the options are valid by %comparing them
  to the list of declared options. This is called for each option
  passed as argument. The real sorting of all options is done in
  SetFlags().
*/
Int_t StBFChain::kOpt (const TString *tag, Bool_t Check) const {
  TString Tag = *tag;
  Tag.ToLower();
  TString opt, nopt;
  for (Int_t i = 1; i< fNoChainOptions; i++) {
    opt = TString(fBFC[i].Key); //check nick name
    opt.ToLower();
    nopt = TString("-");
    nopt += opt;
    if       (Tag ==  opt) {return  i;}
    else {if (Tag == nopt) {return -i;}}
    opt   = fBFC[i].Maker; //check full maker name2
    nopt  = "-";
    nopt += opt;
    if       (Tag ==  opt) {return  i;}
    else {if (Tag == nopt) {return -i;}}
  }
  //
  // JL - sdt and dbv for timestamp
  //
  // Gopt for arbitrary property on 3 letter name (wildcard would be added) and length
  // 6 for a value. Not advertized / not used and implementation is not complete (needed
  // a case and di not have a clear one). TBD.
  //
  // 2011/11 added the possibility of detector sub-system specific timestamps.
  // DBV only for now, logic is similar if we equally parse.
  //
  // {sdt|dbv}YYYYMMDD -> {sdt|dbv} 3 / YYYYMMDD 8 => 11 || YYYYMMDD.HHMMSS = 15 => 18
  if (Tag.BeginsWith("dbv") || Tag.BeginsWith("sdt")) {
    Check = kTRUE;

    if (Tag.Length() == 11  || Tag.Length() == 18) return 0;

    // Else we need to parse some more - assume a pattern {dbv|sdt}YYYYMMDD[.HHMMSS]_XXX_ZZZZZ
    // First, detect it using quick counting 
    Tag.ToLower();
    if ( TPRegexp(BFC_DBREGEXP).Match(Tag)  == 7) return 0;
  }
  if (Tag.BeginsWith("rung")) {
    Check = kTRUE;
    return 0;
  }
  // GoptXXXvvvvvv -> Gopt 4 / XXX 3 / vvvvvv 6 = 13
  if ( Tag.BeginsWith("gopt") && Tag.Length() == 13 ) return 0;

  if ( Tag.BeginsWith("FiltTrg",TString::kIgnoreCase) ) {
    Check = kTRUE;
    if ( TPRegexp("^FiltTrg(Inc|Exc)?(_.*)*$").Match(Tag,"i") > 0) return 0;
  }
  // Check possible Generator files
  if (Check) {
    gMessMgr->Error() << "Option " << Tag.Data() << " has not been recognized" << endm;
    assert(0);
  }
  return 0;
}
//_____________________________________________________________________
void StBFChain::SetOptions(const Char_t *options, const Char_t *chain) {
  TString tChain(options);
  TObjArray Opts;
  Int_t nParsed = ParseString(tChain,Opts,kTRUE);
  for (Int_t l = 0; l < nParsed; l++) {
    TString TagC = ((TObjString *)Opts[l])->GetString();
    TString Tag(TagC);
    Tag.ToLower();
    Int_t kgo;
    Int_t in = Tag.Index("=");
    if (in > 0) {// string with  "="
      TString subTag(Tag.Data(),in);
      kgo = kOpt(subTag.Data());
      if (kgo > 0) {
	int len= sizeof(fBFC[kgo].Comment);
	memset(fBFC[kgo].Comment,0,len); // be careful size of Comment
	TString Comment(Tag.Data()+in+1,Tag.Capacity()-in-1);
	if ( Comment.Length() <= len ){
	  strncpy (fBFC[kgo].Comment, Comment.Data(),sizeof(fBFC[kgo].Comment));
	  gMessMgr->QAInfo() << Form(" Set        %s = %s", fBFC[kgo].Key,fBFC[kgo].Comment) << endm;
	} else {
	  gMessMgr->Error()  << Form(" Cpy problem [%s] is > %d - adjust BFC Comment field size", 
				     Comment.Data(),len) << endm;
	}
      }
    } else {
      // printf ("Chain %s\n",tChain.Data());
      kgo = kOpt(Tag.Data(),kFALSE);
      if (kgo != 0) {
	SetOption(kgo,chain);
	if (kgo > 0) {
	  TString Comment(fBFC[kgo].Comment);
	  TString Opts(fBFC[kgo].Opts);
	  if (Tag.BeginsWith("Test.",TString::kIgnoreCase) && ! Comment.BeginsWith("/star/") && 
	      Opts.BeginsWith("test_",TString::kIgnoreCase)) {
	    SetOptions(Opts,Tag);
	  } else {
	    if ((Tag.BeginsWith("Test.",TString::kIgnoreCase) ||
		 Tag.BeginsWith("test_",TString::kIgnoreCase) ||
		 Tag.BeginsWith("eval_",TString::kIgnoreCase)) && Comment.BeginsWith("/star/") ) {
	      fkChain = kgo;
	      gMessMgr->QAInfo() << "Default Test chain set " << fBFC[fkChain].Key << " with input " << fBFC[fkChain].Comment << endm;
	    } 
	  }
	}
	continue;
      }
      // it is 0 i.e. was not recognized. 
      // Check if it is a (dbv|sdt)YYYYMMDD or (dbv|sdt)YYYYMMDD.HHMMSS and derivative
      // We really set the options only once later in SetDbOptions() (removing the fBFC[i].Flag check) 
      // but the goal here is to avoid user's histeria by displaying extra messages NOW.
      //
      // Note that kOpt() has already validated the pattern (so it has to be right here).
      //
      // !!! Debug: dbv20040917
      if (Tag.BeginsWith("dbv")) {
	if (Tag.Length() == 11)  (void) sscanf(Tag.Data(),"dbv%8d",&FDate);
	if (Tag.Length() == 18)  (void) sscanf(Tag.Data(),"dbv%8d.%6d",&FDate,&FTime);
	if (Tag.Length() == 11 || Tag.Length() == 18) {
	  gMessMgr->QAInfo() << Tag.Data() << " ... but still will be considered as a dynamic timestamp (Max DB EntryTime) "
			     << FDate  << "." << FTime << endm;
#ifdef USE_BFCTIMESTAMP
	} else {
	  // we passed kOpt() parsing was fine
	  //if ( TPRegexp(BFC_DBREGEXP).Match(Tag)  == 7) return 0;
	  TObjArray *subStrL = TPRegexp(BFC_DBREGEXP).MatchS(Tag);
	  BFCTimeStamp TS;
	  TString realm;
	  
	  TS.Type     = 1;
	  TS.Date     = (((TObjString *) subStrL->At(2))->GetString()).Atoi();
	  TS.Time     = 0; // for now, avoid parsing this as user use simple TS 99% of the time
	  TS.Detector = ((TObjString *) subStrL->At(4))->GetString();
	  TS.Realm    = ((TObjString *) subStrL->At(6))->GetString();
	  
	  if ( TS.Realm.IsNull() ){ realm = "*";}
	  else {                    realm = TS.Realm;}
	  
	  GTSOptions.push_back(TS);
	  
	  LOG_WARN << "Override timestamp for detector requested\n\t" 
		   << "Detector " << TS.Detector  << "\n\t"
		   << "Realm    " << realm        << "\n\t"
		   << "Date     " << TS.Date      << "\n\t"
		   << "Time     " << TS.Time      << endm;
#endif /*  USE_BFCTIMESTAMP */
	}
	continue;
      }
      if (Tag.BeginsWith("sdt")) {
	if (Tag.Length() == 11)  (void) sscanf(Tag.Data(),"sdt%8d",&FDateS);
	if (Tag.Length() == 18)  (void) sscanf(Tag.Data(),"sdt%8d.%6d",&FDateS,&FTimeS);
	if (Tag.Length() == 11 || Tag.Length() == 18) {
	  gMessMgr->QAInfo() << Tag.Data() << " ... but still will be considered as a dynamic timestamp (Event Time) "
			     << FDateS  << "." << FTimeS << endm;
	  
	  // <<< same logic for GTSOptions can be inserted here
	  // <<< if so, use TS.Type     = 2
	  continue;
	}
      }  
      if ( Tag.BeginsWith("gopt") && Tag.Length() == 13){
	char GOptName[4],GOptValue[7];
	//TString property(".gopt.");
	//TString pattern("*");
	
	(void) sscanf(Tag.Data(),"gopt%3s%6s",GOptName,GOptValue);
	// sscanf adds null terminators for %s, so buffers need to be 1 longer
	
	// see StBFChain::Setup() for default values
	Gproperty += GOptName;
	
	// JL - this is not finished, see comment in kOpt()
	
	// pattern is case sensitive, need more checks on this before
	// setting to something else than "*"
	//Gpattern  += GOptName;
	//Gpattern  += "*";
	Gvalue = GOptValue;
	
	gMessMgr->QAInfo() << Tag.Data() << " ... this will set an general attribute "
			   << Gproperty.Data() << " with value " << GOptValue << " to "
			   << Gpattern.Data() << endm;
	// Attr() need to be set after the maker exist
	//SetAttr(property.Data(),GOptValue,pattern.Data());
	//SetAttr(property.Data(),GOptValue,pattern.Data());
	continue;
      } 
      if (Tag.BeginsWith("rung")) {
	fRunG = 1;
	if (Tag.Length() > 4)  (void) sscanf(Tag.Data(),"rung.%d",&fRunG);
	gMessMgr->QAInfo() << Tag.Data() << " will be considered as Run number (& rndm seed set) " 
			   << fRunG << " for simulation." << endm; 
	continue;
      }
      if (Tag.BeginsWith("FiltTrg",TString::kIgnoreCase)) {
	TString filtTrgTag = Tag;
	Ssiz_t flavorIdx = Tag.Index('_');
	if (flavorIdx > 0) {
	  filtTrgTag = Tag(0,flavorIdx);
	  fFiltTrg = Tag(flavorIdx+1,64);
	}
	if (filtTrgTag.CompareTo("FiltTrgExc",TString::kIgnoreCase)==0) {
	  gMessMgr->QAInfo() << "Trigger Filtering exclude with flavor=" << fFiltTrg << endm;
	  fFiltTrg.Prepend('-');
	} else if (filtTrgTag.CompareTo("FiltTrgInc",TString::kIgnoreCase)==0 ||
		   filtTrgTag.CompareTo("FiltTrg"   ,TString::kIgnoreCase)==0) {
	  gMessMgr->QAInfo() << "Trigger Filtering include with flavor=" << fFiltTrg << endm;
	  fFiltTrg.Prepend('+');
	} else {
	  // not a match, disable
	  fFiltTrg = "";
	}
	continue;
      } 
      // Check for predefined db time stamps ?
      kgo = kOpt(Tag.Data(),kFALSE);
      if (kgo != 0){
	SetOption(kgo,chain);
	continue;
      }
      {
	// Check that option can be library name or / and Maker
	static const Char_t *path = ".:.$STAR_HOST_SYS/lib::.$STAR_HOST_SYS/LIB:$STAR/.$STAR_HOST_SYS/lib:$STAR/.$STAR_HOST_SYS/LIB";
	TString File = Tag; File += ".so";
	Char_t *file = gSystem->Which(path,File.Data(),kReadPermission);
	if (file) {
	  TString Maker("");
	  Bfc_st row = {"","","","","","","",kTRUE};
	  memcpy (&row.Key, Tag.Data(), Tag.Length());
	  if (Tag.Contains("Maker")) memcpy (&row.Maker, Tag.Data(), Tag.Length());
	  memcpy (&row.Libs, Tag.Data(), Tag.Length());
	  fchainOpt->AddAt(&row);
	  fNoChainOptions = fchainOpt->GetNRows();
	  fBFC = fchainOpt->GetTable();
	  delete [] file;
	}
	kgo = kOpt(Tag.Data(),kFALSE);
	if (kgo != 0) {
	  SetOption(kgo,chain);
	  continue;
	} 
      }
      {
	static const Char_t *path  = ".:./StarDb/Generators:$STAR/StarDb/Generators";
	TString CintF = TagC;
	CintF += ".C";
	Char_t *file = gSystem->Which(path,CintF,kReadPermission);
	if (file) {
	  Warning("StBFChain::Init","File %s has been found as %s",CintF.Data(),file);
	  SetAttr("GeneratorFile",file);
	  delete [] file;
	  continue;
	}
      }
      gMessMgr->QAInfo() << " Invalid Option " << Tag.Data() << ". !! ABORT !! " << endm;
      assert(0);
      return;
    }
  }
  Opts.Delete();
}
//_____________________________________________________________________
/// Enable/disable valid command line options
void StBFChain::SetOption(const Int_t k, const Char_t *chain) {
  if (k > 0) {
    assert(k<fNoChainOptions);
    Int_t n = strlen(fBFC[k].Opts);
    if (n >  0) SetOptions(fBFC[k].Opts,fBFC[k].Key);
    if (!fBFC[k].Flag) {
      fBFC[k].Flag = kTRUE;
      gMessMgr->QAInfo() << Form(" Switch On  %20s by %s", fBFC[k].Key, chain) << endm;
    }
  } else {
    assert(-k<fNoChainOptions);
    if (k < 0 && fBFC[-k].Flag) {
      fBFC[-k].Flag = kFALSE;
      gMessMgr->QAInfo() << Form(" Switch Off %20s by %s", fBFC[-k].Key, chain) << endm;
    }
  }
}

//_____________________________________________________________________
/// Returns chain-option state (on/off)
Bool_t StBFChain::GetOption(const Int_t k) const
{
  return (k>0 && k <fNoChainOptions) ? fBFC[k].Flag : kFALSE;
}

/// Returns the comment string associated to an option
/*!
 * Any option passed a bla=XX is reshaped as follow ...
 * - The SetFlags() function strip out the =XX part and replaces
 *   the comment by the value XX
 * - This GetOptionString() returns the comment part so makers
 *   can globally access the option string.
 *
 * <i>Note</i> : If the requested option is not part of the global BFC[]
 * array, the kOpt() method is going to scream at you but it will still
 * work. You can ask for that option to be added to the chain official
 * options later whenever your code debugging is done. In other words,
 * this method allows you to pass ANY options not officially declared
 * and use it as test/work-around to pass any parameters to your maker.
 *
 * However, if the parameters are to be used in production, we DO
 * request/require that they are declared as a valid option.
 *
 *
 */
Char_t *StBFChain::GetOptionString(const Char_t *Opt)
{
  Int_t o = kOpt(Opt);
  if(!o) return NULL;
  else if(!GetOption(o)) return NULL;
  else return(fBFC[o].Comment);
}


//_____________________________________________________________________________
/// Scan all flags, check if they are correct, manipulate the comment if necessary
/*!
  This method checks if the options passed are correct and/or sorts out
  the extraneous information passed through the comment if applies. Two
  special flags exists, that is, dbv and sdt for database interraction ...
  - The <tt>dbv</tt> tag is used to setup the end-time for entries (time after
  which any database insertions will be ignored). This is used to provide a
  mechanism by which we can run production with a stable set of calibration
  constant and still allow for development value insertion to get in.
  - The <tt>sdt</tt> tag is used to setup the database look-up time stamp
  that is, on which date to look at the database. This is usefull if we
  want to decouple Geant geometry and database calibration constants.
  This may also be used to get database values when you do not run over
  a raw data file.
  - The geometry options are now dynamic. This includes BOTH Y(ear) based
  geometries and RY geometry (Real data) options. They are defined now in
  StMaker as a static map.

  The <tt>GOptXXXvvvvvv</tt> options are used to pass an flexible option
  value <tt>vvvvvv</tt> to makers in the chain matching the name <tt>XXX</tt>.
  The three letter acronym will be following the detector sub-systems naming.
  If a maker uses such convenience, the basic rule applies:
  - the Maker MUST use a centralized conversion function of <tt>vvvvvv</tt>
  to a private structure with explicit naming of each switches
  - explaination on usage and the thourough explaination of the meaning
  of the values <tt>vvvvvv</tt> must be clearly added to the documentation

*/
void StBFChain::SetFlags(const Char_t *Chain)
{
  TString tChain(Chain);
  Int_t mode = 1;
  Setup(mode);
  Int_t k=0;
  if (tChain == "" || tChain.CompareTo("ittf",TString::kIgnoreCase) == 0) {
    gMessMgr->QAInfo() << "\tPossible Chain Options are:" << endm;
    for (k=0;k<fNoChainOptions;k++)
    gMessMgr->QAInfo()
      << Form(" %4d: %-30s:%-12s:%-12s:%s:%s:%s:%s"
	      ,k,fBFC[k].Key,fBFC[k].Name,fBFC[k].Chain,fBFC[k].Opts,fBFC[k].Maker,fBFC[k].Libs,fBFC[k].Comment)
      << endm;

    return;
  }
  TString STAR_VERSION("$STAR_VERSION");
  gSystem->ExpandPathName(STAR_VERSION);
  gMessMgr->QAInfo() << "=============================================="  << endm;
  gMessMgr->QAInfo() << "============= You are in " << STAR_VERSION.Data() << " ===============" << endm;
  gMessMgr->QAInfo() << "Requested chain " << GetName() << " is :\t" << tChain.Data() << endm;
  if (tChain == "") return;
  SetOptions(tChain,"Chain");
  if (GetOption("NewTpcAlignment")) {
    gMessMgr->QAInfo() << "Set environment to use NewTpcAlignment" << endm;
    gEnv->SetValue("NewTpcAlignment",1);
  }
  if (!GetOption("NoDefault")) { // Default
    // Check flags consistency
    if (gClassTable->GetID("TGiant3") >= 0) { // root4star
      SetOption("-VMC","Default,TGiant3");
      SetOption("-VMCPassive","Default,TGiant3");
      SetOption("-VMCAppl","Default,TGiant3");
      SetOption("-RootVMC","Default,TGiant3");
#if 1 /* Not Active geant is not needed any more, except BTofUtil */
      if (!( GetOption("fzin")   || GetOption("fzinSDT")        ||
	     GetOption("ntin")   || 
	     GetOption("gstar" ) || 
	     GetOption("pythia") || 
	     GetOption("PrepEmbed"))) {// Not Active geant
	SetOption("geant","Default,TGiant3");
	SetOption("MagF","Default,TGiant3");
      }
#endif
      if (GetOption("xgeometry")) {
	SetOption("-geometry","Default,-xgeometry");
	SetOption("-geomNoField","Default,-xgeometry");
      }
      if (GetOption("fzin") &&  FDateS) {
	SetOption("-fzin","Default,SDT");
	SetOption("fzinSDT","Default,SDT");
      }
      if (GetOption("GenIn")) SetOption("AgStar","GenIn,TGiant3");
    } else {                                  // root
      if (GetOption("fzin") || GetOption("fzinSDT")) {
	gMessMgr->Error() << "Option fzin cannot be used in root.exe. Use root4star" << endm;
	abort();
      }
      if (GetOption("ntin")) {
	gMessMgr->Error() << "Option ntin cannot be used in root.exe. Use root4star" << endm;
	abort();
      }
      if (! (GetOption("Stv"))) {
	if (GetOption("gstar") || GetOption("pythia")) {
	  SetOption("VMC","Default,-TGiant3,gstar");
	  SetOption("-gstar","Default,-TGiant3");
	  SetOption("-pythia","Default,-TGiant3");
	}
      }
      SetOption("-geant","Default,-TGiant3");
      SetOption("-geantL","Default,-TGiant3");
      SetOption("-geometry","Default,-TGiant3");
      SetOption("-geomNoField","Default,-TGiant3");
      SetOption("-UseXgeom","Default,-TGiant3");
      if (! (GetOption("Stv"))) {
	if (! (GetOption("VMC") || GetOption("VMCPassive"))) {
	  SetOption("VMCPassive","Default,-TGiant3");
	}
	SetOption("pgf77","Default,-TGiant3");
	SetOption("mysql","Default,-TGiant3");
	SetOption("minicern","Default,-TGiant3");
      }
    }
    if (GetOption("ITTF") && ! (GetOption("Sti") || GetOption("StiCA")  || GetOption("Stv") || GetOption("StiVMC"))) {
      TString STAR_LEVEL(gSystem->Getenv("STAR_LEVEL"));
      if (STAR_LEVEL == ".DEV2")  SetOption("StiCA","Default,ITTF");
      else                        SetOption("Sti"  ,"Default,ITTF");
    }  
    if (GetOption("Stv")) {
      SetOption("-TpcIT","Default,Stv");
      SetOption("-SvtIT","Default,Stv");
      SetOption("-SsdIT","Default,Stv");
      SetOption("-HpdIT","Default,Stv");
      SetOption("-BTofIT","Default,Stv");
      SetOption("-PxlIT","Default,Stv");
      SetOption("-IstIT","Default,Stv");
    }  
#if 0
    if (TString(SAttr("GeneratorFile")) != "") {
      SetOption("geant","GeneratorFile");
      SetOption("-fzin","GeneratorFile");
      SetOption("-fzinSDT","GeneratorFile");
    }
#endif
  }
  if (! GetOption("AgML") && ! GetOption("VmcGeo") && ! GetOption("Agi")) SetOption("Agi","Default Geometry");
  if (!GetOption("Eval") && GetOption("AllEvent"))  SetOption("Eval","-Eval,AllEvent");
  // Print set values
  St_Bfc *Bfc = new St_Bfc("BFChain",fNoChainOptions);
  AddRunco(Bfc);
  for (k = 1; k<fNoChainOptions;k++) {
    if (GetOption(k)) {
      gMessMgr->QAInfo() << Form("==================  %20s\tis ON \t: %s",
				 (char *) fBFC[k].Key, (char *) fBFC[k].Comment) << endm;
      Bfc->AddAt(&fBFC[k]);
    }
  }
  //  gSystem->Exit(1);
}
//_____________________________________________________________________
void StBFChain::Set_IO_Files (const Char_t *infile, const Char_t *outfile){
  TString gc("");
  if (infile) {
    if (strlen(infile) > 2) {
      gc = TString(infile,3);
      gc.ToLower();
    }
  }
  SetInputFile(infile);
  if (! GetOption("NoOutput")) SetOutputFile(outfile);
}
//_____________________________________________________________________
void StBFChain::SetInputFile (const Char_t *infile){
  // define input file
  if (infile) fInFile = infile;
  if (fInFile != "") {
    fInFile.ReplaceAll("\n",";");
    fInFile.ReplaceAll("#",";");
//     fInFile.ReplaceAll(":",";");
    gMessMgr->QAInfo() << "Input file name = " << fInFile.Data() << endm;
  } else {
    if (fkChain >= 0) {
      fInFile = fBFC[fkChain].Comment;
      fInFile.ReplaceAll("\n",";");
      if (gSystem->AccessPathName(fInFile.Data())) fInFile = fInFile.Prepend("/direct");
      gMessMgr->QAInfo() << "Default Input file name = " << fInFile.Data() << " for chain : " << fBFC[fkChain].Key << endm;
    }
  }
  if (fInFile == "") {SetOption("-in","No Input File"); SetOption("-InTree","NoInput File"); return;}
  if (!GetOption("fzin") && !GetOption("fzinSDT") &&!GetOption("ntin")) {
    fSetFiles= new StFile();
    TObjArray Files;
    ParseString(fInFile,Files);
    TIter next(&Files);
    TObjString *File;
    while ((File = (TObjString *) next())) {
      TString string = File->GetString();
      if (!string.Contains("*") && ! string.BeginsWith("@") &&
	  gSystem->AccessPathName(string.Data())) {// file does not exist
	gMessMgr->Error() << "StBFChain::SetInputFile  *** NO FILE: " << string.Data() << ", exit!" << endm;
	gSystem->Exit(1);
      }
      else fSetFiles->AddFile(File->String().Data());
    }
    Files.Delete();
  }
}
//_____________________________________________________________________
/// Takes care of output file name (extension)
void StBFChain::SetOutputFile (const Char_t *outfile){
  if (!  GetOption("NoOutput")) {
    if (outfile) { 
      fFileOut = outfile;
    }
    if ((fFileOut == "") & (fInFile != "")) {
      if (GetOption("fzin") || GetOption("fzinSDT") ||GetOption("ntin")) {
	TObjArray words;
	ParseString(fInFile,words);
	TIter nextL(&words);
	TObjString *word = 0;
	while ((word = (TObjString *) nextL())) {
	  if (word->GetString().Contains(".fz") ||
	      word->GetString().Contains(".nt")) {
	    fFileOut = gSystem->BaseName(word->GetName());
	    break;
	  }
	}
      } else {
	fFileOut = gSystem->BaseName(fInFile.Data());
      }
    } 
    if (fFileOut == "") {
      if      (GetOption("pythia")) fFileOut = "pythia.root";
      else if (GetOption("hijing")) fFileOut = "hijing.root";
      else if (GetOption("VMC"))    fFileOut = "VMC.root";
      else if (GetOption("gstar"))  fFileOut = "gtrack.root";
    }
    if (  fFileOut != "") {
      fFileOut.ReplaceAll("*","");
      fFileOut.ReplaceAll("@","");
      fFileOut.ReplaceAll("..",".");
      fFileOut.ReplaceAll(".daq","");
      fFileOut.ReplaceAll(".fzd","");
      fFileOut.ReplaceAll(".fz","");
      fFileOut.ReplaceAll(".nt","");
      fFileOut.ReplaceAll(".root","");
      fFileOut.ReplaceAll(".list","");
      fFileOut.ReplaceAll(".lis","");
      fFileOut.Strip();
      fFileOut.Append(".root");
    }
  }
  if (fFileOut != "")  gMessMgr->QAInfo() << "Output root file name " <<  fFileOut.Data() << endm;
  else                 SetOption("NoOutput","No Output File");
  if (!GetTFile()) {
    if ( ( GetOption("tags")  || GetOption("lana") ) &&  (fFileOut != "") ){
      TString TagsName = fFileOut;
      if( GetOption("lana") ){
	TagsName.ReplaceAll(".root",".laser.root");
      } else if ( GetOption("mtdEvtFilt") ){
	TagsName.ReplaceAll(".root",".pretags.root");
      } else {
	TagsName.ReplaceAll(".root",".tags.root");
      }
      SetTFile(new TFile(TagsName.Data(),"RECREATE"));
    }
  }
}
//_____________________________________________________________________
/// Handles all geant options
/*!
  This method sets the Geant options that is the Geometry loading
  part. Depends on St_geant_Maker instantiated in the Instantiate()
  method.
  Please, change SetDbOptions()
*/
void StBFChain::SetGeantOptions(StMaker *geantMk){
  if (! geantMk || ! geantMk->InheritsFrom("St_geant_Maker")) return;
  SetInput("geant",".make/geant/.data");
  TString GeomVersion("");
#if 0
  if (! (GetOption("fzin") || GetOption("fzinSDT") |! GetOption("ForceGeometry")) || TString(SAttr("GeneratorFile")) != "") {
    GeomVersion = "y2004x";
    const DbAlias_t *DbAlias = GetDbAliases();
    Int_t found = 0;
    for (Int_t i = 0; DbAlias[i].tag; i++) {
      TString r("r");
      r +=  DbAlias[i].tag;
      if ( !GetOption(DbAlias[i].tag,kFALSE) && !GetOption(r,kFALSE)) continue;
      GeomVersion = DbAlias[i].geometry;
      found = i; 
      break;
    }
    if (! found) gMessMgr->QAInfo() << "StBFChain::SetGeantOptions() Chain has not found geometry tag. Use " << GeomVersion << endm;
    TString GeometryOpt;
    if (GetOption("phys_off")) {GeometryOpt += "detp phys_off=1;"; geantMk->SetAttr("phys_off",1);}
    if (GetOption("hadr_off")) {GeometryOpt += "detp hadr_off=1;"; geantMk->SetAttr("hadr_off",1);}
    GeometryOpt += ("detp geom ");
    GeometryOpt += GeomVersion;
    ProcessLine(Form("((St_geant_Maker *) %p)->LoadGeometry(\"%s\");",geantMk,GeometryOpt.Data()));
  }
#else
  if (! GetOption("fzin") && !  GetOption("fzinSDT")) {
    if (GetOption("phys_off")) {geantMk->SetAttr("phys_off",1);}
    if (GetOption("hadr_off")) {geantMk->SetAttr("hadr_off",1);}
  }
#endif      
  if ((GetOption("fzin") || GetOption("fzinSDT") ||GetOption("ntin") || GetOption("mtin") || fInFile.Data()[0] == ';') && fInFile != "") {
    ProcessLine(Form("((St_geant_Maker *) %p)->SetInputFile(\"%s\")",geantMk,fInFile.Data()));
  }
}
//_____________________________________________________________________
/// Treats the DbV options used for database timestamp.
/*!
  Re-scan all options and search for dbv options. This method also sorts
  out the string-based database timestamp for reconstruction. Those have
  to be in phase with the geant geometry (see SetGeantOptions()) if
  simulation is being reconstructed.

  The order matters since a later option would overwrite an earlier one.
  The mechanism introduced for a dynamic (i.e. not pre-defined) timestamp is that
  it will be used ONLY if there are no other timestamp options.
  <b>Be aware of this precedence ...</b>

*/
void StBFChain::SetDbOptions(StMaker *mk){
  if (! mk ) return;
  if      (GetOption("Agi"))    mk->SetAlias("VmcGeometry","db/.const/StarDb/AgiGeometry");
  else if (GetOption("AgML")  ) {
#ifndef __AgMLonFly__
    // Requires root-files generated
    Int_t ok = -1;
    for (Int_t i = 0; i < 10; i++) {
      if (Dirs[i] == "") continue;
      TString ddir = Dirs[i]; ddir += "/"; ddir = "AgMLGeometry/*.root";
      TDirIter iter(ddir);
      if (iter.NoFiles()) {
	mk->SetAlias("VmcGeometry","db/.const/StarDb/AgMLGeometry");
	ok = i;
	break;
      }
    }
    if (ok == -1) mk->SetAlias("VmcGeometry","db/.const/StarDb/AgiGeometry");
#else /* __AgMLonFly__ */
    mk->SetAlias("VmcGeometry","db/.const/StarDb/AgMLGeometry");
#endif
  }
  else if (GetOption("VmcGeo")) mk->SetAlias("VmcGeometry","db/.const/StarDb/VmcGeo");
  else                          mk->SetAlias("VmcGeometry","db/.const/StarDb/AgiGeometry");
  Int_t i;
  Int_t Idate=0,Itime=0;

  // First possibility
  for (i = 1; i < fNoChainOptions; i++) {
    if (fBFC[i].Flag && !strncmp(fBFC[i].Key ,"DbV",3)){
      // JL - we use to set timestamp as a chain option (any) starting with dbv and followed
      // by an arbitrary set of numbers. The real stamp was taken from the comment.
      // This supports this old mode.
      gMessMgr->QAInfo() << "StBFChain::SetDbOptions  Found time-stamp " << fBFC[i].Key << " [" << fBFC[i].Comment << "]" << endm;
      (void) sscanf(fBFC[i].Comment,"%d/%d",&Idate,&Itime);
    }
  }

  // If FDate is set and we do not have the old mode, then a dynamic timestamp was used
  // Overwrite
  if( ! Idate && FDate){
    gMessMgr->QAInfo() << "StBFChain::SetDbOptions Switching to user chosen dynamic time-stamp (MaxEntry) "
			 << FDate << " " << FTime << endm;
    gMessMgr->QAInfo() << "Chain may crash if time-stamp is not validated by db interface" << endm;

    Idate = FDate;
    Itime = FTime;
  }

  St_db_Maker *db = (St_db_Maker *) mk;
  // Startup date over-write
  if (FDateS){
    gMessMgr->QAInfo() << "StBFChain::SetDbOptions Switching to user chosen dynamic time-stamp (Start)"
		       << FDateS << " " << FTimeS << endm;
    gMessMgr->QAInfo() << "Chain may crash if time-stamp is not validated by db interface" << endm;

    db->SetDateTime(FDateS,FTimeS);
  } else {
    if (GetOption("simu") || ! NoMakersWithInput) {
      const DbAlias_t *DbAlias = GetDbAliases();
      Int_t found = 0;
      for (Int_t i = 0; DbAlias[i].tag; i++) {
	for (Int_t r = 0; r < 2; r++) {
	  TString dbTag("");
	  if (r) dbTag += "r";
	  dbTag += DbAlias[i].tag;
	  if (GetOption(dbTag,kFALSE)) {
	    db->SetDateTime(DbAlias[i].tag);
	    found = i;
	    break;
	  }
	  if (found) break;
	}
      }
      if (! found) {gMessMgr->QAInfo() << "StBFChain::SetDbOptions() Chain has not set a time-stamp" << endm;}
      // Show date settings
      gMessMgr->QAInfo() << db->GetName()
			 << " Maker set time = "
			 << db->GetDateTime().GetDate() << "."
			 << db->GetDateTime().GetTime() << endm;
      if (GetOption("SIMU") && m_EvtHddr) {
	gMessMgr->QAInfo() << GetName() << " Chain set time from  " << db->GetName() << endm;
	m_EvtHddr->SetDateTime(db->GetDateTime());
      }
    }
  }

  // MaxEntry over-write - default and global for all realm and detectors
  if (Idate) {
    db->SetMaxEntryTime(Idate,Itime);
    gMessMgr->Info() << "\tSet DataBase max entry time " << Idate << "/" << Itime
		     << " for St_db_Maker(\"" << db->GetName() <<"\")" << endm;
  } 
#ifdef USE_BFCTIMESTAMP
  //
  // Now treat the detector specific options
  //
  TString realm;
  for (UInt_t i = 0; i < GTSOptions.size() ; i++){
    if ( (GTSOptions[i].Realm).IsNull() ){ realm = "*";}
    else {                                 realm = GTSOptions[i].Realm;}

    //LOG_INFO << "DEBUG MORE [" << (GTSOptions[i].Realm).Data() << "]" << endm;
    //LOG_INFO << "DEBUG MORE [" << realm.Data() << "]" << endm;

    if ( GTSOptions[i].Type == 1){
      db->AddMaxEntryTimeOverride(GTSOptions[i].Date,0,
				  (char *) realm.Data(),
				  (char *) GTSOptions[i].Detector.Data());

      LOG_INFO << "Recovering override stamp " << i << " :: " 
	       << GTSOptions[i].Detector << ", " << realm << ", "  
	       << GTSOptions[i].Date     << ", " << GTSOptions[i].Time  << endm;
    } else {
      LOG_WARN << "Found override type " << GTSOptions[i].Type << " no treated yet" 
	       << GTSOptions[i].Detector << ", " << realm << ", "  
	       << GTSOptions[i].Date     << ", " << GTSOptions[i].Time  << endm;
    }
  }

  //abort();
#endif /*  USE_BFCTIMESTAMP */

  if (!GetOption("fzin") && !GetOption("fzinSDT")) {
    struct Field_t {
      const Char_t *name;
      Float_t scale;
    };
    Field_t FieldOptions[5] = {
      {"FullMagFNegative", -1.0},
      {"FullMagFPositive",  1.0},
      {"HalfMagFNegative", -0.5},
      {"HalfMagFPositive",  0.5},
      {"ZeroMagF",          0.0}
    };
    Int_t k = -1;
    if         (GetOption("FieldON")) {
      if       (GetOption("ReverseField"))  k = 0;
      else                                  k = 1;
    } else if (GetOption("HalfField")) {
      if      (GetOption("ReverseField"))   k = 2;
      else                                  k = 3;
    } else if (GetOption("FieldOff"))       k = 4;
    if (k >= 0) {
      SetFlavor(FieldOptions[k].name,        "MagFactor");
      gMessMgr->QAInfo() << "StBFChain::SetDbOptions SetFlavor(\"" << FieldOptions[k].name
			 << "\",\"MagFactor\")" << endm;
      if ( gClassTable->GetID("StarMagField") >= 0) {
	TString cmd =
	  Form("if (!StarMagField::Instance()) new StarMagField( 2, %f, kTRUE);",
	       FieldOptions[k].scale);
	ProcessLine(cmd);
      }
    }
  }
  if (this == GetTopChain()) {
    // Db blacklist (remove black listed system from St_Db_Maker Calibrations configuration)
    if (! GetOption("TpcDb")                        ) {mk->SetAttr("blacklist", "tpc");  gMessMgr->QAInfo() << "blacklist tpc" << endm;}
    if (!(GetOption("SvtDb")||GetOption("SvtCalDb"))) {mk->SetAttr("blacklist", "svt");  gMessMgr->QAInfo() << "blacklist svt" << endm;}
    if (!(GetOption("SsdDb")||GetOption("SsdCalDb"))) {mk->SetAttr("blacklist", "ssd");  gMessMgr->QAInfo() << "blacklist ssd" << endm;}
    if (!(GetOption("SstDb")||GetOption("SstCalDb"))) {mk->SetAttr("blacklist", "sst");  gMessMgr->QAInfo() << "blacklist sst" << endm;}
    if (! GetOption("EemcDb")                       ) {mk->SetAttr("blacklist", "eemc"); gMessMgr->QAInfo() << "blacklist eemc"<< endm;}
    if (! GetOption("FmsDb")                        ) {mk->SetAttr("blacklist", "fms");  gMessMgr->QAInfo() << "blacklist fms" << endm;}
  } else {// for Embedding chain trigger black list by NoSsdIT and NoSvtIT, could be some problems if you try to run svt or ssd clusters, ...
    if (GetOption("NoSvtIt"))                         {mk->SetAttr("blacklist", "svt");  gMessMgr->QAInfo() << "blacklist svt" << endm;}
    if (GetOption("NoSsdIt"))                         {mk->SetAttr("blacklist", "ssd");  gMessMgr->QAInfo() << "blacklist ssd" << endm;}
    if (GetOption("NoSstIt"))                         {mk->SetAttr("blacklist", "sst");  gMessMgr->QAInfo() << "blacklist sst" << endm;}
  }
}
//_____________________________________________________________________
/// Creates output-tree branches
void StBFChain::SetTreeOptions()
{

  StTreeMaker *treeMk = (StTreeMaker *) GetMaker("outputStream");
  if (!treeMk) return;
  if (GetOption("EvOut")){
    gMessMgr->QAInfo() << "Will Write StEvent out, treeMk->GetFile() = "  << treeMk->GetFile() << endm;
    treeMk->IntoBranch("eventBranch","StEvent");
    if (GetOption("EvOutOnly")) return;
  }
  if (! GetOption("nohistos"))     treeMk->SetBranch("histBranch");
  if (! GetOption("norunco"))    treeMk->SetBranch("runcoBranch");
  if (GetOption("McEvent") && GetOption("McEvOut")){
    gMessMgr->QAInfo() << "Will Write StMcEvent out, treeMk->GetFile() = "  << treeMk->GetFile() << endm;
    treeMk->IntoBranch("McEventBranch","StMcEvent");
  }
  if (GetOption("GeantOut")) treeMk->IntoBranch("geantBranch","geant");
  if (GetOption("AllEvent")) {
    if (GetOption("fzin")   || GetOption("fzinSDT")   ||
	GetOption("ntin")   || 
	GetOption("gstar")  || 
	GetOption("pythia") || 
	GetOption("VMC")    || 
	GetOption("PrepEmbed")) {
      treeMk->IntoBranch("geantBranch","geant");
      treeMk->IntoBranch("geantBranch","geant/.data/particle");
      treeMk->IntoBranch("geantBranch","geant/.data/g2t_rch_hit");
    }
  }
}
//________________________________________________________________________________
Long_t  StBFChain::ProcessLine(const char *line) {
  if (! line ||  !strlen(line)) return -1;
  if (Debug()) gMessMgr->QAInfo() << "ProcessLine:" << line << endm;
  TInterpreter::EErrorCode error = TInterpreter::kNoError;
  Long_t  res = gInterpreter->ProcessLine(line, &error);
  if (error != TInterpreter::kNoError) {
    gMessMgr->Error() << "StBFChain::ProcessLine command:" << line << " has failed. Quit job." << endm;
    gSystem->Exit(1);
  }
  return res;
}
//________________________________________________________________________________
TString StBFChain::GetGeometry() const
{
  Int_t n = fchainOpt->GetNRows();
  for (Int_t i=0;i<n;i++) {
    if (!fchainOpt->GetTable()[i].Flag) continue;
    TString k(fchainOpt->GetTable()[i].Key);
    k.ToLower();
    if (k[0]!='y') 		continue;
    if (k[1]!='2') 		continue;
    if (k[2] <'0' || k[2]>'9') 	continue;
    if (k[3] <'0' || k[3]>'9') 	continue;
    if (k[4] <'0' || k[4]>'9') 	continue;
    return k;
  }
  return TString(""); 
}   
//________________________________________________________________________________
void StBFChain::Clear(Option_t *option) {
  StMaker::Clear(option);
  if (GetOption("TObjTable"))  gObjectTable->Print();
}
