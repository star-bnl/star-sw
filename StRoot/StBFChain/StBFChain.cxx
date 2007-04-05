//_____________________________________________________________________
// @(#)StRoot/StBFChain:$Name:  $:$Id: StBFChain.cxx,v 1.477.2.1 2007/04/05 20:15:48 jeromel Exp $
//_____________________________________________________________________
#include "TROOT.h"
#include "TString.h"
#include "TObjString.h"
#include "TRegexp.h"
#include "TSystem.h"
#include "TMemStat.h"
#include "StBFChain.h"
#include "StEvtHddr.h"
#include "StChain.h"
#include "St_geant_Maker/St_geant_Maker.h"
#include "StEventMaker/StEventMaker.h"
#include "StDbBroker/StDbBroker.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StTreeMaker/StTreeMaker.h"
#include "StIOMaker/StIOMaker.h"
#include "StChallenger/StChallenger.h"
#include "St_tcl_Maker/St_tcl_Maker.h"
#include "St_dst_Maker/StPrimaryMaker.h"
#include "St_dst_Maker/StVertexMaker.h"
#include "St_tpcdaq_Maker/St_tpcdaq_Maker.h"
#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StDbUtilities/StMagUtilities.h"
#include "St_QA_Maker/StEventQAMaker.h"
#include "StMessMgr.h"
#include "StiMaker/StiDefaultToolkit.h"
#include "StiMaker/StiMakerParameters.h"
#include "StiMaker/StiMaker.h"
#include "StSecondaryVertexMaker/StV0FinderMaker.h"
#include "StSecondaryVertexMaker/StXiFinderMaker.h"
#include "StHitFilterMaker/StHitFilterMaker.h"
#include "StTpcHitMoverMaker/StTpcHitMoverMaker.h"
#include "St_tpt_Maker/St_tpt_Maker.h"
#include "StGenericVertexMaker/StGenericVertexMaker.h"
#include "StAssociationMaker/StAssociationMaker.h"
#include "StTpcDb/StTpcDbMaker.h"

//_____________________________________________________________________
// PLease, preserve the comment after = { . It is used for documentation formatting
//

#define STR_OBSOLETE "WARNING *** Option is OBSOLETE ***"

Bfc_st BFC1[] = { // standard chains
#include "BFC.h"
};
// ITTF Chain will be put here. Option list starting from minimalistic requirements
// and may not initially work.
// Please, preserve the comment after = { . It is used for documentation formatting
//
Bfc_st BFC2[] = { // ITTF Chains
#include "BFC2.h"
};


// NoChainOptions -> Number of chain options auto-calculated
Int_t NoChainOptions;
Int_t NoChainOptions1 = sizeof (BFC1)/sizeof (Bfc_st);
Int_t NoChainOptions2 = sizeof (BFC2)/sizeof (Bfc_st);

class StEvent;
StEvent *Event;
class StIOMaker;
class St_geant_Maker; St_geant_Maker *geantMk = 0;
class St_db_Maker;
static St_db_Maker *dbMk    = 0;
class StTreeMaker;
ClassImp(StBFChain)

//_____________________________________________________________________________
/// Default Constructor
StBFChain::StBFChain(const char *name, const Bool_t UseOwnHeader):
  StChain(name,UseOwnHeader),fTFile(0),fSetFiles(0),fInFile(0),fFileOut(0) {

  gMessMgr->Info("StBFChain::StBFChain Default Constructor called.");
  fBFC = new Bfc_st[NoChainOptions1];
  memcpy (fBFC, &BFC1, sizeof (BFC1));
  NoChainOptions = NoChainOptions1;
  FDate  = FTime  = 0;
  FDateS = FTimeS = 0;
}


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
StBFChain::StBFChain(Int_t mode, const char *name):
  StChain(name,kFALSE),fTFile(0),fSetFiles(0),fInFile(0),fFileOut(0) {

  if(mode == 2){
    gMessMgr->Info("StBFChain::StBFChain Special Constructor called using chain-setup 2");
    fBFC = new Bfc_st[NoChainOptions2];
    memcpy (fBFC, &BFC2, sizeof (BFC2));
    FDate  = FTime  = 0;
    FDateS = FTimeS = 0;
    NoChainOptions= NoChainOptions2;
  } else {
    gMessMgr->Info("StBFChain::StBFChain Special Constructor called using chain-setup 1");
    fBFC = new Bfc_st[NoChainOptions1];
    memcpy (fBFC, &BFC1, sizeof (BFC1));
    FDate  = FTime  = 0;
    FDateS = FTimeS = 0;
    NoChainOptions= NoChainOptions1;
  }
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
  Int_t status = kStOk;
  Int_t i, iok;
  for (i = 1; i< NoChainOptions; i++) { // Load Libraries if any
    if (fBFC[i].Flag) {
      if (strlen(fBFC[i].Libs) > 0) {
	TObjArray Libs;
	ParseString(fBFC[i].Libs,Libs);
	TIter nextL(&Libs);
	TObjString *libe = 0;
	while ((libe = (TObjString *) nextL())) {
	  TObjArray LoadedLibs;
	  TString lib(gSystem->GetLibraries(0,"D"));
	  ParseString(lib,LoadedLibs);
	  TIter next(&LoadedLibs);
	  TObjString *LoadedLib;
	  while ((LoadedLib = (TObjString *) next())){
	    TString Base(gSystem->BaseName(LoadedLib->GetName()));
	    Base.ReplaceAll(".so","");
	    Base.ReplaceAll(".sl","");
	    if (Base == libe->GetString()) goto ENDL;
	  }
	  //	  if (!strstr(lib,libe.Data())) {
	  iok = gSystem->Load(libe->GetName());
          {  TString ts("load "); ts += libe->GetString();
	     TMemStat::PrintMem(ts.Data());
	  }
	  if (iok < 0)  {
	    (void) printf("QAInfo: problem with loading\t%s\nQAInfo: %s is switched off \t!!!!\n"
			  ,libe->GetName(),fBFC[i].Key);
	    fBFC[i].Flag = kFALSE;
	    status = kStErr;
 	    assert(iok >= 0);
	    break;
	  } else {
	    (void) printf("QAInfo: Library %-20s\t(%s)\tis loaded\n",libe->GetName(),
			  gSystem->DynamicPathName(libe->GetName()));
	  }
	ENDL: //yf ? continue;
	  LoadedLibs.Delete();
	}
	Libs.Delete();
      }
    }
  }
  return status;
}

//_____________________________________________________________________________
/// Maker-instantiation handler.
/*!
  This routine contains it all (make instantiation that is), from calibration
  precedence to parameter setting depending on option etc ... Other thing done
  here which deserves attention
  - The maker's SetMode() mechanism is treated here.
  - Calibration options like NoMySQLDb or NoCintDb and path are set
  - SetFlavor() sim+ofl or sim is made

  If a maker is added along with some flag options, this is the place to
  implement the switches.
*/
Int_t StBFChain::Instantiate()
{
  Int_t status = kStOk;
  Int_t i, iFail=0;
  for (i = 1; i< NoChainOptions; i++) {// Instantiate Makers if any
    if (fBFC[i].Flag) {
      if (strlen(fBFC[i].Maker) > 0){// && strlen(fBFC[i].Name) > 0){// && GetMaker(fBFC[i].Name))) {
	TString maker(fBFC[i].Maker);
	TString Key(fBFC[i].Key);
	Key.ToLower();
	StMaker *myChain = 0;
	if (strlen(fBFC[i].Chain) > 0) myChain = GetMaker(fBFC[i].Chain);
	if (maker == "St_db_Maker"){
	  St_db_Maker *mk = 0;
	  if (Key.CompareTo("db",TString::kIgnoreCase) == 0) {
            if (!dbMk && ! GetChain()->GetMaker(fBFC[i].Name)) {
	      if (!GetOption("NoMySQLDb") && !GetOption("NoCintDb"))
		if ( GetOption("NoCintCalDb") ){
		  dbMk = new St_db_Maker(fBFC[i].Name,"MySQL:StarDb","$STAR/StarDb");
		} else {
		  dbMk = new St_db_Maker(fBFC[i].Name,"MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
		}
	      else {
		if (GetOption("NoMySQLDb") && GetOption("NoCintDb"))
		  dbMk = new St_db_Maker(fBFC[i].Name,"$PWD/StarDb");
		else {
		  if (GetOption("NoMySQLDb"))
		    dbMk = new St_db_Maker(fBFC[i].Name,"$STAR/StarDb","$PWD/StarDb");
		  if (GetOption("NoCintDb"))
		    dbMk = new St_db_Maker(fBFC[i].Name,"MySQL:StarDb","$PWD/StarDb");
		}
	      }
	    }
	    mk = dbMk;
	  }
	  if (!mk) status = kStErr;
	  else {
	    strcpy (fBFC[i].Name, (Char_t *) mk->GetName());
	    if (GetOption("Simu"))    mk->SetFlavor("sim+ofl");
	    else                      mk->SetFlavor("ofl");
	  }
	  if (myChain) myChain->AddMaker(mk);
	  continue;
	}
	if (maker == "StIOMaker" && fSetFiles) {
	  StIOMaker *inpMk=0;
	  if (GetOption("InTree")) {
	    Char_t line[80] = "bfcTree";
	    Int_t k = kOpt("InTree");
	    sscanf(fBFC[k].Comment,"%s",line);
	    inpMk = new StIOMaker("inputStream","r",fSetFiles,line);
	  }
	  else inpMk = new StIOMaker("inputStream","r",fSetFiles);
	  if (inpMk) {
	    strcpy (fBFC[i].Name,(Char_t *) inpMk->GetName());
	    SetInput("StDAQReader",".make/inputStream/.make/inputStream_DAQ/.const/StDAQReader");
	    if (GetOption("ReadAll")) inpMk->SetBranch("*",0,"r");	//activate all branches
	  }
	  else status = kStErr;
	  if (myChain) myChain->AddMaker(inpMk);
	  continue;
	}
	if (maker == "StTreeMaker" && fFileOut) {
	  StTreeMaker    *treeMk  = 0;
	  if (GetOption("OutTree")) {
	    Char_t line[80] = "bfcTree";
	    Int_t k = kOpt("OutTree");
	    sscanf(fBFC[k].Comment,"%s",line);
	    treeMk = new StTreeMaker("outputStream",fFileOut->Data(),line);
	  }
	  else treeMk = new StTreeMaker("outputStream",fFileOut->Data());
	  if (treeMk) {
	    strcpy (fBFC[i].Name,(Char_t *) treeMk->GetName());
	    treeMk->SetIOMode("w");
	    SetTreeOptions();
	    if (myChain) myChain->AddMaker(treeMk);
	    continue;
	  }
	}
	if (Key == "geant") {
	  if (!geantMk) {
	    Int_t                    NwGeant = 10;
	    if (!GetOption("fzin") && !GetOption("gstar"))
	                             NwGeant =  5;
	    if (GetOption("big"))    NwGeant = 20;
	    if (GetOption("bigbig")) NwGeant = 40;
	    Int_t IwType = 0;
	    if (GetOption("Higz"))   IwType = 1;
	    Int_t NwPaw = 0;
	    if (GetOption("paw"))    NwPaw  = 2;
	    geantMk = new St_geant_Maker("geant",NwGeant,NwPaw,IwType);
	    if (geantMk) {
	      strcpy (fBFC[i].Name,(Char_t *) geantMk->GetName());
	      geantMk->SetActive(kFALSE);
	      if (GetOption("fzin") || GetOption("gstar")) geantMk->SetActive(kTRUE);
	      SetGeantOptions();
	    }
	  }
	  if (!geantMk) status = kStErr;
	  if (myChain) myChain->AddMaker(geantMk);
	  continue;
	}

	StMaker *mk = 0;

	// Special makers already created or action which
	// need to take place before 'maker' is created.
	if (maker == "StTpcDbMaker") mk = GetChain()->GetMaker(fBFC[i].Name);
	if (maker == "StiMaker")     StiToolkit::setToolkit( new StiDefaultToolkit() );

	// All Makers created here
	if (!mk) {
	  if (strlen(fBFC[i].Name) > 0) mk = New(fBFC[i].Maker,fBFC[i].Name);
	  else  {
	    mk = New(fBFC[i].Maker);
	    if (mk) strcpy (fBFC[i].Name,(Char_t *) mk->GetName());
	  }
	}


	// special maker options
	if (mk) {
	  if (maker == "StiMaker") {
	    StiMaker   *stiMk = (StiMaker*) mk;


	    StiToolkit         * tk   = stiMk->getToolkit();
	    if (! tk)            tk   = new StiDefaultToolkit();

	    StiMakerParameters * pars = stiMk->getParameters();
	    if ( ! pars ){       pars = new StiMakerParameters(); //default Sti parameters
	                         stiMk->setParameters(pars);
	    }

	    // ready to set options and parameters
	    tk->setGuiEnabled(kFALSE);
	    tk->setMcEnabled(kFALSE);
	    pars->useGui = kFALSE;
	    pars->useMcAsRec = kFALSE;
	    pars->doSimulation = kFALSE;
	    pars->doAssociation = kFALSE;
	    pars->doPlots       = kFALSE;
	    pars->useTpc        = kTRUE;
	    pars->activeTpc     = kTRUE;
	    pars->doStEventInput=kTRUE; // We always want to have this I think?
	    pars->doStEventOutput=kTRUE;
	    //pars->trackerInputFile = "StRoot/StiMaker/macros/trackFinderPars.dat";
	    //pars->fitterInputFile  = "StRoot/StiMaker/macros/trackFitterPars.dat";
	    //pars->emcInputFile     = "none";
	    //pars->eemcInputFile    = "none";
	    //pars->svtInputFile     = "StRoot/StiMaker/macros/svtInputFile.dat";
	    //pars->ssdInputFile     = "none";
	    //pars->tpcInputFile     = "StRoot/StiMaker/macros/tpcInputFile.dat";
	    //pars->ftpcInputFile    = "none";
	    //pars->pixelInputFile   = "none";

	    pars->useSvt=kTRUE;         // SVT used in Sti but not active. ??
	                                // Pre-2001 data, will build only 1 ladder?
	    //pars->useSsd=kTRUE;         // use SSD in Sti

	    if (GetOption("SvtIT")) pars->activeSvt=kTRUE;

	    if (GetOption("SsdIT")){
	      pars->useSsd=kTRUE;       // use SSD in Sti
	      pars->activeSsd=kTRUE;
	    }
	    if (GetOption("FtpcIT")){
	      pars->useFtpc=kTRUE;
	      pars->activeFtpc=kTRUE;
	    }
	    cout << "Sti Parameters (seen in bfc):" << endl
		 << *pars << endl;

	    //if (GetOption("Simu")) tk->setMcEnabled(kTRUE);
	    if ( GetOption("clearmem") )  stiMk->SetMode(1);

	  }
	  if (maker=="StGenericVertexMaker") {
	      StGenericVertexMaker* gvtxMk = (StGenericVertexMaker*) mk;
	      int                   VtxOpt = 0;

	      // VertexFinder methods
	      if ( GetOption("VFMinuit") ){  VtxOpt |= (0x1 << 0);} //  1
	      if ( GetOption("VFppLMV") ){   VtxOpt |= (0x1 << 1);} //  2 0X02
	      if ( GetOption("VFppLMV5") ){  VtxOpt |= (0x1 << 2);} //  4 0X04
	      if ( GetOption("VFPPV") ){     VtxOpt |= (0x1 << 3);} //  8 0X08
	      if ( GetOption("VFPPVNOCTB") ){VtxOpt |= (0x1 << 4);} // 16 0X10
	      if ( GetOption("VFFV") ){      VtxOpt |= (0x1 << 5);} // 32 0x20
	      if ( GetOption("VFMCE") ){     VtxOpt |= (0x1 << 6);} // 64 0x40
	      gvtxMk->SetMode(VtxOpt);

	      // All VertexFinders implement those (or not)
	      if (GetOption("beamLine")) {      gvtxMk->UseBeamLine();}
	      if (GetOption("CtbMatchVtx")) {   gvtxMk->UseCTB();}

	  }
	  if (maker=="StAssociationMaker") {
	      if (GetOption("ITTF")) {
		  StAssociationMaker* assMk = (StAssociationMaker*) mk;
		  assMk->useInTracker();
	      }
	  }

	  // usually, we do maker first and option second but the
	  // logic is more readable with option first here (as it
	  // got a bit out of hand)
	  if (GetOption("ppOpt") ) {                         // pp specific stuff
	    if (maker == "StTrsMaker"){
	      mk->SetMode(1);       // Pile-up correction
	    }
	    if (maker == "StVertexMaker"){
	      cout << "QAInfo: ppOpt (pp mode) is turned ON" << endl;
	      if( GetOption("SvtMatchVtx"))    mk->SetMode(4); // Switch vertex finder to ppLMV using EST
	      else if ( GetOption("VFppLMV5")) mk->SetMode(8); // LMV4 with LMV5 cuts 
	      else                             mk->SetMode(1); // Switch vertex finder to ppLMV
	      StVertexMaker *pMk = (StVertexMaker*) mk;
	      if( GetOption("beamLine")){
		  pMk->SetBeam4ppLMV();                      // Add beam-line constrain
	      }

	      if( GetOption("fzin")){                        // if fzin, get CTB's from MC
		cout << "QAInfo: fzin used, setting CTB Mode to 1" << endl;
		pMk->SetCTBMode(1);
	      } else if ( GetOption("clearDAQCTB") ){        // remove CTB from DAQ (embedding)
		cout << "QAInfo: clearDAQCTB used, will removed CTB hits from DAQ"  << endl;
		pMk->SetCTBMode(2);
	      } else{
		cout << "QAInfo: Will get CTB from DAQ file" << endl;
		pMk->SetCTBMode(0);                          // Else get from DAQ
	      }
	    }
	  } else {
	    // See section above associated to GetOption("ppOpt") for pp specific.
	    // This part was reshaped to avoid confusion and possible
	    // option overwrite if one is not carefull enough ... VtxOffset
	    // way used in RY1h and RY2000 only and abandonned later. If re-appear,
	    // maker == StVertexmaker would have to be its own block with a ppOpt
	    // embedded logic.
	    if ((maker == "StVertexMaker"  || maker == "StPreVertexMaker") &&
		GetOption("VtxOffSet")){
	      cout << "QAInfo: VtxOffSet is ON" << endl;
	      if ( GetOption("SvtMatchVtx") )  mk->SetMode(3);
	      else                             mk->SetMode(2);
	    }
	  }

	  if (maker == "StStrangeMuDstMaker"){
	    if (GetOption("CMuDST") ){
	      if ( GetOption("StrngMuDST") ) {
		StStrangeMuDstMaker *pMk = (StStrangeMuDstMaker*) mk;
		pMk->DoV0();                                  // Set StrangeMuDstMaker parameters
		pMk->DoXi();
		pMk->DoKink();
		pMk->SetNoKeep();                             // Set flag for output OFF
	      }
	    }
	  }
	   
	  // Alex requested an option (not turned by default) to disable all
	  // hit reco cuts. This will make allm hits saved to MuDST /ezTree.
	  if ( maker == "StEmcRawMaker"){
	    if (GetOption("BEmcDebug")){
	      mk->SetMode(1); // only one option now, bit a bitmask
	    }
	  }


	  // MuDST and ezTree. Combinations are
	  //  ezTree         -> ezTree only
	  //  CMuDST         -> regular MuDST only
	  //  ezTree,CMuDST  -> both
	  if (maker == "StMuDstMaker"){
	    if ( GetOption("ezTree") ){
	      StMuDstMaker *pMk = (StMuDstMaker *) mk;
	      if ( ! GetOption("CMuDST")) pMk->SetStatus("*",0); 
	      pMk->SetStatus("EztAll",1); 
	    }
	  }


	  if (maker == "St_dst_Maker") SetInput("dst",".make/dst/.data/dst");
	  if (maker == "St_dst_Maker" && GetOption("HitsBranch")) mk->SetMode(2);
	  if (maker == "StMatchMaker" && !GetOption("Kalman")) mk->SetMode(-1);
	  if (maker == "StLaserEventMaker"){
	    // Bill stuff - Empty place-holder
	  }
	  //if (maker == "St_tpt_Maker" && GetOption("ExB")){
	  if ( (maker == "StTpcHitMover" || maker == "St_tpt_Maker") && GetOption("ExB")){
	    // bit 0 is ExB ON or OFF
	    // The next 3 bits are reserved for yearly changes.
	    // Backward compatibility preserved.
	    int mask=1;                                    // Al Saulys request
	    if( GetOption("EB1") ){
	      // Do nothing (i.e. bit 1 at 0)
	    } else if ( GetOption("EB2") ){
	      // Force bit 1 at 1 regardless
	      mask = mask | 2;
	    } else {
	      // depend on RY option i.e. take default for that RealYear data
	      // expectations.
	      if( GetOption("RY2001")  ||
		  GetOption("RY2003")  ||
		  GetOption("RY2003a") ||
		  GetOption("RY2003b") ||
		  GetOption("RY2003X")) mask = mask | 2 ;  // Jim Thomas request
	    }
	    // Other options introduced in October 2001 for distortion corrections
	    // studies and year1 re-production. Those are OR additive to the mask.
	    //(void) printf("StBFChain:: Options list : %d %d %d %d %d %d %d %d\n",
	    //		  kPadrow13,kTwist,kClock,kMembrane,kEndcap,
	    //            kIFCShift,kSpaceCharge,kSpaceChargeR2);
	    if( GetOption("OBmap") ){	      mask |=   (kBMap          << 1); }
	    if( GetOption("OBMap2d") ){       mask |=   (kFast2DBMap    << 1); }
	    if( GetOption("OPr13") ){	      mask |=   (kPadrow13      << 1); }
	    if( GetOption("OTwist") ){	      mask |=   (kTwist         << 1); }
	    if( GetOption("OClock") ){	      mask |=   (kClock         << 1); }
	    if( GetOption("OCentm") ){	      mask |=   (kMembrane      << 1); }
	    if( GetOption("OECap") ){	      mask |=   (kEndcap        << 1); }
	    if( GetOption("OIFC") ){	      mask |=   (kIFCShift      << 1); }
	    if( GetOption("OSpaceZ") ){	      mask |=   (kSpaceCharge   << 1); }
	    if( GetOption("OSpaceZ2") ){      mask |=   (kSpaceChargeR2 << 1); }
	    if( GetOption("OShortR") ){       mask |=   (kShortedRing   << 1); }
	    if( GetOption("OGridLeak") ){     mask |=   (kGridLeak      << 1); }
	    if( GetOption("OGridLeak3D") ){   mask |=   (k3DGridLeak    << 1); }


	    (void) printf("StBFChain:: ExB The option passed will be %d 0x%X\n",mask,mask);
	    mk->SetMode(mask);
	  }
	  if (maker == "St_tpt_Maker" && GetOption("AlignSectors")){
	    St_tpt_Maker *tptMk = (St_tpt_Maker*)mk;
	    tptMk->AlignHits(kTRUE);
	  }
	  if (maker == "StTpcHitMover" && GetOption("AlignSectors")){
	    StTpcHitMover *hitMk = (StTpcHitMover *) mk;
            hitMk->AlignHits(kTRUE);
	  }
	  if (maker == "St_tcl_Maker") {
	    St_tcl_Maker *tclMk = (St_tcl_Maker *) mk;
	    if (GetOption("EastOff")) tclMk->EastOff();
	    if (GetOption("WestOff")) tclMk->WestOff();
	    if (GetOption("AllOn"))   tclMk->AllOn();
	  }
	  if (maker == "St_tpcdaq_Maker") {
	    Int_t DMode=0;
	    St_tpcdaq_Maker *tcpdaqMk = (St_tpcdaq_Maker *) mk;

	    // Beware of those ...
	    if      (GetOption("Trs"))   tcpdaqMk->SetMode(1); // trs
	    else if (GetOption("Simu"))  tcpdaqMk->SetMode(2); // daq, no gain
	    else                         tcpdaqMk->SetMode(0); // daq

	    // DAQ100 or Raw switch options -- Please, adjust StRTSClientFCFMaker block as well
	    if ( GetOption("onlcl") )   DMode = DMode | 0x2;  // use the online TPC clusters (DAQ100) info if any
	    if ( GetOption("onlraw") )  DMode = DMode | 0x1;  // use the TPC raw hit information
	    if (DMode != 0) tcpdaqMk->SetDAQFlag(DMode);      // set flag, leave default = 1

	    // Correction depending on DAQ100 or not
	    // bit 0  =   do GAIN_CORRECTION
	    // bit 1  =   do NOISE_ELIM
	    // bit 2  =   do ASIC_THRESHOLDS
	    // WARNING Option FCF is checked in StDAQMaker
	    if ( GetOption("fcf")   ){
	      tcpdaqMk->SetSequenceMerging(0);
	      if ( GetOption("Trs")   )  tcpdaqMk->SetCorrection(0x5); // ASIC + GAIN
	      else                       tcpdaqMk->SetCorrection(0x0); // fcf && ! trs => no corrections
	    } else {
	      tcpdaqMk->SetSequenceMerging(1);
	      tcpdaqMk->SetCorrection(0x7);
	    }


	    (void) printf("StBFChain:: maker==St_tpcdaq_Maker SetDAQFlag(%d) SetMode(%d) SetCorrection(%d)\n",
			  DMode,tcpdaqMk->GetMode(),tcpdaqMk->GetCorrection());
	  }
	  if (maker == "StRTSClientFCFMaker"){
	    Int_t DMode=0;
	    // use the online TPC clusters (DAQ100) info if any
	    if ( GetOption("onlcl") && ! GetOption("onlraw") )  DMode = DMode | 0x2;
	    // use the TPC raw hit information
	    if ( GetOption("onlraw")&& ! GetOption("onlcl")  )  DMode = DMode | 0x1;
	    if (DMode) mk->SetMode(DMode);                 // set flag (matches tcpdaqMk->SetDAQFlag())
	  }

	  if (maker == "StTpcT0Maker"){
	    Int_t mask = 0;
	    if ( GetOption("tcl") ) mask = mask | 0x0;
	    if ( GetOption("fcf") ) mask = mask | 0x1;
	    (void) printf("StBFChain:: For StTpcT0Maker tcl is %s, fcf is %s => mask = %d\n",
			  GetOption("tcl")?"on":"off",GetOption("fcf")?"on":"off",mask);
	    mk->SetMode(mask);
	  }

	  if (maker == "StRchMaker") {
	    if (GetOption("Rrs")) mk->SetMode(1); // rrs
	    else                  mk->SetMode(0); // daq
	  }

	  // Place-holder. Would possibly be a bitmask
	  if (maker == "StTofrMatchMaker"){
	    mk->SetMode(0);
	  }

	  // Turn on alternative V0 method
	  if (maker == "StV0Maker" && GetOption("Ev03")) mk->SetMode(1);
	  //if (maker == "StKinkMaker"){
	  // // Placeholder for KinkMaker control.
	  // // - SetMode(1)   TPT,
	  // // - SetMode(2)   ITTF
	  // // Default = Both
	  //}
	  if (maker == "StSpaceChargeEbyEMaker") {
	    if ( GetOption("SpcChgCal") ){
	      mk->SetMode(2);
	    }
	  }
	  if (maker == "StEventQAMaker") {
	    if ( GetOption("alltrigger") ){
	      StEventQAMaker *QAmk = (StEventQAMaker *) mk;
	      QAmk->AllTriggers();
	    }
	  }
	  //Special options for V0s and Xis using estGlobal tracks
	  if(maker=="StV0FinderMaker" && Key=="v0svt"){
	    StV0FinderMaker *V0mk=(StV0FinderMaker*)mk;
	    V0mk->SetLanguageUsage(0);
	    V0mk->SetSVTUsage(1);
	    V0mk->SetV0LanguageUsage(3);
	  }
	  if(maker=="StXiFinderMaker" && Key=="xisvt"){
	    StXiFinderMaker *Ximk=(StXiFinderMaker*)mk;
	    Ximk->SetLanguageUsage(0);
	    Ximk->SetSVTUsage(1);
	    Ximk->SetV0LanguageUsage(3);
	    Ximk->SetXiLanguageUsage(5);
	  }

	  if (maker == "St_trg_Maker") {
	    Int_t mode = 0;
	    if (! GetOption("alltrigger")){
	      if (GetOption("Physics"))   mode += 1;
	      if (GetOption("LaserTest")) mode += 2;
	      if (GetOption("PulserSvt")) mode += 4;
	    } else {
	      gMessMgr->QAInfo() << "'alltrigger' option on. All others ignored" << endm;
	    }
	    if (mode) mk->SetMode(mode);
	  }
	  if ((maker == "StdEdxMaker" || maker == "StdEdxY2Maker" ) &&
	      GetOption("Simu"))  mk->SetMode(-10);
	  if (maker == "StTpcDbMaker"){
	    StTpcDbMaker *Tmk=(StTpcDbMaker *) mk;
            Tmk->SetMode(0);
	    // this change may be temporary i.e. if Simulation includes
	    // rotation/translation, this won't be necessarily true.
	    // Will investigate further.
            if (GetOption("Simu")) Tmk->SetMode(1);
	    // This is commented for now but may be used. Those extensions
	    // were implemented by David H. on Jan 2 2002. DEfault is ofl+laserDV
	    if ( GetOption("useLDV") ) Tmk->UseOnlyLaserDriftVelocity();  // uses laserDV database
	    if ( GetOption("useCDV") ) Tmk->UseOnlyCathodeDriftVelocity();// uses ofl database
	  }
	  if (maker == "StSvtDbMaker"){
            mk->SetMode(0);
            // If simulation running make sure pick up simu stuff from db
            if (GetOption("Simu")) mk->SetMode(1);
          }


	  // Hit filtering will be made from a single maker in
	  // future with flexible filtering method
	  if (maker == "StHitFilterMaker"){
	    if( GetOption("SvtHitFilt")){
	      // Filter out SVT bad hits, TPC hits not on tracks and all hits if fabs(ZVert)>30
	      gMessMgr->QAInfo() << "SVT hit filter is ON" << endm;
	      StHitFilterMaker *Filtmk=(StHitFilterMaker*)mk;
	      Filtmk->setPtLowerCut(-99.);
	      Filtmk->setPtUpperCut(-99.);
	      Filtmk->setAbsEtaCut(-99);
	      Filtmk->setAbsZVertCut(30);
	    } else {
	      gMessMgr->QAInfo() << "Default hit filtering is ON" << endm;
	    }
	  }


	  if (GetOption("dst") && GetOption("NoHits") && maker == "StEventMaker") {
	    StEventMaker *EvMk = (StEventMaker *) mk;
	    EvMk->doLoadTpcHits  = kFALSE;
	    EvMk->doLoadFtpcHits = kFALSE;
	    EvMk->doLoadSvtHits  = kFALSE;
	    EvMk->doLoadSsdHits  = kFALSE;
	  }
	  if (myChain) myChain->AddMaker(mk);
	}
        else status = kStErr;
	if (status != kStOk && i != iFail) {
	  gMessMgr->QAInfo()
	    << " ======================================\n"
	    << " problem with Instantiation of "         << fBFC[i].Maker << "\n"
	    << " ======================================" << endm;
	  iFail = i;
	}
      }
    }
  }

  SetDbOptions();
  //  PrintQAInfo();
  PrintInfo();
  // START the chain (may the force be with you)
  // Create HTML docs of all Maker's inv
  if (GetOption("MakeDoc"))  MakeDoc();
  if (GetOption("Debug"))    SetDEBUG(1);
  if (GetOption("Debug1"))   SetDEBUG(1);
  if (GetOption("Debug2"))   SetDEBUG(2);
  if (GetOption("nohistos")) SetAttr(".histos",0,"*");
  else                       SetAttr(".histos",1,"*");
  if (GetOption("NoRepeat")) gMessMgr->IgnoreRepeats();
  return status;
}
//_____________________________________________________________________
/// Skip events by selecting according maker 
Int_t StBFChain::Skip(int nskip)
{
  St_geant_Maker *geant = 0;
  StIOMaker      *inpMk = 0;
  if (nskip<=0) return 0;
  if (GetOption("fzin")) 
   geant = (St_geant_Maker *)GetMaker("geant");
   if (geant && !geant->InheritsFrom("St_geant_Maker")) geant = 0;
   if (geant && !geant->IsActive()) 			geant = 0;
   if (geant) {
     (void) printf ("St_Geant_Make::Skip(%d) Events\n",nskip);
     geant->Skip(nskip);return 0;}

   inpMk = (StIOMaker *) GetMaker("inputStream");
   if (inpMk && !inpMk->InheritsFrom("StIOMaker")) 	inpMk = 0;
   if (inpMk) {
     (void) printf ("StIOMaker::Skip(%d)  Events\n",nskip);
     inpMk->Skip(nskip); return 0;}
     
   Error("Skip","No maker to Skip"); 
   return kStErr;  
}  

//_____________________________________________________________________
/// Really the destructor (close files, delete pointers etc ...)
Int_t StBFChain::Finish()
{
  if (fBFC) {
    delete [] fBFC; fBFC = 0;
    if (fTFile) {fTFile->Write(); fTFile->Flush(); fTFile->Close(); SafeDelete (fTFile);}
    return StMaker::Finish();
  }
  else return kStOK;
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
Int_t StBFChain::ParseString (const TString &tChain, TObjArray &Opt) {
  Int_t nParsed = -1;
  Ssiz_t begin, index, end, end2;
  begin = index = end = end2 = 0;
  TRegexp separator("[^ ;,\\t\\s]+");
  while ( (begin < tChain.Length()) && (index != kNPOS) ) {
    // loop over given Chain options
    index = tChain.Index(separator,&end,begin);
    if (index >= 0 && end >= 1) {
      TString substring(tChain(index,end));
      Opt.Add(new TObjString(substring.Data()));
    }
    begin += end+1;
  }
  return nParsed;
}

//_____________________________________________________________________
/// Check option if defined (Char_t argument interface)
Int_t StBFChain::kOpt (const Char_t *tag) const {
  TString *Tag = new TString(tag);
  Int_t kO = kOpt(Tag);
  delete Tag;
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
Int_t StBFChain::kOpt (const TString *tag) const {
  TString Tag = *tag;
  Tag.ToLower();
  TString opt, nopt;
  for (Int_t i = 1; i< NoChainOptions; i++) {
    opt = TString(fBFC[i].Key); //check nick name
    opt.ToLower();
    nopt = TString("-");
    nopt += opt;
    if       (Tag ==  opt) {return  i;}
    else {if (Tag == nopt) {return -i;}}
    opt = TString(fBFC[i].Maker); //check full maker name2
    nopt = TString("-");
    nopt += opt;
    if       (Tag ==  opt) {return  i;}
    else {if (Tag == nopt) {return -i;}}
  }
#if 0
  // Check that option can be library name or / and Maker
  static Char_t *path = ".:.$STAR_HOST_SYS/lib::.$STAR_HOST_SYS/LIB:$STAR/.$STAR_HOST_SYS/lib:$STAR/.$STAR_HOST_SYS/LIB";
  TString File = *tag; File += ".so";
  Char_t *file = gSystem->Which(path,File.Data(),kReadPermission);
  if (file) {
    Bfc_st *temp = fBFC;
    fBFC = new Bfc_st[NoChainOptions+1];
    memcpy (fBFC, temp, NoChainOptions*sizeof(Bfc_st));
    delete [] temp;
    memcpy (&fBFC[NoChainOptions].Key, tag->Data(), strlen(tag->Data()));
    fBFC[NoChainOptions].Name = "";
    fBFC[NoChainOptions].Chain = "";
    fBFC[NoChainOptions].Opts = "";
    fBFC[NoChainOptions].Maker = "";
    if (tag->Contains("Maker")) memcpy (&fBFC[NoChainOptions].Maker, tag->Data(), strlen(tag->Data()));
    memcpy (&fBFC[NoChainOptions].Libs, tag->Data(), strlen(tag->Data()));
    fBFC[NoChainOptions].Comment = "";
    fBFC[NoChainOptions].Flag = 0;
    NoChainOptions++;
    return NoChainOptions-1;
  }
#endif
  if ( (strncmp( Tag.Data() ,"dbv",3) || 
	strncmp( Tag.Data() ,"sdt",3)   ) &&
       strlen(Tag.Data()) == 11 ){
  } else {
    (void) printf (" Option %s has not been recognized\n", Tag.Data());
    abort(); //assert(1);
  }
  return 0;
}

//_____________________________________________________________________
/// Enable/disable valid command line options
void StBFChain::SetOption(const Int_t k, const Char_t *chain) {
  // set all off
  if (k > 0 && !fBFC[k].Flag) {
    //    printf ("SetOption: %s %i",fBFC[k].Key,k);
    if (strlen(fBFC[k].Opts) > 0) {
      TObjArray Opts;
      ParseString(fBFC[k].Opts,Opts);
      TIter next(&Opts);
      TObjString *Opt;
      while ((Opt = (TObjString *) next())) SetOption(Opt->GetName(),fBFC[k].Key);
      Opts.Delete();
    }
    fBFC[k].Flag = kTRUE;
    printf (" Switch On  %20s by %s\n", fBFC[k].Key, chain);
  } else {
    if (k < 0 && fBFC[-k].Flag) {
      //      printf ("SetOption: %s %i",fBFC[-k].Key,k);
      fBFC[-k].Flag = kFALSE;
      printf (" Switch Off %20s by %s\n", fBFC[-k].Key, chain);
    } else {
      // 0
      return;
    }
  }
}

//_____________________________________________________________________
/// Returns chain-option state (on/off)
Bool_t StBFChain::GetOption(const Int_t k) const
{
  return (k>0 && k <NoChainOptions) ? fBFC[k].Flag : kFALSE;
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
  int o = kOpt(Opt);
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
    which any database insertions will be ignored). It used to provide a
    mechanism by which we can run production with a stable set of calibration
    constant and still allow for development value insertion to get in.
  - The <tt>sdt</tt> tag is used to setup the database look-up time stamp
    that is, on which date to look at the database. This is usefull if we
    want to decouple Geant geometry and database calibration constants.
    This may also be used to get database values when you do not run over
    a raw data file.
*/
void StBFChain::SetFlags(const Char_t *Chain)
{
  Int_t k=0;
  if (!Chain || !Chain[0] || !strlen(Chain)) {
    printf ("\tPossible Chain Options are: \n");
    for (k=0;k<NoChainOptions;k++)
      printf (" %3d:[-]%-13s:%-12s:%-12s:%-12s :%s :%s :%s\n"
	      ,k,fBFC[k].Key,fBFC[k].Name,fBFC[k].Chain,fBFC[k].Opts,fBFC[k].Maker,fBFC[k].Libs,fBFC[k].Comment);
    return;
  }
  TString STAR_VERSION("$STAR_VERSION");
  gSystem->ExpandPathName(STAR_VERSION);
  TString tChain(Chain);
  gMessMgr->QAInfo() << "=============================================="  << endm;
  gMessMgr->QAInfo() << "============= You are in " << STAR_VERSION.Data() << " ===============" << endm;
  gMessMgr->QAInfo() << "Requested chain " << GetName() << " is :\t" << tChain.Data() << endm;
  TObjArray Opts;
  ParseString(tChain,Opts);
  TIter next(&Opts);
  TObjString *Opt;
  while ((Opt = (TObjString *) next())) {
    TString string = Opt->GetString();
    Int_t in = string.Index("=");
    Int_t kgo;
    if (in <= 0) {
      string.ToLower();
      // printf ("Chain %s\n",tChain.Data());
      kgo = kOpt(string.Data());
      if (kgo != 0){
	SetOption(kgo);
      } else {
	// it is 0 i.e. was not recognized. Check if it is a dbvXXXXXXXX
	// with a 8 digit long time stamp. We can do all of that in the
	// SetDbOptions() only (removing the fBFC[i].Flag check) but the
	// goal here is to avoid user's histeria by displaying extra
	// messages NOW !!! Debug: dbv20040917
	if( ! strncmp( string.Data() ,"dbv",3) && strlen(string.Data()) == 11){
	  (void) sscanf(string.Data(),"dbv%d",&FDate);
	  cout << " ... but still will be considered as a dynamic timestamp (MaxEntryTime) "
	       << FDate  << endl;
	} else if( ! strncmp( string.Data() ,"sdt",3) && strlen(string.Data()) == 11){
	  (void) sscanf(string.Data(),"sdt%d",&FDateS);
	  cout << " ... but still will be considered as a dynamic timestamp (DateTime)     "
	       << FDateS << endl;
	} else {
	  cout << " Invalid Option " << string.Data() << ". !! ABORT !! " << endl;
	  abort(); //assert(1);
	  return;
	}
      }
    } else {
      // string with  "="
      TString substring(string.Data(),in);
      substring.ToLower(); //printf ("Chain %s\n",tChain.Data());
      kgo = kOpt(substring.Data());
      if (kgo > 0) {
	memset(fBFC[kgo].Comment,0,200); // be careful size of Comment
	SetOption(kgo,fBFC[k].Key);
	TString Comment(string.Data()+in+1,string.Capacity()-in-1);
	strcpy (fBFC[kgo].Comment, Comment.Data());
	printf (" Set        %s = %s\n", fBFC[kgo].Key,fBFC[kgo].Comment);
      }
    }
  }
  Opts.Delete();
  if (!GetOption("NoDefault")) {
    // Check flags consistency
    if (!GetOption("NoInput")) {
      if (!GetOption("fzin") && !GetOption("gstar") &&
	  !GetOption("in")  && !GetOption("tdaq")) {
	SetOption("fzin","-NoDefault,-NoInput,-fzin,-gstar,-in,-tdaq");
	SetOption("geant","-NoDefault,-NoInput,-fzin,-gstar,-in,-tdaq");
      }
    }
    else {
      if (GetOption("fzin")) SetOption("-fzin","NoDefault,NoInput");
      if (GetOption("in")) {
	SetOption("-in","-NoDefault,NoInput");
	SetOption("-in","-NoDefault,NoInput");
      }
    }
    if (!(GetOption("fzin") || GetOption("gstar"))) SetOption("magF","-fzin || -gstar");
    if (!GetOption("Eval") && GetOption("AllEvent"))  SetOption("Eval","-Eval && AllEvent");
    //    if (GetOption("NoDb")) SetOption("-db","NoDb");
  }
  // Print set values
  St_Bfc *Bfc = new St_Bfc("BFChain",NoChainOptions);
  AddRunco(Bfc);
  for (k = 1; k<NoChainOptions;k++) {
    if (GetOption(k)) {
      (void) printf("QAInfo: ================== %4d %15s\tis ON \t: %s\n",
		    k, (char *) fBFC[k].Key, (char *) fBFC[k].Comment);
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
    if (gc == "gc:") {SetGC(infile+3); goto SetOut;}
  }
  SetInputFile(infile);
 SetOut:
  SetOutputFile(outfile);
}
//_____________________________________________________________________
void StBFChain::SetGC (const Char_t *queue){
  TString Queue(queue);
  gMessMgr->QAInfo() << "Requested GC queue is :\t" << Queue.Data() << endm;
  TObjArray Opts;
  ParseString(Queue,Opts);
  TIter next(&Opts);
  TObjString *Opt;
  static TString ARGV[40];
  Int_t Argc = -1;
  while ((Opt = (TObjString *) next())) {
    TString string = Opt->GetString();
    const Char_t *argv = string.Data();
    if (argv[0] == '-') {
      switch (argv[1]) {
      case 'o':
      case 'i':
      case 'c':
      case 'q':
      case 's':
      case 'n':
      case 'm':
      case 't':
      case '-': // now do --options, they get added to Config
        ARGV[++Argc] = string.Data();
        Argc++;
	break;
      default :
	gMessMgr->QAInfo() << "Unrecognized option :\t" << string << endm;
	break;
      }
    }
    else if (Argc > 0) {ARGV[Argc] += " "; ARGV[Argc] += string;}
  }
  Opts.Delete();
  fSetFiles = (StFileI *)StChallenger::Challenge();
  fSetFiles->SetDebug();
  Argc++;
  Char_t **Argv = new Char_t* [Argc];
  for (int i=0;i<Argc;i++)  {Argv[i] = (Char_t *) ARGV[i].Data();}
  fSetFiles->Init(Argc,(const Char_t **) Argv);
}
//_____________________________________________________________________
void StBFChain::SetInputFile (const Char_t *infile){
  // define input file
  if (infile) fInFile = new TString(infile);
  if (!GetOption("NoInput") && !GetOption("NoDefault")) {
    if (!fInFile && GetOption("fzin")) {
      fInFile = new TString("/star/rcf/simu/cocktail/hadronic/default/lowdensity/");
      if (GetOption("y1h")) fInFile->Append("year_1h/hadronic_on/Gstardata/rcf0078/hc_lowdensity.400_evts.fz");
      else
	if (GetOption("y2a")) fInFile->Append("year_2a/hadronic_on/Gstardata/rcf0079/hc_lowdensity.400_evts.fz");
	else {printf ("for fzin Option In file has not been defined. Exit!\n"); gSystem->Exit(1);}
      printf ("Use default input file %s for %s \n",fInFile->Data(),"fzin");
    }
  }
  if (fInFile) {
    if (!GetOption("fzin")) {
      fSetFiles= new StFile();
      TObjArray Files;
      ParseString((const TString )*fInFile,Files);
      TIter next(&Files);
      TObjString *File;
      while ((File = (TObjString *) next())) {
	TString string = File->GetString();
	if (!strstr(string.Data(),"*") &&
	    gSystem->AccessPathName(string.Data())) {// file does not exist
	  printf (" *** NO FILE: %s, exit!\n", string.Data());
	  gSystem->Exit(1);
	}
	else fSetFiles->AddFile(File->String().Data());
      }
      Files.Delete();
    }
  }
  if (fInFile) gMessMgr->QAInfo() << "Input file name = " << fInFile->Data() << endm;
}


//_____________________________________________________________________
/// Takes care of output file name (extension)
void StBFChain::SetOutputFile (const Char_t *outfile){
  if (outfile)               fFileOut = new TString(outfile);
  else {
    if (GetOption("gstar"))  fFileOut = new TString("gtrack.root");
    else {
      if (fInFile) {
	if (GetOption("fzin")) {
	  TObjArray words;
	  ParseString(*fInFile,words);
	  TIter nextL(&words);
	  TObjString *word = 0;
	  while ((word = (TObjString *) nextL())) {
	    if (word->GetString().Contains(".fz")) {
	      fFileOut = new TString(gSystem->BaseName(word->GetName()));
	      break;
	    }
	  }
	}
	else fFileOut = new TString(gSystem->BaseName(fInFile->Data()));
	if (  fFileOut) {
	  fFileOut->ReplaceAll("*","");
	  fFileOut->ReplaceAll("..",".");
	  fFileOut->ReplaceAll(".daq","");
	  fFileOut->ReplaceAll(".fzd","");
	  fFileOut->ReplaceAll(".fz","");
	  fFileOut->Strip();
	  fFileOut->Append(".root");
	}
      }
    }
  }
  if (fFileOut)  gMessMgr->QAInfo() << "Output root file name " <<  fFileOut->Data() << endm;
  if (!fTFile) {
    if (GetOption("tags")  && fFileOut ||
	GetOption("lana") ||  GetOption("Laser")) {
      TString TagsName = TString(fFileOut->Data());
      if(GetOption("LaserCal")){
	TagsName.ReplaceAll(".root",".laser.root");
      } else {
	TagsName.ReplaceAll(".root",".tags.root");
      }
      fTFile = new TFile(TagsName.Data(),"RECREATE");
    }
  }
  //    gSystem->Exit(1);
}



//_____________________________________________________________________
/// Handles all geant options
/*!
  This method sets the Geant options that is the Geometry loading
  part. Depends on St_geant_Maker instantiated in the Instantiate()
  method.
  Please, change SetDbOptions()
 */
void StBFChain::SetGeantOptions(){
  if (geantMk) {
    SetInput("geant",".make/geant/.data");

    if (!GetOption("fzin")) {
      if      (GetOption("SD97") ||
	       GetOption("SD98") ||
	       GetOption("Y1a")  ||
	       GetOption("ES99") ||
	       GetOption("ER99") ||
	       GetOption("DC99"))     geantMk->LoadGeometry("detp geometry YEAR_1A");
      else if (GetOption("Y1b"))      geantMk->LoadGeometry("detp geometry YEAR_1B");
      else if (GetOption("Y1E"))      geantMk->LoadGeometry("detp geometry YEAR_1E");
      else if (GetOption("Y1h") ||
	       GetOption("RY1h"))     geantMk->LoadGeometry("detp geometry YEAR_1H");
      else if (GetOption("Y1s"))      geantMk->LoadGeometry("detp geometry YEAR_1S");
      else if (GetOption("Y2000")  ||
	       GetOption("RY2000") ||
	       GetOption("RY2000a"))  geantMk->LoadGeometry("detp geometry year2000");
      else if (GetOption("Y2a"))      geantMk->LoadGeometry("detp geometry YEAR_2A");
      else if (GetOption("Y2001")  ||
	       GetOption("Y2001n") ||
	       GetOption("RY2001"))   geantMk->LoadGeometry("detp geometry year2001");
      else if (GetOption("Y2003")  ||
	       GetOption("RY2003"))   geantMk->LoadGeometry("detp geometry year2003");
      else if (GetOption("Y2003a") ||
	       GetOption("RY2003a"))  geantMk->LoadGeometry("detp geometry y2003a");
      else if (GetOption("Y2003b") ||
	       GetOption("RY2003b"))  geantMk->LoadGeometry("detp geometry y2003b");
      else if (GetOption("Y2003X") ||
	       GetOption("RY2003X"))  geantMk->LoadGeometry("detp geometry y2003x");
      else if (GetOption("Y2004") ||
	       GetOption("RY2004"))   geantMk->LoadGeometry("detp geometry y2004");
      else if (GetOption("Y2004a") ||
	       GetOption("RY2004a"))  geantMk->LoadGeometry("detp geometry y2004a");
      else if (GetOption("Y2004b") ||
	       GetOption("RY2004b"))  geantMk->LoadGeometry("detp geometry y2004b");
      else if (GetOption("Y2004x") ||
	       GetOption("RY2004x"))  geantMk->LoadGeometry("detp geometry y2004x");
      else if (GetOption("Y2004y") ||
	       GetOption("RY2004y"))  geantMk->LoadGeometry("detp geometry y2004y");
      else if (GetOption("Y2004c") ||
	       GetOption("RY2004c"))  geantMk->LoadGeometry("detp geometry y2004c");
      else if (GetOption("Y2005") ||
	       GetOption("RY2005"))   geantMk->LoadGeometry("detp geometry y2005");
                                      // gMessMgr->Error() << "Y2005/RY2005 not yet implemented" << endm;
      else if (GetOption("Y2005x") ||
	       GetOption("RY2005x"))  geantMk->LoadGeometry("detp geometry y2005x");
      else if (GetOption("Y2005b") ||
	       GetOption("RY2005b"))  geantMk->LoadGeometry("detp geometry y2005b");


      else if (GetOption("Y2b"))      geantMk->LoadGeometry("detp geometry YEAR_2b");
      else if (GetOption("Complete")) geantMk->LoadGeometry("detp geometry complete");
      else if (GetOption("Ist1"))     geantMk->LoadGeometry("detp geometry ist1");
      else                            geantMk->LoadGeometry("detp geometry year2001");


      if (GetOption("gstar")) {
	geantMk->Do("subevent 0;");
	// gkine #particles partid ptrange yrange phirange vertexrange
	geantMk->Do("gkine 80 6 1. 1. -4. 4. 0 6.28  0. 0.;");
	geantMk->Do("mode g2tm prin 1;");
	//  geantMk->Do("next;");
	//  geantMk->Do("dcut cave z 1 10 10 0.03 0.03;");
	if (GetOption("Debug") || GetOption("Debug2")) {
	  geantMk->Do("debug on;");
	  if (GetOption("Debug2")) geantMk->Do("swit 2 3;");
	}
      }

    } else {
      if (fInFile) {
	TObjArray words;
	ParseString(*fInFile,words);
	TIter nextL(&words);
	TObjString *word = (TObjString *) nextL();
	if (word->GetString().Contains(".fz")) {
	  if (geantMk->SetInputFile(fInFile->Data()) > kStOK) {
	    printf ("File %s cannot be opened. Exit! \n",fInFile->Data());
	    gSystem->Exit(1);
	  }
	} else {
	  geantMk->Do(fInFile->Data());
	}
      }
    }

  } // geantMk
}


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
void StBFChain::SetDbOptions(){
  Int_t i;
  Int_t Idate=0,Itime=0;

  for (i = 1; i < NoChainOptions; i++) {
    if (fBFC[i].Flag && !strncmp(fBFC[i].Key ,"DbV",3)){
      (void) printf("QAInfo: Found time-stamp %s [%s]\n",fBFC[i].Key,fBFC[i].Comment);
      (void) sscanf(fBFC[i].Comment,"%d/%d",&Idate,&Itime);
    }
  }

  if( ! Idate && FDate){
    (void) printf("QAInfo: Switching to user chosen dynamic time-stamp (MaxEntry) %d %d\n",
		  FDate,FTime);
    (void) printf("QAInfo: Chain may crash if time-stamp is not validated by db interface\n");
    Idate = FDate;
    Itime = FTime;
  }

  StMakerIter nextMaker(this);
  StMaker *maker;
  while ((maker = nextMaker.NextMaker())) {
    if (!strcmp(maker->ClassName(),"St_db_Maker")) {
      St_db_Maker *db = (St_db_Maker *) maker;

      // Startup date over-write
      if (FDateS){
	(void) printf("QAInfo: Switching to user chosen dynamic time-stamp (Start) %d %d\n",
		      FDateS,FTimeS);
	(void) printf("QAInfo: Chain may crash if time-stamp is not validated by db interface\n");
	db->SetDateTime(FDateS,FTimeS);
      } else {
	if (GetOption("SD97"))       db->SetDateTime("sd97");
	else if (GetOption("SD98"))  db->SetDateTime("sd98");
	else if (GetOption("Y1a"))   db->SetDateTime("year_1a");
	else if (GetOption("Y1b"))   db->SetDateTime("year_1b");
	else if (GetOption("Y1s"))   db->SetDateTime("year_1s");
	else if (GetOption("ES99"))  db->SetDateTime("es99");
	else if (GetOption("ER99"))  db->SetDateTime("er99");
	else if (GetOption("DC99"))  db->SetDateTime("dc99");
	else if (GetOption("Y1d"))   db->SetDateTime("year_1d");
	else if (GetOption("Y1e"))   db->SetDateTime("year_1e");
	else if (GetOption("Y1h"))   db->SetDateTime("year_1h");
	else if (GetOption("Y2000")) db->SetDateTime("year_1h");
	else if (GetOption("Y2a"))   db->SetDateTime("year_2a");
	else if (GetOption("Y2b"))   db->SetDateTime("year_2b");
	else if (GetOption("Y2001")) db->SetDateTime("year_2b");
	// Year_2b ** db ** timestamp does not reflect
	// svt shift. Small hack to make it work.
	else if (GetOption("Y2001n"))db->SetDateTime("year2001");
	else if (GetOption("Y2003")) db->SetDateTime("year2003");
	else if (GetOption("Y2003a"))db->SetDateTime("y2003a");
	else if (GetOption("Y2003b"))db->SetDateTime("y2003b");
	else if (GetOption("Y2003X"))db->SetDateTime("y2003x");
	else if (GetOption("Y2004")) db->SetDateTime("y2004");
	else if (GetOption("Y2004a"))db->SetDateTime("y2004a");
	else if (GetOption("Y2004b"))db->SetDateTime("y2004b");
	else if (GetOption("Y2004x"))db->SetDateTime("y2004x");
	else if (GetOption("Y2004y"))db->SetDateTime("y2004x"); // same timestamp used
	else if (GetOption("Y2004c"))db->SetDateTime("y2004c");
	else if (GetOption("Y2005")) db->SetDateTime("y2005");
	else if (GetOption("Y2005x"))db->SetDateTime("y2005x");
	else if (GetOption("Y2005b"))db->SetDateTime("y2005b");
	else gMessMgr->QAInfo() << "StBFChain::SetDbOptions() Chain has not set a time-stamp" << endm;
      }


      // Show date settings
      gMessMgr->QAInfo() << db->GetName()
			 << " Maker set time = "
			 << db->GetDateTime().GetDate() << "."
			 << db->GetDateTime().GetTime() << endm;

      // MaxEntry over-write
      if (Idate) {
	db->SetMaxEntryTime(Idate,Itime);
	gMessMgr->Info() << "\tSet DataBase max entry time " << Idate << "/" << Itime
			 << " for St_db_Maker(\"" << db->GetName() <<"\")" << endm;
      }
    } // check if maker is St_db_Maker
  }   // loop over makers
}
//_____________________________________________________________________
/// Creates output-tree branches
void StBFChain::SetTreeOptions()
{
  StTreeMaker *treeMk = (StTreeMaker *) GetMaker("outputStream");
  if (!treeMk) return;
  treeMk->SetBranch("histBranch");
  if (GetOption("dstOut"))      {
    treeMk->IntoBranch("dstBranch","dst");
    if (GetOption("HitsBranch")) {
      treeMk->SetBranch("dstHitsBranch");
      treeMk->IntoBranch("dstHitsBranch","dst/.data/Hits");
    }
    else treeMk->IntoBranch("dstBranch","dst/.data/Hits");
    treeMk->IntoBranch("dstBranch","dst/.data/dst");
    treeMk->SetBranch("runcoBranch");
  }
  if (GetOption("Event") && GetOption("EvOut")){
    cout << "Will Write StEvent out, treeMk->GetFile() = "  << treeMk->GetFile() << endl;
    treeMk->IntoBranch("eventBranch","StEvent");
  }
  if (GetOption("AllEvent")) {
    if (geantMk) {
      treeMk->IntoBranch("geantBranch","geant");
      treeMk->IntoBranch("geantBranch","geant/.data/particle");
      treeMk->IntoBranch("geantBranch","geant/.data/g2t_rch_hit");
    }
    if (GetOption("fss"))    treeMk->IntoBranch("ftpc_rawBranch","ftpc_raw/.data");
    if (GetOption("tpc_daq") || GetOption("TrsMini"))
                             treeMk->IntoBranch("tpc_rawBranch","tpc_raw/.data");
    if (GetOption("ems"))    treeMk->IntoBranch("emc_rawBranch","emc_raw/.data");
    if (GetOption("tcl"))    treeMk->IntoBranch("tpc_hitsBranch","tpc_hits/.data");
    if (GetOption("fcf"))    treeMk->IntoBranch("tpc_hitsBranch","tpc_hits/.data");
    if (GetOption("tpt"))    treeMk->IntoBranch("tpc_tracksBranch","tpc_tracks/.data");
    if (GetOption("srs"))    treeMk->IntoBranch("svt_hitsBranch","svt_hits/.data");
    if (GetOption("stk"))    treeMk->IntoBranch("svt_tracksBranch","svt_tracks/.data");
    if (GetOption("trg"))    treeMk->IntoBranch("trgBranch","ctf mwc trg");
    if (GetOption("global")) treeMk->IntoBranch("globalBranch","global/.data");
  }
  else if (GetOption("GeantOut") && geantMk) treeMk->IntoBranch("geantBranch","geant");
}
