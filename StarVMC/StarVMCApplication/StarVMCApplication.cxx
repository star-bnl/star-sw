// $Id: StarVMCApplication.cxx,v 1.13 2013/12/16 22:58:53 fisyak Exp $
// Class StarVMCApplication
// ----------------------- 
// Implementation of the TVirtualMCApplication
#include <assert.h>
#include "StEnumerations.h"
#include "StarVMCApplication.h"
#include "StarMCHits.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"
#include "TGeoPhysicalNode.h"
#include "TGeoParallelWorld.h"
#include "TROOT.h"
#include "TMath.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "TVector3.h"
#include "TSystem.h"
#include "TInterpreter.h"
#include "TVirtualMC.h"
#include "TPDGCode.h"
#include "TApplication.h"
#include "TGeant3TGeo.h"
#include "TObjString.h"
#include "TDataSetIter.h"
#include "StChain.h"
#include "tables/St_svtWafersPosition_Table.h"
#include "tables/St_ssdWafersPosition_Table.h"
#include "tables/St_Survey_Table.h"
#include "TBenchmark.h"
#include "TEnv.h"
#include "TGeoShapeAssembly.h"
// Alignment
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "StDetectorDbMaker/St_tpcGlobalPositionC.h"
#include "StDetectorDbMaker/St_tpcSectorPositionC.h"
#include "StDetectorDbMaker/StSsdSurveyC.h"
#include "StDetectorDbMaker/StSvtSurveyC.h"
#include "StSvtDbMaker/StSvtDbMaker.h"
#include "StPxlDbMaker/StPxlDb.h"
#include "StSsdDbMaker/StSsdDbMaker.h"
#include "StDetectorDbMaker/StTpcSurveyC.h"
#include "StDetectorDbMaker/StPxlSurveyC.h"
#include "StDetectorDbMaker/StIstSurveyC.h"
#include "StDetectorDbMaker/StSstSurveyC.h"
#include "StMessMgr.h"
#include "StTpcDb/StTpcDb.h"
#include "StIstDbMaker/StIstDb.h"
#include "StSsdDbMaker/StSstDbMaker.h"
#include "StSstUtil/StSstBarrel.hh"
#include "StarGenerator/BASE/StarPrimaryMaker.h"
#include "Stypes.h"
#include "TGraph.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "StMaker.h"
TableClassImpl(St_VMCPath2Detector,VMCPath2Detector_st);
ClassImp(StarVMCApplication);
#define PrPV(B)      if (Debug())                {std::cout << (#B) << " = \t"; (B).Print();} 
static const TString separator("/_"); 
Bool_t StarVMCApplication::flux = kFALSE;
TGeant3TGeo *StarVMCApplication::fgGeant3 = 0;
//_____________________________________________________________________________
StarVMCApplication::StarVMCApplication(const char *name, const char *title) : 
  TVirtualMCApplication(name,title),
  fStarStack(0),
  fPrimaryGenerator(0),
  fMcHits(0),
  fDebug(0),
  fAlignment(kTRUE),
  fAlignmentDone(kFALSE) {
  // Standard constructor
  TString program(gSystem->BaseName(gROOT->GetApplication()->Argv(0)));
  assert (! program.BeginsWith("root4star"));
  // Create a user stack
  fStarStack = new StarStack(100); 
}
//_____________________________________________________________________________
StarVMCApplication::~StarVMCApplication() {  // Destructor  
  delete fStarStack;
  //  SafeDelete(TVirtualMC::GetMC());
}
//_____________________________________________________________________________
void StarVMCApplication::InitMC(const Char_t *setup) {  // Initialize MC.
  if (setup && TString(setup) != "" && gSystem->AccessPathName(setup,kReadPermission)) {
    gROOT->LoadMacro(setup);
    gInterpreter->ProcessLine("Config()");
  }
  TVirtualMC::GetMC()->SetStack(fStarStack);
  TVirtualMC::GetMC()->Init();
  TVirtualMC::GetMC()->BuildPhysics(); 
  //  MisalignGeometry(); // Called from TGeant3TGeo::FinishGeometry
}
//_____________________________________________________________________________
Bool_t StarVMCApplication::RunMC(Int_t nofEvents) {    // MC run.
  Bool_t ok = TVirtualMC::GetMC()->ProcessRun(nofEvents);
  if (! ok) FinishRun();
  return ok;
}
//_____________________________________________________________________________
void StarVMCApplication::FinishRun() {    // Finish MC run.
}
//_____________________________________________________________________________
void StarVMCApplication::ConstructGeometry() {    // Initialize geometry
  InitGeometry();
}
//_____________________________________________________________________________
void StarVMCApplication::InitGeometry() {    
  if (TVirtualMC::GetMC()->IsA()->InheritsFrom("TGeant3")) {
    assert(gGeoManager); 
    TVirtualMC::GetMC()->SetRootGeometry();
  }  
  if (TVirtualMC::GetMC()->IsA()->InheritsFrom("TGeant3TGeo")) {
    fgGeant3 = (TGeant3TGeo *)TVirtualMC::GetMC();
    if (Debug()) {
      fgGeant3->Gprint("mate");
      fgGeant3->Gprint("tmed");
    }
  }
  if (fMcHits) fMcHits->Init();
}
//_____________________________________________________________________________
void StarVMCApplication::GeneratePrimaries() {    
  if (! StarMCPrimaryGenerator::Instance()) new StarMCPrimaryGenerator(fStarStack);
  fStatus = kStOK;
  if (StarPrimaryMaker::instance()) {
    // put TParticle from StarParticleStack from StarPrimaryMaker to StarStack
    StarMCPrimaryGenerator::Instance()->SetStack(StarPrimaryMaker::instance()->stack());
  } else {
    StarMCPrimaryGenerator::Instance()->SetStack(fStarStack);
    StarMCPrimaryGenerator::Instance()->GeneratePrimaries();
    fStatus = StarMCPrimaryGenerator::Instance()->Status();
  } 
  if (fStatus == kStEOF) {TVirtualMC::GetMC()->StopRun();}
  if (fMcHits) {
    fMcHits->BeginEvent();
  }
  return;;
}
//_____________________________________________________________________________
void StarVMCApplication::BeginEvent() {    // User actions at beginning of event
  fStarStack->Reset();
}
//_____________________________________________________________________________
void StarVMCApplication::BeginPrimary() {    // User actions at beginning of a primary track
  if (fMcHits) fMcHits->BeginPrimary();
}
//_____________________________________________________________________________
void StarVMCApplication::PreTrack() {    // User actions at beginning of each track
  if (fMcHits) fMcHits->PreTrack();  
}
//_____________________________________________________________________________
void StarVMCApplication::Stepping() {    // User actions at each step
  if (flux) usflux();
  else if (fMcHits) fMcHits->Step();
}
//_____________________________________________________________________________
void StarVMCApplication::PostTrack() {    // User actions after finishing of each track
#if 0
  Fatal("StarVMCApplication::PostTrack","Is not implemented");
  // delete stack only track
  TParticle *current =  0; // fStarStack->GetCurrentParticle();
  TObjArray *objs = fStarStack->GetParticles();
  if (objs->IndexOf(current) < objs->LowerBound()) delete current;
#else
  if (fMcHits) fMcHits->PostTrack();
#endif
}
//_____________________________________________________________________________
void StarVMCApplication::FinishPrimary() {    // User actions after finishing of a primary track
  if (fMcHits) fMcHits->FinishPrimary();
}
//_____________________________________________________________________________
void StarVMCApplication::FinishEvent() {    // User actions after finishing of an event
  if (TString(TVirtualMC::GetMC()->GetName()) == "TGeant3") {
    // add scale (1.4)
  }  
  if (Debug()) {
    fStarStack->Print();
  }
  if (fMcHits) fMcHits->FinishEvent(); // add kine info
} 
//________________________________________________________________________________
Bool_t StarVMCApplication::MisalignGeometry() {
#if 0
  if (! fAlignment || St_tpcPadConfigC::instance()->numberOfRows(20) != 45) {
    cout << "No MisalignGeometry has been applied" << endl;
    if (St_tpcPadConfigC::instance()->numberOfRows(20) != 45) 
      cout << "Alignmnet for iTPC configuration has not been implemented yet." << endl;
    return fAlignmentDone;
  } 
#endif
  //  TGeoParallelWorld *pw = 
  gGeoManager->CreateParallelWorld("priority_sensors");
  //  SetDebug(3);
  // Misalignment introduced after 2013 TPC survey
  /*
1       HALL[1]/CAVE[1]/TpcRefSys[1]/IDSM[1]/SFMO[1]/SFLM[20]/SFSW[16]/SFSL[1]/SFSD[1]
2       HALL[1]/CAVE[1]/TpcRefSys[1]/IDSM[1]/IBMO[1]/IBAM[24]/IBLM[6]/IBSS[1]
3       HALL[1]/CAVE[1]/TpcRefSys[1]/IDSM[1]/PXMO[1]/PXLA[10]/LADR[4]/PXSI[10]/PLAC[1]
4       HALL[1]/CAVE[1]/TpcRefSys[1]/TPCE[1]/TPGV[2]/TPSS[12]/TPAD[73]
        HALL[1]/CAVE[1]/TpcRefSys[1]/TPCE[1]/TPGV[2]/TPSS[12]/TPAD[76] with iTPC
5       HALL[1]/CAVE[1]/TpcRefSys[1]/BTOF[1]/BTOH[2]/BSEC[60]/BTRA[1]/BXTR[1]/BRTC[1]/BGMT[1]/BRMD[32]/BRDT[1]/BRSG[6]
6       HALL[1]/CAVE[1]/TpcRefSys[1]/BTOF[1]/BTOH[2]/BSEC[48]/BTRA[1]/BXTR[1]/BRTC[1]/BGMT[1]/GMTS[2]/GSBE[1]/GEMG[1]
7       HALL[1]/CAVE[1]/VPDD[2]/VRNG[1]/VDET[19]/VDTI[1]/VCNV[1]/VRAD[1]
8       HALL[1]/CAVE[1]/CALB[1]/CHLV[2]/CPHI[60]/CSUP[2]/CSCI[19]
9       HALL[1]/CAVE[1]/CALB[1]/CHLV[2]/CPHI[60]/CSUP[2]/CSMD[1]/CSDA[4]/CSME[30]/CSHI[2]
10      HALL[1]/CAVE[1]/ECAL[1]/EAGA[2]/EMSS[1]/ECVO[2]/EMOD[6]/ESEC[3]/EMGT[17]/EPER[5]/ETAR[12]/ESCI[1]
11      HALL[1]/CAVE[1]/ECAL[1]/EAGA[2]/EMSS[1]/ESHM[1]/ESPL[3]/EXSG[6]/EHMS[288]
12      HALL[1]/CAVE[1]/BBCM[2]/BBCA[2]/THXM[6]/SHXT[3]/BPOL[1]
13      HALL[1]/CAVE[1]/ZCAL[2]/QCAL[1]/QDIV[260]/QSCI[1]
14      HALL[1]/CAVE[1]/MUTD[1]/MTTG[28]/MTRA[5]/MIGS[1]/MIGG[5]
15      HALL[1]/CAVE[1]/FBOX[4]/FTOW[238]/FWAL[1]/FLGR[1]
16      HALL[1]/CAVE[1]/FBOX[4]/FTOW[238]/FPCT[1]
17      HALL[1]/CAVE[1]/FBOX[2]/FSHM[1]/FHMS[100]
18      HALL[1]/CAVE[1]/FBOX[4]/FLXF[394]
19      HALL[1]/CAVE[1]/FPRS[1]/FPLY[4]/FPSC[80]
  y2018
 1    HALL[1]/CAVE[1]/TpcRefSys[1]/TPCE[1]/TPGV[2]/TPSS[12]/TPAD[76]
   */
  Int_t NoOfInnerRows = -1;
  Int_t NoOfRows = -1;
  enum EDetector2Align {
    kUnknown,
    kTPC, kTpcRefSys, kTpcHalf, kTpcSecInn, kTpcSecOut, kTpcPad, kTpcLast,
    kHft, 
    kPxl, kPxlSector, kPxLadder, kPxlWafer, kPxlSensor,
    kIst, kIstLadder, kIstWafer, kIstSensor,
    kSst, kSstLadder, kSstWafer, kSstSensor,
    kTof, kTofTray, kTofModule, kTofCell,
    kGMT, kGMTModule
  };
  struct listOfDetectorToAlign_t {
    const Char_t *Name; 
    EDetector2Align kDet;
    const Char_t *path;
    Int_t  Ndim;
    Int_t  NVmax[3];
    St_SurveyC *chair;
  };
  static const listOfDetectorToAlign_t listOfDet2Align[] = {                                                                
    {"TpcRefSys-%d",   kTpcRefSys,"/HALL_1/CAVE_1/TpcRefSys_%d",                                       	 1, { 1, 0, 0}, 0},    
    {"TpcHalf-%d",     kTpcHalf  ,"/HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TPGV_%d",         	       	       	 1, { 2, 0, 0}, 0},  
    {"TpcPad-%02d",       kTpcPad,"/HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TPGV_%d/TPSS_%d/TPAD_%d",	       	 3, { 2,12,76}, 0}, // 73 -> TPC; 76 -> iTPC
    {"Hft-%d",               kHft,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_%d",                                	 1, { 1, 0, 0}, 0},
    {"Pixel-%d",             kPxl,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_%d",                        	 1, { 1, 0, 0}, 0},
    {"PxlSector-%d",   kPxlSector,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1/PXLA_%d",                 	 1, {10, 0, 0}, 0},
    {"PxLadder-%d",     kPxLadder,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1/PXLA_%d/LADR_%d",         	 2, {10, 4, 0}, 0},
    //    {"PxlWafer-%d",   kPxlWafer,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1/PXLA_%d/LADR_%d/LADX_%d/PXSI_1",       3, {10, 4,10}, 0},
    {"PxlWafer-%d",   kPxlWafer,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1/PXLA_%d/LADR_%d/LADX_%d",       3, {10, 4,10}, 0},
    {"PxlSensor-%d",kPxlSensor, "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1/PXLA_%d/LADR_%d/LADX_%d/PXSI_1/PLAC_1",3, {10, 4,10}, 0},
    {"Ist-%d",             kIst,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_%d",                             1, { 1, 0, 0}, 0},
    {"IstLadder-%d", kIstLadder,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1/IBAM_%d",                      1, {24, 0, 0}, 0},
    {"IstWafer-%d",   kIstWafer,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1/IBAM_%d/IBLM_%d",              2, {24, 6, 0}, 0},
    {"IstSensor-%d", kIstSensor,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1/IBAM_%d/IBLM_%d/IBSS_1",       2, {24, 6, 0}, 0},
    {"Sst-%d",             kSst,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SFMO_%d",                             1, { 1, 0, 0}, 0},
    {"SstLadder-%d", kSstLadder,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SFMO_1/SFLM_%d",                      1, {20, 0, 0}, 0},
    {"SstWafer-%d", kSstWafer,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SFMO_1/SFLM_%d/SFSW_%d",                2, {20,16, 0}, 0},
    {"SstSensor-%d", kSstSensor,"/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SFMO_1/SFLM_%d/SFSW_%d/SFSL_1/SFSD_1",2, {20,16, 0}, 0}
  };
  static const Int_t  NoDetectos2Align = sizeof(listOfDet2Align)/sizeof(listOfDetectorToAlign_t);
  static Int_t iBreak = 0;
  TGeoHMatrix I("Indentity");
  I.SetRotation(kIdentityMatrix);
  I.SetTranslation(kNullVector);
  TGeoNode *nodeT = 0;
  TGeoHMatrix PLAC, PXSI;
  TString path2PLAC("/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1/PXLA_1/LADR_1/LADX_1/PXSI_1/PLAC_1");
  if (gGeoManager->CheckPath(path2PLAC)) {
    gGeoManager->cd(path2PLAC);
    nodeT = gGeoManager->GetCurrentNode();
    PLAC = *(nodeT->GetMatrix());
    gGeoManager->cd(gSystem->DirName(path2PLAC));
    nodeT = gGeoManager->GetCurrentNode();
    PXSI = *(nodeT->GetMatrix());
  }
  TGeoHMatrix PixelWaferT = PXSI * PLAC; PrPV(PixelWaferT);
  TGeoTranslation PixelLadderT(-0.2381, 0.000010, -4.8750);//   PXLA->AddNode(LADR,4,new TGeoCombiTrans(-4.51636,6.93489,-4.875,rot));
  //  TGeoTranslation PixelSensorT(0.1533,0.1300000E-02,0.1604000E-01);//                        PXSI->AddNode(PLAC,1,new TGeoTranslation(0.1533,0.1300000E-02,0.1604000E-01));
  //  TGeoTranslation PixelSensorX(0.0163-0.0326, -0.0037+0.00745, -0.01604+0.03208); // Extra ?
  Double_t tIstLadder[3] = {-3.1253  , 13.6559,  -4.4929}; 
  Double_t rIstLadder[9] = {-0.944925,  0.327287, 0, -0.327287, -0.944925, 0, 0, 0, 1};
  TGeoHMatrix IstLadderH;
  IstLadderH.SetTranslation(tIstLadder);
  IstLadderH.SetRotation(rIstLadder);
  Double_t tIstWafer[3] = {0.487351, 0, 0};
  Double_t rIstWafer[9] = {-1, 0, 0, 0,-1, 0, 0, 0, 1};
  TGeoHMatrix IstWaferH;
  IstWaferH.SetTranslation(tIstWafer);
  IstWaferH.SetRotation(rIstWafer);
  Double_t tSstLadder[3] = {-2.71769, 22.1338, 0};
  Double_t rSstLadder[9] = {-0.992546, 0.121869, 0, -0.121869, -0.992546, 0, 0, 0, 1};
  TGeoHMatrix SstLadderH;
  SstLadderH.SetTranslation(tSstLadder);
  SstLadderH.SetRotation(rSstLadder);
  TGeoHMatrix SstSensorR;
  Double_t rSstSensor[9] = {-1, 0, 0, 0, -1, 0, 0, 0, 1}; 
  SstSensorR.SetRotation(rSstSensor);
  TGeoHMatrix Tpc2Global, HftOnTpc, PxlOnHft, SectorOnPxl, LadderOnSector, LadderOnPxl, SensorOnLadder, SensorOnGlobal;
  TGeoHMatrix temp, oscOnTpc, sstOnOsc, sstLadderOnSst, sstSensorOnLadder;
  for (Int_t i = 0; i < NoDetectos2Align; i++) {
    EDetector2Align kDetector = listOfDet2Align[i].kDet;
    Int_t NoPerfMatch = 0;
    Int_t Ntot = 1;
    for (Int_t k = 0; k < listOfDet2Align[i].Ndim; k++) Ntot *= listOfDet2Align[i].NVmax[k];
    Double_t NN = 0, NNG = 0;
    TArrayD Xyz(12), Xyz2(12), XyzG(12), Xyz2G(12);
    for (Int_t j = 0; j < Ntot; j++) {
      Int_t ind = j;
      TArrayI Indx(listOfDet2Align[i].Ndim); Int_t *indx = Indx.GetArray();
      for (Int_t k =  listOfDet2Align[i].Ndim - 1; k >= 0; k--) {
	indx[k] = ind%listOfDet2Align[i].NVmax[k]+1; ind /= listOfDet2Align[i].NVmax[k];
      }
      TString path(StarVMCDetector::FormPath(listOfDet2Align[i].path,listOfDet2Align[i].Ndim,indx));
      TString pathA(gSystem->BaseName(path));
      if (! gGeoManager->CheckPath(path)) continue;
      TGeoPNEntry *pnEntry = gGeoManager->GetAlignableEntry(path);
      if (pnEntry) continue; 
      pnEntry = gGeoManager->SetAlignableEntry(path, path); // use pash as name
      TGeoPhysicalNode *nodeP = pnEntry->GetPhysicalNode();
      if (! nodeP) {
	nodeP = gGeoManager->MakePhysicalNode(path);
	pnEntry->SetPhysicalNode(nodeP);
      }
      TGeoHMatrix rotL(*(nodeP->GetNode()->GetMatrix())); // ideal matrix before alignment
      TGeoHMatrix rotA = rotL; // After alignment
      TGeoHMatrix A, B, C, D, E, AB, BC;
      TGeoHMatrix *rotm = 0;
      //      TGeoShapeAssembly *shape = 0;
      //      TGeoVolumeAssembly *volume = 0;
      Int_t Id = -1;
      Int_t ID = -1;
      Int_t half   = -1; // Tpc half
      Int_t sector = -1; // Tpc sector;
      Int_t row    = -1; // Tpc pad row
      Int_t ladder = -1;
      Int_t sensor = -1;
      Int_t NLevel = nodeP->GetLevel();
      //      Int_t level  = nodeP->GetLevel();
      //      TGeoNode *mother = nodeP->GetMother();
      //      TGeoVolume *motherV = mother->GetVolume();
      //      TGeoVolume *motherVV = mother->GetMotherVolume();
      Id = Ntot;
      St_SurveyC *chair = 0;
      TGeoPhysicalNode *nodeU = 0;
      switch (kDetector) {
      case kTpcRefSys:
	Tpc2Global = StTpcPosition::instance()->GetMatrix();
	rotA = Tpc2Global; 
	rotA.SetName(Form(listOfDet2Align[i].Name,Id));
	break;
      case kTpcHalf: 
	if (indx[0] == 1) half = west;
	if (indx[0] == 2) half = east;
	Id = indx[0];
	A = StTpcHalfPosition::instance()->GetMatrix4Id(half);
	rotA = A * rotL;
	//?	rotA = rotL * A;
	rotA.SetName(Form(listOfDet2Align[i].Name,Id));
	break;
      case kTpcPad:
	sector = 12*(indx[0]-1) + indx[1];
	NoOfInnerRows = St_tpcPadConfigC::instance()->innerPadRows(sector);
	NoOfRows = St_tpcPadConfigC::instance()->numberOfRows(sector);
	if (listOfDet2Align[i].Ndim == 3) {
	  if (NoOfInnerRows == 13) {// listOfDet2Align[i].NVmax[2] == 73) { // old Tpc
	    if (indx[2] <= 39) row = (indx[2]-1)/3 + 1;
	    else               row = 14 + (indx[2]-40);
	    if (row > 45)      row = 45;
	  } else if (NoOfInnerRows == 40) {// listOfDet2Align[i].NVmax[2] == 76) { // iTpc
	    if (indx[2] <= NoOfInnerRows + 2) {
	      row = indx[2] - 1; 
	      if (row < 1) row = 1; 
	      if (row > NoOfInnerRows) row = NoOfInnerRows;
	    } else {
	      row = indx[2] - 2;
	      if (row < NoOfInnerRows + 1) row = NoOfInnerRows + 1;
	      if (row > NoOfRows) row = NoOfRows;
		}
	  } else {
	    assert(0);
	  }
	}
	if (row <= NoOfInnerRows) chair = StTpcInnerSectorPosition::instance();
	else                      chair = StTpcOuterSectorPosition::instance();
	A = chair->GetMatrix4Id(sector);
	rotA = A * rotL;
	rotA.SetName(Form(listOfDet2Align[i].Name,sector,row));
	break;
	/*
	  kHft:             : HftOnTpc = StidsOnTpc
	  PXMO -> kHft      : PxlOnHft = StPxlpstOnIds * StpxlOnPst
	  PXLA -> kPxlSector: PXLA       
	  LADR -> kPxLadder : PXLA^-1 * StpxlHalfOnPxl * StpxlwafertSectorOnHalf * StpxlLadderOnSector * PixelLadderT
	  LADX 
	  PXSI -> kPxlWafer : PSXI 
	  PLAC -> kPxlSensor: (PixelLadderT * PXSI)^-1 * StpxlSensorOnLadder
Pxl: SensorOnGlobal[sector]ladder[sensor] = TpcOnGlobal * IdsOnTpc * PstOnIds * PxlOnPst * HalfOnPxl[sector / 5] * SectorOnHalf[sector] * 
		                             LadderOnSector[sector]ladder * SensorOnLadder[sector]ladder[sensor];
         					     
	*/
      case kHft:  // IDSM
	HftOnTpc = StidsOnTpc::instance()->GetMatrix(0);
	rotA = HftOnTpc;
	rotA.SetName(Form(listOfDet2Align[i].Name,Id));
	break;
      case kPxl:  // PXMO
	A = StPxlpstOnIds::instance()->GetMatrix();
	B = StpxlOnPst::instance()->GetMatrix();
	PxlOnHft = A * B;
	rotA = PxlOnHft;
	rotA.SetName(Form(listOfDet2Align[i].Name,indx[0]));
	break;
      case kPxlSector: // PXLA
	sector = indx[0];
	half   = (sector-1)/5 + 1; 
	B = StpxlHalfOnPxl::instance()->GetMatrix4Id(half); PrPV(B);
	C = StpxlSectorOnHalf::instance()->GetMatrix4Id(sector); PrPV(C);
	//	D = rotL; PrPV(D);// * PixelLadderT;
        rotA = B * C * rotL; PrPV(rotL); PrPV(rotA);	
	rotA.SetName(Form(listOfDet2Align[i].Name,sector));
	break;
      case kPxLadder: // LADR
	sector = indx[0];
	ladder = indx[1];
	Id = 4*(sector-1) + ladder;
	LadderOnSector = StpxlLadderOnSector::instance()->GetMatrix4Id(Id) * PixelLadderT; PrPV(LadderOnSector);
	nodeU = (TGeoPhysicalNode *) gGeoManager->GetListOfPhysicalNodes()->FindObject(gSystem->DirName(path)); 
	A = nodeU->GetOriginalMatrix();  PrPV(A);
	rotA = A.Inverse() * LadderOnSector; PrPV(rotA);
	rotA.SetName(Form(listOfDet2Align[i].Name,sector,ladder));
	break;
      case kPxlWafer: // LADX
	sector = indx[0];
	ladder = indx[1];
	sensor = indx[2];
	Id = sensor + 10*(ladder+4*(sector-1) - 1);
	//	rotA = rotL;
	SensorOnLadder = StpxlSensorOnLadder::instance()->GetMatrix4Id(Id); PrPV(SensorOnLadder);
	rotA = PixelLadderT.Inverse() * SensorOnLadder * PixelWaferT.Inverse();
	rotA.SetName(Form(listOfDet2Align[i].Name,Id));
	break;
      case kPxlSensor: // PLAC
	sector = indx[0];
	ladder = indx[1];
	sensor = indx[2];
	Id = sensor + 10*(ladder+4*(sector-1) - 1);
	rotA = rotL;
	rotA.SetName(Form(listOfDet2Align[i].Name,Id));
	break;
	/*
Ist: SensorGlobal = TpcOnGlobal * IdsOnTpc * PstOnIds * IstOnPst * LadderOnIst[ladder - 1] * SensorOnLadder[ladder - 1][sensor - 1];
	 */
      case kIst:
	A = StpstOnIds::instance()->GetMatrix(0);
	B = StistOnPst::instance()->GetMatrix(0);
	rotA = A * B  * rotL;
	break;
      case kIstLadder:
	ladder = indx[0];
	Id = ladder;
	A = StLadderOnIst::instance()->GetMatrix4Id(Id);
	rotA   = A * IstLadderH;
	rotA.SetName(Form(listOfDet2Align[i].Name,Id));
	break;
      case kIstWafer:
	ladder = indx[0]; // 1-24
	sensor = indx[1]; // 1-6
	Id = 1000 + sensor + 6*(ladder - 1);
	B = StistSensorOnLadder::instance()->GetMatrix4Id(Id) * IstWaferH.Inverse();
	rotA = IstLadderH.Inverse() * B;
	rotA.SetName(Form(listOfDet2Align[i].Name,Id));
	break;
      case kIstSensor:
	ladder = indx[0]; // 1-24
	sensor = indx[1]; // 1-6
	Id = 1000 + sensor + 6*(ladder - 1);
	rotA = rotL; 
	break;
      case kSst:
	// Yi Guo: tpc2global * oscOnTpc *sstOnOsc * sstLadderOnSst * sstSensorOnLadder
	// Xin: tpc2global * sstOnOsc * sstLadderOnSst * sstSensorOnLadder
	//                   ????????
	// WG = Tpc2Global * SG       * LS             * WLL
	// SG = oscOnGlobal
	// LS = sstLadderOnSst
	// WLL = sensorOnLadder
	// 
	//    0      1           2      3                            4                          5               6     7       8
	// NL-8     -7          -6     -5                           -4                         -3              -2    -1       0     
	//                           kHft                         kSst                 kSstLadder kSstWafer           kSstSensor
	// HALL * CAVE * TpcRefSys * IDSM                       * SFMO *           SFLM          * SFSW  * SFSL * SFSD
	//              tpc2global * idsOnTpc * idsOnTpc^-1 * sstOnOsc * sstLadderOnSst * H      * H^1                * sstSensorOnLadder
	A = nodeP->GetNode(NLevel-1)->GetMatrix()->Inverse();
	oscOnTpc = StoscOnTpc::instance()->GetMatrix(0);
	sstOnOsc = StsstOnOsc::instance()->GetMatrix(0);
	rotA = A * oscOnTpc * sstOnOsc;
	break;
      case kSstLadder:
	ladder = indx[0];
	Id = 100 + ladder;
	B = StsstLadderOnSst::instance()->GetMatrix4Id(Id);
	rotA   = B * SstLadderH;
	rotA.SetName(Form(listOfDet2Align[i].Name,Id));
	break;
      case kSstWafer:
	ladder = indx[0];
	sensor = indx[1];
	ID     = 7000 + ladder + 100*sensor;
	B = StsstSensorOnLadder::instance()->GetMatrix4Id(ID);
	C = *nodeP->GetNode(NLevel-1)->GetMatrix();
	rotA   = SstLadderH.Inverse() * B * SstSensorR;
	rotA.SetName(Form(listOfDet2Align[i].Name,ID));
	break;
      case kSstSensor:
	ladder = indx[0];
	sensor = indx[1];
	rotA = rotL;
	break;
      default:
	assert(0);
	break;
      }
      D = rotA.Inverse() * rotL;
      if (D == I) {NoPerfMatch++;}
      else {
	rotm = new TGeoHMatrix(rotA);
	St_SurveyC::Normalize(*rotm);
	if (Debug()) {
	  if (Debug() > 1) {
	    cout << "Id : " << Id << "\tBefore\t" << nodeP->GetName() << "\t"; nodeP->GetMatrix()->Print();
	    //	if (Debug() > 1) cout << "Old Local:\t"; nodeP->GetNode()->GetMatrix()->Print();
	    if (Debug() > 2) nodeP->Print();
	  }
	  cout << "========================================" << endl;
	  cout << "rotL:"; rotL.Print();
	  cout << "rotm:"; rotm->Print();
	  cout << "----------------------------------------" << endl;
	  D.Print();
	  cout << "========================================" << endl;
	  Double_t *tran = D.GetTranslation();
	  Double_t *r    = D.GetRotationMatrix();
	  // Check Ideal case
	  NN++;
	  for (Int_t l = 0; l < 3; l++) {
	    Xyz[l]  += tran[l];
	    Xyz2[l] += tran[l]*tran[l];
	  }
	  for (Int_t l = 0; l < 9; l++) {
	    Xyz[l+3]  += r[l];
	    Xyz2[l+3] += r[l]*r[l];
	  }
	}
	nodeP->Align(rotm);
	gGeoManager->GetParallelWorld()->AddNode(nodeP->GetName());
	if (Debug() > 1) {
	  cout << "Id : " << Id << "\tAfter\t" << nodeP->GetName() << "\t"; nodeP->GetMatrix()->Print();
	  if (Debug() > 2) {
	    nodeP->Print();
#define __CHECK_NODE__
#ifdef __CHECK_NODE__
	    Int_t check = 1;
	    TGeoHMatrix *comb = 0;
	    TGeoHMatrix temp;
	    Int_t matIst = -1;
	    // Check node
	    switch (kDetector) {
	    case kPxLadder:
	      Tpc2Global = StTpcPosition::instance()->GetMatrix();	    PrPV(Tpc2Global);
	      HftOnTpc = StidsOnTpc::instance()->GetMatrix(0);	    PrPV(HftOnTpc);
	      A        = StPxlpstOnIds::instance()->GetMatrix();    PrPV(A);
	      B        = StpxlOnPst::instance()->GetMatrix();	    PrPV(B);
	      PxlOnHft = A * B;                                     PrPV(PxlOnHft);
	      SectorOnPxl = StpxlHalfOnPxl::instance()->GetMatrix4Id((sector-1)/5+1)
		* StpxlSectorOnHalf::instance()->GetMatrix4Id(sector);	    PrPV(SectorOnPxl);
	      LadderOnSector = StpxlLadderOnSector::instance()->GetMatrix4Id(4*(sector-1) + ladder) * PixelLadderT; 	  PrPV(LadderOnSector);
	      temp = Tpc2Global * HftOnTpc * PxlOnHft * SectorOnPxl * LadderOnSector; temp.SetName("temp");
	      St_SurveyC::Normalize(temp);
	      comb = &temp;
	      break;
	    case kPxlSensor:
	      Tpc2Global = StTpcPosition::instance()->GetMatrix();	    PrPV(Tpc2Global);
	      HftOnTpc = StidsOnTpc::instance()->GetMatrix(0);	    PrPV(HftOnTpc);
	      PxlOnHft = StPxlpstOnIds::instance()->GetMatrix() 
		* StpxlOnPst::instance()->GetMatrix();	            PrPV(PxlOnHft);
	      SectorOnPxl = StpxlHalfOnPxl::instance()->GetMatrix4Id((sector-1)/5+1)
		* StpxlSectorOnHalf::instance()->GetMatrix4Id(sector);	    PrPV(SectorOnPxl);
	      LadderOnSector = StpxlLadderOnSector::instance()->GetMatrix4Id(4*(sector-1) + ladder) * PixelLadderT; 	  PrPV(LadderOnSector);
	      SensorOnLadder = PixelLadderT.Inverse() * StpxlSensorOnLadder::instance()->GetMatrix4Id(sensor + 10*(ladder+4*(sector-1) - 1));PrPV(SensorOnLadder);
	      SensorOnGlobal = Tpc2Global * HftOnTpc * PxlOnHft * SectorOnPxl * LadderOnSector * SensorOnLadder;PrPV(SensorOnGlobal);
	      // 	    mGeoHMatrixSensorOnGlobal[i][j][k] = (StTpcPosition::instance()->GetMatrix())  x
	      // 	      * mGeoHMatrixIdsOnTpc                                                         x
	      //            * mGeoHMatrixPstOnIds * mGeoHMatrixPxlOnPst                                   x
	      //            * mGeoHMatrixHalfOnPxl[i / 5] * mGeoHMatrixSectorOnHalf[i]
	      // 	      * mGeoHMatrixLadderOnSector[i][j] * mGeoHMatrixSensorOnLadder[i][j][k];
	      comb = (TGeoHMatrix *) StPxlDb::instance()->geoHMatrixSensorOnGlobal(sector, ladder,sensor); PrPV(*(comb));
	      //	      comb = &SensorOnGlobal;
	      break;
	      //	  case kIstWafer:
	    case kIstSensor:
	      matIst = 1000 + (ladder - 1) * kIstNumSensorsPerLadder + sensor;
	      comb = (TGeoHMatrix *) StIstDb::instance()->getRotations()->FindObject(Form("R%04i", matIst));
	      break;
	    case kSst:
	      Tpc2Global = StTpcPosition::instance()->GetMatrix();	    PrPV(Tpc2Global);
	      sstOnOsc = StsstOnOsc::instance()->GetMatrix(0); PrPV(sstOnOsc);
	      temp = Tpc2Global * sstOnOsc; temp.SetName("kSst"); PrPV(temp);
	      comb = &temp;
	      break;
#if 0
	    case kSstLadder:
	      Tpc2Global = StTpcPosition::instance()->GetMatrix();	    PrPV(Tpc2Global);
	      HftOnTpc = StidsOnTpc::instance()->GetMatrix(0);	    PrPV(HftOnTpc);
	      sstOnOsc = StsstOnOsc::instance()->GetMatrix(0); PrPV(sstOnOsc);
	      sstLadderOnSst = StsstLadderOnSst::instance()->GetMatrix4Id(100 + ladder); PrPV(sstLadderOnSst);
	      temp = Tpc2Global * HftOnTpc * sstOnOsc * sstLadderOnSst; temp.SetName("kSstLadder"); PrPV(temp);
	      comb = &temp;
	      break;
#endif
	    case kSstWafer:
	      Tpc2Global = StTpcPosition::instance()->GetMatrix();	    PrPV(Tpc2Global);
	      sstOnOsc = StsstOnOsc::instance()->GetMatrix(0); PrPV(sstOnOsc);
	      sstLadderOnSst = StsstLadderOnSst::instance()->GetMatrix4Id(100+ladder); PrPV(sstLadderOnSst);
	      sstSensorOnLadder = StsstSensorOnLadder::instance()->GetMatrix4Id(7000 + ladder + 100*sensor); PrPV(sstSensorOnLadder);
	      //	      temp = Tpc2Global * HftOnTpc * sstOnOsc * sstLadderOnSst * sstSensorOnLadder * SstSensorR; temp.SetName("kSstWafer"); PrPV(temp);
	      temp = Tpc2Global * sstOnOsc * sstLadderOnSst * sstSensorOnLadder * SstSensorR; temp.SetName("kSstWafer"); PrPV(temp);
	      comb = &temp;
	      break;
	    case kSstSensor:
	      comb = (TGeoHMatrix *) StSstDbMaker::instance()->getRotations()->FindObject(Form("R%04i", 7000 + 100*sensor + ladder));
	      break;
	    default:
	      check = 0;
	      break;
	    }
	    if (check && comb) {
	      cout << "++++++++++++++++++++++++++++++++++ Check Node " << endl;
	      TGeoHMatrix *nMat = nodeP->GetMatrix();
	      nMat->Print();
	      cout << "---------------------------------+ Matrix from Db  ---------------------------------------++" << endl;
	      comb->Print();
	      cout << "---------------------------------+ Inverse     ---------------------------------------------+" << endl;
	      TGeoHMatrix combI = comb->Inverse(); combI.Print();
	      D = combI * (*nMat);
	      if (!(D == I)) {
		cout << "---------------------------------+ Diff for Node = " << nodeP->GetName() << endl;
		D.Print();
	      }
	      Double_t *tran = D.GetTranslation();
	      Double_t *r    = D.GetRotationMatrix();
	      // Check Ideal case
	      NNG++;
	      for (Int_t l = 0; l < 3; l++) {
		XyzG[l]  += tran[l];
		Xyz2G[l] += tran[l]*tran[l];
	      }
	      for (Int_t l = 0; l < 9; l++) {
		XyzG[l+3]  += r[l];
		Xyz2G[l+3] += r[l]*r[l];
	      }
	      cout << "++++++++++++++++++++++++++++++++++ Check Node ++++++++++++++++++++++++++++++++++++++++++++++" << endl;
	    }
#endif
#undef __CHECK_NODE__
	  }
	  iBreak++;
	}
      }
    }
    if (NN) {
      cout << "++++++++++++++++++++++++++++++++++ Check Node Local " <<  listOfDet2Align[i].Name << endl;
      for (Int_t l = 0; l < 12; l++) {
	Xyz[l] = Xyz[l]/NN;
	Xyz2[l] = Xyz2[l]/NN;
	Double_t RMS2 = Xyz2[l] - Xyz[l]*Xyz[l];
	if (RMS2 < 0) RMS2 = 0;
	Xyz2[l] = TMath::Sqrt(RMS2);
	if (l < 3) cout << "tra:\t";
	else       cout << "rot:\t";
	cout << Xyz[l] << " +/- " << Xyz2[l] << endl;
      }
      cout << "++++++++++++++++++++++++++++++++++++++++" << endl;
      iBreak++;
    }
    if (NNG) {
      cout << "++++++++++++++++++++++++++++++++++ Check Node Global " <<  listOfDet2Align[i].Name << endl;
      for (Int_t l = 0; l < 12; l++) {
	XyzG[l] = XyzG[l]/NNG;
	Xyz2G[l] = Xyz2G[l]/NNG;
	Double_t RMS2 = Xyz2G[l] - XyzG[l]*XyzG[l];
	if (RMS2 < 0) RMS2 = 0;
	Xyz2G[l] = TMath::Sqrt(RMS2);
	if (l < 3) cout << "traG:\t";
	else       cout << "rotG:\t";
	cout << XyzG[l] << " +/- " << Xyz2G[l] << endl;
      }
      cout << "++++++++++++++++++++++++++++++++++++++++" << endl;
      iBreak++;
    }
    if (Debug() && NoPerfMatch) {
      cout << "++++++++++++++++++++++++++++++++++ Check Node " <<  listOfDet2Align[i].Name << "\tNo. Perfect Match = " << NoPerfMatch << endl;
      iBreak++;
    }
  }
#if 0
  //  gGeoManager->Voxelize("ALL");
  const TObjArray *Volumes = gGeoManager->GetListOfVolumes();
  TIter next(Volumes);
  TGeoVolume *vol = 0;
  while ((vol = (TGeoVolume*)next())) {
    vol->SortNodes();
    vol->Voxelize("ALL");
    vol->FindOverlaps();
   }
  // Freeze misaligned geometry
  //  gGeoManager->RefreshPhysicalNodes();
#else
  gGeoManager->GetParallelWorld()->CloseGeometry();
  // gGeoManager->SetUseParallelWorldNav(kTRUE); // It does not work ?
#endif
  fAlignmentDone = kTRUE;
  return fAlignmentDone;
}
//________________________________________________________________________________
void StarVMCApplication::SetDebug(Int_t m) {
  fDebug = m;
  if (fDebug > 1) {
    Gcflag_t* cflag = fgGeant3->Gcflag();
    cflag->idebug = fDebug;
    cflag->idemin =     1;
    cflag->idemax = 10000;
    cflag->iswit[0] = 2;
    cflag->iswit[1] = 2;
    cflag->iswit[2] = 2; 
  }
}			  
//________________________________________________________________________________
void StarVMCApplication::ForceDecay(const Char_t *nameP, 
				    const Char_t *mode1A, const Char_t *mode1B, const Char_t *mode1C, Float_t branch1,
				    const Char_t *mode2A, const Char_t *mode2B, const Char_t *mode2C, Float_t branch2,
				    const Char_t *mode3A, const Char_t *mode3B, const Char_t *mode3C, Float_t branch3) {
  if (! TVirtualMC::GetMC()->IsA()->InheritsFrom("TGeant3TGeo")) {
    LOG_ERROR << "StarVMCApplication::ForceDecay does not work without TGeant3TGeo" << endm;
    return;
  }
  TParticlePDG *p = TDatabasePDG::Instance()->GetParticle(nameP);
  assert(p);
  Int_t pdg = p->PdgCode();
  if (! pdg) return;
  Int_t iD  = fgGeant3->IdFromPDG(pdg);
  const Char_t *modes[3][3] = {
    {mode1A, mode1B, mode1C},
    {mode2A, mode2B, mode2C},
    {mode3A, mode3B, mode3C}};
  Float_t branches[3] = {branch1, branch2, branch3};
  Int_t NB = 0;
  Float_t total = 0;
  for (Int_t m = 0; m < 3; m++) {
    if (branches[m] > 0 && modes[m][0]) {NB++; total += branches[m];}
  }
  // Default forced branching 
  if (total <= 0) {
    TString name(nameP);
    if      (name == "Lambda0"    )  
      ForceDecay(name, "proton",     "pi-", 0, 100);
    else if (name == "Lambda0_bar")  
      ForceDecay(name, "antiproton", "pi+", 0, 100);
    else if (name == "Xi-bar"        ) {
      ForceDecay(name, "Lambda0_bar", "pi+", 0, 100);
      ForceDecay("Lambda0_bar", "antiproton", "pi+", 0, 100);
    } else if (name == "Xi-"        ) {
      ForceDecay(name, "Lambda0", "pi-", 0, 100);
      ForceDecay("Lambda0", "proton", "pi-", 0, 100);
    } else if (name == "Omega-")  {
      ForceDecay(name, "Lambda0", "K-", 0, 100);
	ForceDecay("Lambda0", "proton", "pi-", 0, 100);
    } else if (name == "Omega+")  {
      ForceDecay(name, "Lambda0", "K-", 0, 100);
      ForceDecay("Lambda0", "proton", "pi-", 0, 100);
    } else if (name == "K_S0") {
      ForceDecay(name, "pi+", "pi-", 0, 100);
    }
    return;
  }
  Int_t mode[6] = {0};
  Float_t bratio[6] = {0};
  for (Int_t m = 0; m < NB; m++) {
    TString Line;
    mode[m] = 0;
    for (Int_t j = 0; j < 3; j++) {
      if (! modes[m][j] || TString(modes[m][j]) == "") continue;
      p = TDatabasePDG::Instance()->GetParticle(modes[m][j]);
      if (! p) { 
	LOG_ERROR << "p for " << modes[m][j] << " is not found" << endm; 
	assert(p);
      }
      pdg = p->PdgCode();
      Line += modes[m][j];
      Line += " ";
      Int_t Id = fgGeant3->IdFromPDG(pdg);
      mode[m] = 100*mode[m] + Id;
    }
    bratio[m] = 100*branches[m]/total;
    LOG_INFO << "Force decay of " << nameP << " => " << Line.Data() << endm;
  }
  fgGeant3->Gsdk(iD, bratio, mode);
}
//________________________________________________________________________________
Int_t StarVMCApplication::ipartx(Int_t id) {
  Int_t                            ipartxf = -1;
  if      (id == 1)                ipartxf = 1;  // gamma
  else if (id == 2 || id == 3)     ipartxf = 2;  // e+/-
  else if (id == 5 || id == 6)     ipartxf = 3;  // mu+/-
  else if (id == 13)               ipartxf = 4;  // neutron
#if 0
  else if ((id >=  8 && id <=  9) ||
	   (id >= 11 && id <= 15)) ipartxf = 0;  // all other charged particles
#endif
  else                             ipartxf = 0;  // all other particles
  return ipartxf;
}
//________________________________________________________________________________
Float_t StarVMCApplication::dose(Float_t Z) {
  /*                                                                      *
   *  Function    : Interpolation of gamma dose rate                      *
   *                                                                      *
   *  Arguments   : Z   Atom number                                       */
  static TGraph *graph = 0;
  if (! graph) {
    Int_t n = 43;
    Double_t x[] = {11., 12., 13., 14., 15., 17., 18., 19., 20., 22.,
		    23., 24., 25., 26., 27., 28., 29., 30., 32., 33.,
		    34., 35., 38., 40., 41., 42., 43., 46., 47., 48.,
		    49., 50., 51., 53., 57., 66., 73., 74., 78., 79.,
		    80., 82., 83.}; // Z
    Double_t y[] = {8.8033795E-02, 0.1175197    , 0.1043398    , 8.3658338E-02,
		    7.6843806E-02, 5.7563554E-02, 4.0977564E-02, 4.6153713E-02,
		    4.0977564E-02, 0.1257856    , 0.1797267    , 0.1679161    ,
		    0.1650867    , 0.1828069    , 0.1828069    , 0.1956649    ,
		    0.2094273    , 0.1923680    , 0.1923680    , 0.2612006    ,
		    0.2795725    , 0.3095814    , 0.4499212    , 0.6880791    ,
		    0.7240669    , 0.6213810    , 0.5          , 0.5          ,
		    0.5          , 0.3546627    , 0.3796084    , 0.3428115    ,
		    0.2941946    , 0.3370352    , 0.3          , 0.2702305    ,
		    0.2941946    , 0.2567994    , 0.3607410    , 0.3607410    ,
		    0.3428115    , 0.3          , 0.4}; // Dose
    graph = new TGraph(n,x,y);
  }
  return 13.1286072899874E-6*TMath::Max(0., TMath::Min(0.5, graph->Eval(Z)));
}
//________________________________________________________________________________
void StarVMCApplication::usflux() {
  Int_t        id;
  Float_t      OmegaN;
  Int_t        i;
  Float_t      ZZ, RR;
  Int_t        NstepB;
  Float_t      stepF, destepF, XYZ[3];
  Float_t      RADIUS;
  Int_t        p;
  enum {Nregions = 1, Nparts = 5, NH1T = 3, NH1TE = NH1T + 1, NH2T = 9};
  const Char_t *NameV[Nregions] = {""}; 
  static TH1F *histV1[Nregions][NH1TE][Nparts];
  //#define __3DPLOTS__
#ifdef __3DPLOTS__
  static TH3F *histV2[NH2T][Nparts];
#else
  static TH2F *histV2[NH2T][Nparts];
#endif
  static TH2F *tofg = 0;
  static Bool_t first = kTRUE;
  if (first) {
    assert(StMaker::GetChain()->GetTFile());
    StMaker::GetChain()->GetTFile()->cd();
    first = kFALSE;
    memset(histV1, 0, sizeof(histV1));
    memset(histV2, 0, sizeof(histV2));
    Double_t xstep =  5;
    Double_t ystep =  2;
    Double_t xmax  = 2000, xmin = - xmax;
    Double_t ymax  = 1500, ymin =      0;
    Int_t nx = (xmax - xmin)/xstep;
    Int_t ny = (ymax - ymin)/ystep;
    struct Name_t {
      const Char_t *Name;
      const Char_t *Title;
    };
    struct NameX_t {
      Name_t name;
      Int_t nX;
      Double_t xMin, xMax;
      Int_t nY;
      Double_t yMin, yMax;
    };
    tofg = new TH2F("tofg","log_{10} (tof [nsec]) @ step versus particle type",140,-1,13,51,0.5,51.5);
    Name_t Particles[Nparts] = {
      {"", "#pi/K/p and others"}, // 0
      {"g","#gamma"},             // 1
      {"e","e^{#pm}"},            // 2
      {"m","#mu^{#pm}"},          // 3
      {"n","neutron"}             // 4
    };
    NameX_t Types1[NH1TE] = {
      {{"Ekin10"    ,"Log_{10}(GEKIN) for %s for %s"                 }, 340, -14., 3.0, 0, 0, 0},  //5 -> 0 300
      {{"Ekin10s"   ,"Log_{10}(GEKIN) for %s at step for %s weighted with L"   }, 340, -14., 3.0, 0, 0, 0},  //6 -> 1 320
      {{"Ekin10V"   ,"Log_{10}(GEKIN) for %s at production Vx for %s"}, 340, -14., 3.0, 0, 0, 0},   //7 -> 2 400
      {{"Ekin10overV","Log_{10}(GEKIN) for %s at step weight with L/v for %s"}, 340, -14., 3.0, 0, 0, 0} 
    };
    NameX_t Types2[NH2T] = {
      {{"flux"      ,"flux from %s * step "                    }, nx, xmin, xmax, ny, ymin, ymax},  //0 100
      {{"flux100keV","flux from %s * step E_{kin} > 100 keV "  }, nx, xmin, xmax, ny, ymin, ymax},  //1 800
      {{"flux250meV","flux from %s * step E_{kin} < 250 meV "  }, nx, xmin, xmax, ny, ymin, ymax},  //2 500
      {{"entries"   ,"entries from %s "                        }, nx, xmin, xmax, ny, ymin, ymax},  //3 900
      {{"VxProd"    ,"Vertex Production of %s "                }, nx, xmin, xmax, ny, ymin, ymax},  //4 200
      {{"dose"      ,"dose from %s "                           }, nx, xmin, xmax, ny, ymin, ymax},  //8 ->5  600
      {{"star"      ,"star density  from %s "                  }, nx, xmin, xmax, ny, ymin, ymax},  //9 ->6 700
      {{"RD"        ,"Residual Dose  from %s "                 }, nx, xmin, xmax, ny, ymin, ymax},  //0 ->7 701
      {{"DepEnergy" ,"Deposited energy at step (keV)  from %s "}, nx, xmin, xmax, ny, ymin, ymax}
    };
    for (p = 0; p < Nparts; p++) {
      TH1::SetDefaultSumw2(kTRUE);
      for (Int_t r = 0; r < Nregions; r++) {
	for (Int_t t = 0; t < NH1TE; t++) {
	  //	  if (p != Nparts - 1 && t > NH1T) continue;
	  TString Name(Types1[t].name.Name); Name += Particles[p].Name; Name += NameV[r];
	  TString Title(Form(Types1[t].name.Title,Particles[p].Title,NameV[r]));
	  histV1[r][t][p] = 
	    new TH1F(Name,Title,Types1[t].nX,Types1[t].xMin,Types1[t].xMax);
	}
      }
      TH1::SetDefaultSumw2(kFALSE);
      for (Int_t t = 0; t < NH2T; t++) {
	if (p == 3 && t > 1) continue;
	TString Name(Types2[t].name.Name); Name += Particles[p].Name; 
	TString Title(Form(Types2[t].name.Title,Particles[p].Title));
	histV2[t][p] = 
#ifdef __3DPLOTS__
	  new TH3F(Name,Title,Types2[t].nX,Types2[t].xMin,Types2[t].xMax,Types2[t].nY,Types2[t].yMin,Types2[t].yMax,24,-180,180);
#else
	new TH2F(Name,Title,Types2[t].nX,Types2[t].xMin,Types2[t].xMax,Types2[t].nY,Types2[t].yMin,Types2[t].yMax);//24,-180,180);
#endif
      }
    }
    return;
  }
  // Fill histograms
  Gckine_t *ckine = fgGeant3->Gckine();
  Gctrak_t *ctrak = fgGeant3->Gctrak();
  Gcking_t *cking = fgGeant3->Gcking();
  Gcmate_t *cmate = fgGeant3->Gcmate();
  Int_t Ipart = ckine->ipart%100;
  p = ipartx (Ipart);
  Double_t tofg10 = - 1;
  if (ctrak->tofg > 0) tofg10 = TMath::Log10(1e9*ctrak->tofg);
  tofg->Fill(tofg10,Ipart);
  if (Ipart == 13 && ctrak->gekin <= 0.25E-9) tofg->Fill(tofg10,51);
  if (p < 0) return;
  Double_t Log10gekin = -20;
#if 0
  if (ctrak->gekin <= 0.0)       {
    ctrak->istop = 2;
    return;
  }
#else
  if (ctrak->gekin > 0) Log10gekin = TMath::Log10(ctrak->gekin);
#endif
  if (ctrak->upwght < 1) ctrak->upwght = 1;
  Int_t r = 0;
  RR = TMath::Sqrt(ctrak->vect[0]*ctrak->vect[0] + ctrak->vect[1]*ctrak->vect[1]);
  ZZ = ctrak->vect[2];
  Double_t Phi = TMath::RadToDeg()*TMath::ATan2(ctrak->vect[1],ctrak->vect[0]);
  if (Phi < -180) Phi += 360;
  if (Phi >  180) Phi -= 360;
  // calculate particle flux in sensitive volumes
  if (! (ckine->charge == 0 && p < 0)) {
    /*
     * *** step cannot be bigger then 10 cm => suppose stright line in R/Z
     */
    if (ctrak->step > 0.0)        {
      NstepB = ctrak->step + 0.5;
      NstepB = TMath::Max (1, NstepB);
      stepF  = ctrak->step/NstepB;
      destepF  = 1e6*ctrak->destep/NstepB;
      for (i = 1; i <= NstepB; i++) {
	XYZ[0] = ctrak->vect[0] + ctrak->vect[3]*stepF*(0.5 - i);
	XYZ[1] = ctrak->vect[1] + ctrak->vect[4]*stepF*(0.5 - i);
	XYZ[2] = ctrak->vect[2] + ctrak->vect[5]*stepF*(0.5 - i);
	RADIUS = TMath::Sqrt(XYZ[0]*XYZ[0] + XYZ[1]*XYZ[1]);
	Double_t phi = TMath::RadToDeg()*TMath::ATan2(XYZ[1],XYZ[0]);
	if (phi < -180) phi += 360;
	if (phi >  180) phi -= 360;
#ifdef __3DPLOTS__
	if (histV2[0][p])                             histV2[0][p]->Fill(XYZ[2], RADIUS, Phi, stepF);
	if (ctrak->gekin >= 1.E-4 && histV2[1][p])    histV2[1][p]->Fill(XYZ[2], RADIUS, Phi, stepF);
	if (ctrak->gekin <= 0.25E-9 && histV2[2][p])  histV2[2][p]->Fill(XYZ[2], RADIUS, Phi, stepF);
	if (cmate->dens > 2e-5 && ctrak->destep > 0.0 && histV2[5][p]) 
 	  histV2[5][p]->Fill(XYZ[2], RADIUS, Phi, destepF/cmate->dens);
	if (histV2[8][p])                             histV2[8][p]->Fill(XYZ[2], RADIUS, Phi, destepF);
#else
	if (histV2[0][p])                             histV2[0][p]->Fill(XYZ[2], RADIUS, stepF);
	if (ctrak->gekin >= 1.E-4 && histV2[1][p])    histV2[1][p]->Fill(XYZ[2], RADIUS, stepF);
	if (ctrak->gekin <= 0.25E-9 && histV2[2][p])  histV2[2][p]->Fill(XYZ[2], RADIUS, stepF);
	if (cmate->dens > 2e-5 && ctrak->destep > 0.0 && histV2[5][p]) 
	  histV2[5][p]->Fill(XYZ[2], RADIUS, destepF/cmate->dens);
	if (histV2[8][p])                             histV2[8][p]->Fill(XYZ[2], RADIUS, destepF);
#endif
      }
      histV1[0][0][p]->Fill(Log10gekin);
      histV1[0][1][p]->Fill(Log10gekin,ctrak->step);
      if (r > 0) {
	histV1[r][0][p]->Fill(Log10gekin);
	histV1[r][1][p]->Fill(Log10gekin,ctrak->step);
      }
      if (p == 4 && ctrak->gekin < 1e-3) { // neutrons < 1 MeV
	static Double_t EkinThermal = 25.8e-12; // meV
	static Double_t MassNeutron = 0.9396;
	static Double_t velTherm = TMath::Sqrt(2*EkinThermal/MassNeutron);
	Double_t vel = TMath::Sqrt(2*ctrak->gekin/MassNeutron);
	histV1[0][3][p]->Fill(Log10gekin,velTherm/vel*ctrak->step);
	if (r > 0) histV1[r][3][p]->Fill(Log10gekin,velTherm/vel*ctrak->step);
      }
      if (ctrak->inwvol == 1) {
#ifdef __3DPLOTS__
	if(histV2[3][p]) histV2[3][p]->Fill(ZZ, RR, Phi);
#else
	if(histV2[3][p]) histV2[3][p]->Fill(ZZ, RR);
#endif
      }
      for (Int_t i = 0; i < cking->ngkine; i++) {
	id = ((Int_t)cking->gkin[i][4])%100;
	p = ipartx (id);
	if (p >= 0) {
	  Char_t name[12];
	  Int_t itrtyp;
	  Float_t mass, charge, tlife;
	  fgGeant3->Gfpart(id,name,itrtyp,mass,charge,tlife);
#ifdef __3DPLOTS__
	  if (histV2[4][p]) histV2[4][p]->Fill(ZZ, RR, Phi);
#else
	  if (histV2[4][p]) histV2[4][p]->Fill(ZZ, RR);
#endif
	  Double_t ekin = 
	    TMath::Sqrt(cking->gkin[i][0]*cking->gkin[i][0] +
			cking->gkin[i][1]*cking->gkin[i][1] +
			cking->gkin[i][2]*cking->gkin[i][2] + mass*mass) - mass;
	  ekin = TMath::Max (1e-14, ekin);
	  histV1[0][2][p]->Fill(TMath::Log10(ekin));
	  if (r > 0)  histV1[r][2][p]->Fill(TMath::Log10(ekin));
	}
      }
    }
    // Star density
    if (cking->ngkine > 0)        {
      if (ctrak->vect[6] > 0.300 && p <= 0) {
	for (Int_t i = 0; i < ctrak->nmec; i++) {
	  if (ctrak->lmec[i] >= 12 && ctrak->lmec[i] <= 20 && p >= 0) {
	    OmegaN = dose(cmate->z);
#ifdef __3DPLOTS__
	    if (histV2[6][p]) histV2[6][p]->Fill(ZZ, RR, Phi); 
	    if (histV2[7][p]) histV2[7][p]->Fill(ZZ, RR, Phi, OmegaN ); 
#else
	    if (histV2[6][p]) histV2[6][p]->Fill(ZZ, RR); 
	    if (histV2[7][p]) histV2[7][p]->Fill(ZZ, RR, OmegaN ); 
#endif
	    break;
	  }
	}
      }
    }
  }
#undef __3DPLOTS__
}
#undef PrPV
// $Log: StarVMCApplication.cxx,v $
// Revision 1.13  2013/12/16 22:58:53  fisyak
// Add g2t_volume_id
//
// Revision 1.12  2012/06/11 23:21:12  fisyak
// std namespace
//
// Revision 1.11  2011/02/11 16:12:52  fisyak
// Fixes for gcc451
//
// Revision 1.10  2009/04/21 20:51:07  fisyak
// GeanE propagator, starting version
//
// Revision 1.9  2009/04/15 20:46:07  fisyak
// Revise nameing convention toward to Sti usage
//
// Revision 1.8  2009/03/16 21:37:04  fisyak
// Clean up
//
// Revision 1.7  2009/03/16 19:42:33  fisyak
// Freeze alignment
//
// Revision 1.6  2009/03/02 00:16:54  fisyak
// Fix svt alignment
//
// Revision 1.5  2009/02/26 16:42:19  fisyak
// Proper handle of debug flag
//
// Revision 1.4  2009/02/25 20:22:26  fisyak
// Add the first version for misalignment
//
