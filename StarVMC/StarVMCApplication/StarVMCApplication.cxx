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
TableClassImpl(St_VMCPath2Detector,VMCPath2Detector_st);
ClassImp(StarVMCApplication);
#define PrPV(B)      if (Debug())                {std::cout << (#B) << " = \t"; (B).Print();} 
static const TString separator("/_"); 
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
    TGeant3TGeo *geant3 = (TGeant3TGeo *)TVirtualMC::GetMC();
    if (Debug()) {
      geant3->Gprint("mate");
      geant3->Gprint("tmed");
    }
  }
  if (fMcHits) fMcHits->Init();
}
//_____________________________________________________________________________
void StarVMCApplication::GeneratePrimaries() {    
  if (! fPrimaryGenerator) fPrimaryGenerator = new StarMCPrimaryGenerator(fStarStack);
  if (StarPrimaryMaker::instance()) {
    // put TParticle from StarParticleStack from StarPrimaryMaker to StarStack
    fPrimaryGenerator->SetStack(StarPrimaryMaker::instance()->stack());
  } else {
    fPrimaryGenerator->GeneratePrimaries();
  } 
  Int_t NPrimary = fStarStack->GetNtrack();
  if (! NPrimary) TVirtualMC::GetMC()->StopRun();
  if (fMcHits) {
    fMcHits->BeginEvent();
  }
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
  if (fMcHits) fMcHits->Step();
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
    TGeant3TGeo *geant3 = (TGeant3TGeo *)gMC;
    Gcflag_t* cflag = geant3->Gcflag();
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
  TGeant3TGeo *g3 = (TGeant3TGeo *)TVirtualMC::GetMC();
  TParticlePDG *p = TDatabasePDG::Instance()->GetParticle(nameP);
  assert(p);
  Int_t pdg = p->PdgCode();
  if (! pdg) return;
  Int_t iD  = g3->IdFromPDG(pdg);
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
  if (total <= 0) return;
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
      Int_t Id = g3->IdFromPDG(pdg);
      mode[m] = 100*mode[m] + Id;
    }
    bratio[m] = 100*branches[m]/total;
    LOG_INFO << "Force decay of " << nameP << " => " << Line.Data() << endm;
  }
  g3->Gsdk(iD, bratio, mode);
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
