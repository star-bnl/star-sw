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
// Alignment
#include "StDetectorDbMaker/St_tpcPadPlanesC.h"
#include "StDetectorDbMaker/St_tpcGlobalPositionC.h"
#include "StDetectorDbMaker/St_tpcSectorPositionC.h"
#include "StDetectorDbMaker/StSsdSurveyC.h"
#include "StDetectorDbMaker/StSvtSurveyC.h"
#include "StSvtDbMaker/StSvtDbMaker.h"
#include "StSsdDbMaker/StSsdDbMaker.h"
#include "StDetectorDbMaker/StTpcSurveyC.h"
TableClassImpl(St_VMCPath2Detector,VMCPath2Detector_st);
ClassImp(StarVMCApplication);

static const TString separator("/_"); 
TDataSet        *StarVMCApplication::fgDetSets = 0;
#if 0
THashList       *StarVMCApplication::fgRotMHash = 0;
#endif
//_____________________________________________________________________________
StarVMCApplication::StarVMCApplication(const char *name, const char *title) : 
  TVirtualMCApplication(name,title),
  fStack(0),
  fPrimaryGenerator(0),
  fMagField(0),
  fMcHits(0),
  fFieldB(0),
  fDebug(0),
  fAlignment(kFALSE), //(kTRUE)
  fAlignmentDone(kFALSE)
{
  // Standard constructor
  TString program(gSystem->BaseName(gROOT->GetApplication()->Argv(0)));
  assert (! program.BeginsWith("root4star"));
  if (name) {
    // Create a user stack
    fStack = new StarMCStack(100); 
    // Constant magnetic field (in kiloGauss)
    fFieldB = new Double_t[3];
    fFieldB[0] = 0.;
    fFieldB[1] = 0.;
    fFieldB[2] = 5.;
  }
}
//_____________________________________________________________________________
StarVMCApplication::~StarVMCApplication() {  // Destructor  
  delete fStack;
  delete fFieldB;
  SafeDelete(gMC);
  SafeDelete(fgDetSets);
}
//_____________________________________________________________________________
void StarVMCApplication::InitMC(const char* setup) {  // Initialize MC.
  if (setup) {
    gROOT->LoadMacro(setup);
    gInterpreter->ProcessLine("Config()");
  }
  gMC->SetStack(fStack);
  gMC->Init();
  gMC->BuildPhysics(); 
  MisalignGeometry();
}
//_____________________________________________________________________________
void StarVMCApplication::RunMC(Int_t nofEvents) {    // MC run.
  gMC->ProcessRun(nofEvents);
  FinishRun();
}
//_____________________________________________________________________________
void StarVMCApplication::FinishRun() {    // Finish MC run.
}
//_____________________________________________________________________________
void StarVMCApplication::ConstructGeometry() {    // Initialize geometry
  if (gMC->IsA()->InheritsFrom("TGeant3TGeo")) {
    assert(gGeoManager); 
    gMC->SetRootGeometry();
  }
}
//_____________________________________________________________________________
void StarVMCApplication::InitGeometry() {    
  if (gMC->IsA()->InheritsFrom("TGeant3")) {
    // Set drawing options
  }  
  if (gMC->IsA()->InheritsFrom("TGeant3TGeo")) {
    TGeant3TGeo *geant3 = (TGeant3TGeo *)gMC;
    geant3->Gprint("mate");
    geant3->Gprint("tmed");
  }
  if (fMcHits) fMcHits->Init();
}
//_____________________________________________________________________________
void StarVMCApplication::GeneratePrimaries() {    
  if (fPrimaryGenerator) {
    fPrimaryGenerator->GeneratePrimaries();
  }
  else {
    // Fill the user stack (derived from TVirtualMCStack) with primary particles.
    // ---
    // Track ID (filled by stack)
    Int_t ntr;
    
    // Option: to be tracked
    Int_t toBeDone = 1; 
    
    // Particle type
    //Int_t pdg  = 0;    // geantino
    Int_t pdg  = kProton;
    
    // Polarization
    Double_t polx = 0.; 
    Double_t poly = 0.; 
    Double_t polz = 0.; 
    
    // Position
    Double_t vx  = 0.; 
    Double_t vy  = 0.; 
    Double_t vz = 10.;
    Double_t tof = 0.;
    
    // Energy
    Double_t kinEnergy = 3.0;
    Double_t mass = 0.9382723;
    Double_t e  = mass + kinEnergy;
    
    // Momentum
    Double_t px, py, pz;
    px = 0.; 
    py = 0.; 
    pz = sqrt(e*e - mass*mass); 
    
    // Add particle to stack 
    fStack->PushTrack(toBeDone, -1, pdg, px, py, pz, e, vx, vy, vz, tof, polx, poly, polz, 
		      kPPrimary, ntr, 1., 0);
  }
  Int_t NPrimary = fStack->GetNtrack();
  if (! NPrimary) gMC->StopRun();
}
//_____________________________________________________________________________
void StarVMCApplication::BeginEvent() {    // User actions at beginning of event
  fStack->Reset();
  if (fMcHits) fMcHits->Clear();
}
//_____________________________________________________________________________
void StarVMCApplication::BeginPrimary() {    // User actions at beginning of a primary track
}
//_____________________________________________________________________________
void StarVMCApplication::PreTrack() {    // User actions at beginning of each track
}
//_____________________________________________________________________________
void StarVMCApplication::Stepping() {    // User actions at each step
  if (fMcHits) fMcHits->Step();
}
//_____________________________________________________________________________
void StarVMCApplication::PostTrack() {    // User actions after finishing of each track
  // delete stack only track
  StarMCParticle *current =  fStack->GetCurrentParticle();
  TObjArray *objs = fStack->GetParticles();
  if (objs->IndexOf(current) < objs->LowerBound()) delete current;
}
//_____________________________________________________________________________
void StarVMCApplication::FinishPrimary() {    // User actions after finishing of a primary track
}
//_____________________________________________________________________________
void StarVMCApplication::FinishEvent() {    // User actions after finishing of an event
  if (TString(gMC->GetName()) == "TGeant3") {
    // add scale (1.4)
  }  
  fStack->Print();
  if (fMcHits) fMcHits->FinishEvent(); // add kine info
} 
//_____________________________________________________________________________
void StarVMCApplication::Field(const Double_t* x, Double_t* b) const {
  if (fMagField) {
    fMagField->BField(x,b);
  } else {
    // Uniform magnetic field
    // ---
    for (Int_t i=0; i<3; i++) b[i] = fFieldB[i];
  }
}
//_____________________________________________________________________________
Int_t StarVMCApplication::LoopOverTgeo(TGeoNode *nodeT, TString pathT) {
  Int_t NoSensVolumes = 0;
  if (! nodeT) { 
    if (! gGeoManager) return NoSensVolumes;
    gGeoManager->RestoreMasterVolume();
    //    gGeoManager->cd("HALL_1/CAVE_1/SVTT_1/SFMO_1");
    gGeoManager->CdTop();
    nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) return NoSensVolumes;
    TString path = nodeT->GetName();
    cout << "top " << nodeT->GetName() << "\t" << nodeT->GetVolume()->GetName() 
	 << "\t" << path << endl;
    NoSensVolumes += LoopOverTgeo(nodeT,path);
    return NoSensVolumes;
  } 
  TGeoVolume *vol = nodeT->GetVolume();
  if (! vol->TestBit(StarVMCDetector::kChecked)) vol->SetBit(StarVMCDetector::kChecked);
  TObjArray *nodes = vol->GetNodes();
  Int_t nd = nodeT->GetNdaughters();
  //     cout << nd << "\t" << nodeT->GetName() 
  // 	 << "\t" << vol->GetName() 
  // 	 << "\t" << gGeoManager->GetCurrentNode()->GetName() << "\t" << pathT 
  // 	 << endl;
  Int_t NoSensDauthers = 0;
  TGeoMedium     *med = vol->GetMedium();
  Int_t           isvol = 0;
  if (med) isvol  = (Int_t) med->GetParam(0);
  for (Int_t id = 0; id < nd; id++) {
    TGeoNode *node = (TGeoNode*)nodes->UncheckedAt(id);
    if (! node) continue;
    vol = node->GetVolume();
    if (! vol) continue; 
    TString path = pathT;
    if (path != "") path += "/";
    path += node->GetName();
    //      gGeoManager->cdDown(node->GetIndex());
    //    Int_t nodeid = gGeoManager->GetCurrentNode()->GetVolume()->GetIndex(node);
    //    gGeoManager->CdDown(nodeid);
    gGeoManager->CdDown(id);
    //      cout << "path " << path << endl;
    //      gGeoManager->cd(node->GetName());
    //      gGeoManager->cdNode(node->GetIndex());
    NoSensDauthers += LoopOverTgeo(node,path);
    gGeoManager->CdUp();
  }
  NoSensVolumes += NoSensDauthers;
  if (NoSensDauthers == 0 && isvol) {
    NoSensVolumes++;
    //      cout << "sens. vol. " << pathT << endl;
    TGeoVolume *vol = nodeT->GetVolume();
    vol->SetBit(StarVMCDetector::kActive);
    TObjString *objs;
    TObjArray *array = pathT.Tokenize(separator); 
    Int_t N = array->GetEntriesFast();
    St_VMCPath2Detector *dpath = (St_VMCPath2Detector *) fgDetSets->Find(vol->GetName());
    VMCPath2Detector_st dpathT;
    if (! dpath) {
      dpath = new St_VMCPath2Detector(vol->GetName(),N/2);
      fgDetSets->Add(dpath);
      for (Int_t i = 0; i < N; i +=2) {
	objs = (TObjString *) array->At(i); // cout << objs->GetString().Data() << endl;
	TString Name(objs->GetString());
	if (Name == "") continue;
	objs = (TObjString *) array->At(i+1); // cout << objs->GetString().Data() << endl;
	Int_t j = atoi(objs->GetString().Data());
	memset(&dpathT.VName[0], 0, sizeof(VMCPath2Detector_st));
	strcpy(&dpathT.VName[0],Name.Data());
	dpathT.Ncopy = j;
	dpath->AddAt(&dpathT);
      }
      //	dpath->Print(0,N);
    } else {
      for (Int_t i = 0; i < N; i +=2) {
	objs = (TObjString *) array->At(i); // cout << objs->GetString().Data() << endl;
	TString Name(objs->GetString());
	objs = (TObjString *) array->At(i+1); // cout << objs->GetString().Data() << endl;
	Int_t j = atoi(objs->GetString().Data());
	
	VMCPath2Detector_st *row = dpath->GetTable();
	Int_t Nr = dpath->GetNRows();
	Int_t l = -1;
	for (Int_t k = 0; k < Nr; k++, row++) {
	  if (TString(row->VName) == Name) {
	    l = k;
	    if (j > row->Ncopy) {
	      row->Ncopy = j;
	      //		dpath->Print(k,1);
	    }
	    break;
	  }
	}
	if (l < 0) {
	  memset(&dpathT.VName[0], 0, sizeof(VMCPath2Detector_st));
	  strcpy(&dpathT.VName[0],Name.Data());
	  dpathT.Ncopy = j;
	  dpath->AddAt(&dpathT);
	  //	    dpath->Print(0,Nr+1);
	}
      }
    }
    delete array;
  }
  return NoSensVolumes;
}
//________________________________________________________________________________
void StarVMCApplication::GeometryDb(TDataSet *Detectors) {
  // Check consistency the current geometry with respect to DB
  // Mark sensitive volumes
  // Correct volume position accordingly DB
  gBenchmark->Reset();
  gBenchmark->Start("StarVMCApplication::GeometryDb");
  
  fgDetSets = new TDataSet("DetSets");
  fgDetSets->SetTitle("Star Detector sets: contains description of path to sensitive volumes");
  
  LoopOverTgeo();

  TDataSet *set = fgDetSets;
  if (! set) {cout << "Can't find Detectors " << endl; return;}
  TDataSetIter next(set,99);
  TDataSet *d = 0;
  Int_t k = 0;
  while ((d = next())) {
    if (! d->HasData()) continue;
    St_VMCPath2Detector *table = (St_VMCPath2Detector *) d;
    Int_t N = table->GetNRows();
    VMCPath2Detector_st *path = table->GetTable();
    k++;
    cout << k << "\t";
    TString title("");
    for (Int_t i = 0; i < N; i++, path++) {
      title += Form("/%s_%i", path->VName,path->Ncopy);
    }
    table->SetTitle(title);
    cout << title << endl;
    if (Detectors) {
      // Check consistency 
      StarVMCDetector *det = (StarVMCDetector *) Detectors->Find(table->GetName());
      if (! det) {
	cout << "Detector description for " << table->GetName() << "\t" << table->GetTitle() << " is missing" << endl;
      } else {
	Int_t N = det->GetNVmax().GetSize();
	Int_t Numbv[15];
	det->GetNumbv(title,Numbv);
	cout << "Read Detector path :" << title << endl;
	cout << "with format        :" << det->GetFMT();
	if (N <= 0) {
	  cout << "\tCan't read it" << endl;
	} else {
	  Int_t fails = 0;
	  for (Int_t i = 0; i < N; i++) {
	    if (det->GetNVmax()[i] != Numbv[i]) {
	      fails++;
	      if (fails == 1) cout << "\tFails" << endl;
	      cout << "field " << i << "\tis mismatched from Detector descriptor " << det->GetNVmax()[i] 
		   << "\tfrom detector path " << Numbv[i] << endl;
	    }
	  }
	  if (! fails) {
	    cout << "\tRead o.k." << endl;
	  }
	}
      }
    }
  }
  //  SafeDelete(fgDetSets);
  gBenchmark->Show("StarVMCApplication::GeometryDb");
  TObjArray *vols = gGeoManager->GetListOfVolumes();
  UInt_t nvol = vols->GetEntriesFast();
  TGeoVolume *vol = 0;
  for (UInt_t i = 0; i < nvol; i++) {
    vol = (TGeoVolume *) vols->UncheckedAt(i);
    if (! vol->IsVolumeMulti() && ! vol->TestBit(StarVMCDetector::kChecked)) {
      cout << "hanging volume:\t " << vol->GetName() << "\t" << vol->GetTitle() << endl;
    }
  }
}
//________________________________________________________________________________
Bool_t StarVMCApplication::MisalignGeometry() {
  if (! fAlignment) {
    cout << "No MisalignGeometry has been applied" << endl;
    return fAlignmentDone;
  } 
  // Misalignment introduced after 2013 TPC survey
  /*
    HALL[1]/CAVE[1]/TpcRefSys[1]/TPCE[1]/TPGV[2]/TPSS[12]/TPAD[73]

1       HALL[1]/CAVE[1]/TpcRefSys[1]/IDSM[1]/SFMO[1]/SFLM[20]/SFSW[16]/SFSL[1]/SFSD[1]
2       HALL[1]/CAVE[1]/TpcRefSys[1]/IDSM[1]/IBMO[1]/IBAM[24]/IBLM[6]/IBSS[1]
3       HALL[1]/CAVE[1]/TpcRefSys[1]/IDSM[1]/PXMO[1]/PXLA[10]/LADR[4]/PXSI[10]/PLAC[1]
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

   */
  Int_t NoOfInnerRows = St_tpcPadPlanesC::instance()->innerPadRows();
  enum EDetector2Align {
    kUnknown,
    kTPC, kTpcRefSys, kTpcHalf, kTpcSecInn, kTpcSecOut, kTpcPad, kTpcLast,
    kSVT, kSvtWhole, kSvtShell, kSvtLadder, kSvtWafer, 
    kSSD, kSsdWhole, kSsdSector, kSsdLadder, kSsdWafer};
  struct listOfDetectorToAlign_t {
    const Char_t *Name; 
    EDetector2Align kDet;
    const Char_t *path;
    Int_t  Ndim;
    Int_t  NVmax[6];
    St_SurveyC *chair;
  };
  static const listOfDetectorToAlign_t listOfDet2Align[] = {                                                                
    //    HALL[1]/CAVE[1]/TpcRefSys[1]/TPCE[1]/TPGV[2]/TPSS[12]/TPAD[73]
    {"TpcRefSys-%d",    kTpcRefSys,"/HALL_1/CAVE_1/TpcRefSys_%d",                       1, {1,  0, 0, 0, 0, 0}, StTpcPosition::instance()}, 
    {"TpcHalf-%d",      kTpcHalf  ,"/HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TPGV_%d",         1, {2,  0, 0, 0, 0, 0}, StTpcHalfPosition::instance()}, 
    {"TpcSecInn-%02d", kTpcSecInn,"/HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TPGV_%d/TPSS_%d", 2, {2, 12, 0, 0, 0, 0}, StTpcInnerSectorPosition::instance()},
    {"TpcSecOut-%02d", kTpcSecOut,"/HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TPGV_%d/TPSS_%d", 2, {2, 12, 0, 0, 0, 0}, StTpcOuterSectorPosition::instance()},
    {"TpcPad-%02d",        kTpcPad,"/HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TPGV_%d/TPSS_%d/TPAD_%d",3, {2, 12, 73, 0, 0, 0}, 0}
   };
  static const Int_t  NoDetectos2Align = sizeof(listOfDet2Align)/sizeof(listOfDetectorToAlign_t);
  for (Int_t i = 0; i < NoDetectos2Align; i++) {
    EDetector2Align kDetector = listOfDet2Align[i].kDet;
    EDetector2Align kdetIO = kUnknown;
    Int_t Ntot = 1;
    for (Int_t k = 0; k < listOfDet2Align[i].Ndim; k++) Ntot *= listOfDet2Align[i].NVmax[k];
    for (Int_t j = 0; j < Ntot; j++) {
      Int_t ind = j;
      TArrayI Indx(listOfDet2Align[i].Ndim); Int_t *indx = Indx.GetArray();
      for (Int_t k =  listOfDet2Align[i].Ndim - 1; k >= 0; k--) {
	indx[k] = ind%listOfDet2Align[i].NVmax[k]+1; ind /= listOfDet2Align[i].NVmax[k];
      }
      TString path(StarVMCDetector::FormPath(listOfDet2Align[i].path,listOfDet2Align[i].Ndim,indx));
      if (! gGeoManager->CheckPath(path)) continue;
      TObjArray *objs = gGeoManager->GetListOfPhysicalNodes();
      TGeoPhysicalNode *nodeP = 0;
      if (objs) nodeP = (TGeoPhysicalNode *) objs->FindObject(path);
      if (nodeP) {
	if (nodeP->IsAligned()) {
	  cout << nodeP->GetName() << " has been aligned (?)" << endl;
	  continue;
	}
      } else {
	nodeP = gGeoManager->MakePhysicalNode(path);
      }
      TGeoHMatrix rotL(*(nodeP->GetNode()->GetMatrix())); // ideal matrix before alignment
      TGeoHMatrix rotA = rotL; // After alignment
      TGeoHMatrix *rotm = 0;
      Int_t Id = -1;
      Int_t half   = -1; // Tpc half
      Int_t sector = -1; // Tpc sector;
      Int_t row    = -1; // Tpc pad row
      Int_t layer  = -1; // layer or Ssd Sector
      Int_t ladder = -1;
      Int_t wafer  = -1;
      St_SurveyC *chair = 0;
      switch (kDetector) {
      // TPC Reference System
      case kTpcRefSys:
	Id = j;
	rotA = listOfDet2Align[i].chair->GetMatrix(0);
	rotA.SetName(Form(listOfDet2Align[i].Name,Id));
	break;
      case kTpcHalf: 
	if (indx[0] == 1) half = west;
	if (indx[0] == 2) half = east;
	Id = indx[0]+1;
	rotA = listOfDet2Align[i].chair->GetMatrix(half);
	rotA.SetName(Form(listOfDet2Align[i].Name,Id));
	break;
      case kTpcSecInn:
      case kTpcSecOut:
	break;
      case kTpcPad:
	sector = 12*(indx[0]-1) + indx[1];
	if (listOfDet2Align[i].Ndim == 3 && listOfDet2Align[i].NVmax[2] == 73) {
	  if (indx[2] <= 39) row = (indx[2]-1)/3 + 1;
	  else               row = 14 + (indx[2]-40);
	  if (row > 45)      row = 45;
	} else {
	  assert(0);
	}
	if (row <= NoOfInnerRows) chair = StTpcInnerSectorPosition::instance();
        else                      chair = StTpcOuterSectorPosition::instance();
	rotA = chair->GetMatrix(sector);
	rotA.SetName(Form(listOfDet2Align[i].Name,sector,row));
	break;
      default:
	assert(0);
	break;
      }
      if (rotA == rotL) continue;
      rotm = new TGeoHMatrix(rotA);
      //      rotm->SetName(Form(listOfDet2Align[i].Name,Id));
      if (Debug() > 1) {
	cout << "Id : " << Id << "\tBefore\t" << nodeP->GetName() << "\t"; nodeP->GetMatrix()->Print();
	//	if (Debug() > 1) cout << "Old Local:\t"; nodeP->GetNode()->GetMatrix()->Print();
	if (Debug() > 2) nodeP->Print();
      }
      nodeP->Align(rotm);
      if (Debug() > 1) {
	cout << "Id : " << Id << "\tAfter\t" << nodeP->GetName() << "\t"; nodeP->GetMatrix()->Print();
	if (Debug() > 2) nodeP->Print();
      }
    }
  }
  // Freeze misaligned geometry
  //  gGeoManager->RefreshPhysicalNodes();
  fAlignmentDone = kTRUE;
  return fAlignmentDone;
}
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
