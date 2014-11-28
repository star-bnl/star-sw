// $Id: StarVMCApplication.cxx,v 1.13 2013/12/16 22:58:53 fisyak Exp $
// Class StarVMCApplication
// ----------------------- 
// Implementation of the TVirtualMCApplication
#include <assert.h>
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
// Alignment
#include "StDetectorDbMaker/St_tpcGlobalPositionC.h"
#include "StDetectorDbMaker/St_tpcSectorPositionC.h"
#include "StDetectorDbMaker/StSsdSurveyC.h"
#include "StDetectorDbMaker/StSvtSurveyC.h"
#include "StSvtDbMaker/StSvtDbMaker.h"
#include "StSsdDbMaker/StSsdDbMaker.h"
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
  fAlignment(kFALSE) //(kTRUE)
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
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,22,0)
//________________________________________________________________________________
Bool_t StarVMCApplication::MisalignGeometry() {
  if (! fAlignment) {
    cout << "No MisalignGeometry has been applied" << endl;
    return fAlignment;
  }
  enum EDetector2Align {
    kTPC, kTpcRefSys, kTPCSector, 
    kSVT, kSvtWhole, kSvtShell, kSvtLadder, kSvtWafer, 
    kSSD, kSsdWhole, kSsdSector, kSsdLadder, kSsdWafer};
  struct listOfDetectorToAlign_t {
    const Char_t *Name; 
    EDetector2Align kDet;
    const Char_t *path;
    const Char_t *tableName;
    Int_t  Ndim;
    Int_t  NVmax[6];
  };
  static const listOfDetectorToAlign_t listOfDet2Align[] = {                                                                
#if 0
    {"TpcRefSys-%d",    kTpcRefSys,"/HALL_1/CAVE_1/TpcRefSys_%d",                                                 
     "tpcGlobalPosition", 1, {1,  0, 0, 0, 0, 0}}, 
    {"TpcPadPlane-%03d",kTPCSector,"/HALL_1/CAVE_1/TpcRefSys_%d/TPCE_1/TpcSectorWhole_%d/TpcGas_1/TpcPadPlane_%d",
     "tpcSectorPosition", 3, {1, 24, 2, 0, 0, 0}},
#endif
    {"SVTT%d",          kSvtWhole, "/HALL_1/CAVE_1/TpcRefSys_%d/SVTT_%d",
     "SvtOnGlobal",       2, {1,  1, 0, 0, 0, 0}},
    {"ClamShell-%d",    kSvtShell, "/HALL_1/CAVE_1/TpcRefSys_%d/SVTT_%d/ClamShell_%d",                            
     "ShellOnGlobal",     3, {1,  1, 2, 0, 0, 0}},                   //              L        l          W   
    {"SvtLadder-%d",    kSvtLadder,"/HALL_1/CAVE_1/TpcRefSys_%d/SVTT_%d/ClamShell_%d/Layer_%d/Ladder_%d",
     "LadderOnShell",     5, {1,  1, 2, 6,16, 0}}, 
    {"SvtWafer-%04d",   kSvtWafer, "/HALL_1/CAVE_1/TpcRefSys_%d/SVTT_%d/ClamShell_%d/Layer_%d/Ladder_%d/SLDI_1/svtd_%d",
     "WaferOnLadder",     6, {1,  1, 2, 6,16, 7}}, 
    {"SFMO",            kSsdWhole, "/HALL_1/CAVE_1/TpcRefSys_%d/SVTT_%d/SFMO_%d",
     "SsdOnGlobal",       3, {1,  1, 1, 0, 0, 0}},
    {"SsdSector-%d",    kSsdSector,"/HALL_1/CAVE_1/TpcRefSys_%d/SVTT_%d/SFMO_%d/SsdSector_%d",
     "SsdSectorsOnGlobal",4, {1,  1, 1, 4, 0, 0}},
    {"SsdLadder-%d",    kSsdLadder,"/HALL_1/CAVE_1/TpcRefSys_%d/SVTT_%d/SFMO_%d/SsdSector_%d/SFLM_%d/SFDM_1",
     "SsdLaddersOnSectors",5, {1,  1, 1, 4,20, 0}},
    {"SsdWafer-%04d",   kSsdWafer, "/HALL_1/CAVE_1/TpcRefSys_%d/SVTT_%d/SFMO_%d/SsdSector_%d/SFLM_%d/SFDM_1/SFSW_%d",
     "SsdWafersOnLadders",6, {1,  1, 1, 4,20,16}},
  };
  static const Int_t  NoDetectos2Align = sizeof(listOfDet2Align)/sizeof(listOfDetectorToAlign_t);
  TString path; 
  TGeoHMatrix *rotm = 0;
  // TPC subsectors
  TGeoTranslation T123(0,123,0); T123.SetName("T123"); if (Debug() > 1) T123.Print();
  TGeoTranslation TIO[2]; 
  TIO[0].SetName("TI");
  TIO[1].SetName("TO");
  TGeoRotation    RIO[2];
  RIO[0].SetName("RI"); 
  RIO[1].SetName("RO"); 
#if 0
  Double_t offset;
  Double_t phi;
#endif
  TGeoTranslation SvtShift, SvtShiftI;
  Double_t yWaferAverShift = -9999;
  //
  Int_t indx[25];
  TGeoPhysicalNode *nodeP = 0;

  for (Int_t i = 0; i < NoDetectos2Align; i++) {
    EDetector2Align kDetector = listOfDet2Align[i].kDet;
    memset(indx, 0, 25*sizeof(Int_t));
    Int_t Ntot = 1;
    for (Int_t k = 0; k < listOfDet2Align[i].Ndim; k++) Ntot *= listOfDet2Align[i].NVmax[k];
    for (Int_t j = 0; j < Ntot; j++) {
      Int_t ind = j;
      for (Int_t k =  listOfDet2Align[i].Ndim - 1; k >= 0; k--) {
	indx[k] = ind%listOfDet2Align[i].NVmax[k]+1; ind /= listOfDet2Align[i].NVmax[k];
      }
      rotm = 0;
      Int_t Id = -1;
      Int_t id =  0; // standard numeration
      Int_t sector = indx[2]; // shell or Tpc sector
      Int_t layer  = indx[3]; // layer or Ssd Sector
      Int_t barrel = (layer-1)/2 + 1;
      Int_t ladder = indx[4];
      Int_t wafer  = indx[5];
      THashList *RotList = 0;
      path = StarVMCDetector::FormPath(listOfDet2Align[i].path,listOfDet2Align[i].Ndim,indx);
      if (! gGeoManager->CheckPath(path)) continue;
      TObjArray *objs = gGeoManager->GetListOfPhysicalNodes();
      if (objs) nodeP = (TGeoPhysicalNode *) objs->FindObject(path);
      if (nodeP) {
	if (nodeP->IsAligned()) {
	  cout << nodeP->GetName() << " has been aligned (?)" << endl;
	  continue;
	}
      } else {
	nodeP = gGeoManager->MakePhysicalNode(path);
      }
      TGeoHMatrix rotL = *(nodeP->GetNode()->GetMatrix()); // ideal matrix
      TGeoHMatrix rotA;
      // TPC Reference System
      if (kDetector == kTpcRefSys) {
#if 0
	St_tpcGlobalPositionC *tpcGlobalPosition = St_tpcGlobalPositionC::instance();
	assert(tpcGlobalPosition);
	Id = 1;
	Double_t rad2deg = 180./TMath::Pi();
	Double_t phi   = 0.0;  //large uncertainty, so set to 0
	Double_t theta = tpcGlobalPosition->PhiXZ_geom()*rad2deg;
	Double_t psi   = tpcGlobalPosition->PhiYZ_geom()*rad2deg; 
	rotA.RotateX(-psi);
	rotA.RotateY(-theta);
	rotA.RotateZ(-phi);
	Double_t transTpcRefSys[3] = {tpcGlobalPosition->LocalxShift(),
				      tpcGlobalPosition->LocalyShift(),
				      tpcGlobalPosition->LocalzShift()};
	rotA.SetTranslation(transTpcRefSys);
	rotA.SetName(Form(listOfDet2Align[i].Name,Id));
#endif
      } 
      // TPC sub sectors
      else if (kDetector == kTPCSector) {
#if 0
	sector = indx[1];
	Int_t io     = indx[2];  // Inner Outer subsectors
	Id = 10*sector + io;
	Double_t s = -1;
	if (sector > 12) s = +1;
	if (io == 1) {
	  offset = s*St_tpcSectorPositionC::instance()->innerPositionOffsetX(sector-1);
	  phi    = s*St_tpcSectorPositionC::instance()->innerRotation(sector-1);
	} else {
	  offset = s*St_tpcSectorPositionC::instance()->outerPositionOffsetX(sector-1);
	  phi    = s*St_tpcSectorPositionC::instance()->outerRotation(sector-1);
	}
	TIO[io-1].SetTranslation(-offset, -123, 0); if (Debug() > 1) TIO[io-1].Print();
	RIO[io-1].SetAngles(-phi,0,0);              if (Debug() > 1) RIO[io-1].Print();
	rotA = T123 * RIO[io-1] * TIO[io-1];
#endif
      } else {
	// SVT and SSD
	St_SurveyC *survey = St_SurveyC::instance(listOfDet2Align[i].tableName);
	assert(survey);
	/* SVT
	   Id = 0                                for SvtOnGlobal
	   Id = [0, 1]                           for ShellOnGlobal, 
	   0 is the x (South) Shell, 1 is the -x (North) Shell"
	   Id = 1000*barrel + ladder             for LadderOnSurvey
	   Id = 1000*barrel + ladder             for LadderOnShell
	   Id = 1000*barrel + 100*wafer + ladder for WaferOnLadder
	   
	   SSD
	   Id = 0                                for SsdOnGlobal
	   Id = sector [1-4]                     SsdSectorsOnGlobal
	   Id = 100*sector + ladder              SsdLaddersOnSectors
	   Id = 7000 + 100*wafer + ladder        SsdWafersOnLadders
	*/
	switch (kDetector) { 
	case kSvtWhole:   Id = id = 0; break;
	case kSvtShell:   Id = id = sector - 1; break;
	case kSvtLadder:  Id = id = 1000*barrel + ladder; break;
	case kSvtWafer:   
	  Id = 1000*barrel + 100*wafer + ladder; 
	  id = 1000*layer  + 100*wafer + ladder; 
	  RotList = gStSvtDbMaker->GetRotations(); break;
	case kSsdWhole:   Id = id = 0; break;
	case kSsdSector:  Id = id = layer; break;
	case kSsdLadder:  Id = id = 100*layer + ladder; break;
	case kSsdWafer:   Id = id = 7000 + 100*wafer + ladder; RotList =  gStSsdDbMaker->GetRotations(); break;
	default: Id = id = -1; break;
	};
	if (Id < 0) {
	  cout << "StarVMCApplication::MisalignGeometry unrecognized detector " 
	       << listOfDet2Align[i].Name << endl;
	  continue;
	}
	Int_t Nrows = survey->getNumRows();
	Int_t l;
	for (l = 0; l < Nrows; l++) {
	  if (Id == survey->Id(l)) break;
	}
	if (l >= Nrows) {
	  cout << "Id = " << Id << " has not been found in " << survey->GetName() << endl;
	  break;
	}
	rotA.SetRotation(survey->r(l));
	rotA.SetTranslation(survey->t(l));
	if (kDetector == kSsdWhole) {
	  St_SurveyC *surv = St_SurveyC::instance("SvtOnGlobal");
	  TGeoHMatrix rotB;
	  rotB.SetRotation(surv->r(0));
	  rotB.SetTranslation(surv->t(0));
	  TGeoHMatrix rotC = rotB.Inverse();
	  rotA = rotC * rotA;
	}
	if (kDetector == kSvtLadder) {
	  if (yWaferAverShift < -999) {
	    St_SurveyC *surw = St_SurveyC::instance("WaferOnLadder");
	    Int_t N = surw->getNumRows();
	    Int_t l;
	    yWaferAverShift = 0;
	    for (l = 0; l < N; l++) {
	      yWaferAverShift += surw->t1(l);
	    }
	    yWaferAverShift /= N;
	    // Check that it was used wafer survey (H.Ward) or ideal position
	    SvtShift = TGeoTranslation(0,yWaferAverShift,23.525);
	    SvtShiftI = SvtShift.Inverse();
	  }
	  TGeoHMatrix LSU;
	  St_SurveyC *surv = St_SurveyC::instance("LadderOnSurvey");
	  for (l = 0; l < Nrows; l++) {
	    if (Id == surv->Id(l)) break;
	  }
	  if (l >= Nrows) {
	    cout << "Id = " << Id << " has not been found in " << surv->GetName() << endl;
	    break;
	  }
	  LSU.SetRotation(surv->r(l));
	  Double_t tr[3] = {surv->t0(l), surv->t1(l), surv->t2(l)};
	  LSU.SetTranslation(tr);
	  rotA = rotA * LSU * SvtShift;
	}
	if (kDetector == kSvtWafer) { // <<<<
	  assert(yWaferAverShift > -999);
	  rotA = SvtShiftI * rotA;
	}
	if (kDetector == kSsdLadder) { // remove (L-1) Matrix to avoid double counting
	  Int_t l = nodeP->GetLevel() -1 ;
	  TGeoHMatrix *rotL1 = (TGeoHMatrix *) nodeP->GetNode(l)->GetMatrix();
	  TGeoHMatrix  rotLI = rotL1->Inverse();
	  rotA = rotLI * rotA;
	}
      }
      // Normalize
      Double_t *r = rotA.GetRotationMatrix();
      Double_t norm;
      TVector3 d(r[0],r[3],r[6]); norm = 1/d.Mag(); d *= norm;
      TVector3 t(r[2],r[5],r[8]); norm = 1/t.Mag(); t *= norm;
      TVector3 n(r[1],r[4],r[7]);
      TVector3 c = d.Cross(t);
      if (c.Dot(n) < 0) c *= -1;
      Double_t rot[9] = {
      d[0], c[0], t[0],
      d[1], c[1], t[1],
      d[2], c[2], t[2]};
      rotA.SetRotation(rot);
      
      if (rotA == rotL) continue;
      rotm = new TGeoHMatrix(rotA);
      rotm->SetName(Form(listOfDet2Align[i].Name,Id));
      if (Debug() > 1) {
	cout << "Id : " << id << "\tBefore\t" << nodeP->GetName() << "\t"; nodeP->GetMatrix()->Print();
	//	if (Debug() > 1) cout << "Old Local:\t"; nodeP->GetNode()->GetMatrix()->Print();
	if (Debug() > 2) nodeP->Print();
      }
      nodeP->Align(rotm);
      if (Debug() > 1) {
	cout << "Id : " << id << "\tAfter\t" << nodeP->GetName() << "\t"; nodeP->GetMatrix()->Print();
	if (RotList) {
	  TGeoHMatrix *comb = (TGeoHMatrix *) RotList->FindObject(Form("R%04i",id));
	  if (comb) comb->Print();
	}
	//	if (Debug() > 1) cout << "New Local:\t"; nodeP->GetNode()->GetMatrix()->Print();
	if (Debug() > 2) nodeP->Print();
      }
      continue;
    }
  }
  // Freeze misaligned geometry
  //  gGeoManager->RefreshPhysicalNodes();
  fAlignment = kTRUE;
  return fAlignment;
}
#endif
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
