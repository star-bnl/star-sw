// $Id: StarVMCDetectorSet.cxx,v 1.4 2013/12/16 23:00:14 fisyak Exp $
// Support for detector description
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "Stiostream.h"
#include "StarVMCApplication.h"
#include "StarVMCDetectorSet.h"
#include "TGeoManager.h"
#include "TGeant3TGeo.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "TDataSetIter.h"
#include "TPDGCode.h"
#include "TVirtualMC.h"
#include "TArrayI.h"
#include "TObjArray.h"
#include "TObjString.h"
#include "TClass.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TRandom.h"
#include "TLorentzVector.h"
#include "TFile.h"
#include "StMaker.h"
#include "tables/St_det_path_Table.h"
#include "TObjectSet.h"
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TInterpreter.h"
#include "TGeoExtension.h"
ClassImp(StarVMCDetectorSet);
StarVMCDetectorSet *StarVMCDetectorSet::fgInstance = 0;
TDataSet           *StarVMCDetectorSet::fDetectorDescriptors = 0;
//________________________________________________________________________________
StarVMCDetectorSet::StarVMCDetectorSet(const Char_t *name,const Char_t *title) : 
  TDataSet(name,title),  fDetHash(0) { 
  fgInstance = this; 
  fDetHash = new THashList(100,0);
  fDetHash->SetOwner(kTRUE);
  fDetectorDescriptors = new TDataSet("Index");
  Init();
}
//________________________________________________________________________________
StarVMCDetectorSet *StarVMCDetectorSet::instance() {
  if (! fgInstance) fgInstance = new StarVMCDetectorSet();
  return fgInstance;
}
//________________________________________________________________________________
Int_t StarVMCDetectorSet::Init() {
  cout << "StarVMCDetectorSet::Init() -I- Get Detectors" <<endl;
  MakeDetectorDescriptors();
  return 0;
}
//________________________________________________________________________________
Int_t StarVMCDetectorSet::LoopOverTgeo(TGeoNode *nodeT, TString pathT) {
  if (! gGeoManager) {
    cout << "No gGeoManager" << endl;
    return 0; 
  }
  Int_t NoSensVolumes = 0;
  if (! nodeT) { 
    if (! gGeoManager) (TObjectSet *) gDirectory->Get("Geometry"); 
    if (! gGeoManager) return NoSensVolumes;
    gGeoManager->RestoreMasterVolume();
    gGeoManager->cd("HALL_1");
    nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) return NoSensVolumes;
    TString path = nodeT->GetName();
#if 0
    cout << "top " << nodeT->GetName() << "\t" << nodeT->GetVolume()->GetName() 
	 << "\t" << path << endl;
#endif
    NoSensVolumes += LoopOverTgeo(nodeT,path);
    return NoSensVolumes;
  } 
  TGeoVolume *vol = nodeT->GetVolume();
  TObjArray *nodes = vol->GetNodes();
  Int_t nd = nodeT->GetNdaughters();
#if 0
  cout << nd << "\t" << nodeT->GetName() 
       << "\t" << vol->GetName() 
       << "\t" << gGeoManager->GetCurrentNode()->GetName() << "\t" << pathT 
       << endl;
  if (pathT.Contains("TpcRefSys")) {
    cout << "Got TpcRefSys" << endl;
  }
#endif
  Int_t NoSensDauthers = 0;
  for (Int_t id = 0; id < nd; id++) {
    TGeoNode *node = (TGeoNode*)nodes->UncheckedAt(id);
    if (! node) continue;
    vol = node->GetVolume();
    if (! vol) continue; 
    TString path = pathT;
    if (path != "") path += "/";
    path += node->GetName();
    //      gGeoManager->cdDown(node->GetIndex());
    Int_t nodeid = gGeoManager->GetCurrentNode()->GetVolume()->GetIndex(node);
    gGeoManager->CdDown(nodeid);
#if 0
    cout << "path " << path << endl;
#endif
    //      gGeoManager->cd(node->GetName());
    //      gGeoManager->cdNode(node->GetIndex());
    NoSensDauthers += LoopOverTgeo(node,path);
    gGeoManager->CdUp();
  }
  NoSensVolumes += NoSensDauthers;
  TGeoMedium     *med = vol->GetMedium();
  Int_t           isvol = 0;
  if (med)        isvol = (Int_t) med->GetParam(0);
  TString Name(vol->GetName());
  if (NoSensDauthers == 0 && isvol) {
    //      cout << "sens. vol. " << pathT << endl;
    if (! vol->GetUserExtension()) {
      StarVMCDetector *det = (StarVMCDetector *) fDetHash->FindObject(vol->GetName());
      if (! det) {
	det = new StarVMCDetector(vol->GetName());
	fDetHash->Add(det);
      }
      if (!det->TestBit(kInvalidObject)) {
	TGeoRCExtension *ext = new TGeoRCExtension(det);
	vol->SetUserExtension(ext);
      }
    }
    TObjString *objs;
    static const TString separator("/_");
    TObjArray *array = pathT.Tokenize(separator); 
    Int_t N = array->GetEntriesFast();
    det_path_st dpathT;
    St_det_path *dpath = (St_det_path *) fDetectorDescriptors->Find(vol->GetName());
    if (! dpath) {
      dpath = new St_det_path(vol->GetName(),N/2);
      cout << "Create detector path for volume " << vol->GetName() << endl;
      fDetectorDescriptors->Add(dpath);
      for (Int_t i = 0; i < N; i +=2) {
	objs = (TObjString *) array->At(i); // cout << objs->GetString().Data() << endl;
	TString Name(objs->GetString());
	if (Name == "") continue;
	objs = (TObjString *) array->At(i+1); // cout << objs->GetString().Data() << endl;
	Int_t j = atoi(objs->GetString().Data());
	strcpy(&dpathT.VName[0],Name.Data());
	dpathT.Ncopy = j;
	dpathT.Nb = 0;
	dpath->AddAt(&dpathT);
      }
    } else {
      for (Int_t i = 0; i < N; i +=2) {
	objs = (TObjString *) array->At(i); // cout << objs->GetString().Data() << endl;
	TString Name(objs->GetString());
	objs = (TObjString *) array->At(i+1); // cout << objs->GetString().Data() << endl;
	Int_t j = atoi(objs->GetString().Data());
	det_path_st *row = dpath->GetTable();
	Int_t Nr = dpath->GetNRows();
	Int_t l = -1;
	for (Int_t k = 0; k < Nr; k++, row++) {
	  if (TString(row->VName) == Name) {
	    l = k;
	    if (j > row->Ncopy) {
	      row->Ncopy = j;
	      //	      dpath->Print(k,1);
	    }
	    break;
	  }
	}
	if (l < 0) {
	  strcpy(dpathT.VName,Name.Data());
	  dpathT.Ncopy = j;
	  dpathT.Nb = 0;
	  dpath->AddAt(&dpathT);
	  //	  dpath->Print(0,Nr+1);
	}
      }
    }
    delete array;
    //      dpath->Print(0,N);
  }
  return NoSensVolumes;
}
//________________________________________________________________________________
void StarVMCDetectorSet::MakeDetectorDescriptors() {
  if (! gGeoManager) {
    cout << "StarVMCDetectorSet::MakeDetectorDescriptors No gGeoManager" << endl;
    return;
  }

  LoopOverTgeo();

  if (! fDetectorDescriptors) {cout << "Can't find Detector Descriptors " << endl; return;}
  TDataSetIter next(fDetectorDescriptors,99);
  TDataSet *d = 0;
  Int_t Nd = 0;
  while ((d = next())) {
    if (! d->HasData()) continue;
    St_det_path *table = (St_det_path *) d;
    Int_t N = table->GetNRows();
    det_path_st *path = table->GetTable();
    Nd++;
    cout << Nd << "\t";
    Int_t NV   = 0;
    Int_t NVMax[20];
    Int_t NVp10[20];
    TString FMT;
    TString Name;
    for (Int_t i = 0; i < N; i++, path++) {
      cout << path->VName << "[" << path->Ncopy << "]";
      if (i != N-1) cout << "/";
      FMT += "/";  FMT += path->VName; FMT += "_";
      if (path->Ncopy == 1) {
	FMT += "1";
      } else {
	FMT += "%d";
	NVMax[NV] = path->Ncopy;
	Int_t m = TMath::Log10(NVMax[NV]) + 1;
	NVp10[NV] = TMath::Power(10.,m);
	NV++;
      }
      Name = path->VName;
    }
    StarVMCDetector *det = (StarVMCDetector *) fDetHash->FindObject(Name);
    if (!det) {
      cout << "Missing Detector Set for " << Name.Data() << endl;
      continue;
    }
    cout << endl;
    det->SetFMT(FMT);
    det->SetNVmax(NV,NVMax);
    det->SetNVp10(NV,NVp10);
    N++;
  }
  fDetHash->Rehash(Nd);
}
//________________________________________________________________________________
// $Log: StarVMCDetectorSet.cxx,v $
// Revision 1.4  2013/12/16 23:00:14  fisyak
// Add g2t_volume_id
//
// Revision 1.3  2011/10/10 20:16:08  fisyak
// Handle for multiple geometries
//
// Revision 1.2  2009/04/15 23:04:33  fisyak
// Handle StarVMCDetector with care
//
// Revision 1.1  2009/04/15 20:46:08  fisyak
// Revise nameing convention toward to Sti usage
//
