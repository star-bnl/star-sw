#include "Riostream.h"
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoNode.h"
#include "TObjArray.h"
#include "TArrayI.h"
//________________________________________________________________________________
const Char_t *MakeDetectorDescriptor(const Char_t *det) {
  enum Limits {nlvMAX=15,nskMAX=20,nvMAX=20};
  static TString path;
  path = "";
  UInt_t ivK[nskMAX][nlvMAX]; // Volume Ids found at the level nlv 
   Int_t muL[nskMAX][nlvMAX]; // No. of copies of Volume Id found at the level nvl
  UInt_t nsk[nlvMAX];         // No. of volume Ids found at the level nlv
  memset (ivK, 0, nskMAX*nlvMAX*sizeof(UInt_t));
  memset (muL, 0, nskMAX*nlvMAX*sizeof( Int_t));
  memset (nsk, 0,        nlvMAX*sizeof(UInt_t));
  TObjArray *vols = gGeoManager->GetListOfVolumes();
  UInt_t nvol = vols->GetEntriesFast();
  UInt_t nvl = 0;
  TString name(det);
  TGeoVolume *volD, *vol;
  TGeoNode *node;
  for (UInt_t ivol = 1; ivol <= nvol; ivol++) {
    vol = (TGeoVolume *) vols->At(ivol-1);
    if (! vol) {
      cout << "zero pointer in vols at i = " << ivol << endl;
      continue;
    }
    vol->SetUniqueID(ivol);
    if (name == TString(vol->GetName())) {
      ivK[nsk[nvl]][nvl] = ivol;
      nsk[nvl]++;
      if (nsk[nvl] > 1) {
	cout << name.Data() << " has been found " << nsk[nvl] << " times at level = " << nvl << endl;
      }
    }
  }
  if (! nsk[nvl]) {
    cout << "Detector " << det << " has not been found in geometry tree" << endl;
    return path.Data();
  }
  nvl = 0;
  UInt_t noSolutions = 0;
  while (nsk[0] > 0) {
    UInt_t ivos = ivK[nsk[nvl]-1][nvl];
    vol = (TGeoVolume *) vols->At(ivos-1);
    if (vol->IsTopVolume() && nvl  > 0) {
      if (path != "") path += ";";
      for (Int_t i = nvl; i >= 0; i--) {
	UInt_t ivol = ivK[nsk[nvl]-1][i];
	path += "/"; path += ((TGeoVolume *) vols->At(ivol-1))->GetName(); path += "_"; path += muL[nsk[nvl]-1][i];
      }
      noSolutions++;
      nsk[nvl]--;
      UInt_t ivl = 0;
      for (ivl = 0; ivl < nvl; ivl++) {if (nsk[ivl]) nvl = ivl;}
      continue;
    }
    for (UInt_t ivol = 1; ivol <= nvol; ivol++) {
      if (ivol == ivos) continue;
      vol = (TGeoVolume *) vols->At(ivol-1);
      UInt_t nd = vol->GetNdaughters();
      if (! nd) continue;
      Int_t multi = 0;
      for (UInt_t i = 0; i < nd; i++) {
	node = vol->GetNode(i);
	if (! node) continue;
	volD = node->GetVolume();
	if ( ivos != volD->GetUniqueID()) continue;
	multi = TMath::Max(multi,node->GetNumber());
      }
      if (! multi) continue;
      // New level found
      if (nvl == nlvMAX) {
	cout << "Parameter nlvMAX too small" << endl;
	break;
      }
      if (!nsk[nvl+1]) nvl++;
      if (nsk[nvl] == nskMAX) {
	cout << "Parameter nskMAX too small" << endl;
	break;
      }
      nsk[nvl]++;
      muL[nsk[nvl]-1][nvl] = multi;
      ivK[nsk[nvl]-1][nvl] = ivol;
    }
    if (nsk[nvl] == 0) {
      cout << "Hanging volume " << vols->At(ivos-1)->GetName() << endl;
      break;
    }
  }
  return path.Data();
}
//_____________________________________________________________________________
void MakeDetectorDescriptors() {
  if (! gGeoManager) {
    cout << "No gGeoManager" << endl;
    return;
  }
#if 0
  TDataSet *Detectors = StMaker::GetChain()->GetDataBase("VmcGeometry/Index");
  if (! Detectors) {
    cout << "No Detectors found in VmcGeometry/Index" << endl;
  }
#endif
  // Make List of sensitive volumes
  TObjArray *vols = gGeoManager->GetListOfVolumes();
  Int_t nvol = vols->GetEntriesFast();
  Int_t nSensVol = 0;
  Int_t nsize = 100;
  TArrayI Indx(nsize); Int_t *indx = Indx.GetArray();
  for (Int_t i = 0; i < nvol; i++) {
    TGeoVolume *vol = (TGeoVolume *) vols->At(i);
    if (! vol) continue;
    TGeoMedium *med = vol->GetMedium();
    if (! med) continue;
    Int_t       isvol = (Int_t) med->GetParam(0);
    if (! isvol) continue;
    indx[nSensVol] = i;
    nSensVol++;
    if (nSensVol >= nsize) {
      nsize *= 2;
      Indx.Set(nsize); 
      indx = Indx.GetArray();
    }
    TString Path(MakeDetectorDescriptor(vol->GetName()));
#if 0
    if (Detectors) {
      // Check consistency 
      StarVMCDetector *det = (StarVMCDetector *) Detectors->Find(vol->GetName());
      if (! det) {
	cout << "Detector description for " << vol->GetName() << "\t" << vol->GetTitle() << " is missing" << endl;
      } else {
	TString FMT = det->GetFMT();
	cout << "Found path:\t" << Path.Data() << endl;
	cout << "Set   path:\t" << FMT.Data();
	if (Path == FMT) cout << " are the same" << endl;
	else             cout << " are the different" << endl;
      }
    }
#endif    
  }
}
