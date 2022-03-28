#include "Riostream.h"
#include "THnSparseProject.h"
#include "TObjString.h"
Int_t THnSparseProject::_debug = 0;
ClassImp(THnSparseProject)
//______________________________________________________________________________
THnSparseProject::THnSparseProject(const THnSparse *hs) : fHs(hs) {
  if (! fHs) return;
  fNdim = fHs->GetNdimensions();
  fCoord.Set(fNdim);
  fBins.Set(fNdim);
  fX.Set(fNdim);
  Int_t *coord = fCoord.GetArray();
  fAxis = (TAxis *) (*(fHs->GetListOfAxes()))[fNdim-1];
  fProjMap = new THashList(10000);
  Long64_t myLinBin = 0;
  THnIter iter(fHs, kTRUE /*use axis range*/);
  while ((myLinBin = iter.Next()) >= 0) {
    Double_t v = fHs->GetBinContent(myLinBin, coord);
    TString pName(hs->GetName());
    for (Int_t k = 0; k < fNdim - 1; k++) {
      pName += "_"; pName += coord[k];
    }
    TH1D *proj = (TH1D *) fProjMap->FindObject(pName);
    if (! proj) {
      proj = new TH1D(pName, fHs->GetTitle(), fAxis->GetNbins(), fAxis->GetXmin(), fAxis->GetXmax());
#if 0
      cout << "Histogram " << proj->GetName() << " has been created" << endl;
#endif      
      fProjMap->AddLast(proj);
    }
    Int_t bin = coord[fNdim-1];
    proj->AddBinContent(bin, v);
  }
}
//________________________________________________________________________________
TH1D* THnSparseProject::Next() {
  static TIter next(fProjMap);
  TH1D *proj = (TH1D *) next();
  if (proj) {
    TString Name(proj->GetName());
    TObjArray *obj = Name.Tokenize("_");
    Int_t nParsed = obj->GetEntries();
    Int_t j = 0;
    for (Int_t k = 1; k < nParsed; k++) {
      if (obj->At(k)) {
	TString A(((TObjString *) obj->At(k))->GetName());
	Int_t bin = A.Atoi();
	fBins[j] = bin;
	TAxis *axis = (TAxis *) (*(fHs->GetListOfAxes()))[j];
	fX[j] = axis->GetBinCenter(bin);
      }
      j++;
    }
    delete obj;
  }
  return proj;
}
