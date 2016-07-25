#include "Riostream.h"
#include "TString.h"
#include "THnSparse.h" 
#include "TAxis.h"
THnSparseF *CompressTHn(THnSparseF *hist, Double_t compress = 1e3) { 
  if (! hist) return 0;
  Int_t nd = hist->GetNdimensions();
  Int_t *nbins = new  Int_t[nd];
  for (Int_t i = 0; i < nd; i++) nbins[i] = hist->GetAxis(i)->GetNbins();
  THnSparseF *hnew = new THnSparseF(Form("%sC",hist->GetName()),hist->GetTitle(),nd, nbins, 0, 0, hist->GetChunkSize());
  delete [] nbins;
  for (Int_t i = 0; i < nd; i++) {
    TAxis *ax = hist->GetAxis(i);
    if (ax->IsVariableBinSize()) hnew->GetAxis(i)->Set(ax->GetNbins(), ax->GetXbins()->GetArray());
    else                         hnew->GetAxis(i)->Set(ax->GetNbins(), ax->GetXmin(), ax->GetXmax());
  }
  Int_t *bins = new Int_t[nd];
  Long64_t N = hist->GetNbins(); cout << hist->GetName() << " has " << N << " bins." << endl;
  Double_t max = -1;
  for (Long64_t i = 0; i < N; ++i) {
    Double_t cont = hist->GetBinContent(i, bins);
    if (cont > max) max = cont;
  }
  for (Long64_t i = 0; i < N; ++i) {
    Double_t cont = hist->GetBinContent(i, bins);
    if (cont < max/compress) continue;
    Long64_t bin = hnew->GetBin(bins);
    hnew->Fill(bin,cont);
  }
  cout << hnew->GetName() << " has " << hnew->GetNbins() << " bins." << endl;

  return hnew;
}
