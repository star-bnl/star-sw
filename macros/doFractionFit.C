/*
  root.exe MudEdx1_Sparse_pT100_eta24.NewdX.root 'doFractionFit.C("sZdEdx",-1,-1,1,-1,1,-1,1,kTRUE,kTRUE,3)'
*/
void doFractionFit(const Char_t *name = "sZdEdx",
		   Int_t r1 =-1, Int_t r2 = -1, // RefMult, r1 < 0 -> for all, r2 = -1 => NrefMult
		   Int_t i1 = 1, Int_t i2 = -1, // pT
		   Int_t j1 = 1, Int_t j2 = -1, // eta
		   Int_t slide = 1,             // sum over [bin-slide,bin+slide] for pT and eta bins
		   Bool_t doStop = kFALSE,
		   Bool_t doMakePng = kFALSE,
		   Int_t Niter = 3)
{
  gROOT->ProcessLine(".x lMuDst.C");
  //  gROOT->LoadMacro("FitP_t.h+");
  gROOT->LoadMacro("MudEdx.C+");
  Fraction(name,r1,r2,i1,i2,j1,j2,slide,doStop,doMakePng,Niter);
}
