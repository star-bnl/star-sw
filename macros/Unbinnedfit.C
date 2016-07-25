// for ntuple generation

void Unbinnedfit() {

  gROOT->Reset();
  TFile *hfile = gROOT->FindObject("hsimple.root");
  if (hfile) hfile->Close();
  hfile = new TFile("hsimple.root","RECREATE","Random Root File");
  ntuple = new TNtuple("ntuple","Random ntuple","px:i");

  gRandom->SetSeed();
  Float_t px,py;

  for ( Int_t i=0; i<25000; i++) {
    gRandom->Rannor(px,py);
    ntuple->Fill(px,i);
  }
  hfile->Write();
}


// for fitting
void
example2() {

  TFile *f = gROOT->FindObject("hsimple.root");
  if (!f) f= new TFile("hsimple.root");
  TTree *tree= (TTree*)f.Get("ntuple");

  TF1* f1 = new TF1("f1", "gaus(0)/sqrt(2*3.14159)/[2]", 0, 5);
  f1->SetParameters(1.0, 0.2, 0.3);
  f1->SetParLimits(0, 1, 1); // for correct normalization

  tree->Draw("px");
  tree->UnbinnedFit("f1", "px" );
}


