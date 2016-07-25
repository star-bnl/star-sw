static const Int_t       nrp      = 200;       //  number of R nodes in the map
static const Int_t       nzp      = 800;       //  number of Z nodes in the map
static const Float_t     zm       = 800.0;     //  map max length
static const Float_t     rm       = 400.0;     //  map max radius
static const Int_t       nphi     =  36;
//________________________________________________________________________________
void MagFldCheck(const Char_t *field = "Check") {
  TFile *f = new TFile("StarFieldZ.root");
  if (! f) return;
  TH2F *Br0 = (TH2F *) f->Get("Br0");
  TH2F *Bz0 = (TH2F *) f->Get("Bz0");
  if (! Br0 || ! Bz0) return;
  Br0->SetDirectory(0);
  Bz0->SetDirectory(0);
  delete f;
//   if ( gClassTable->GetID("TGiant3") >= 0) { // root4star
//     gROOT->LoadMacro("bfc.C");
//     bfc(-1,"MagF,nodefault",0);
//   } else {
//   }
  if (gClassTable->GetID("StarMagField") < 0) {
    gSystem->Load("StarMagField");
    new StarMagField();
  }
  Float_t X[3];
  Float_t B[3];
  Float_t Brpz[3];
  
  //  TString Mag(gSystem->Getenv("STAR_VERSION"));
  TString Mag(field);
  Mag += ".root";
  TFile *F = new TFile(Mag,"RECREATE");
  Br0->Write();
  Bz0->Write();
  TH2F *Br = new TH2F(*Br0); Br->SetName("Br");
  TH2F *Bz = new TH2F(*Bz0); Bz->SetName("Bz");
  Int_t ny = Br->GetNbinsY();
  Int_t nx = Br->GetNbinsX();
  X[1] = 0;
  for (Int_t i = 1; i <= nx; i++) {
    X[2] = Br->GetXaxis()->GetBinCenter(i);
    for (Int_t j = 1; j <= ny; j++) {
      StarMagField::Instance()->BField(X,B);
      Br->SetBinContent(i,j,1e3*B[0]);
      Bz->SetBinContent(i,j,1e3*B[2]);
      X[0] = Br->GetYaxis()->GetBinCenter(j);
    }
  }
  F->Write();
  //  delete F;
}
  
