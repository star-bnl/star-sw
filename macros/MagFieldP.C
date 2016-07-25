#include "Ask.h"
static const Int_t       nrp      = 20;       //  number of R nodes in the map
static const Int_t       nzp      = 40;       //  number of Z nodes in the map
static const Float_t     zm       = 200.0;     //  map max length
static const Float_t     rm       = 100.0;     //  map max radius
//________________________________________________________________________________
void MagFieldP(const Char_t *field = "FieldOn") {
//   if ( gClassTable->GetID("TGiant3") >= 0) { // root4star
//     gROOT->LoadMacro("bfc.C");
//     bfc(-1,"MagF,nodefault",0);
//   } else {
//   }
#if 1
  gROOT->LoadMacro("bfc.C");
  TString Chain("MagF,mysql,y2011,nodefault,");
  Chain += field;
  bfc(1,Chain,0);
#else
  gSystem->Load("StarMagField");
  new StarMagField();
#endif
  Float_t x[3];
  Float_t f[3];
  //  TString Mag(gSystem->Getenv("STAR_VERSION"));
  TString Mag(field);
  Mag += ".root";
  TFile *F = new TFile(Mag,"RECREATE");
  const Int_t ND = 4;
  TPrincipal *pB[3];
  const Char_t *pBnames[3] = {"Bx","By","Bz"};
  for (Int_t i = 0; i < 3; i++) {
    pB[i] = new TPrincipal(ND,"D");  F->Add(pB[i]);
    pB[i]->SetName(Form("p%s",pBnames[i]));
    pB[i]->SetTitle(Form("%s versus x y z",pBnames[i]));
  }
  Double_t data[4];
  Float_t dz =   zm/nzp;
  Float_t dr =   rm/nrp;
  TBenchmark *bench = new TBenchmark();
  for (Int_t i = -nrp; i <= nrp; i++) {
    x[0] = dr*i;
    for (Int_t j = -nrp; j <= nrp; j++) {
      x[1] = dr*j;
      for (Int_t k = -nzp; k <= nzp; k++) {
	x[2] = dz*k;
	bench->Start("Gufld");
	StarMagField::Instance()->B3DField(x,f);
	data[0] = 1e3*f[0];
	data[1] = x[0];
	data[2] = x[1];
	data[3] = x[2];
	pB[0]->AddRow(data);
	data[0] = 1e3*f[1];
	pB[1]->AddRow(data);
	data[0] = 1e3*f[2];
	pB[2]->AddRow(data);
	bench->Stop("Gufld");
      }
    }
  }
  bench->Print("Gufld");
  F->Write();
  for (Int_t i = 0; i < 3; i++) {
    pB[i]->MakePrincipals();
    pB[i]->Print("MSEV");
    pB[i]->Test();
    pB[i]->MakeCode(Form("%s.C",pB[i]->GetName()));
    if (Ask()) return;
  }
}
/*
Bx : 
   0 |1 
   1 |6.26679e-07 
   2 |-1.00991e-06 
   3 |-2.3281e-07 
------------------
   0 |-0.999999 
   1 |-0.000627046 
   2 |0.00101051 
   3 |0.000232845 
By :
   0 |1 
   1 |-1.02095e-06 
   2 |-8.76375e-07 
   3 |-3.32047e-07 
------------------
   0 |-0.999999 
   1 |0.00102152 
   2 |0.000876864 
   3 |0.000332095 
Bz :
   0 |1 
   1 |3.3938e-07 
   2 |8.77461e-07 
   3 |-8.07855e-06 
------------------
   0 |-0.999967 
   1 |-0.000340125 
   2 |-0.000879389 
   3 |0.00808289 

 */
