static const Int_t       nrp      = 200;       //  number of R nodes in the map
static const Int_t       nzp      = 800;       //  number of Z nodes in the map
static const Float_t     zm       = 800.0;     //  map max length
static const Float_t     rm       = 400.0;     //  map max radius
static const Int_t       nphi     =  36;
//________________________________________________________________________________
void Agufld(const Char_t *field = "FieldOn") {
//   if ( gClassTable->GetID("TGiant3") >= 0) { // root4star
//     gROOT->LoadMacro("bfc.C");
//     bfc(-1,"MagF,nodefault",0);
//   } else {
//   }
  if (gClassTable->GetID("StarMagField") < 0) {
#if 0
    gROOT->LoadMacro("bfc.C");
    TString Chain("MagF,mysql,y2011,nodefault,");
    Chain += field;
    bfc(1,Chain,0);
#else
    if (! gROOT->GetClass("StarMagField")) {
      gSystem->Load("StarMagField");
      new StarMagField();
    }
#endif
  }
  Float_t X[3];
  Float_t B[3];
  Float_t Brpz[3];
  //  TString Mag(gSystem->Getenv("STAR_VERSION"));
  TString Mag(field);
  Mag += ".root";
  TFile *F = new TFile(Mag,"RECREATE");
  TH3D* fieldZ = new TH3D("fieldZ","B_{Z};#rho;#phi;z",nrp, 0, rm, nphi, 0., 360., nzp, -zm, zm);
  TH3D* fieldR = new TH3D("fieldR","B_{R};#rho;#phi;z",nrp, 0, rm, nphi, 0., 360., nzp, -zm, zm);
  TH3D* fieldP = new TH3D("fieldP","B_{#phi};#rho;#phi;z",nrp, 0, rm, nphi, 0., 360., nzp, -zm, zm);
  Float_t dz = 2*zm/nzp;
  Float_t dr =   rm/nrp;
  Float_t dphi = 360./nphi;
  TBenchmark *bench = new TBenchmark();
  for (Float_t phi = dphi/2; phi < 360; phi += dphi ) {
    Double_t cx = TMath::Cos(TMath::DegToRad()*phi);
    Double_t cy = TMath::Sin(TMath::DegToRad()*phi);
    for (Float_t r = dr/2; r < rm; r += dr ) {
      Float_t x = r*cx;
      Float_t y = r*cy;
      for (Float_t z = -zm + dz/2; z < zm; z += dz) {
	X[0] = x;
	X[1] = y;
	X[2] = z;
	bench->Start("Gufld");
#if 0
	StMagF::Agufld(X,B);
#else
	StarMagField::Instance()->BField(X,B);
#endif
	bench->Stop("Gufld");
	Brpz[0] =  B[0]*cx + B[1]*cy;
	Brpz[1] = -B[0]*cy + B[1]*cx;
	Brpz[2] =  B[2];
	fieldR->Fill(r,phi,z,Brpz[0]);
	fieldP->Fill(r,phi,z,Brpz[1]);
	fieldZ->Fill(r,phi,z,Brpz[2]);
      }
    }
  }
  bench->Print("Gufld");
  F->Write();
  //  delete F;
}
  
