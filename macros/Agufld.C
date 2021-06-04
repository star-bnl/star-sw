#define __3D__
//________________________________________________________________________________
//void Agufld(const Char_t *sdt = "sdt20210303.052529", const Char_t *field="RF") {
void Agufld(const Char_t *sdt = "sdt20210522.024326", const Char_t *field="FF") {
//void Agufld(const Char_t *sdt = "sdt20210510.134727", const Char_t *field="RF") {
//   if ( gClassTable->GetID("TGiant3") >= 0) { // root4star
//     gROOT->LoadMacro("bfc.C");
//     bfc(-1,"MagF,nodefault",0);
//   } else {
//   }
  if (gClassTable->GetID("StarMagField") < 0) {
#if 1
    gROOT->LoadMacro("bfc.C");
    //    TString Chain("MagF,mysql,y2011,nodefault,");
    TString Chain("MagF,mysql,nodefault,");
    Chain += sdt;
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
#ifdef __3D__
  const Int_t       nrp      = 200;       //  number of R nodes in the map
  const Int_t       nzp      = 800;       //  number of Z nodes in the map
  const Float_t     zm       = 200; // 800.0;     //  map max length
  const Float_t     rm       = 200; //400.0;     //  map max radius
  const Int_t       nphi     =  36;
  TH3D* fieldZ = new TH3D("fieldZ","B_{Z};#rho;#phi;z",nrp, 0, rm, nphi, 0., 360., nzp, -zm, zm);
  TH3D* fieldR = new TH3D("fieldR","B_{R};#rho;#phi;z",nrp, 0, rm, nphi, 0., 360., nzp, -zm, zm);
  TH3D* fieldP = new TH3D("fieldP","B_{#phi};#rho;#phi;z",nrp, 0, rm, nphi, 0., 360., nzp, -zm, zm);
  TH3D* fieldRoverZ = new TH3D("fieldRoverZ","B_{R}/B_{Z};#rho;#phi;z",nrp, 0, rm, nphi, 0., 360., nzp, -zm, zm);
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
	fieldRoverZ->Fill(r,phi,z,Brpz[0]/Brpz[2]);
      }
    }
  }
  bench->Print("Gufld");
#else /* __2D__ */
  const Int_t       nrp      = 200;       //  number of R nodes in the map
  const Int_t       nzp      = 200;       //  number of Z nodes in the map
  const Float_t     zm       = 200.0;     //  map max length
  const Float_t     rm       = 200.0;     //  map max radius
  TH2D* fieldZ = new TH2D("fieldZ","B_{Z};z:#rho at #phi = 90"                , nzp, -zm, zm, nrp, -rm, rm);
  TH2D* fieldR = new TH2D("fieldR","B_{R};z:#rho at #phi = 90"                , nzp, -zm, zm, nrp, -rm, rm);
  TH2D* fieldRoverZ = new TH2D("fieldRoverZ","B_{R}/B_{Z};z:#rho at #phi = 90", nzp, -zm, zm, nrp, -rm, rm);
  Float_t dz = 2*zm/nzp;
  Float_t dr = 2*rm/nrp;
  for (Float_t r = -rm + dr/2; r < rm; r += dr ) {
    Float_t x = 0;
    Float_t y = r;
    for (Float_t z = -zm + dz/2; z < zm; z += dz) {
      X[0] = x;
      X[1] = y;
      X[2] = z;
#if 0
      StMagF::Agufld(X,B);
#else
      StarMagField::Instance()->BField(X,B);
#endif
      fieldR->Fill(z,y,B[1]);
      fieldZ->Fill(z,y,B[2]);
      fieldRoverZ->Fill(z,y,B[1]/B[2]);
    }
  }
#endif
  F->Write();
  //  delete F;
}
  
