void CheckDistortion(const Char_t *opt="Corr4,OSpaceZ2,OGridLeak3D", Int_t date = 20120317, Int_t time = 150000, const Char_t *Out = 0) {
  
  if (gClassTable->GetID("StMagUtilities") < 0) {
    gROOT->LoadMacro("bfc.C");
    TString Chain("noinput,StarMagField,mysql,tpcDb,TpcHitMover,ExB,"); // 2D corrections
    Chain += opt;
    Chain += ",NoDefault";
    bfc(0,Chain.Data());
    St_db_Maker *dbMk = (St_db_Maker *) chain->Maker("db");
    dbMk->SetDateTime(date,time);
    StTpcDbMaker *tpcDb = (StTpcDbMaker *) chain->Maker("tpcDB");
    chain->MakeEvent();
  }
  TString File;
  TString magF("Undef");
  if (Out) {
    File = Out;
    magF = Out;
    magF.ReplaceAll(".root","");
    magF.ReplaceAll(opt,"");
  } else {
    File = opt;
    Float_t factor = St_MagFactorC::instance()->ScaleFactor();
    if (TMath::Abs(factor) < 1e-2) {magF = "ZF";}
    else if       (factor  < -0.8) {magF = "RFF";}
    else if       (factor  < -0.4) {magF = "RHF";}
    else if       (factor  >  0.8) {magF = "FF";}
    else if       (factor  >  0.4) {magF = "HF";}
    gStyle->SetOptStat("n");
    File += Form(".%8i.%06i.%s.root",date,time,magF.Data());
  }
  TFile *fOut = new TFile(File,"recreate");
  Int_t nz = 210;
  Double_t zmin = -210;
  Double_t zmax = - zmin;
  Int_t nr = 135;
  Double_t Rmin =  57;
  Double_t Rmax = 192;
  TString Name("ddR");
  Name += opt; 
  TString Title("Radial Distortion ( cm) versus R and Z at X = 0 for ");
  Title += opt; Title += " and "; Title += magF;
  TH2F *dR    = new TH2F(Name,  Title, nz, zmin, zmax, nr, Rmin, Rmax);
  dR->SetXTitle("Z (cm)");
  dR->SetYTitle("R (cm)");
  Name = "dRPhiY";
  Name += opt; 
  Title = "R*Phi Distortion ( cm) versus Y and Z at   X = 0 for ";
  Title += opt; Title += " and "; Title += magF;
  TH2F *dRPhiY = new TH2F(Name,Title, nz, zmin, zmax, nr, Rmin, Rmax);
  dRPhiY->SetXTitle("Z (cm)");
  dRPhiY->SetYTitle("Y (cm)");
  Name = "dRPhiX";
  Name += opt; 
  Title = "R*Phi Distortion ( cm) versus X and Z at   Y = 0 for ";
  Title += opt; Title += " and "; Title += magF;
  TH2F *dRPhiX = new TH2F(Name,Title, nz, zmin, zmax, nr, Rmin, Rmax);
  dRPhiX->SetXTitle("Z (cm)");
  dRPhiX->SetYTitle("X (cm)");
  Float_t xIn[3], xOut[3];
  for (Int_t ir = 1; ir <= nr; ir++) {
    xIn[0] = 0;
    xIn[1] = dR->GetYaxis()->GetBinCenter(ir);
    for (Int_t iz = 1; iz < nz; iz++) {
      xIn[2] = dR->GetXaxis()->GetBinCenter(iz);
      StMagUtilities::Instance()->UndoDistortion(xIn,xOut);
      Double_t rIn  = TMath::Sqrt(xIn[0]*xIn[0] + xIn[1]*xIn[1]);
      Double_t rOut = TMath::Sqrt(xOut[0]*xOut[0] + xOut[1]*xOut[1]);
      dR->Fill(xIn[2],xIn[1],(rOut - rIn));
      //      dRPhiY->Fill(xIn[2],xIn[1],(rOut*TMath::ATan2(xOut[1],xOut[0]) - rIn*TMath::ATan2(xIn[1],xIn[0])));
      dRPhiY->Fill(xIn[2],xIn[1],(xOut[0] - xIn[0]));
    }
    xIn[0] = dR->GetYaxis()->GetBinCenter(ir);
    xIn[1] = 0;
    for (Int_t iz = 1; iz < nz; iz++) {
      xIn[2] = dR->GetXaxis()->GetBinCenter(iz);
      StMagUtilities::Instance()->UndoDistortion(xIn,xOut);
      Double_t rIn  = TMath::Sqrt(xIn[0]*xIn[0] + xIn[1]*xIn[1]);
      Double_t rOut = TMath::Sqrt(xOut[0]*xOut[0] + xOut[1]*xOut[1]);
      dRPhiX->Fill(xIn[2],xIn[0],(xOut[1] - xIn[1]));
    }
  }
  fOut->Write();
}
