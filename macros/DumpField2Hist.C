//________________________________________________________________________________
Double_t *bins(Int_t N, Float_t *zList, const Char_t *tag = "") {
  Double_t *zbins = new Double_t[N+1];
  for (Int_t k = 0; k <= N; k++) {
    if      (k == 0)  zbins[k] = zList[k] - 0.5*(zList[k+1] - zList[k]);
    else if (k == N) zbins[k] = zList[k-1] + 0.5*(zList[k-1] - zList[k-2]);
    else              zbins[k] = 0.5*(zList[k-1] + zList[k]);
    cout << tag;
    if (k < N) cout << "\tzList[" << k << "] = " << zList[k];
    cout << "\tzbins[" << k << "] = " << zbins[k] << endl;
  }
  return zbins; 
}
//________________________________________________________________________________
void DumpField2Hist(const Char_t *opt="OBmap2D", Int_t date = 20110204, Int_t time = 150000) {
  
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
  TString File(opt);
  Float_t factor = St_MagFactorC::instance()->ScaleFactor();
  TString magF("Undef");
  if (TMath::Abs(factor) < 1e-2) {magF = "ZF";}
  else if       (factor  < -0.8) {magF = "RFF";}
  else if       (factor  < -0.4) {magF = "RHF";}
  else if       (factor  >  0.8) {magF = "FF";}
  else if       (factor  >  0.4) {magF = "HF";}
  gStyle->SetOptStat("n");
  cout << " Number of Z points in table. Measured STAR B field Maps from Steve T.            " << BMap_nZ   << endl;
  cout << " Number of R points in table.						     " << BMap_nR   << endl;
  cout << " Number of Phi points in table.						     " << BMap_nPhi << endl;
  cout << " Number of Z points in table. Standard STAR distortion tables for interpolating.  " << EMap_nZ   << endl;
  cout << " Number of R points in table					         	     " << EMap_nR   << endl;
  cout << " Number of Phi points in table ( add one for 360 == 0 )			     " << EMap_nPhi << endl;
  TString FileOut("MagField");
  FileOut += opt;
  FileOut += magF;
  FileOut += ".root";
  TFile *fOut = new TFile(FileOut,"RECREATE");
  Double_t *zbins = bins(BMap_nZ,StMagUtilities::Instance()->ZList,"ZList");
  Double_t *rbins = bins(BMap_nR,StMagUtilities::Instance()->Radius,"Radius");
  TH2F *Bz = new TH2F("Bz","2D STAR Mag. Field: Bz versus Z and R",BMap_nZ,zbins,BMap_nR,rbins);
  for (Int_t i = 1; i <= BMap_nZ; i++) 
    for (Int_t j = 1; j <= BMap_nR; j++) 
      Bz->SetBinContent(i,j, StMagUtilities::Instance()->Bz[i-1][j-1]);
  TH2F *Br = new TH2F("Br","2D STAR Mag. Field: Br versus Z and R",BMap_nZ,zbins,BMap_nR,rbins);
  for (Int_t i = 1; i <= BMap_nZ; i++) 
    for (Int_t j = 1; j <= BMap_nR; j++) 
      Br->SetBinContent(i,j, StMagUtilities::Instance()->Br[i-1][j-1]);
  Double_t *p3Dbins = bins(BMap_nPhi, StMagUtilities::Instance()->Phi3D, "Phi3D");
  Double_t *z3Dbins = bins(BMap_nZ,StMagUtilities::Instance()->Z3D,"Z3D");
  Double_t *r3Dbins = bins(BMap_nR,StMagUtilities::Instance()->R3D,"R3D");
  TH3F *Bz3D   = new TH3F("Bz3D",  "3D STAR Mag. Field: Bz   versus phi, Z and R",BMap_nPhi,p3Dbins,BMap_nZ,z3Dbins,BMap_nR,r3Dbins);
  for (Int_t i = 1; i <= BMap_nPhi; i++) 
    for (Int_t j = 1; j <= BMap_nZ; j++) 
      for (Int_t k = 1; k <= BMap_nR; k++) 
	Bz3D->SetBinContent(i,j, StMagUtilities::Instance()->Bz3D[i-1][j-1][k-1]);
  
  TH3F *Br3D   = new TH3F("Br3D",  "3D STAR Mag. Field: Br   versus phi, Z and R",BMap_nPhi,p3Dbins,BMap_nZ,z3Dbins,BMap_nR,r3Dbins);
  for (Int_t i = 1; i <= BMap_nPhi; i++) 
    for (Int_t j = 1; j <= BMap_nZ; j++) 
      for (Int_t k = 1; k <= BMap_nR; k++) 
	Br3D->SetBinContent(i,j, StMagUtilities::Instance()->Br3D[i-1][j-1][k-1]);
  TH3F *Bphi3D = new TH3F("Bphi3D","3D STAR Mag. Field: Bphi versus phi, Z and R",BMap_nPhi,p3Dbins,BMap_nZ,z3Dbins,BMap_nR,r3Dbins);
  for (Int_t i = 1; i <= BMap_nPhi; i++) 
    for (Int_t j = 1; j <= BMap_nZ; j++) 
      for (Int_t k = 1; k <= BMap_nR; k++) 
	Bphi3D->SetBinContent(i,j, StMagUtilities::Instance()->Bphi3D[i-1][j-1][k-1]);
  
  Double_t *pEbins = bins(EMap_nPhi,StMagUtilities::ePhiList,"ePhiList");
  Double_t *zEbins = bins(EMap_nZ  ,StMagUtilities::eZList,"eZList");
  Double_t *rEbins = bins(EMap_nR  ,StMagUtilities::eRList,"eRList");
  TH2F *shiftEr = new TH2F("shiftEr","shiftEr",EMap_nZ,zEbins,EMap_nR,rEbins);
  for (Int_t i = 1; i <= EMap_nZ; i++) 
    for (Int_t j = 1; j <= EMap_nR; j++) 
      shiftEr->SetBinContent(i,j, StMagUtilities::Instance()->shiftEr[i-1][j-1]);
  TH2F *spaceEr = new TH2F("spaceEr","spaceEr",EMap_nZ,zEbins,EMap_nR,rEbins);
  for (Int_t i = 1; i <= EMap_nZ; i++) 
    for (Int_t j = 1; j <= EMap_nR; j++) 
      spaceEr->SetBinContent(i,j, StMagUtilities::Instance()->spaceEr[i-1][j-1]);
  TH2F *spaceR2Er = new TH2F("spaceR2Er","spaceR2Er",EMap_nZ,zEbins,EMap_nR,rEbins);
  for (Int_t i = 1; i <= EMap_nZ; i++) 
    for (Int_t j = 1; j <= EMap_nR; j++) 
      spaceR2Er->SetBinContent(i,j, StMagUtilities::Instance()->spaceR2Er[i-1][j-1]);
  TH2F *shortEr = new TH2F("shortEr","shortEr",EMap_nZ,zEbins,EMap_nR,rEbins);
  for (Int_t i = 1; i <= EMap_nZ; i++) 
    for (Int_t j = 1; j <= EMap_nR; j++) 
      shortEr->SetBinContent(i,j, StMagUtilities::Instance()->shortEr[i-1][j-1]);
  TH2F *GGVoltErrorEr = new TH2F("GGVoltErrorEr","GGVoltErrorEr",EMap_nZ,zEbins,EMap_nR,rEbins);
  for (Int_t i = 1; i <= EMap_nZ; i++) 
    for (Int_t j = 1; j <= EMap_nR; j++) 
      GGVoltErrorEr->SetBinContent(i,j, StMagUtilities::Instance()->GGVoltErrorEr[i-1][j-1]);
  fOut->Write();
}
