//________________________________________________________________________________
void DrawPng(TCanvas *c) {
  static Int_t nPng = 0;
  if (! c) return;
  TString pngName("");
  c->Update(); pngName = c->GetName();
  pngName.ReplaceAll(" ","_");
  pngName.ReplaceAll("(","_");
  pngName.ReplaceAll(")","_");
  pngName.ReplaceAll("{","_");
  pngName.ReplaceAll("}","_");
  pngName.ReplaceAll("<","lt");
  pngName.ReplaceAll(">","gt");
  pngName.ReplaceAll("old_new_","");
  pngName.ReplaceAll("old_new","");
  pngName.ReplaceAll("Old_New_","");
  pngName.ReplaceAll("Old_New","");
  pngName.ReplaceAll("GeV/c","");
  pngName.ReplaceAll(".","_");
  pngName.ReplaceAll("/","_");
  pngName.ReplaceAll("^","_");
  pngName.ReplaceAll("__","_");
  pngName.ReplaceAll("__","_");
  pngName.ReplaceAll("#","");
  //  pngName += ".png"; 
  pngName += ".svg"; 
  c->SaveAs(pngName);
  nPng++;
  cout << "Draw #\t" << nPng << "\t" << pngName << endl;
#ifdef __SAVE_ROOT_PICTURES_
  //  pngName.ReplaceAll(".png",".root");
  pngName.ReplaceAll(".svg",".root");
  c->SaveAs(pngName);
#endif
}
//________________________________________________________________________________
void CheckDistortion(const Char_t *opt="CorrY,OSpaceZ2,OGridLeakFull", const Char_t *Out = 0, const Char_t *magF = "RF") {
  TString Opt(opt);
  Int_t idx = Opt.Index(",sdt");
  if (idx > 0) {
    TString dt(Opt(idx+4,8));
    Int_t date = dt.Atoi();
    cout << Opt.Data() << "\tidx = " << idx << "\tdt = " << dt.Data() << "\tdate " << date << endl;
    if (date < 2019) {
      Opt.ReplaceAll("CorrY","CorrX");
      Opt.ReplaceAll("OPr40","Opr13");
      cout << "Opt\t" << Opt.Data() << endl;
    }
  }
  if (gClassTable->GetID("StMagUtilities") < 0) {
    gROOT->LoadMacro("bfc.C");
    TString Chain("noinput,StarMagField,mysql,tpcDb,TpcHitMover,ExB,"); // 2D corrections
    Chain += Opt;
    Chain += ",NoDefault";
    bfc(0,Chain.Data());
    St_db_Maker *dbMk = (St_db_Maker *) chain->Maker("db");
    StTpcDbMaker *tpcDb = (StTpcDbMaker *) chain->Maker("tpcDB");
    chain->MakeEvent();
  }
  TString File(Out);
  TFile *fOut = new TFile(File,"recreate");
  Int_t nz = 210;
  Double_t zmin = -210;
  Double_t zmax = - zmin;
  Int_t nr = 135;
  Double_t Rmin =  57;
  Double_t Rmax = 192;
  TString Name("dX");
  TString Opt(opt);
  Int_t index = Opt.Index(",New");
  TString Title("R*Phi Distortion ( cm) versus R and Z at X = 0 for ");
  Title += Opt(0,index); Title += " and "; Title += magF;
  TH2F *dX    = new TH2F(Name,  Title, nz, zmin, zmax, nr, Rmin, Rmax);
  dX->SetXTitle("Z (cm)");
  dX->SetYTitle("R (cm)");
  TString Name("dY");
  TString Title("Radial Distortion ( cm) versus R and Z at X = 0 for ");
  Title += Opt(0,index); Title += " and "; Title += magF;
  TH2F *dY    = new TH2F(Name,  Title, nz, zmin, zmax, nr, Rmin, Rmax);
  dY->SetXTitle("Z (cm)");
  dY->SetYTitle("R (cm)");
#if 0
  Name = "dZ";
  Title = "dZ ( cm) versus Y and Z at   X = 0 for ";
  Title += Opt(0,index); Title += " and "; Title += magF;
  TH2F *DZ = new TH2F(Name,Title, nz, zmin, zmax, nr, Rmin, Rmax);
  DZ->SetXTitle("Z (cm)");
  DZ->SetYTitle("Y (cm)");
  StTpcCoordinateTransform transform;
  StGlobalCoordinate gC;
  StTpcLocalCoordinate locTpc;
  StTpcLocalCoordinate locTpcMoved;
  StTpcLocalSectorCoordinate locSec;
  StTpcLocalSectorCoordinate locSecMoved;
#endif
  Float_t xIn[3], xOut[3];
  for (Int_t ir = 1; ir <= nr; ir++) {
    xIn[0] = 0;
    xIn[1] = dY->GetYaxis()->GetBinCenter(ir);
#if 0
    gC = StGlobalCoordinate(xIn);
#endif    
    for (Int_t iz = 1; iz < nz; iz++) {
      xIn[2] = dY->GetXaxis()->GetBinCenter(iz);
      StMagUtilities::Instance()->UndoDistortion(xIn,xOut);
      Double_t dx = xOut[0] - xIn[0];
      Double_t dy = xOut[1] - xIn[1];
      Double_t dz = xOut[2] - xIn[2];
      //      cout << "Y = " << xIn[1] << "\tZ = " << xIn[2] << "\t dX/dY/dZ = " << dx << " / " << dy << " / " << dz << endl;
      dX->Fill(xIn[2],xIn[1], dx);
      dY->Fill(xIn[2],xIn[1], dy);
#if 0
      dZ->Fill(xIn[2],xIn[1], dz);
#endif
    }
  }
  gStyle->SetOptStat(0);
  Opt = opt;
  index = Opt.Index(",sdt");
  TString cNameX("dX"); cNameX += Opt(0,index);
  TCanvas *c1dX = new TCanvas(cNameX,cNameX);
  //  c1dX->SetRightMargin(0.15);
  dX->Draw("colz");
  c1dX->Update();
  DrawPng(c1dX);
  TString cNameY("dY"); cNameY += Opt(0,index);
  TCanvas *c1dY = new TCanvas(cNameY,cNameY);
  //  c1dY->SetRightMargin(0.2);
  dY->Draw("colz");
  c1dY->Update();
  DrawPng(c1dY);
  fOut->Write();
}
