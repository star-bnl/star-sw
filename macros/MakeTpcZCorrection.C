/*
  mu->ProjectionY("bin1",1,1,"e")->Draw(); 
  pol4->SetRange(22,209); 
  bin1->Fit(pol4,"r"); 
  TF1* Pol4 = pol4;
  mu->FitSlicesY(Pol4,1,45,10,"r");  
  TH1 *Mu = mu;
  .L MakeTpcZCorrection.C
  MakeTpcZCorrection(Mu,Pol4);
 */
void MakeTpcZCorrection(TH1 *hist = 0, TF1 *func = 0, const Char_t *TableName = "TpcZCorrection", Int_t d=20031120,Int_t t=0 ){
  if (! hist) return;
  if (! func) return;
  Int_t Npar = func->GetNpar();
  TH1D **fitPar = new TH1D*[Npar];
  Double_t xmin = func->GetXmin();
  Double_t xmax = func->GetXmax();
  Int_t p = 0;
  for (p = 0; p < Npar; p++) {
    fitPar[p] = gDirectory->Get(Form("%s_%i",hist->GetName(),p));
    if (! fitPar[p]) return;
    cout << "Histogram " << fitPar[p]->GetName() << " has been found" << endl;
  }
  if (gClassTable->GetID("TTable") < 0) gSystem->Load("libTable");
  if (gClassTable->GetID("St_tpcCorrection") < 0) gSystem->Load("libStDb_Tables");
  St_tpcCorrection *zCor = new St_tpcCorrection(TableName,45);
  Int_t NbinsX = hist->GetNbinsX(); cout << "NbinsX = " << NbinsX << endl;
  Int_t NbinsY = hist->GetNbinsY(); cout << "NbinsY = " << NbinsY << endl;
  tpcCorrection_st row;
  for (Int_t i=1; i<=NbinsX; i++) {
    memset(&row,0,zCor->GetRowSize());
    row.idx   = i;
    row.nrows = 45;
    row.min   = xmin;
    row.max   = xmax;
    row.npar  = Npar;
    for (p = 0; p < Npar; p++) {
      row.a[p] = fitPar[p]->GetBinContent(i);
    }
    zCor->AddAt(&row);
  }
  zCor->Print(0,NbinsX);
  //  TDatime  time(20010701,120000);
  TDatime  time(d,t);
  TString filename(Form("%s.%08d.%06d",TableName,time.GetDate(),time.GetTime()));
  //  sprintf(filename,"./StarDb/Calibrations/tpc/TpcZCorTest.%08d.%06d.C",time.GetDate(),time.GetTime());
  //  sprintf(filename,"TpcZCor.%08d.%06d.root",time.GetDate(),time.GetTime());
  printf("Create %s\n",filename.Data());
#if 1
  filename += ".C";
  TString dirname = gSystem->DirName(filename);
  if (gSystem->OpenDirectory(dirname.Data())==0) { 
    if (gSystem->mkdir(dirname.Data())) {
      cout << "Directory " << dirname << " creation failed" << endl;
      cout << "Putting " << TableName << ".C in current directory" << endl;
    }
  }
  ofstream *out = new ofstream(filename.Data());
  zCor->SavePrimitive(*out,"");
  delete out;
#else
  filename += ".root";
  TFile *f = new TFile(filename.Data(),"recreate");
  zCor->Write();
  //  delete f;
#endif
  //    break;
}
