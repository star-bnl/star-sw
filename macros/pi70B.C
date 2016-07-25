//________________________________________________________________________________
void pi70B(const Char_t *files = "*.root") {
  TFile *fout = new TFile("pi70BN.root","recreate");
  fout->cd();
  TString Tuple("Run:run:Bad");
  TNtuple *SumT = new TNtuple("SumT","Bad Runs",Tuple.Data()); 
  Float_t  row[15];
  TDataSet *set = 0; 
  Int_t run = 0;
  Int_t Run = 0;
  Int_t nFile = 0;
  TString title;
  TDirIter Dir(files);
  Char_t *file = 0;
  TF1 *gg = new TF1("gg","gaus(0)+gaus(3)");
  Int_t nf = 0;
  while ( (file = Dir.NextFile()) ) {
    TString File = file;
    cout << "File::Name:" << File.Data() << endl;
    if (! File.EndsWith(".root")) continue;
    if ( File.BeginsWith("pi70BN")) continue;
    if ( File.BeginsWith("All") || File.BeginsWith("SecRow") ) continue;
    cout << "Open " <<  File;
    TFile *f = new TFile(File.Data());
    if (! f) {cout << "====================== failed " << endl; return; continue;}
    File.ReplaceAll("adc_",""); 
    File.ReplaceAll("st_W_",""); 
    sscanf(File.Data(),"%i",&run); 
    Run = run%1000000;
    cout << " for Run " << run << "/" << Run << "\t" << File << "\t" << nFile++ << endl;
    row[0] = run;
    row[1] = Run;
    row[2] = 0;
    Int_t p = 2;
    TH2F *piN70B = (TH2F *) f->Get("piN70B");
    if (! piN70B) continue;
    TH2F *piP70B = (TH2F *) f->Get("piP70B");
    if (! piP70B) continue;
    TH2F *pi70B = new TH2F(*piN70B); pi70B->SetName("pi70B");
    pi70B->Add(piP70B);
    TH1D *proj = pi70B->ProjectionY("proj",51,100);
    Double_t Integ =  proj->Integral();
    if (Integ > 0) row[2] = proj->Integral(241,307);
    cout << " bad = " << row[2] << " from total " << Integ << endl;
    if (Integ > 0) row[2] /= Integ;
    else           row[2]  = 0;
    delete f;
    SumT->Fill(&row[0]);
    nf++;
    //    if (nf > 10) break;
  }
  SumT->Write();
  fout->Close();
   delete fout;
}
