void dEdxDraw(const Char_t *treeName = "PicoDst") {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TIter next(files);
  TFile *f = 0;
  Int_t NF = 0;
  while ( (f = (TFile *) next()) ) { 
    TTree *tree = (TTree *) f->Get(treeName);
    if (! tree) continue;
    FitFiles[NF] = f; 
    NF++;
  }
  if (! NF) return;
  Int_t nx = TMath::Sqrt(NF) + 0.5;
  Int_t ny = NF/nx + 0.5;
  TCanvas *c1 = new TCanvas("c1","c1");
  c1->Divide(nx,ny);
  for (Int_t i = 0; i < NF; i++) {
    FitFiles[i]->cd();
    c1->cd(i+1)->SetLogy(1);
    TTree *tree = (TTree *) gDirectory->Get(treeName);
    tree->Draw("Track.mDedx>>dEdx","Track.mDedxError>0&&Track.mDedxError<0.2");
    TH1 *dEdx = (TH1 *) gDirectory->Get("dEdx");
    dEdx->SetTitle(gDirectory->GetName());
    cout << gSystem->BaseName(gSystem->DirName(gDirectory->GetName())) << "\t" << dEdx->GetEntries() << "\t" << dEdx->GetMean() << " +/- " << dEdx->GetRMS() << endl;
  }
}
/*
 job10/st_physics_20118051_raw_5500001B.log:St_db_Maker:INFO  - MysqlDb::Connect(host,user,pw,database,port) line=527 Server Connecting: DB=StarDb  Host=db07.star.bnl.gov:3316
good job11/st_physics_20118051_raw_5500001B.log:St_db_Maker:INFO  - MysqlDb::Connect(host,user,pw,database,port) line=527 Server Connecting: DB=StarDb  Host=db06.star.bnl.gov:3316
good job12/st_physics_20118051_raw_5500001B.log:St_db_Maker:INFO  - MysqlDb::Connect(host,user,pw,database,port) line=527 Server Connecting: DB=StarDb  Host=db05.star.bnl.gov:3316
good job1/st_physics_20118051_raw_5500001B.log:St_db_Maker:INFO  - MysqlDb::Connect(host,user,pw,database,port) line=527 Server Connecting: DB=StarDb  Host=db02.star.bnl.gov:3316
good job2/st_physics_20118051_raw_5500001B.log:St_db_Maker:INFO  - MysqlDb::Connect(host,user,pw,database,port) line=527 Server Connecting: DB=StarDb  Host=db02.star.bnl.gov:3316
good job3/st_physics_20118051_raw_5500001B.log:St_db_Maker:INFO  - MysqlDb::Connect(host,user,pw,database,port) line=527 Server Connecting: DB=StarDb  Host=db06.star.bnl.gov:3316
bad  job4/st_physics_20118051_raw_5500001B.log:St_db_Maker:INFO  - MysqlDb::Connect(host,user,pw,database,port) line=527 Server Connecting: DB=StarDb  Host=db02.star.bnl.gov:3316
good job4/st_physics_20118051_raw_5500001B.log:StChain:INFO  - MysqlDb::Connect(host,user,pw,database,port) line=527 Server Connecting: DB=Conditions_trg  Host=db08.star.bnl.gov:3316
good job5/st_physics_20118051_raw_5500001B.log:St_db_Maker:INFO  - MysqlDb::Connect(host,user,pw,database,port) line=527 Server Connecting: DB=StarDb  Host=db06.star.bnl.gov:3316
bad  job6/st_physics_20118051_raw_5500001B.log:St_db_Maker:INFO  - MysqlDb::Connect(host,user,pw,database,port) line=527 Server Connecting: DB=StarDb  Host=db06.star.bnl.gov:3316
good job7/st_physics_20118051_raw_5500001B.log:St_db_Maker:INFO  - MysqlDb::Connect(host,user,pw,database,port) line=527 Server Connecting: DB=StarDb  Host=db06.star.bnl.gov:3316
bad  job8/st_physics_20118051_raw_5500001B.log:St_db_Maker:INFO  - MysqlDb::Connect(host,user,pw,database,port) line=527 Server Connecting: DB=StarDb  Host=db06.star.bnl.gov:3316
good job9/st_physics_20118051_raw_5500001B.log:St_db_Maker:INFO  - MysqlDb::Connect(host,user,pw,database,port) line=527 Server Connecting: DB=StarDb  Host=db06.star.bnl.gov:3316
*/
