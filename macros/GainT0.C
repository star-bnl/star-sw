class St_db_Maker;
St_db_Maker *dbMk = 0;
class St_tpcGain;
struct BPoint_t {
  Float_t s, r, p, g, t;
};
BPoint_t BPoint;
//________________________________________________________________________________
void GainT0(Int_t date = 20140508, Int_t time = 190000) {
  const Char_t *GainName  = "Calibrations/tpc/tpcPadGainT0B";
  gSystem->Load("libmysqlclient.so");
  gROOT->LoadMacro("bfc.C");
  //  TString Chain("in dEdxY2 StEvent debug");
  TString Chain("db,NoDefault");
  bfc(-2,Chain.Data(),0,0,0);
  dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  dbMk = (St_db_Maker *) chain->Maker("db");
  dbMk->SetDebug(1);
  dbMk->SetDateTime(date,time); 
  chain->Init();
  //  chain->Make();
  // to browse 1 database, use this one
  St_tpcPadGainT0B *table  = (St_tpcPadGainT0B *) dbMk->GetDataBase(GainName);
  if (! table) {
    cout << "Table:" << GainName << " has not been found" << endl;
    return;
  }
  tpcPadGainT0B_st *gain;
  TDatime t[2];
  dbMk->GetValidity(table,t);
  cout << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
       << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endl;
  TFile *f = new TFile(Form("GainT0.%8i.%06i.root", t[0].GetDate(),t[0].GetTime()),"RECREATE");
  FitP = new TNtuple("FitP","GainT0","s:r:p:g:t");
  Int_t i = 0;
  Int_t NoSectors = table->GetNRows();
  for (int s = 0; s < NoSectors; s++) {
    gain = table->GetTable() + s;
    for (int r = 0; r < 45; r++)
      for (int p = 0; p < 182; p++) {
	if (gain->Gain[r][p] > 0) {
	  if (i%1000 == 0) 
	    cout << "s/p/r\t" << s+1 << "/" << r+1 << "/" << p+1 
		 << "\tGain\t" << gain->Gain[r][p] 
		 << "\tT0\t" << gain->T0[r][p] << endl;
	  BPoint.s = s+1;
	  BPoint.r = r+1; 
	  BPoint.p = p+1; 
	  BPoint.g = gain->Gain[r][p]; 
	  BPoint.t = gain->T0[r][p]; 
	  FitP->Fill(&BPoint.s);
	  i++;
	}
    }
  }
  f->Write();
}
/*
TFile *_file0 = TFile::Open("GainT0.root")
FitP->Draw("g-(9.73766e-01+t*(5.35227e-01+t*1.36385e-01)):t","","prof")
FitP->Draw("g-(9.73766e-01+t*(5.35227e-01+t*1.36385e-01)):t")
FitP->Draw("g:t","","prof")

 */
