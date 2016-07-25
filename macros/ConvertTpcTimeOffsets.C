struct DateTime_t {
  Int_t date, time;
};
const Int_t NDT = 29;
DateTime_t beginTimes[29] = {
  {19970101,     0},
  {19990615,     0},
  {20000501,     0},
  {20000615,     0},
  {20010601,     0},
  {20010911,     0},
  {20010924,     0},
  {20011205,     0},
  {20030101,     0},
  {20030101,     1},
  {20031228,     0},
  {20040416,145000},
  {20050111,190700},
  {20050209,152800},
  {20050311,153700},
  {20050412,184300},
  {20060308,115800},
  {20060510,150600},
  {20070330,135100},
  {20070420,142400},
  {20070503,     0},
  {20070524,161900},
  {20070524,161901},
  {20071127,151800},
  {20080101,     0},
  {20080101,     1},
  {20080115, 93300},
  {20080208,174700},
  {20080208,174701}
};
//________________________________________________________________________________
void ConvertTpcTimeOffsets() {

  gROOT->LoadMacro("bfc.C");
  bfc(-1,"mysql,tpcDb,FieldOn,nodefault",0,0,0);
  StMaker *dbMk = chain->Maker("db");
  if (! dbMk) return;
  dbMk->SetDebug(1);
  chain->Init();
  StEvtHddr *header = chain->GetEvtHddr();
  for (Int_t idt = 0; idt < NDT; idt++) {
    header->SetRunNumber(idt+1);
    dbMk->SetDateTime(beginTimes[idt].date,beginTimes[idt].time); 
    header->SetDateTime(beginTimes[idt].date,beginTimes[idt].time);
    chain->MakeEvent();
    St_tpcT0 *tpcT0 = new St_tpcT0("tpcT0",24);
    for (Int_t sec = 1; sec <= 24; sec++) {
      tpcT0_st rowT0;
      memset(&rowT0, 0, sizeof(tpcT0_st));
      StTpcT0I* T0 = gStTpcDb->T0(sec);
      for (Int_t row = 1; row <= 45; row++) {
	for (Int_t pad = 1; pad <= 182; pad++) {
	  rowT0.T0[row-1][pad-1] = T0->getT0(row,pad);
	}
      }
      tpcT0->AddAt(&rowT0,sec-1);
    }
    TFile *f = new TFile(Form("tpcT0.%8i.%06i.root",beginTimes[idt].date,beginTimes[idt].time),"recreate");
    tpcT0->Write();
    delete f;
    delete tpcT0;
  }
}
