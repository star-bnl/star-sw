/* 
   root.exe TpcSecRowB2Nt.C


   root.exe  /star/subsys/tpc/fisyak/Tpc/Gain4Tonko/TpcSecRowB.root TpcSecRowB.root
   .L ~fisyak/macros/DrawTime.C
   T->Draw("gain:time>>S1I","row<=13&&sector==1&&gain>0&&time>4.5e8&&time<7e8","prof")
   DrawTime(S1I)
   TLegend *l = new TLegend(0.2,0.2,0.5, 0.5);
   l->AddEntry(S1I,"Inner Sector 1");
   T->SetMarkerColor(2);
   T->Draw("gain:time>>S1O","row>13&&sector==1&&gain>0&&time>4.5e8&&time<7e8","profsame")
   l->AddEntry(S1O,"Outer Sector 1");
   l->Draw();


   root.exe /star/subsys/tpc/fisyak/Tpc/Gain4Tonko/TpcSecRowB.root TpcSecRowB.root
   .L ~fisyak/macros/DrawTime.C
   .L ~fisyak/macros/TpcSecRowB2Nt.C
   DrawSector(2);
*/

class St_db_Maker;
St_db_Maker *dbMk = 0;
struct Date_t {
  Int_t date;
  Int_t time;
};
//________________________________________________________________________________
void DrawSector(Int_t sector = 1) {
  TTree *T = (TTree *) gDirectory->Get("T");
  if (! T) return;
  TCanvas *c1 = new TCanvas(Form("sector%i",sector),Form("sector%i",sector));
  T->SetMarkerColor(1);
  T->Draw(Form("gain:time>>S%iI",sector),Form("row<=13&&sector==%i&&gain>0&&time>4.5e8&&time<7e8",sector),"prof");
  TH1 *sI = (TH1 *) gDirectory->Get(Form("S%iI",sector));
  DrawTime(sI);
  TLegend *l = new TLegend(0.2,0.2,0.5, 0.5);
  l->AddEntry(sI,Form("Inner Sector %i",sector));
  T->SetMarkerColor(2);
  T->Draw(Form("gain:time>>S%iO",sector),Form("row>13&&sector==%i&&gain>0&&time>4.5e8&&time<7e8",sector),"profsame");
  TH1 *sO = (TH1 *) gDirectory->Get(Form("S%iO",sector));
  l->AddEntry(sO,Form("Outer Sector %i",sector));
  l->Draw();
}
//________________________________________________________________________________
void Load() {
  if (gClassTable->GetID("StDbManager") < 0) {
    // Baseline shared libraries
    //    gSystem->Load("libTable");
    gSystem->Load("St_base"); 
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    // DB-specific libs
    // may only need libStDb_Tables.so
    // but may need St_Tables.so... so I'm using this one
    //  gSystem->Load("libStDb_Tables.so");
    gSystem->Load("libmysqlclient");
    gSystem->Load("libStDb_Tables.so");
    gSystem->Load("StDbLib.so");
    gSystem->Load("StDbBroker.so"); 
    gSystem->Load("St_db_Maker.so");
  }
  dbMk = new St_db_Maker("db","MySQL:StarDb");
  //  dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  dbMk->SetDebug(1);
  //  dbMk->SetFlavor("ofl+sim");
//   dbMk->SetFlavor("simu","svtWafersPosition"); 
//   dbMk->SetFlavor("sim","tpcGlobalPosition");
//   dbMk->SetFlavor("sim","tpcSectorPosition");
//   dbMk->SetFlavor("sim","tpcISTimeOffsets");
//   dbMk->SetFlavor("sim","tpcOSTimeOffsets");
  dbMk->Init();
}
//________________________________________________________________________________
void TpcSecRowB2Nt(const Char_t *tabNam  = 	"Calibrations/tpc/TpcSecRowB"	){ 
  Date_t dates[] = {
    { 19980101,      0},
    { 20000614, 175430},
    { 20010701,      0},
    { 20010911,      0},
    { 20010924,      0},
    { 20011205,      0},
    { 20011221,  24512},
    { 20021105,      0},
    { 20030106,      0},
    { 20031120,      0},
    { 20031120,      1},
    { 20040104,      0},
    { 20040104,      1},
    { 20040205,      0},
    { 20040205,      1},
    { 20040217,      0},
    { 20040217,      1},
    { 20040324,      0},
    { 20040324,      1},
    { 20040404,      0},
    { 20040404,      1},
    { 20050111, 220000},
    { 20050403,  10000},
    { 20060308, 115800},
    { 20060406,  50000},
    { 20060424, 210001},
    { 20060510, 150601},
    { 20060519, 160001},
    { 20070321,     40},
    { 20070524,     44},
    { 20071101,     11},
    { 20080128,     11},
    { 20090301,      1},
    { 20090301,     41},
    { 20090301,     48},
    { 20090301,    102},
    { 20090415,     48},
    { 20090415,     57},
    { 20090419,  80000},
    { 20090527, 200000},
    { 20100101,      0},
    { 20100101,     32},
    { 20100101,     35},
    { 20100103,     32},
    { 20100103,     35},
    { 20100204, 180032},
    { 20100204, 180035},
    { 20100318, 200032},
    { 20100409,      0},
    { 20100409,     32},
    { 20100409,     35},
    { 20100424,  40032},
    { 20100424,  40035},
    { 20100527,  20032},
    { 20100527,  20035},
    { 20110101,     13},
    { 20110101,    113},
    { 20110101,    340},
    { 20110421,    119},
    { 20110421,    206},
    { 20110503,      2},
    { 20110503,      3},
    { 20110602,      2},
    { 20110620, 150000},
    { 20110624, 150002},
    { 20111210,    100},
    { 20111220,    100},
    { 20120207, 100500},
    { 20120207, 100600},
    { 20120312,    301},
    { 20120313, 140026},
    { 20120423, 112400},
    { 20120423, 112413},
    { 20120515, 120001},
    { 20121210,    100},
    { 20121220,    100},
    { 20130305,     14},
    { 20131210,    100},
    { 20131220,    100},
    { 20140101,      6},
    { 20140101,     36},
    { 20140315,     44},
    { 20140315,     57},
    { 20141210,    100},
    { 20141220,    100},
    { 20150131,      7},
    { 20150131,     15},
    { 20150131,     48},
    { 20150504, 200049},
    { 20151210,    100},
    { 20151220,    100},
    { 20201210,    100},
    { 20201215,    100},
    {        0,      0}
  }; 
  //  dbMk = (St_db_Maker*) chain->Maker("db");
  // dbMk->Init();
  if (dbMk == 0) Load();
  //  dbMk->SetDebug(4);
  //  dbMk->SetFlavor("ofl+laserDV","tpcDriftVelocity");
  //  dbMk->SetMaxEntryTime(20060620,0);
  // to browse 1 database, use this one
  TDataSet *set = dbMk->GetDataBase(gSystem->DirName(tabNam));
  Int_t i = 0;
  struct Row_t {
    Float_t time, sector, row, gain, rms;
  };
  Int_t tZero= 19950101;
  TDatime t0(tZero,0);
  Int_t timeOffSet = t0.Convert();
  Row_t row;
  f = new TFile("TpcSecRowB.root","RECREATE");
  TNtuple *FitP = new TNtuple("T","TpcSecRowB","time:sector:row:gain:rms");
  
  while (dates[i].date) {
    TDatime t(dates[i].date,dates[i].time);
    row.time = t.Convert() - timeOffSet;
    dbMk->SetDateTime(dates[i].date,dates[i].time+1); 
    //    dbMk->InitRun(1000+i);
    TDataSet *set = dbMk->GetDataBase(gSystem->DirName(tabNam));
    St_TpcSecRowCor *table =  (St_TpcSecRowCor *) set->FindByName(gSystem->BaseName(tabNam));
    if (table) {
      cout << "got " << tabNam << " for d/t " << dates[i].date << "/" << dates[i].time << endl;
      TpcSecRowCor_st *gains = table->GetTable();
      for (Int_t sector = 1; sector <= 24; sector++, gains++) {
	row.sector = sector;
	for (Int_t r = 1; r <= 45; r++) {
	  row.row = r;
	  row.gain = gains->GainScale[r-1];
	  row.rms  = gains->GainRms[r-1];
	  FitP->Fill(&row.time);
	  if (sector == 1 && r == 1) {
	    cout << "time " << t.AsString() << " s/r " << sector << "/" << r << " gain " <<  row.gain << row.rms << endl;
	  }
	}
      }
    } else {
      cout << "did not get " << tabNam << " for d/t " << dates[i].date << "/" << dates[i].time << endl;
    }
    i++;
  }
  f->Write();
}






