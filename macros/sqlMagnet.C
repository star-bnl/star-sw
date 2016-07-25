/*  
    root.exe Mag*.root 'Chain.C("MagT")'

  gStyle->SetOptStat(0);
  chain->SetMarkerColor(1);
  chain->Draw("mainMagnetCurrent:date>>MC2(100,2000e4,2014e4,100,4490,4515)","mainMagnetCurrent>4490&&mainMagnetCurrent<4515","colz");
  chain->SetMarkerColor(2);
  chain->Draw("pttWestCurrent:date>>pW2(100,2000e4,2014e4,100,1300,1360)","mainMagnetCurrent>4490&&mainMagnetCurrent<4515&&pttWestCurrent<1e4","colz")
  chain->Draw("trimWestCurrent:date>>tW2(100,2000e4,2014e4,100,570,577)","mainMagnetCurrent>4490&&mainMagnetCurrent<4515&&pttWestCurrent<1e4","colz")
  chain->SetMarkerColor(3);
  chain->Draw("pttEastCurrent:date>>pE2(100,2000e4,2014e4,100,1300,1360)","mainMagnetCurrent>4490&&mainMagnetCurrent<4515&&pttWestCurrent<1e4","colz")
  chain->Draw("trimEastCurrent:date>>tE2(100,2000e4,2014e4,100,572,580)","mainMagnetCurrent>4490&&mainMagnetCurrent<4515&&pttWestCurrent<1e4","colz")
  c1->Clear();
  c1->Divide(1,3);
  c1->cd(1); MC2->ProfileX()->Draw();
  c1->cd(2); 
  TLegend *l1 = new TLegend(0.8,0.7,0.9,0.8);
  pW2->ProfileX()->Draw(); l1->AddEntry(pW2_pfx,"West pole tip coil current");
  pE2->ProfileX()->Draw("same"); l1->AddEntry(pE2_pfx,"East pole tip coil current");
  l1->Draw();
  c1->cd(3);
  TLegend *l2 = new TLegend(0.8,0.7,0.9,0.8);
  tW2->ProfileX()->Draw(); l2->AddEntry(tW2_pfx,"West trim coil current");
  tE2->ProfileX()->Draw("same"); l2->AddEntry(tE2_pfx,"East trim coil current");
  l2->Draw();
 */
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TSQLServer.h"
#include "TSQLResult.h"
#include "TSQLRow.h"
#include "TNtuple.h"
#include "TFile.h"
#include "TStopwatch.h"
#endif
using namespace std;
struct Vars_t {
  Float_t utime, date, time, mainMagnetCurrent, pttWestCurrent, pttEastCurrent, trimWestCurrent, trimEastCurrent, mainMagnetStatus, pttWestStatus, pttEastStatus, trimWestStatus, trimEastStatus, readoutMode;
};
Vars_t Vars;
  const Char_t *vVars = "utime:date:time:mainMagnetCurrent:pttWestCurrent:pttEastCurrent:trimWestCurrent:trimEastCurrent";
  //":mainMagnetStatus:pttWestStatus:pttEastStatus:trimWestStatus:trimEastStatus:readoutMode";
//________________________________________________________________________________
void PrintRes(TSQLResult *res, TNtuple *tuple = 0){
  if (! res) return;
  Int_t nrows = res->GetRowCount();
  printf("\nGot %d rows in result\n", nrows);
  
  Int_t nfields = res->GetFieldCount();
  for (Int_t i = 0; i < nfields; i++)
    printf("%20s", res->GetFieldName(i));
  printf("\n");
  for (Int_t i = 0; i < nfields*20; i++)
    printf("=");
  printf("\n");
  TSQLRow *row;
  Float_t *ars = &Vars.utime;
  for (Int_t i = 0; i < nrows; i++) {
    row = res->Next();
    memset (ars, 0, sizeof(Vars_t));
    for (Int_t j = 0; j < nfields; j++) {
      if (i < 40) {
	printf("%20s", row->GetField(j));
      }
      if (ars) {
	TString a(row->GetField(j));
	ars[j] = a.Atof();
	//	if (i < 40)  printf(" => %f",ars[j]);
      }
    }
    if (i < 40) printf("\n");
    delete row;
    if (tuple && ars) tuple->Fill(ars);
  }
}
//________________________________________________________________________________
void Magnet(Int_t year = 2013) {
  // http://www.star.bnl.gov/protected/common/common2012/trigger2012/sampleEfficiencypp500GeV/getSampleFrac.pl
  TString database("db04.star.bnl.gov:");
  database += (1400 + year - 1);
  TString dbT("mysql://"); dbT += database; dbT += "/Conditions_rhic";
  TSQLServer *db = TSQLServer::Connect(dbT.Data(),"test", ""); if (! db) {cout << "Can't connect " << dbT.Data() << endl; return;}
  TSQLResult *res;
#if 1
  printf("\nList all databases on server %s\n", db->GetHost());
  res = db->GetDataBases();
  TSQLRow *row;
  while ((row = res->Next())) {
    printf("%s\n", row->GetField(0));
    delete row;
  }
  delete res;
#endif
  const Char_t *sql = "SELECT "
"UNIX_TIMESTAMP(mag.beginTime) AS utime,"
"DATE_FORMAT(mag.beginTime,\"%Y%m%d\") AS date,"
"DATE_FORMAT(mag.beginTime,\"%H%i%s\") AS time,"
"mag.mainMagnetCurrent,mag.pttWestCurrent,mag.pttEastCurrent,mag.trimWestCurrent,mag.trimEastCurrent"
    //,"mag.mainMagnetStatus,mag.pttWestStatus,mag.pttEastStatus,mag.trimWestStatus,mag.trimEastStatus,mag.readoutMode"
    " FROM Conditions_rhic.starMagnet AS mag order by mag.beginTime;";// limit 40";
  cout << "sql: " << sql << endl;
  TFile *fout = new TFile(Form("Mag%i.root",year),"recreate");
  TNtuple *tuple = new TNtuple("MagT",Form("Mag. current for y%i",year),vVars);
  res = db->Query(sql);
  PrintRes(res,tuple);
  fout->Write();
}
//________________________________________________________________________________
void sqlMagnet() {
  for (Int_t y = 2013; y > 2001; y -= 1) {
    Magnet(y);
  }
}
