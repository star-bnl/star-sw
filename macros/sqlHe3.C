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
//________________________________________________________________________________
void PrintRes(TSQLResult *res, TNtuple *tuple = 0, Float_t *ars = 0){
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
  for (Int_t i = 0; i < nrows; i++) {
    row = res->Next();
    for (Int_t j = 0; j < nfields; j++) {
      if (i < 40) {
	printf("%20s", row->GetField(j));
      }
      if (ars) {
	TString a(row->GetField(j));
	ars[j] = a.Atof();
      }
    }
    if (i < 40) printf("\n");
    delete row;
    if (tuple && ars) tuple->Fill(ars);
  }
}
#include "OnDb.h"
//________________________________________________________________________________
void sqlHe3(Int_t year = 2013) {
  // http://www.star.bnl.gov/protected/common/common2012/trigger2012/sampleEfficiencypp500GeV/getSampleFrac.pl
  TString database = OnDb(year);
  TString dbT("mysql://"); dbT += database; dbT += "/Conditions_rich";
  TSQLServer *db = TSQLServer::Connect(dbT.Data(),"", ""); if (! db) {cout << "Can't connect " << dbT.Data() << endl; return;}
  TSQLResult *res;
#if 0
  printf("\nList all databases on server %s\n", db->GetHost());
  res = db->GetDataBases();
  TSQLRow *row;
  while ((row = res->Next())) {
    printf("%s\n", row->GetField(0));
    delete row;
  }
  delete res;
#endif
  const Char_t *sql = "select "
"UNIX_TIMESTAMP(rich.beginTime) AS utime,"
"DATE_FORMAT(rich.beginTime,\"%Y%m%d\") AS date,"
"DATE_FORMAT(rich.beginTime,\"%H%i%s\") AS time,"
"rich.rs1 as BBCE,"
"rich.rs2 as BBCW,"
"rich.rs3 as BBCEandW,"
"rich.rs4 as YellowBackground,"
"rich.rs5 as BlueBackground,"
"rich.rs6 as ZDCE,"
"rich.rs7 as ZDCW,"
"rich.rs8 as ZDCEandW,"
"rich.rs9 as VPDE,"
"rich.rs10 as VPDW,"
"rich.rs11 as VPDEandW,"
"rich.rs12 as ZDCWnokiller,"
"rich.rs13 as ZDCEnokiller,"
"rich.rs14 as BBCEandWnokiller,"
"rich.rs15 as ZDCEandWnokiller,"
"rich.rs16 as MTD "
"FROM Conditions_rich.richScalar AS rich;";// order by rich.beginTime";
  cout << "sql: " << sql << endl;
  const Char_t *vVars = "utime:date:time:BBCE:BBCW:BBCEandW:YellowBackground:BlueBackground:ZDCE:ZDCW:ZDCEandW:VPDE:VPDW:VPDEandW:ZDCWnokiller:ZDCEnokiller:BBCEandWnokiller:ZDCEandWnokiller:MTD";
  struct Vars_t {
    Float_t utime, date, time,  BBCE, BBCW, BBCEandW, YellowBackground, BlueBackground, 
      ZDCE, ZDCW, ZDCEandW, VPDE, VPDW, VPDEandW, 
      ZDCWnokiller, ZDCEnokiller, BBCEandWnokiller, ZDCEandWnokiller, MTD;
  };
  Vars_t Vars;
  Float_t *ars = &Vars.utime;
  TFile *fout = new TFile(Form("He3R%i.root",year),"recreate");
  TNtuple *tuple = new TNtuple(Form("THe3%i",year),Form("BBC, Zdc Conincidence rate and He3 for y%i",year),vVars);
  res = db->Query(sql);
  PrintRes(res,tuple,ars);
  fout->Write();
}
