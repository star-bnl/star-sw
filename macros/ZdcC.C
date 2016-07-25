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
//________________________________________________________________________________
void ZdcC(Int_t year = 2012) {
  // http://www.star.bnl.gov/protected/common/common2012/trigger2012/sampleEfficiencypp500GeV/getSampleFrac.pl
  TString database;
  if      (year == 2012)     database = "dbbak.starp.bnl.gov:3411";
  else if (year == 2011)     database = "dbbak.starp.bnl.gov:3410";
  else if (year == 2010)     database = "dbbak.starp.bnl.gov:3409";
  else if (year == 2009)     database = "dbbak.starp.bnl.gov:3408";
  else if (year == 2008)     database = "dbbak.starp.bnl.gov:3407";
  else if (year == 2007)     database = "dbbak.starp.bnl.gov:3406";
  else if (year == 2006)     database = "dbbak.starp.bnl.gov:3405";
  else if (year == 2005)     database = "dbbak.starp.bnl.gov:3404";
  else if (year == 2004)     database = "dbbak.starp.bnl.gov:3403";
  else if (year == 2003)     database = "dbbak.starp.bnl.gov:3402";
  else if (year == 2002)     database = "dbbak.starp.bnl.gov:3401";
  else if (year == 2001)     database = "dbbak.starp.bnl.gov:3400";
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
  // query database and print results 30 secs average
  // http://online.star.bnl.gov/cgi-bin/db_scripts/cgi/database_scaler.pl
  // y2012
  const char *sql = "select "
"runL.runNumber as run,"
"UNIX_TIMESTAMP(rich.beginTime) AS time,"
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
"rich.rs16 as MDT"
    " FROM RunLog.runUpdateStatus as runL,Conditions_rich.richScalar AS rich where rich.beginTime >= from_unixtime(runL.startRunDaq) and rich.beginTime <= from_unixtime(runL.endRunRTS)";// order by rich.beginTime";
  //                     "WHERE beginTime > '2010-04-05' limit 60";
  cout << "sql: " << sql << endl;
  const Char_t *vVars = "run:time:BBCE:BBCW:BBCEandW:YellowBackground:BlueBackground:ZDCE:ZDCW:ZDCEandW:VPDE:VPDW:VPDEandW:ZDCWnokiller:ZDCEnokiller:BBCEandWnokiller:ZDCEandWnokiller:MDT";
  struct Vars_t {
    Float_t run, time, BBCE, BBCW, BBCEandW, YellowBackground, BlueBackground, 
      ZDCE, ZDCW, ZDCEandW, VPDE, VPDW, VPDEandW, 
      ZDCWnokiller, ZDCEnokiller, BBCEandWnokiller, ZDCEandWnokiller, MDT;
  };
  Vars_t Vars;
  Float_t *ars = &Vars.run;
  TFile *fout = new TFile(Form("ZdcCR%i.root",year),"recreate");
  TNtuple *tuple = new TNtuple(Form("TZdcC%i",year),Form("Zdc Conincidence rate for y%i",year),vVars);
  res = db->Query(sql);
  PrintRes(res,tuple,ars);
  fout->Write();
}
