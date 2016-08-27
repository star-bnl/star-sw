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
void sqlmq() {
  // http://www.star.bnl.gov/protected/common/common2012/trigger2012/sampleEfficiencypp500GeV/getSampleFrac.pl
  TString database = "mq02.starp.bnl.gov:3606";
  TString dbT("mysql://"); dbT += database; dbT += "/mq_collector_Conditions_cdev";
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
"UNIX_TIMESTAMP(beginTime)         AS utime,"
"DATE_FORMAT(beginTime,\"%Y%m%d\") AS date,"
"DATE_FORMAT(beginTime,\"%H%i%s\") AS time,"
"`rbpm.b-g5-bhx.avgOrbPositionM`          AS b5hx,"
"`rbpm.b-g5-bvx.avgOrbPositionM`          AS b5vx,"
"`rbpm.b-g6-bhx.avgOrbPositionM`          AS b6hx,"
"`rbpm.b-g6-bvx.avgOrbPositionM`          AS b6vx,"
"`rbpm.y-g5-bhx.avgOrbPositionM`          AS y5hx,"
"`rbpm.y-g5-bvx.avgOrbPositionM`          AS y5vx,"
"`rbpm.y-g6-bhx.avgOrbPositionM`          AS y6hx,"
"`rbpm.y-g6-bvx.avgOrbPositionM`          AS y6vx,"
"`ringSpec.blue.beamEnergyM`              AS blue_beamEnergyM           ,"
"`ringSpec.blue.fillNumberM`              AS blue_fillNumberM           ,"
"`ringSpec.blue.ionSpeciesS`              AS blue_ionSpeciesS           ,"
"`ringSpec.blue.ringStateS`               AS blue_ringStateS            ,"
"`ringSpec.blue.timeOfFillStartS`         AS blue_timeOfFillStartS      ,"
"`ringSpec.blue.timeOfLuminosityStartS`   AS blue_timeOfLuminosityStartS,"
"`ringSpec.yellow.beamEnergyM`            AS yellow_beamEnergyM           ,"
"`ringSpec.yellow.fillNumberM`            AS yellow_fillNumberM           ,"
"`ringSpec.yellow.ionSpeciesS`            AS yellow_ionSpeciesS           ,"
"`ringSpec.yellow.ringStateS`             AS yellow_ringStateS            ,"
"`ringSpec.yellow.timeOfFillStartS`       AS yellow_timeOfFillStartS      ,"
"`ringSpec.yellow.timeOfLuminosityStartS` AS yellow_timeOfLuminosityStartS,"
"`buckets.blue.filledBucketsS`            AS blue_filledBucketsS          ,"
"`buckets.yellow.filledBucketsS`          AS yellow_filledBucketsS         "
"FROM beamPositionMonitors;";// order by rich.beginTime";
  cout << "sql: " << sql << endl;
  const Char_t *vVars = "utime:date:time:b5hx:b5vx:b6hx:b6vx:y5hx:y5vx:y6hx:y6vx:blue_beamEnergyM:"
"blue_fillNumberM:blue_ionSpeciesS:blue_ringStateS:blue_timeOfFillStartS:blue_timeOfLuminosityStartS:"
"yellow_beamEnergyM:yellow_fillNumberM:yellow_ionSpeciesS:yellow_ringStateS:yellow_timeOfFillStartS:"
    "yellow_timeOfLuminosityStartS:blue_filledBucketsS:yellow_filledBucketsS";
  struct Vars_t {
    Float_t utime,date,time,b5hx,b5vx,b6hx,b6vx,y5hx,y5vx,y6hx,y6vx,blue_beamEnergyM,
      blue_fillNumberM,blue_ionSpeciesS,blue_ringStateS,blue_timeOfFillStartS,blue_timeOfLuminosityStartS,
      yellow_beamEnergyM,yellow_fillNumberM,yellow_ionSpeciesS,yellow_ringStateS,yellow_timeOfFillStartS,
      yellow_timeOfLuminosityStartS,blue_filledBucketsS,yellow_filledBucketsS;};
  Vars_t Vars;
  Float_t *ars = &Vars.utime;
  TFile *fout = new TFile("mb.root","recreate");
  TNtuple *tuple = new TNtuple("mb","CAD beam parameters",vVars);
  res = db->Query(sql);
  PrintRes(res,tuple,ars);
  fout->Write();
}
