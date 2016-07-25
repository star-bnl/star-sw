#ifndef __CINT__
#include "Riostream.h"
#include "TSQLServer.h"
#include "TSQLResult.h"
#include "TSQLRow.h"
#include "TStopwatch.h"
#endif
//________________________________________________________________________________
void LoopOverDbTables(const Char_t *select="beginTime = '2009-12-15 00:00:00'"
		      //		      const Char_t *select="flavor like 'sim%'"
		      ){
  TSQLServer *db = TSQLServer::Connect("mysql://dbx.star.bnl.gov:3316/StarDb","", "");
#if 0  
  cout << "Server info: " <<  db->ServerInfo() << endl;
#endif  
  TSQLRow *row;
  TSQLResult *resDb, *resTab, *res;
#if 0 
  // start timer
  TStopwatch timer;
  timer.Start();
  // list databases available on server
  cout << "List all databases on server " << db->GetHost() << endl;
#endif
  resDb = db->GetDataBases();
  if (resDb) {
    while ((row = resDb->Next())) {
      cout << row->GetField(0) << endl;
      TString Db(row->GetField(0));
      delete row;
      if (!Db.BeginsWith("Condition") && !Db.BeginsWith("Calibration") && !Db.BeginsWith("Geometry")) continue;
#if 0
      cout << "List all tables in database " << Db << " on server " << db->GetHost() << endl;
#endif
      resTab = db->GetTables(Db);
      if (! resTab) continue;
      while ((row = resTab->Next())) {
#if 0
	cout << row->GetField(0) << endl;
#endif
	TString Table(row->GetField(0));
	delete row;
#if 0
	// list columns in table "tpcAverages" in database "mysql"
	cout << "List all columns in table " << Table 
	     << " in database " << Db 
	     << " on server " << db->GetHost() << endl;
#endif
	res = db->GetColumns(Db, Table);
	if (! res) continue;
#if 0
	Int_t nf = res->GetRowCount();
	Int_t *FieldFl = new Int_t[nf];
	memset(FieldFl, 0, nf*sizeof(Int_t));
	Int_t f = 0; 
	while ((row = res->Next())) {
	  cout << row->GetField(0) << " " <<  row->GetField(1) << endl;
	  TString Value(row->GetField(0));
	  TString Field(row->GetField(1));
	  if (Value != "flavor" && (
	      Field.BeginsWith("varchar") || 
	      Field.BeginsWith("mediumtext") || 
	      Field.BeginsWith("longblob"))) FieldFl[f] = 1;
	  delete row;
	  f++;
	}
	delete res;
#endif
	// query database and print results 30 secs average
	TString Sql("SELECT * from ");
	Sql += Db; Sql += "."; Sql += Table;
	//	Sql += " WHERE deactive =  0 AND (beginTime = '2009-12-15 00:00:00' OR flavor like 'sim%')";
	//	Sql += " WHERE beginTime = '2009-12-15 00:00:00' OR flavor like 'sim%'";
	Sql += " WHERE ";//beginTime = '2009-12-15 00:00:00' OR flavor like 'sim%'";
	Sql += select;
	res = db->Query(Sql);
	if (! res) continue;
	Int_t nrows = res->GetRowCount();
	if (! nrows) {delete res; continue;}
	cout << Db << "/" << Table << " found " << nrows << " rows with selection : " << select << endl;
#if 0
	cout << "Got " << nrows << " rows in result" << endl;
	Int_t nfields = res->GetFieldCount();
	for (Int_t i = 0; i < nfields; i++)
	  cout << Form("%20s", res->GetFieldName(i));
	cout << endl;
#if 1
	for (Int_t i = 0; i < nfields*20; i++) cout << "=";
	cout << endl;
	for (Int_t i = 0; i < nrows; i++) {
	  row = res->Next();
	  for (Int_t j = 0; j < nfields; j++) {
	    if (FieldFl[j]) {
	      cout << "____________________";
	    } else {
	      cout << Form("%20s", row->GetField(j));
	    }
	  }
	  cout << endl;
	  delete row;
	}
#endif
	delete [] FieldFl;
#endif	
	delete res;
      }
      delete resTab;
    }
    delete resDb;
  }
  delete db;
#if 0
  // stop timer and print results
  timer.Stop();
  Double_t rtime = timer.RealTime();
  Double_t ctime = timer.CpuTime(); 
  
  cout << "RealTime=" << rtime << " seconds, CpuTime=" << ctime << " seconds" << endl;
#endif
}
