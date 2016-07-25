#ifndef __CINT__
#include <TSQLServer.h>
#include <TSQLResult.h>
#include <TSQLRow.h>
#include "TNtuple.h"
#include "TFile.h"
#include "TStopwatch.h"
#endif
#define __NTUPLE__
void tpcAveragesZdcC() {
   TSQLServer *db = TSQLServer::Connect("mysql://onldb2.starp.bnl.gov:3502/Conditions_sc","", "");

   printf("Server info: %s\n", db->ServerInfo());
   
   TSQLRow *row;
   TSQLResult *res;
#if 0   
   // list databases available on server
   printf("\nList all databases on server %s\n", db->GetHost());
   res = db->GetDataBases();
   while ((row = res->Next())) {
      printf("%s\n", row->GetField(0));
      delete row;
   }
   delete res;
   // list tables in database "Conditions_sc" (the permission tables)
   printf("\nList all tables in database \"Conditions_sc\" on server %s\n",
          db->GetHost());
   res = db->GetTables("Conditions_sc");
   while ((row = res->Next())) {
      printf("%s\n", row->GetField(0));
      delete row;
   }
   delete res;
   
   // list columns in table "tpcAverages" in database "mysql"
   printf("\nList all columns in table \"tpcAverages\" in database \"Conditions_sc\" on server %s\n",
          db->GetHost());
   res = db->GetColumns("Conditions_sc", "tpcAverages");
   while ((row = res->Next())) {
      printf("%s %s\n", row->GetField(0), row->GetField(1));
      delete row;
   }
   delete res;
#endif

   // start timer
   TStopwatch timer;
   timer.Start();
   
   const Char_t *sql = 
     "SELECT UNIX_TIMESTAMP(tpc.beginTime) AS time,tpc.innerCurrents,tpc.outerCurrents,tpc.innerVoltages,tpc.outerVoltages,"
     "UNIX_TIMESTAMP(rich.beginTime) AS time1,rich.rs8,rich.rs15  FROM Conditions_sc.tpcAverages AS tpc"
     " LEFT JOIN  Conditions_rich.richScalar AS rich "
     "ON (ABS(UNIX_TIMESTAMP(tpc.beginTime) - UNIX_TIMESTAMP(rich.beginTime))<15  )";
     //     " WHERE tpc.beginTime > '2010-04-05' limit 60";
#ifdef  __NTUPLE__

   const Char_t *vVars = "time:innerCurrents:outerCurrents:innerVoltages:outerVoltages:time1:rs8:rs15";
   struct Vars_t {
     Float_t time, innerCurrents, outerCurrents, innerVoltages, outerVoltages, time1, rs8, rs15;
   };
   Vars_t Vars;
   Float_t *ars = &Vars.time;
//   const Char_t *sql = "select count(*) from Conditions_sc.tpcAverages "
//                     "WHERE tag&(1<<2)";
   TFile *fout = new TFile("tpcAveragesZdcC.root","recreate");
   TNtuple *tuple = new TNtuple("tpcAveragesZdcC","Anode Current and ZDC information",vVars);
#endif
   res = db->Query(sql);

   Int_t nrows = res->GetRowCount();
   printf("\nGot %d rows in result\n", nrows);
   
   Int_t nfields = res->GetFieldCount();
   for (Int_t i = 0; i < nfields; i++)
      printf("%20s", res->GetFieldName(i));
   printf("\n");
   for (Int_t i = 0; i < nfields*20; i++)
      printf("=");
   printf("\n");
   
   for (Int_t i = 0; i < nrows; i++) {
      row = res->Next();
      for (Int_t j = 0; j < nfields; j++) {
	if (i < 40) {
	  printf("%20s", row->GetField(j));
	}
#ifdef  __NTUPLE__
	TString a(row->GetField(j));
	if (a == "NULL") ars[j] = -13;
	else             ars[j] = a.Atof();
#endif
      }
      if (i < 40) printf("\n");
      delete row;
#ifdef  __NTUPLE__
      tuple->Fill(ars);
#endif
   }
   delete res;
   delete db;

   // stop timer and print results
   timer.Stop();
   Double_t rtime = timer.RealTime();
   Double_t ctime = timer.CpuTime();
#ifdef  __NTUPLE__
   fout->Write();
#endif
   printf("\nRealTime=%f seconds, CpuTime=%f seconds\n", rtime, ctime);
}
