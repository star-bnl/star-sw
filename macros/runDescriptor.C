#ifndef __CINT__
#include <TSQLServer.h>
#include <TSQLResult.h>
#include <TSQLRow.h>
#include "TNtuple.h"
#include "TFile.h"
#include "TStopwatch.h"
#endif
void runDescriptor() {
   TSQLServer *db = TSQLServer::Connect("mysql://onldb.starp.bnl.gov:3501/RunLog","", "");

   printf("Server info: %s\n", db->ServerInfo());
   
   TSQLRow *row, *row1;
   TSQLResult *res, *res1;
#if 0   
   // list databases available on server
   printf("\nList all databases on server %s\n", db->GetHost());
   res = db->GetDataBases();
   while ((row = res->Next())) {
      printf("%s\n", row->GetField(0));
      delete row;
   }
   delete res;
   // list tables in database "RunLog" (the permission tables)
   printf("\nList all tables in database \"RunLog\" on server %s\n",
          db->GetHost());
   res = db->GetTables("RunLog");
   while ((row = res->Next())) {
      printf("%s\n", row->GetField(0));
      delete row;
   }
   delete res;
   
   // list columns in table "runDescriptor" in database "mysql"
   printf("\nList all columns in table \"runDescriptor\" in database \"RunLog\" on server %s\n",
          db->GetHost());
   res = db->GetColumns("RunLog", "runDescriptor");
   while ((row = res->Next())) {
      printf("%s %s\n", row->GetField(0), row->GetField(1));
      delete row;
   }
   delete res;
#endif

   // start timer
   TStopwatch timer;
   timer.Start();

   // query database and print results 30 secs average
   const char *sql = "select UNIX_TIMESTAMP(beginTime),runNumber,startRunTime,endRunTime,glbSetupName,daqSetupName,trgSetupName from RunLog.runDescriptor order by beginTime";
   //                     "WHERE beginTime > '2010-04-05' limit 60";
#if 0
   const Char_t *vVars = "time:run:star:end:glb:daq:trg";
   struct Vars_t {
     Float_t time, run, star, end, glb, trg, sc;
   };
   Vars_t Vars;
   Float_t *ars = &Vars.time;
//   const char *sql = "select count(*) from RunLog.runDescriptor "
//                     "WHERE tag&(1<<2)";
   TFile *fout = new TFile("runDescriptor.root","recreate");
   TNtuple *tuple = new TNtuple("runDescriptor","Run information",vVars);
#endif
   res = db->Query(sql);

   int nrows = res->GetRowCount();
   printf("\nGot %d rows in result\n", nrows);
   
   int nfields = res->GetFieldCount();
   for (int i = 0; i < nfields; i++)
      printf("%20s", res->GetFieldName(i));
   printf("\n");
   for (int i = 0; i < nfields*20; i++) printf("=");
   printf("\n");
   
   for (int i = 0; i < nrows; i++) {
      row = res->Next();
      for (int j = 0; j < nfields; j++) {
	if (i < 40) {
	  printf("%20s", row->GetField(j));
	}
#if 0
	TString a(row->GetField(j));
	ars[j] = a.Atof();
#endif
      }
      if (i < 40) printf("\n");
      delete row;
#if 0
      tuple->Fill(ars);
#endif
   }
   delete res;
   delete db;

   // stop timer and print results
   timer.Stop();
   Double_t rtime = timer.RealTime();
   Double_t ctime = timer.CpuTime();
#if 0
   fout->Write();
#endif
   printf("\nRealTime=%f seconds, CpuTime=%f seconds\n", rtime, ctime);
}
