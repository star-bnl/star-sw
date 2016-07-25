#ifndef __CINT__
#include "TSQLServer.h"
#include "TSQLResult.h"
#include "TSQLRow.h"
#include "TStopwatch.h"
#endif
   
void sqlselect()
{
   TSQLServer *db = TSQLServer::Connect("mysql://onldb2.starp.bnl.gov:3502/Conditions_sc","", "");

   printf("Server info: %s\n", db->ServerInfo());
   
   TSQLRow *row;
   TSQLResult *res;
   
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

   // start timer
   TStopwatch timer;
   timer.Start();

   // query database and print results 30 secs average
   const char *sql = "select UNIX_TIMESTAMP(beginTime),innerCurrents,outerCurrents,innerVoltages,outerVoltages from Conditions_sc.tpcAverages "
                     "WHERE beginTime > '2010-04-05' limit 60";
//   const char *sql = "select count(*) from Conditions_sc.tpcAverages "
//                     "WHERE tag&(1<<2)";
   
   res = db->Query(sql);

   int nrows = res->GetRowCount();
   printf("\nGot %d rows in result\n", nrows);
   
   int nfields = res->GetFieldCount();
   for (int i = 0; i < nfields; i++)
      printf("%20s", res->GetFieldName(i));
   printf("\n");
   for (int i = 0; i < nfields*40; i++)
      printf("=");
   printf("\n");
   
   for (int i = 0; i < nrows; i++) {
      row = res->Next();
      for (int j = 0; j < nfields; j++) {
         printf("%20s", row->GetField(j));
      }
      printf("\n");
      delete row;
   }
   
   delete res;
   delete db;

   // stop timer and print results
   timer.Stop();
   Double_t rtime = timer.RealTime();
   Double_t ctime = timer.CpuTime();

   printf("\nRealTime=%f seconds, CpuTime=%f seconds\n", rtime, ctime);
}
