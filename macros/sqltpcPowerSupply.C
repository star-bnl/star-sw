#ifndef __CINT__
#include "Riostream.h"
#include "TSQLServer.h"
#include "TSQLResult.h"
#include "TSQLRow.h"
#include "TStopwatch.h"
#include "TString.h"
#endif
   
void sqltpcPowerSupply()
{
   TSQLServer *db = TSQLServer::Connect("mysql://onldb2.starp.bnl.gov:3502/Conditions_daq","", "");

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

   // list tables in database "Conditions_daq" (the permission tables)
   printf("\nList all tables in database \"Conditions_daq\" on server %s\n",
          db->GetHost());
   res = db->GetTables("Conditions_daq");
   while ((row = res->Next())) {
      printf("%s\n", row->GetField(0));
      delete row;
   }
   delete res;
   
   
   // start timer
   TStopwatch timer;
   timer.Start();
   const Char_t *IO[2] = {"Inner","Outer"};
   for (Int_t io = 0; io < 2; io++) {
     printf("\nList all columns in table \"tpcPowerSupply%s\" in database \"Conditions_daq\" on server %s\n",IO[io],
          db->GetHost());
     res = db->GetColumns("Conditions_daq", Form("tpcPowerSupply%s",IO[io]));
     while ((row = res->Next())) {
       printf("%s %s\n", row->GetField(0), row->GetField(1));
       delete row;
     }
     delete res;
     // list columns in table "tpcPowerSupply" in database "mysql"
     // query database and print results 30 secs average
     TString Sql(Form("select UNIX_TIMESTAMP(beginTime),beginTime,Voltages,Currents,Status,Reason from Conditions_daq.tpcPowerSupply%s "
		      "WHERE beginTime > '2012-03-13' limit 60",IO[io]));
     cout << Sql.Data() << endl;
     res = db->Query(Sql.Data());
     
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
   }
   delete db;

   // stop timer and print results
   timer.Stop();
   Double_t rtime = timer.RealTime();
   Double_t ctime = timer.CpuTime();

   printf("\nRealTime=%f seconds, CpuTime=%f seconds\n", rtime, ctime);
}
