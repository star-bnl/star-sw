/*
  root.exe sqlstarMagAvg.C+
  T->Draw("current:time-946684800+978325200>>AvgC(1000,633e6,641e6,100,-4520,-4500","","sames");
  select unix_timestamp("1995-01-01 00:00:00"); 788918400     
  select unix_timestamp("2000-01-01 00:00:00"); 946684800     TDatime tt(2000,1,1,0,0,0); 946702800 => +18000
  select unix_timestamp("2021-01-01 00:00:00"); 1609459200
  select unix_timestamp("2022-01-01 00;00:00"); 1640995200
  T->Draw("current:time-31640400","current>-4540&&current<-4460")
  T->Draw("current:time-946684800>>h2021","current>-4550&&current<-4450&&time>1609459200")
  T->Draw("current:(time-1609459200)/24/60/60+1>>h2021","current>-4550&&current<-4450&&time>1609459200")
*/
#ifndef __CINT__
#include "Riostream.h"
#include "TSQLServer.h"
#include "TSQLResult.h"
#include "TSQLRow.h"
#include "TStopwatch.h"
#include "TFile.h"
#include "TTree.h"
#include "TString.h"
using namespace std;
#endif
   
void sqlstarMagAvg()
{
   TSQLServer *db = TSQLServer::Connect("mysql://dbx.star.bnl.gov:3316","", "");

   printf("Server info: %s\n", db->ServerInfo());
   
   TSQLRow *row;
   TSQLResult *res;
   struct starMagAvg_t {
     Int_t   time; 
     Float_t current;
     Float_t rms;
     Int_t   noEntries;
   };
   static starMagAvg_t vertex;
   static Float_t *x = &vertex.current - 1;
   TFile *fOut = new TFile("starMagAvg.root","recreate");
   TTree *tree = new TTree("T","star Mag Avg");
   tree->Branch("mag",&vertex,"time/I:current/F:rms/F:noEntries/I");
   // start timer
   TStopwatch timer;
   timer.Start();
   // query database and print results 30 secs average
   const char *sql = "select UNIX_TIMESTAMP(beginTime),current,rms,noEntries  from RunLog_onl.starMagAvg "
                     "WHERE flavor='ofl' and deactive=0 and rms > 0 and rms < 10";
//   const char *sql = "select count(*) from Calibrations_rhic.starMagAvg "
//                     "WHERE tag&(1<<2)";
   
   res = db->Query(sql);

   int nrows = res->GetRowCount();
   printf("\nGot %d rows in result\n", nrows);
   
   int nfields = res->GetFieldCount();
   static Int_t debugP = 12;
   if (debugP > 0) {
   for (int i = 0; i < nfields; i++)
      printf("%20s", res->GetFieldName(i));
   printf("\n");
   for (int i = 0; i < nfields*40; i++)
      printf("=");
   printf("\n");
   }
   for (int i = 0; i < nrows; i++) {
      row = res->Next();
      for (int j = 0; j < nfields; j++) {
	 TString Field(row->GetField(j));
	 if (debugP > 0) {
         printf("%20s ", Field.Data());
	 }
	 if      (j == 0) {vertex.time = Field.Atoi(); cout << "time = " << vertex.time << endl;}
	 else if (j == 3) {vertex.noEntries = Field.Atoi(); cout << "noEntries = " << vertex.noEntries << endl;}
	 else             {x[j]        = Field.Atof(); cout << "x[" << j << "] = " << x[j] << endl;}
      }
      if (debugP > 0) {
      printf("\n");
   debugP--;
      }
      delete row;
      tree->Fill();
   }
   
   delete res;
   delete db;

   // stop timer and print results
   timer.Stop();
   Double_t rtime = timer.RealTime();
   Double_t ctime = timer.CpuTime();
   fOut->Write();
   printf("\nRealTime=%f seconds, CpuTime=%f seconds\n", rtime, ctime);
}
