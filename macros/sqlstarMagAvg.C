/*
  root.exe sqlstarMagAvg.C+
  T->Draw("current:time-978307200>>Avg(1000,633e6,641e6,100,-4520,-4500","","sames")
  
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
     Int_t time; 
     Float_t current;
   };
   static starMagAvg_t vertex;
   static Float_t *x = &vertex.current - 1;
   TFile *fOut = new TFile("starMagAvg.root","recreate");
   TTree *tree = new TTree("T","star Mag Avg");
   tree->Branch("mag",&vertex,"time/I:current/F");
   // start timer
   TStopwatch timer;
   timer.Start();
   // query database and print results 30 secs average
   const char *sql = "select UNIX_TIMESTAMP(beginTime),current from RunLog_onl.starMagAvg "
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
	 if (j == 0) {vertex.time = Field.Atoi(); cout << "time " << vertex.time << endl;}
	 else        {x[j]        = Field.Atof(); cout << "x[" << j << "] = " << x[j] << endl;}
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
