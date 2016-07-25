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
   
void sqlstarMagOnl()
{
   TSQLServer *db = TSQLServer::Connect("mysql://dbx.star.bnl.gov:3316","", "");

   printf("Server info: %s\n", db->ServerInfo());
   
   TSQLRow *row;
   TSQLResult *res;
   struct starMagOnl_t {
     Int_t time; 
     Float_t current;
   };
   static starMagOnl_t vertex;
   static Float_t *x = &vertex.current - 1;
   TFile *fOut = new TFile("starMagOnl.root","recreate");
   TTree *tree = new TTree("T","star Mag Onl");
   tree->Branch("mag",&vertex,"time/I:current/F");
   // start timer
   TStopwatch timer;
   timer.Start();
   // query database and print results 30 secs average
   const char *sql = "select UNIX_TIMESTAMP(beginTime),current from RunLog_onl.starMagOnl "
                     "WHERE flavor='ofl' and deactive=0";
//   const char *sql = "select count(*) from Calibrations_rhic.starMagOnl "
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
	 TString Field(row->GetField(j));
         printf("%20s ", Field.Data());
	 if (j == 0) {vertex.time = Field.Atoi(); cout << "time " << vertex.time << endl;}
	 else        {x[j]        = Field.Atof(); cout << "x[" << j << "] = " << x[j] << endl;}
      }
      printf("\n");
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
