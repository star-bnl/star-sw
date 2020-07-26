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
 class JobStatus : public TObject {
  //                                        +--------------------------------+---------------+------+-----+---------------------+----------------+
  //                                        | Field                          | Type          | Null | Key | Default             | Extra          |   
public:					 // +--------------------------------+---------------+------+-----+---------------------+----------------+
  TString jobID; 			 // | jobID                          | varchar(64)   | NO   |     | 0                   |                |
  TString LibLevel; 			 // | LibLevel                       | varchar(20)   | NO   |     |                     |                |
  TString LibTag; 			 // | LibTag                         | varchar(20)   | NO   | MUL |                     |                |
  TString rootLevel; 	         	 // | rootLevel                      | varchar(20)   | NO   |     |                     |                |
  TString path;			         // | path                           | varchar(128)  | NO   | MUL |                     |                |
  Int_t prodyear;			 // | prodyear                       | smallint(6)   | NO   |     | 0                   |                |
  TString logFile;			 // | logFile                        | varchar(64)   | NO   |     |                     |                |
  TString createTime;		         // | createTime                     | datetime      | NO   | MUL | 0000-00-00 00:00:00 |                |
  TString chainOpt;			 // | chainOpt                       | varchar(248)  | NO   |     | NULL                |                |
  TString jobStatus;			 // | jobStatus                      | varchar(32)   | NO   | MUL |                     |                |
  TString crashedCode;		         // | crashedCode                    | varchar(32)   | NO   |     |                     |                |
  TString errMessage;		         // | errMessage                     | varchar(128)  | NO   |     |                     |                |
  Int_t  NoEventDone;			 // | NoEventDone                    | mediumint(9)  | NO   | MUL | 0                   |                |
  Float_t  memUsageF;			 // | memUsageF                      | float(8,2)    | NO   | MUL | 0.00                |                |
  Float_t  memUsageL;			 // | memUsageL                      | float(8,2)    | NO   | MUL | 0.00                |                |
  Float_t  CPU_per_evt_sec;		 // | CPU_per_evt_sec                | float(8,2)    | NO   | MUL | 0.00                |                |
  Float_t  RealTime_per_evt;		 // | RealTime_per_evt               | float(8,2)    | NO   | MUL | 0.00                |                |
  Int_t  percent_of_usable_evt;		 // | percent_of_usable_evt          | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avg_no_tracks;			 // | avg_no_tracks                  | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_tracksnfit15;		 // | avg_no_tracksnfit15            | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  NoEventVtx;			 // | NoEventVtx                     | smallint(6)   | NO   | MUL | 0                   |                |
  Float_t  avgNoVtx_evt;		 // | avgNoVtx_evt                   | float(8,2)    | NO   | MUL | 0.00                |                |
  Int_t  avg_no_primaryT;		 // | avg_no_primaryT                | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_primaryT_1vtx;		 // | avg_no_primaryT_1vtx           | mediumint(9)  | NO   |     | 0                   |                |
  Int_t  avg_no_primaryTnfit15;		 // | avg_no_primaryTnfit15          | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_primaryTnfit15_1vtx;	 // | avg_no_primaryTnfit15_1vtx     | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_V0Vrt;			 // | avg_no_V0Vrt                   | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_XiVrt;			 // | avg_no_XiVrt                   | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avg_no_KinkVrt;		 // | avg_no_KinkVrt                 | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avgNoTrack_usbevt;		 // | avgNoTrack_usbevt              | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avgNoTrackNfit15_usbevt;	 // | avgNoTrackNfit15_usbevt        | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avgNoPrTrack_1vtx_usbevt;	 // | avgNoPrTrack_1vtx_usbevt       | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avgNoPrTrackNfit15_1vtx_usbevt; // | avgNoPrTrackNfit15_1vtx_usbevt | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avgNoV0_usbevt;		 // | avgNoV0_usbevt                 | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avgNoXi_usbevt;		 // | avgNoXi_usbevt                 | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avgNoKink_usbevt;		 // | avgNoKink_usbevt               | smallint(6)   | NO   | MUL | 0                   |                |
  TString nodeID;			 // | nodeID                         | varchar(64)   | NO   |     |                     |                |
  Bool_t avail;				 // | avail                          | enum('Y','N') | NO   | MUL | Y                   |                |
  Int_t  id;				 // | id                             | mediumint(9)  | NO   | PRI | NULL                | auto_increment |
  Int_t  NoEventSkip;			 // | NoEventSkip                    | mediumint(9)  | NO   |     | 0                   |                |
  ClassDef(JobStatus,1)			 // +--------------------------------+---------------+------+-----+---------------------+----------------+
};					 
					  
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
void sqlJobStatus() {
  // http://www.star.bnl.gov/protected/common/common2012/trigger2012/sampleEfficiencypp500GeV/getSampleFrac.pl
  TString database = "duvall.star.bnl.gov:3306";
  TString dbT("mysql://"); dbT += database; dbT += "/LibraryJobs";
  TSQLServer *db = TSQLServer::Connect(dbT.Data(),"starreco", ""); if (! db) {cout << "Can't connect " << dbT.Data() << endl; return;}
  TSQLResult *res;
  TSQLRow *row;
#if 0
  printf("\nList all databases on server %s\n", db->GetHost());
  res = db->GetDataBases();
  while ((row = res->Next())) {
    printf("%s\n", row->GetField(0));
    delete row;
  }
  delete res;
   // list tables in database "LibraryJobs" (the permission tables)
   printf("\nList all tables in database \"LibraryJobs\" on server %s\n",
          db->GetHost());
   res = db->GetTables("LibraryJobs");
   while ((row = res->Next())) {
      printf("%s\n", row->GetField(0));
      delete row;
   }
   delete res;
   
   // list columns in table "JobStatus_4" in database "mysql"
   printf("\nList all columns in table \"JobStatus_4\" in database \"LibraryJobs\" on server %s\n",
          db->GetHost());
   res = db->GetColumns("LibraryJobs", "JobStatus_4");
   printf("GetFieldCount = %i\n", res->GetFieldCount());
   while ((row = res->Next())) {
      printf("%s %s\n", row->GetField(1), row->GetField(0));
      delete row;
   }
   delete res;
#endif
   TString A;
#if 1
   JobStatus job;
   const char *sql = "select * from JobStatus_4 limit 3";
   
   res = db->Query(sql);

   int nrows = res->GetRowCount();
   printf("\nGot %d rows in result\n", nrows);
   
   int nfields = res->GetFieldCount();
#if 0
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
#else
   for (int i = 0; i < nrows; i++) {
     row = res->Next();
     for (int j = 0; j < nfields; j++) {
       printf("%5i %20s : %120s\n", j,  res->GetFieldName(j), row->GetField(j));
     }
     job.jobID = row->GetField(0);
     job.LibLevel = row->GetField(1);

     job.jobID = row->GetField(0);
     job.LibLevel = row->GetField(1);
     job.LibTag = row->GetField(2);
     job.rootLevel = row->GetField(3);
     job.path = row->GetField(4);
     A = row->GetField(5); Int_t prodyear = A.Atoi();
     job.logFile = row->GetField(6);
     job.createTime = row->GetField(7);
     job.chainOpt = row->GetField(8);
     job.jobStatus = row->GetField(9);
     job.crashedCode = row->GetField(10);
     job.errMessage = row->GetField(11);
     A = row->GetField(12); job.NoEventDone = A.Atoi();
     A = row->GetField(13); job.memUsageF = A.Atof();
     A = row->GetField(14); job.memUsageL = A.Atof();
     A = row->GetField(15); job.CPU_per_evt_sec = A.Atof();
     A = row->GetField(16); job.RealTime_per_evt = A.Atof();
     A = row->GetField(17); job.percent_of_usable_evt = A.Atoi();
     A = row->GetField(18); job.avg_no_tracks = A.Atoi();
     A = row->GetField(19); job.avg_no_tracksnfit15 = A.Atoi();
     A = row->GetField(20); job.NoEventVtx = A.Atoi();
     A = row->GetField(21); job.avgNoVtx_evt = A.Atof();
     A = row->GetField(22); job.avg_no_primaryT = A.Atoi();
     A = row->GetField(23); job.avg_no_primaryT_1vtx = A.Atoi();
     A = row->GetField(24); job.avg_no_primaryTnfit15 = A.Atoi();
     A = row->GetField(25); job.avg_no_primaryTnfit15_1vtx = A.Atoi();
     A = row->GetField(26); job.avg_no_V0Vrt = A.Atoi();
     A = row->GetField(27); job.avg_no_XiVrt = A.Atoi();
     A = row->GetField(28); job.avg_no_KinkVrt = A.Atoi();
     A = row->GetField(29); job.avgNoTrack_usbevt = A.Atoi();
     A = row->GetField(30); job.avgNoTrackNfit15_usbevt = A.Atoi();
     A = row->GetField(31); job.avgNoPrTrack_1vtx_usbevt = A.Atoi();
     A = row->GetField(32); job.avgNoPrTrackNfit15_1vtx_usbevt = A.Atoi();
     A = row->GetField(33); job.avgNoV0_usbevt = A.Atoi();
     A = row->GetField(34); job.avgNoXi_usbevt = A.Atoi();
     A = row->GetField(35); job.avgNoKink_usbevt = A.Atoi();
     job.nodeID = row->GetField(36);
     A = row->GetField(37); Bool_t avail = (A == "Y") ? kTRUE : kFALSE;
     A = row->GetField(38); job.id = A.Atoi();
     A = row->GetField(39); job.NoEventSkip = A.Atoi();
     delete row;
   }
#endif   
   delete res;
   delete db;
#endif
}

/*
  MySQL [LibraryJobs]> explain JobStatus_6;
*/
