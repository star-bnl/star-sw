#if !defined(__CINT__) || defined(__MAKECINT__)
#include <vector>
#include <list>
#include "Riostream.h"
#include "TSQLServer.h"
#include "TSQLResult.h"
#include "TSQLRow.h"
#include "TNtuple.h"
#include "TFile.h"
#include "TString.h"
#include "TCanvas.h"
#include "TApplication.h"
#include "TProfile.h"
#include "TList.h"
#include "TLegend.h"
#include "TStyle.h"
#endif
using namespace std;
typedef list<TString> List;
struct Field_t {
  const Char_t *Name;
  Int_t         type; // 0 -> TString, 1 -> Int_t, 2 -> Float_t, 3 -> Bool_t
  Bool_t        plot;
  TProfile     *prof; //
};
Field_t Fields[47] = {
  {"jobID",	0, kFALSE, 0},                          //  0
  {"LibLevel",	0, kFALSE, 0},				//  1
  {"LibTag",	0, kFALSE, 0},				//  2
  {"rootLevel",	0, kFALSE, 0},				//  3
  {"path",	0, kFALSE, 0},				//  4
  {"prodyear",	0, kFALSE, 0},				//  5
  {"logFile",	0, kFALSE, 0},				//  6
  {"createTime",	0, kFALSE, 0},			//  7
  {"chainOpt",	0, kFALSE, 0},				//  8
  {"jobStatus",	0, kFALSE, 0},				//  9
  {"crashedCode",	0, kFALSE, 0},			//  0
  {"errMessage",	0, kFALSE, 0},			// 11
  {"NoEventDone",	1, kTRUE, 0},			// 12
  {"memUsageF",	2, kTRUE, 0},				// 13
  {"memUsageL",	2, kTRUE, 0},				// 14
  {"CPU_per_evt_sec",	2, kTRUE, 0},			// 15
  {"RealTime_per_evt",	2, kTRUE, 0},			// 16
  {"percent_of_usable_evt",	1, kTRUE, 0},		// 17
  {"avg_no_tracks",	1, kTRUE, 0},			// 18
  {"avg_no_tracksnfit15",	1, kTRUE, 0},		// 19
  {"NoEventVtx",	1, kTRUE, 0},			// 20
  {"avgNoVtx_evt",	2, kTRUE, 0},			// 21
  {"avg_no_primaryT",	1, kTRUE, 0},			// 22
  {"avg_no_primaryT_1vtx",	1, kTRUE, 0},		// 23
  {"avg_no_primaryTnfit15",	1, kTRUE, 0},		// 24
  {"avg_no_primaryTnfit15_1vtx",	1, kTRUE, 0},	// 25
  {"avg_no_V0Vrt",	1, kFALSE, 0},			// 26
  {"avg_no_XiVrt",	1, kFALSE, 0},			// 27
  {"avg_no_KinkVrt",	1, kFALSE, 0},			// 28
  {"avgNoTrack_usbevt",	1, kTRUE, 0},			// 29
  {"avgNoTrackNfit15_usbevt",	1, kTRUE, 0},		// 20
  {"avgNoPrTrack_1vtx_usbevt",	1, kTRUE, 0},		// 31
  {"avgNoPrTrackNfit15_1vtx_usbevt",	1, kTRUE, 0},	// 32
  {"avgNoV0_usbevt",	1, kFALSE, 0},			// 33
  {"avgNoXi_usbevt",	1, kFALSE, 0},			// 34
  {"avgNoKink_usbevt",	1, kFALSE, 0},			// 35
  {"nodeID",	0, kFALSE, 0},				// 36
  {"avail",	3, kFALSE, 0},				// 37
  {"id",	1, kFALSE, 0},				// 38
  {"NoEventSkip",	1, kFALSE, 0},			// 39
  {"avg_no_btof",       1, kTRUE, 0},                   // 40                    | float(8,2)    | NO   |     | 0.00                |                |
  {"DatasetChainID",    1, kFALSE, 0},                  // 41 | int(11)       | YES  |     | NULL                |                |
  {"Dataset",           0, kFALSE, 0},                  // 42                        | varchar(128)  | YES  |     | NULL                |                |
  {"ChangeConsider",    0, kFALSE, 0},                  // 43 | enum('Y','N') | YES  |     | N                   |                |
  {"gcc_version",       0, kFALSE, 0},                  // 44                    | varchar(32)   | YES  |     | NULL                |                |
  {"tracker",           0, kFALSE, 0},                  // 45             | varchar(32)   | YES  |     | NULL                |                |
  {"optimized",         0, kFALSE, 0}                   // 46 | varchar(16)   | YES  |     | NULL                |                |
};
class JobStatus : public TObject {
  //                                          +--------------------------------+---------------+------+-----+---------------------+----------------+
  //                                          | Field                          | Type          | Null | Key | Default             | Extra          |  
public:					 //   +--------------------------------+---------------+------+-----+---------------------+----------------+
  TString jobID; 			 //  0| jobID                          | varchar(64)   | NO   |     | 0                   |                |
  TString LibLevel; 			 //  1| LibLevel                       | varchar(20)   | NO   |     |                     |                |
  TString LibTag; 			 //  2| LibTag                         | varchar(20)   | NO   | MUL |                     |                |
  TString rootLevel; 	         	 //  3| rootLevel                      | varchar(20)   | NO   |     |                     |                |
  TString path;			         //  4| path                           | varchar(128)  | NO   | MUL |                     |                |
  TString prodyear;			 //  5| prodyear                       | smallint(6)   | NO   |     | 0                   |                |
  TString logFile;			 //  6| logFile                        | varchar(64)   | NO   |     |                     |                |
  TString createTime;		         //  7| createTime                     | datetime      | NO   | MUL | 0000-00-00 00:00:00 |                |
  TString chainOpt;			 //  8| chainOpt                       | varchar(248)  | NO   |     | NULL                |                |
  TString jobStatus;			 //  9| jobStatus                      | varchar(32)   | NO   | MUL |                     |                |
  TString crashedCode;		         //  0| crashedCode                    | varchar(32)   | NO   |     |                     |                |
  TString errMessage;		         // 11| errMessage                     | varchar(128)  | NO   |     |                     |                |
  Int_t  NoEventDone;			 // 12| NoEventDone                    | mediumint(9)  | NO   | MUL | 0                   |                |
  Float_t  memUsageF;			 // 13| memUsageF                      | float(8,2)    | NO   | MUL | 0.00                |                |
  Float_t  memUsageL;			 // 14| memUsageL                      | float(8,2)    | NO   | MUL | 0.00                |                |
  Float_t  CPU_per_evt_sec;		 // 15| CPU_per_evt_sec                | float(8,2)    | NO   | MUL | 0.00                |                |
  Float_t  RealTime_per_evt;		 // 16| RealTime_per_evt               | float(8,2)    | NO   | MUL | 0.00                |                |
  Int_t  percent_of_usable_evt;		 // 17| percent_of_usable_evt          | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avg_no_tracks;			 // 18| avg_no_tracks                  | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_tracksnfit15;		 // 19| avg_no_tracksnfit15            | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  NoEventVtx;			 // 20| NoEventVtx                     | smallint(6)   | NO   | MUL | 0                   |                |
  Float_t  avgNoVtx_evt;		 // 21| avgNoVtx_evt                   | float(8,2)    | NO   | MUL | 0.00                |                |
  Int_t  avg_no_primaryT;		 // 22| avg_no_primaryT                | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_primaryT_1vtx;		 // 23| avg_no_primaryT_1vtx           | mediumint(9)  | NO   |     | 0                   |                |
  Int_t  avg_no_primaryTnfit15;		 // 24| avg_no_primaryTnfit15          | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_primaryTnfit15_1vtx;	 // 25| avg_no_primaryTnfit15_1vtx     | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_V0Vrt;			 // 26| avg_no_V0Vrt                   | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avg_no_XiVrt;			 // 27| avg_no_XiVrt                   | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avg_no_KinkVrt;		 // 28| avg_no_KinkVrt                 | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avgNoTrack_usbevt;		 // 29| avgNoTrack_usbevt              | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avgNoTrackNfit15_usbevt;	 // 20| avgNoTrackNfit15_usbevt        | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avgNoPrTrack_1vtx_usbevt;	 // 31| avgNoPrTrack_1vtx_usbevt       | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avgNoPrTrackNfit15_1vtx_usbevt; // 32| avgNoPrTrackNfit15_1vtx_usbevt | mediumint(9)  | NO   | MUL | 0                   |                |
  Int_t  avgNoV0_usbevt;		 // 33| avgNoV0_usbevt                 | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avgNoXi_usbevt;		 // 34| avgNoXi_usbevt                 | smallint(6)   | NO   | MUL | 0                   |                |
  Int_t  avgNoKink_usbevt;		 // 35| avgNoKink_usbevt               | smallint(6)   | NO   | MUL | 0                   |                |
  TString nodeID;			 // 36| nodeID                         | varchar(64)   | NO   |     |                     |                |
  Bool_t avail;				 // 37| avail                          | enum('Y','N') | NO   | MUL | Y                   |                |
  Int_t  id;				 // 38| id                             | mediumint(9)  | NO   | PRI | NULL                | auto_increment |
  Int_t  NoEventSkip;			 // 39| NoEventSkip                    | mediumint(9)  | NO   |     | 0                   |                |
  Float_t avg_no_btof;                   // 40| avg_no_btof                    | float(8,2)    | NO   |     | 0.00                |                |
  Int_t  DatasetChainID;                 // 41| DatasetChainID                 | int(11)       | YES  |     | NULL                |                |
  TString Dataset;                       // 42| Dataset                        | varchar(128)  | YES  |     | NULL                |                |
  Bool_t ChangeConsider;                 // 43| ChangeConsider                 | enum('Y','N') | YES  |     | N                   |                |
  TString  gcc_version;                  // 44| gcc_version                    | varchar(32)   | YES  |     | NULL                |                |
  TString tracker;                       // 45| tracker                        | varchar(32)   | YES  |     | NULL                |                |
  TString optimized;                     // 46| optimized                      | varchar(16)   | YES  |     | NULL                |                |
  //                                          +--------------------------------+---------------+------+-----+---------------------+----------------+
  ClassDef(JobStatus,1)
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
TString FormVariable(TString prodyear, TString Dataset, TString LibTag, TString optimized, TString path, TString gcc_version, TString tracker) {
  TString Var(prodyear);
  if (tracker.BeginsWith("daq")) {Var += "/daq";}
  else                           {Var += "/trs";}
  Var += "/";
  Dataset.ReplaceAll("production_","");
  Dataset.ReplaceAll("_opt","");
  Int_t index = Dataset.Index("_fixed");
  if (index > 0) {
    Dataset = Dataset(0,index);
    Dataset += "_FXT";
  }
  index = Dataset.Index("_2");
  if (index > 0) {
    Dataset = Dataset(0,index);
  }
  Var += Dataset;
  if (tracker.Contains("stica")) {Var += "/CA";}
  else if (tracker.Contains("stihr")) {Var = ""; return Var;}
  else                           {Var += "/sti";}
  if (path.Contains("AgML"))     {Var += "/AgML";}
  if (gcc_version.Contains("x8664")) Var += "/64b";
  else                               Var += "/32b";
  if (optimized == "Yes") Var += "/opt";
  else                    Var += "/deb";
  if (LibTag == ".DEV2")  {Var += "/TFGXX";}
  else                    { Var += "/"; Var += LibTag;}
#if 0
  cout << "prodyear = " << prodyear.Data()
       << "\tDataset = " << Dataset.Data()
       << "\tLibTag = " << LibTag.Data()
       << "\toptimized = " << optimized.Data()
       << "\tpath = " << path.Data()
       << "\tgcc_version = " << gcc_version.Data()
       << "\t => Var = " << Var.Data() << endl; 
#endif
  return Var;
}
//________________________________________________________________________________
void SetBinName(TProfile *prof, List &datasets) {
  TAxis *x = prof->GetXaxis();
  Int_t bin = 0;
  for (auto t : datasets) {
    x->SetBinLabel(++bin,t);
  }
}
//________________________________________________________________________________
void sqlJobStatus(Int_t d1 = 20200501, Int_t d2 = 0) {
  TDatime t1(d1,0);
  TDatime t2;
  if (d2 > 0) t2.Set(d2,0); 
  t1.Print(); t2.Print();
  cout << "t1\t" << t1.AsSQLString() << endl;
  cout << "t2\t" << t2.AsSQLString() << endl;
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
  TFile *fOut = new TFile("JobStatus.root","recreate");
  TString A;
  {
    /*
      SELECT DISTINCT prodyear from JobStatus order by prodyear;       2000, ..., 2020
      SELECT DISTINCT LibTag  from JobStatus order by LibTag;          adev, ..., TFG20c 
      SELECT DISTINCT rootLevel  from JobStatus order by rootLevel;
      SELECT DISTINCT chainOpt  from JobStatus order by chainOpt;
      SELECT DISTINCT jobStatus  from JobStatus order by jobStatus;    Done, Abort, ...,, StFATAL
      SELECT DISTINCT Dataset  from JobStatus order by Dataset;         AuAu200_production_mid_2014_64bit_opt
      SELECT DISTINCT gcc_version  from JobStatus order by gcc_version; .sl73_gcc485 .sl73_x8664_gcc485
      SELECT DISTINCT tracker  from JobStatus order by tracker;         daq_sl302.ittf_opt
      SELECT DISTINCT optimized  from JobStatus order by optimized;
    */
    TString sql = Form("SELECT DISTINCT prodyear,Dataset,LibTag,optimized,path,gcc_version,tracker  from JobStatus where LibTag != \"n/a\" and  Dataset != \"\" and jobStatus = \"Done\" and createTime >= \"%s\" order by prodyear;",
		       t1.AsSQLString());
    cout << sql << endl;
    res = db->Query(sql);
    
    int nrows = res->GetRowCount();
    if (! nrows) return;
    int nfields = res->GetFieldCount();
    printf("\nGot %d rows in result\n", nrows);
    List datasets;
    
    for (int i = 0; i < nrows; i++) {
      row = res->Next();
      TString Var = FormVariable(row->GetField(0),row->GetField(1),row->GetField(2),row->GetField(3),row->GetField(4),row->GetField(5),row->GetField(6));
      if (Var == "") continue;
      datasets.push_back(Var);
    }
    datasets.sort();
    datasets.unique();
    Int_t i = 0;
    for (TString x : datasets) {
      cout << i++ << "\t" << x.Data() << endl;
    }
    delete res;
    Int_t nxbin = datasets.size();
    for (Int_t f = 0; f < 40; f++) {
      if (! Fields[f].plot) continue;
      Fields[f].prof = new TProfile(Fields[f].Name,Form("%s versus dataset and library",Fields[f].Name),nxbin,0.5,nxbin+0.5,"s");
      SetBinName(Fields[f].prof,datasets);
    }

  }
  JobStatus job;
  //  const char *sql = "select * from JobStatus limit 3";
  const char *sql = Form("SELECT * from JobStatus WHERE  LibTag != \"n/a\" and  Dataset != \"\" and createTime >= \"%s\" and NoEventDone > 0 ", t1.AsSQLString()) ;
  
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
#if 0
    for (int j = 0; j < nfields; j++) {
      printf("%5i %20s : %120s\n", j,  res->GetFieldName(j), row->GetField(j));
    }
#endif
    job.jobID = row->GetField(0);
    job.LibLevel = row->GetField(1);
    
    job.jobID = row->GetField(0);
    job.LibLevel = row->GetField(1);
    job.LibTag = row->GetField(2);
    job.rootLevel = row->GetField(3);
    job.path = row->GetField(4);
    job.prodyear = row->GetField(5); // Int_t prodyear = A.Atoi();
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
    job.Dataset = row->GetField(42);
    job.gcc_version = row->GetField(44);
    job.tracker = row->GetField(45);
    job.optimized = row->GetField(46);
    TString Var =  FormVariable(job.prodyear, job.Dataset, job.LibTag, job.optimized, job.path,job.gcc_version,job.tracker);
    if (Var == "") continue;
    for (int j = 0; j < nfields; j++) {
      if (! Fields[j].prof) continue;
      A = row->GetField(j);
      if (Fields[j].type == 1) {
	Int_t v = A.Atoi();
	Fields[j].prof->Fill(Var,v);
      } else if (Fields[j].type == 2) {
	Double_t v = A.Atof(); 
	Fields[j].prof->Fill(Var,v);
      }
    }
    delete row;
  }
#endif   
  delete res;
  delete db;
  fOut->Write();
}
//________________________________________________________________________________
void SetYear(TProfile *prof, const Char_t *Year="2020") {
  if (! prof) return;
  TAxis *xax = prof->GetXaxis();
  Int_t nx = xax->GetNbins();
  Int_t ib = 999999;
  Int_t ie = 0;
  for (Int_t i = 1; i <= nx; i++) {
    if (TString(xax->GetBinLabel(i)).BeginsWith(Year)) {
      ib = i;
      break;
    }
  }
  for (Int_t i = nx; i >= 1; i--) {
    if (TString(xax->GetBinLabel(i)).BeginsWith(Year)) {
      ie = i;
      break;
    }
  }
  if (ie > ib) xax->SetRange(ib,ie);
}
//________________________________________________________________________________
void PlotYear(const Char_t *Year = "2020") {
  gStyle->SetOptStat(0);
  gStyle->SetMarkerStyle(20);
  TCanvas *c1 = new TCanvas(Form("c1%s",Year),Form("Usable Events %s",Year),20,20,2000,500);
  c1->SetBottomMargin(0.25);
  TProfile *percent_of_usable_evt = (TProfile *) gDirectory->Get("percent_of_usable_evt");
  if (! percent_of_usable_evt) return;
  SetYear( percent_of_usable_evt, Year);
  percent_of_usable_evt->Draw();
  // 
  TCanvas *c2 = new TCanvas(Form("c2%s",Year),Form("Memory Usage %s",Year),20,520,2000,500);
  c2->SetBottomMargin(0.25);
  TProfile *memUsageL = (TProfile *) gDirectory->Get("memUsageL");
  if (! memUsageL) return;
  SetYear( memUsageL, Year);
  memUsageL->SetMinimum(0);
  memUsageL->Draw();
  TProfile *memUsageF = (TProfile *) gDirectory->Get("memUsageF");
  if (! memUsageF) return;
  memUsageF->SetMarkerColor(2);
  memUsageF->Draw("same");
  TLegend *l2 = new TLegend(0.9,0.9,1.0,1.0);
  l2->AddEntry(memUsageF,"Memory 1st event");
  l2->AddEntry(memUsageL,"Memory last event");
  l2->Draw();
  // 
  TCanvas *c3 = new TCanvas(Form("c3%s",Year),Form("CPU Usage %s",Year),20,1020,2000,500);
  c3->SetBottomMargin(0.25);
  TProfile *RealTime_per_evt = (TProfile *) gDirectory->Get("RealTime_per_evt");
  if (! RealTime_per_evt) return;
  SetYear( RealTime_per_evt, Year);
  RealTime_per_evt->SetMinimum(0);
  RealTime_per_evt->Draw();
  TProfile *CPU_per_evt_sec = (TProfile *) gDirectory->Get("CPU_per_evt_sec");
  if (! CPU_per_evt_sec) return;
  CPU_per_evt_sec->SetMarkerColor(2);
  CPU_per_evt_sec->Draw("same");
  TLegend *l3 = new TLegend(0.9,0.9,1.0,1.0);
  l3->AddEntry(CPU_per_evt_sec,"CPU per event");
  l3->AddEntry(RealTime_per_evt,"Real time");
  l3->Draw();
  // 
  TCanvas *c4 = new TCanvas(Form("c4%s",Year),Form("CPU Usage %s",Year),20,1520,2000,500);
  c4->SetBottomMargin(0.25);
  TLegend *l4 = new TLegend(0.8,0.9,1.0,1.0);
  TProfile *avg_no_tracks = (TProfile *) gDirectory->Get("avg_no_tracks");
  if (! avg_no_tracks) return;
  SetYear( avg_no_tracks, Year);
  avg_no_tracks->SetMinimum(0);
  avg_no_tracks->SetMarkerColor(1);
  
  avg_no_tracks->Draw();
  l4->AddEntry(avg_no_tracks,"Total no. tracks");
  TProfile *avg_no_tracksnfit15 = (TProfile *) gDirectory->Get("avg_no_tracksnfit15");
  if (! avg_no_tracksnfit15) return;
  avg_no_tracksnfit15->SetMarkerColor(2);
  avg_no_tracksnfit15->Draw("same");
  l4->AddEntry(avg_no_tracksnfit15,"tracks with no. fit points >= 15");
  TProfile *avg_no_primaryT_1vtx = (TProfile *) gDirectory->Get("avg_no_primaryT_1vtx");
  if (! avg_no_primaryT_1vtx) return;
  avg_no_primaryT_1vtx->SetMarkerColor(3);
  avg_no_primaryT_1vtx->Draw("same");
  l4->AddEntry(avg_no_primaryT_1vtx,"Primary tracks");
  TProfile *avg_no_primaryTnfit15_1vtx = (TProfile *) gDirectory->Get("avg_no_primaryTnfit15_1vtx");
  if (! avg_no_primaryTnfit15_1vtx) return;
  avg_no_primaryTnfit15_1vtx->SetMarkerColor(4);
  avg_no_primaryTnfit15_1vtx->Draw("same");
  l4->AddEntry(avg_no_primaryTnfit15_1vtx,"Primary tracks with no. fit points >= 15");
  l4->Draw();
}
