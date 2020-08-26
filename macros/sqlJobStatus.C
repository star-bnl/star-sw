#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
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
#include "TDirectory.h"
#include "TGraph.h"
#include "TMultiGraph.h"
#include "TStyle.h"
#include "TFrame.h"
#include "TROOT.h"
#include "Ask.h"
#endif
#define PrPP(PARAM)  if (PARAM ## V == "") PARAM ## V = PARAM; if (PARAM ## V != PARAM) {cout << #PARAM << " = |" << PARAM ## V.Data() << "|\t|" << PARAM.Data() << "|" << endl; }
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
void DrawPng(TCanvas *c) {
  static Int_t nPng = 0;
  TString pngName("");
  if (c) {
    c->Update(); pngName = c->GetTitle();
    pngName.ReplaceAll(" ","_");
    pngName.ReplaceAll("(","_");
    pngName.ReplaceAll(")","_");
    pngName.ReplaceAll("{","_");
    pngName.ReplaceAll("}","_");
    pngName.ReplaceAll("<","lt");
    pngName.ReplaceAll(">","gt");
    pngName.ReplaceAll(".","_");
    pngName.ReplaceAll("/","_");
    pngName.ReplaceAll("^","_");
    pngName.ReplaceAll("__","_");
    pngName.ReplaceAll("__","_");
    pngName += ".png"; 
    TVirtualX::Instance()->WritePixmap(c->GetCanvasID(),-1,-1,(Char_t *)pngName.Data());
    nPng++;
    cout << "Draw #\t" << nPng << "\t" << pngName << endl;
  }
}
//________________________________________________________________________________
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
TString FormVariable(TString prodyearV, TString datasetV, TString LibTagV, TString optimizedV, TString path, TString gcc_versionV, TString trackerV) {
  // path TFG20g/sl73_x8664_gcc485_opt/daq_sl302.stica/year_2019/production_4p59GeV_fixedTarget_2019/st_physics_20181004_raw_2000007.log
  //                /star/rcf/test/dev/daq_sl302.ittf/Fri/year_2020/production_11p5GeV_2020
  /*
|     2020 | production_11p5GeV_2020     | .DEV2  |           | sl73_x8664_gcc485_opt/daq_sl302.ittf/year_2020/production_11p5GeV_2020                                                   |                    | daq_sl302.ittf      | 2020-08-04 14:33:00 |
|     2020 | production_11p5GeV_2020     | .DEV2  |           | sl73_x8664_gcc485_opt/daq_sl302.stica/year_2020/production_11p5GeV_2020                                                  |                    | daq_sl302.stica     | 2020-08-04 14:22:44 |
|     2020 | NULL                        | .DEV2  | NULL      | /gpfs01/star/subsys-tpc/fisyak/nightlies/.DEV2/sl73_x8664_gcc485_debug/daq_sl302.stica/year_2020/production_11p5GeV_2020 | NULL               | NULL                | 2020-08-20 19:41:02 |
|     2020 | NULL                        | .DEV2  | NULL      | /gpfs01/star/subsys-tpc/fisyak/nightlies/.DEV2/sl73_x8664_gcc485_opt/daq_sl302.ittf/year_2020/production_11p5GeV_2020    | NULL               | NULL                | 2020-08-20 18:03:31 |
|     2020 | NULL                        | .DEV2  | NULL      | /gpfs01/star/subsys-tpc/fisyak/nightlies/.DEV2/sl73_x8664_gcc485_opt/daq_sl302.stica/year_2020/production_11p5GeV_2020   | NULL               | NULL                | 2020-08-20 18:03:59 |
|     2020 | NULL                        | DEV    | NULL      | /star/rcf/test/dev/daq_sl302.ittf/Fri/year_2020/production_11p5GeV_2020                                                  | NULL               | NULL                | 2020-08-21 10:26:00 |
|     2020 | NULL                        | DEV    | NULL      | /star/rcf/test/dev/daq_sl302.stica/Fri/year_2020/production_11p5GeV_2020                                                 | NULL               | NULL                | 2020-08-21 11:24:00 |
   */
  TString empty;
  if (path.EndsWith("/year_2014/AuAu200_production_low.nohftge")) return empty;
  if (path.EndsWith("/year_2014/AuAu200_production_low.nohftg")) return empty;
  if (path.EndsWith("/year_2014/AuAu200_production_low.nohft_")) return empty;
  TString prodyear, dataset, LibTag, optimized, gcc_version, tracker;
  TObjArray *obj = path.Tokenize("/");
  Int_t nParsed = obj->GetEntries();
  Int_t k = nParsed - 1;
  optimized =  "No";
  if (path.Contains("test/dev")) {
    LibTag = "DEV";
    if (LibTagV == "n/a") LibTagV = LibTag;
    dataset = ((TObjString *) obj->At(k))->GetName();
    k--; prodyear = ((TObjString *) obj->At(k))->GetName(); prodyear.ReplaceAll("year_","");  
    k--;
    k--; tracker = ((TObjString *) obj->At(k))->GetName();
    if (tracker.Contains("_opt")) {optimized = "Yes";/* tracker.ReplaceAll("_opt","");*/}
    if (gcc_versionV != "") gcc_version  = gcc_versionV;
    else                    gcc_version = ".sl73_gcc485";
    if (dataset.Contains("_64bit") && dataset == datasetV) { 
      if (gcc_version != gcc_versionV) {
	gcc_versionV = ".sl73_x8664_gcc485";
	gcc_version = gcc_versionV; 
      }
    }
  } else {
    dataset = ((TObjString *) obj->At(k))->GetName();
    k--; prodyear = ((TObjString *) obj->At(k))->GetName(); prodyear.ReplaceAll("year_","");
    k--; tracker = ((TObjString *) obj->At(k))->GetName();
    k--; {gcc_version = "."; gcc_version += ((TObjString *) obj->At(k))->GetName();}
    if (gcc_version.Contains("_opt")) {optimized = "Yes"; gcc_version.ReplaceAll("_opt","");}
    if (gcc_version.Contains("_debug")) {optimized =  "No"; gcc_version.ReplaceAll("_debug","");}
    if (gcc_version.Contains("_deb")) {optimized =  "No"; gcc_version.ReplaceAll("_deb","");}
    if (k > 0) {k--; LibTag = ((TObjString *) obj->At(k))->GetName(); LibTag.ReplaceAll(".HOLD","");}
    else       {     LibTag = ".DEV2";}
  }
  //  obj->SetOwner(kFALSE);
  delete obj;
  if (dataset.Contains("_64bit")) {
    dataset.ReplaceAll("_64bit","");  datasetV = dataset;
  }
  dataset.ReplaceAll("_opt","");
  datasetV.ReplaceAll("_opt","");
  prodyear.ReplaceAll(".AgML","");
  PrPP(prodyear);
  PrPP(dataset);
  PrPP(LibTag);
  //  PrPP(optimized);
  PrPP(gcc_version);
  PrPP(tracker);
  if (! (prodyearV == prodyear && datasetV == dataset && LibTagV == LibTag && (gcc_versionV != "" &&  gcc_versionV == gcc_version) && trackerV == tracker)) {//  && optimizedV == optimized
    cout << "path = " << path.Data() << endl;
    assert(0);
  }
  Int_t _debug = 0;
  TString Var(prodyear(2,2));
  TString Dataset(dataset);
  //  if (Dataset == "auau11_production") _debug = 1;
  //  if (tracker.Contains("stihr") )     _debug = 1;
  if (tracker.BeginsWith("daq")) {Var += "RC";}
  else                           {Var += "MC";}
  Var += "_"; // "/";
  Dataset.ReplaceAll("production_","");
  Dataset.ReplaceAll("_production","");
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
  if (tracker.Contains("stica"))      {Var += "/CA";}
  else if (tracker.Contains("stihr")) {Var += "/HR";}
  else                                {Var += "/sti";}
  if (path.Contains("AgML"))          {Var += "/AgML";}
  if (LibTag.Contains(".DEV2") || LibTag.Contains("TFG"))  {Var += "/TFG";}
  else                    { Var += "/DEV";/* Var += LibTag; */}
  if (gcc_version.Contains("x8664")) Var += "/64b";
  else                               Var += "/32b";
  if      (gcc_version.Contains("gcc4"))  Var += "4";
  else if (gcc_version.Contains("gcc5"))  Var += "5"; 
  else if (gcc_version.Contains("gcc6"))  Var += "6"; 
  else if (gcc_version.Contains("gcc7"))  Var += "7"; 
  else if (gcc_version.Contains("gcc8"))  Var += "8"; 
  else if (gcc_version.Contains("gcc9"))  Var += "9"; 
  else if (gcc_version.Contains("gcc10"))  Var += "10"; 
  if      (optimized == "Yes") Var += "-O"; // "/opt";
  else if (optimized == "No")  Var += "-g"; // "/deb";
  else {
    if (path.Contains("_opt")) Var += "-O"; // "/opt";
    else                       Var += "-g"; // /deb";
  }
  Var.ReplaceAll("2007","");
  Var.ReplaceAll("2008","");
  Var.ReplaceAll("2009","");
  //  if (Var.Contains("_We")) {_debug = 1;}
  if (Var.Contains("nohft_")) {_debug = 1;}
  if (Var.Contains("nohftg") && ! Var.Contains("nohftgeo") ) {_debug = 1;}
  if (Var.Contains("64bit")) {_debug = 1;}
  if (_debug) {
    cout << "Var = " << Var.Data() 
	 << "\tprodyear = " << prodyear.Data()
	 << "\tdataset = " << dataset.Data()
	 << "\tLibTag = " << LibTag.Data()
	 << "\toptimized = " << optimized.Data()
	 << "\tpath = " << path.Data()
	 << "\tgcc_version = " << gcc_version.Data()
	 << "\ttracker = " << tracker.Data() << endl;
    static Int_t iBreak = 0;
    iBreak++;
  }
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
  TDatime t0(20000101,0);
  UInt_t  u0 = t0.Convert();
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
    //    TString sql = Form("SELECT DISTINCT prodyear,Dataset,LibTag,optimized,path,gcc_version,tracker  from JobStatus where LibTag != \"n/a\" and  Dataset != \"\" and jobStatus = \"Done\" and createTime >= \"%s\" order by prodyear;",
    //    TString sql = Form("SELECT prodyear,Dataset,LibTag,optimized,path,gcc_version,tracker  from JobStatus where LibTag != \"n/a\" and  Dataset != \"\" and jobStatus = \"Done\" and createTime >= \"%s\" order by prodyear;",
    TString sql = Form("SELECT prodyear,Dataset,LibTag,optimized,path,gcc_version,tracker  from JobStatus where createTime >= \"%s\" order by prodyear;",
		       t1.AsSQLString());
    cout << sql << endl;
    res = db->Query(sql);
    if (! res) return;
    int nrows = res->GetRowCount();
    if (! nrows) return;
    int nfields = res->GetFieldCount();
    printf("\nGot %d rows in result\n", nrows);
    List binsL;
    List dataS;
    for (int i = 0; i < nrows; i++) {
      row = res->Next();
      TString Var = FormVariable(row->GetField(0),row->GetField(1),row->GetField(2),row->GetField(3),row->GetField(4),row->GetField(5),row->GetField(6));
      if (Var == "") continue;
      binsL.push_back(Var);
      Int_t index = Var.Index("/");
      TString S(Var(0,index));
      dataS.push_back(S);
    }
    binsL.sort();
    binsL.unique();
    Int_t i = 0;
    cout << "List of bins" << endl;
    for (TString x : binsL) {
      cout << i++ << "\t" << x.Data() << endl;
    }
    cout << "================================================================================" << endl;
    dataS.sort();
    dataS.unique();
    cout << "List of sets" << endl;
    i = 0;
    for (TString x : dataS) {
      cout << i++ << "\t" << x.Data() << endl;
    }
    cout << "================================================================================" << endl;
    delete res;
    Int_t nxbin = binsL.size();
    for (Int_t f = 0; f < 40; f++) {
      if (! Fields[f].plot) continue;
      Fields[f].prof = new TProfile(Form("%sF",Fields[f].Name),Form("%s versus dataset and library",Fields[f].Name),nxbin,0.5,nxbin+0.5,"s");
      SetBinName(Fields[f].prof,binsL);
    }

  }
  JobStatus job;
  //  const char *sql = "select * from JobStatus limit 3";
  TString sql = Form("SELECT * from JobStatus WHERE  LibTag != \"n/a\" and  Dataset != \"\" and createTime >= \"%s\" and NoEventDone > 0 ", t1.AsSQLString()) ;
  sql += " and Dataset != \"NULL\" and tracker != \"NULL\" and tracker != \"\";";
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
    TDatime t(job.createTime);
    UInt_t u = t.Convert() - u0;
    TString Var =  FormVariable(job.prodyear, job.Dataset, job.LibTag, job.optimized, job.path,job.gcc_version,job.tracker);
    if (Var == "") continue;
    for (int j = 0; j < nfields; j++) {
      if (! Fields[j].prof) continue;
      TDirectory *dir = fOut->GetDirectory(Var);
      if (! dir) dir = fOut->mkdir(Var);
      TGraph *graph = (TGraph *) dir->FindObject(Fields[j].Name);
      Int_t np = 0;
      if (! graph) {
	graph = new TGraph;
	graph->SetName(Fields[j].Name);
	graph->SetTitle(Form("%s versus time",Fields[j].Name));
	dir->Append(graph);
      } else {
	np = graph->GetN();
      }
      A = row->GetField(j);
      if (Fields[j].type == 1) {
	Int_t v = A.Atoi();
	Int_t bin = Fields[j].prof->Fill(Var,v);
	assert(bin > 0 && TString(Fields[j].prof->GetXaxis()->GetBinLabel(bin)) != TString(dir->GetName()));
	graph->SetPoint(np,u,v);
      } else if (Fields[j].type == 2) {
	Double_t v = A.Atof(); 
	Int_t bin = Fields[j].prof->Fill(Var,v);
	assert(bin > 0 && TString(Fields[j].prof->GetXaxis()->GetBinLabel(bin)) != TString(dir->GetName()));
	graph->SetPoint(np,u,v);
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
void DrawMultiGraphs() {
  TDatime t0(20000101,0);
  UInt_t  u0 = t0.Convert();
  gStyle->SetTimeOffset(u0);
  TCanvas *c5 = new TCanvas("c5","c5", 10, 10, 1600,400);
  TProfile *percent_of_usable_evt = (TProfile *) gDirectory->Get("percent_of_usable_evtF");
  if (! percent_of_usable_evt) return;
  TAxis *xax = percent_of_usable_evt->GetXaxis();
  Int_t binF = xax->GetFirst();
  Int_t binL = xax->GetLast();
  Int_t color = 0;
  TMultiGraph *avg_no_tracksnfit15 = new TMultiGraph();
  TLegend *l = new TLegend(0.2,0.25,0.35,0.45);
  Double_t x1 = 1e9;
  Double_t x2 = -1e9;
  Double_t y1 = 1e9;
  Double_t y2 = -1e9;
  for (Int_t bin = binF; bin <= binL; bin++) {
    TString path("/"); path += xax->GetBinLabel(bin); path += "/avg_no_tracksnfit15";
    TGraph  *gr = (TGraph *) gDirectory->Get(path);
    if (! gr) continue;
    Int_t N = gr->GetN();
    Double_t *x = gr->GetX();
    Double_t *y = gr->GetY();
    for (Int_t i = 0; i < N; i++) {
      if (x[i] < x1) x1 = x[i];
      if (x[i] > x2) x2 = x[i];
      if (y[i] < y1) y1 = y[i];
      if (y[i] > y2) y2 = y[i];
    }
    gr->SetMarkerColor(++color);
    gr->SetLineStyle(1);
    gr->SetLineColor(color);
    avg_no_tracksnfit15->Add(gr);
    l->AddEntry(gr,xax->GetBinLabel(bin),"lp");
  }
  cout << "x1 = " << x1 << "\tx2 = " << x2 << "\ty1 = " << y1 << "\ty2 = " << y2 <<endl;
  Double_t dx = x2 - x1;
  Double_t dy = y2 - y1;
  x1 -= 0.05*dx;
  x2 += 0.05*dx;
  y1 -= 0.05*dy;
  y2 += 0.05*dy;
  TH1F *fr = c5->DrawFrame(x1,y1,x2,y2);
  TAxis *xaxm = fr->GetXaxis();
  xaxm->SetTimeDisplay(1);
  xaxm->SetTimeFormat("%d/%m/%y"); //%F2000-01-01 00:00:00");
  fr->Draw();
  avg_no_tracksnfit15->Draw("lp");
  l->Draw();
  c5->Update();
}
//________________________________________________________________________________
void Plot(const Char_t *Year = "20RC_11p5GeV") {
  TDatime t0(20000101,0);
  UInt_t  u0 = t0.Convert();
  gStyle->SetTimeOffset(u0);
  gStyle->SetOptStat(0);
  gStyle->SetMarkerStyle(20);
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) {c1->Clear(); c1->SetTitle(Form("Usable Events %s",Year));}
  else    {c1 = new TCanvas("c1",Form("Usable Events %s",Year),20,20,1000,500);}
  c1->SetBottomMargin(0.25);
  c1->cd(1)->SetRightMargin(0.20);
  TProfile *percent_of_usable_evt = (TProfile *) gDirectory->Get("percent_of_usable_evtF");
  if (! percent_of_usable_evt) return;
  SetYear( percent_of_usable_evt, Year);
  percent_of_usable_evt->Draw();
  // 
  TCanvas *c2 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c2");
  if (c2) {c2->Clear(); c2->SetTitle(Year);}
  else    {c2 = new TCanvas("c2",Year,20,520,1000,3*500);}
  c2->Divide(1,3);
  TVirtualPad *pad3 = c2->cd(3);
  pad3->SetBottomMargin(0.30);
  pad3->SetRightMargin(0.20);
  TProfile *memUsageL = (TProfile *) gDirectory->Get("memUsageLF");
  if (! memUsageL) return;
  SetYear( memUsageL, Year);
  memUsageL->SetMinimum(0);
  memUsageL->Draw();
  TProfile *memUsageF = (TProfile *) gDirectory->Get("memUsageFF");
  if (! memUsageF) return;
  memUsageF->SetMarkerColor(2);
  memUsageF->Draw("same");
  TLegend *l2 = new TLegend(0.8,0.9,0.9,1.0);
  l2->AddEntry(memUsageF,"Memory 1st event");
  l2->AddEntry(memUsageL,"Memory last event");
  l2->Draw();
  // 
  //  TCanvas *c3 = new TCanvas(Form("c3%s",Year),Form("CPU Usage %s",Year),20,1020,1000,500);
  //  c3->SetBottomMargin(0.25);
 TVirtualPad *pad1 =  c2->cd(1);
  pad1->SetBottomMargin(0.01);
  pad1->SetRightMargin(0.20);
  TProfile *RealTime_per_evt = (TProfile *) gDirectory->Get("RealTime_per_evtF");
  if (! RealTime_per_evt) return;
  SetYear( RealTime_per_evt, Year);
  RealTime_per_evt->SetMinimum(0);
  RealTime_per_evt->Draw();
  TProfile *CPU_per_evt_sec = (TProfile *) gDirectory->Get("CPU_per_evt_secF");
  if (! CPU_per_evt_sec) return;
  CPU_per_evt_sec->SetMarkerColor(2);
  CPU_per_evt_sec->Draw("same");
  TLegend *l3 = new TLegend(0.8,0.9,0.9,1.0);
  l3->AddEntry(CPU_per_evt_sec,"CPU per event");
  l3->AddEntry(RealTime_per_evt,"Real time");
  l3->Draw();
  TVirtualPad *pad2 = c2->cd(2);
  pad2->SetBottomMargin(0.01);
  pad2->SetRightMargin(0.20);
  TLegend *l4 = new TLegend(0.7,0.9,0.9,1.0);
  TProfile *avg_no_tracks = (TProfile *) gDirectory->Get("avg_no_tracksF");
  if (! avg_no_tracks) return;
  SetYear( avg_no_tracks, Year);
  avg_no_tracks->SetMinimum(0);
  avg_no_tracks->SetMarkerColor(1);
  
  avg_no_tracks->Draw();
  l4->AddEntry(avg_no_tracks,"Total no. tracks");
  TProfile *avg_no_tracksnfit15 = (TProfile *) gDirectory->Get("avg_no_tracksnfit15F");
  if (! avg_no_tracksnfit15) return;
  avg_no_tracksnfit15->SetMarkerColor(2);
  avg_no_tracksnfit15->Draw("same");
  l4->AddEntry(avg_no_tracksnfit15,"tracks with no. fit points >= 15");
  TProfile *avg_no_primaryT_1vtx = (TProfile *) gDirectory->Get("avg_no_primaryT_1vtxF");
  if (! avg_no_primaryT_1vtx) return;
  avg_no_primaryT_1vtx->SetMarkerColor(3);
  avg_no_primaryT_1vtx->Draw("same");
  l4->AddEntry(avg_no_primaryT_1vtx,"Primary tracks");
  TProfile *avg_no_primaryTnfit15_1vtx = (TProfile *) gDirectory->Get("avg_no_primaryTnfit15_1vtxF");
  if (! avg_no_primaryTnfit15_1vtx) return;
  avg_no_primaryTnfit15_1vtx->SetMarkerColor(4);
  avg_no_primaryTnfit15_1vtx->Draw("same");
  l4->AddEntry(avg_no_primaryTnfit15_1vtx,"Primary tracks with no. fit points >= 15");
  l4->Draw();
  //  avg_no_primaryT->GetHistogram()->GetXaxis()->SetTimeFormat("%d/%m/%y%F2000-01-01 00:00:00");
  DrawPng(c2);

}
//________________________________________________________________________________
void Plots(Int_t i1 = 0, Int_t i2 = -1) {
  const Char_t *Sets[] = {
    "00MC_hc_standard/",  "00RC_central/",  "00RC_minbias/",
    "01MC_hc_standard/",  "01MC_pp_minbias/",  "01RC_central/",  "01RC_minbias/",  "01RC_ppMinBias/",
    "03MC_dau_minbias/",  "03RC_dAuMinBias/",  "03RC_ppMinBias/",
    "04MC_auau_central/",  "04MC_auau_minbias/",  "04RC_AuAuMinBias/",  "04RC_AuAu_prodHigh/",  "04RC_AuAu_prodLow/",  "04RC_prodPP/",
    "05MC_cucu200_minbias/",  "05MC_cucu62_minbias/",  
    "05RC_CuCu200_HighTower/",  "05RC_CuCu200_MinBias/",  "05RC_CuCu200_embedTpc/",  "05RC_CuCu200_embedTpcSvtSsd/",
    "05RC_CuCu22_MinBias/",  "05RC_CuCu62_MinBias/", "05RC_ppProduction/",
    "06MC_pp200_minbias/",  "06RC_ppProdLong/",  "06RC_ppProdTrans/",
    "07MC_auau200_central/",  "07RC_Production/",  "07RC_ProductionMinBias/",  "07RC_auau200_embedTpcSvtSsd/",
    "08MC_dau200_minbias/",  "08MC_pp200_minbias/",  "08RC_dAu/",  "08RC_ppProduction/",
    "09MC_pp200_minbias/",  "09MC_pp500_minbias/",  "09RC_pp200_embed/",  "09RC_production/",  "09RC_production_500GeV/",
    "10MC_auau11_minbias/",  "10MC_auau200_minbias/",  "10MC_auau39_minbias/",  "10MC_auau62_minbias/",  "10MC_auau7_minbias/",
    "10RC_auau11/",  "10RC_auau11_embed/",  "10RC_auau200/",  "10RC_auau200_embed/",  "10RC_auau39/",  "10RC_auau39_embed/",
    "10RC_auau62/",  "10RC_auau7/",  "10RC_auau7_embed/",
    "11MC_auau200_central/",  "11MC_pp500_minbias/",  "11MC_pp500_pileup/",  "11RC_AuAu19/",  "11RC_AuAu200/",
    "11RC_AuAu200_embed/",  "11RC_AuAu27/",  "11RC_pp500/",  "11RC_pp500_embed/",
    "12MC_CuAu200_minbias/",  "12MC_UU200_minbias/",  "12MC_pp200_minbias/",  "12MC_pp500_minbias/",
    "12RC_UU/",  "12RC_UU193_embed/",  "12RC_cuAu/",  "12RC_pp200/",  "12RC_pp200_embed/",  "12RC_pp500/",
    "13MC_pp500/",  "13MC_pp500_We/",  "13MC_pp500_minbias/",  "13RC_pp500/",
    "14MC_AuAu200_low/",  "14MC_He3Au200_minbias/",  "14MC_auau200_minbias/",  "14RC_15GeV/",
    "14RC_AuAu200/",  "14RC_AuAu200_low/",  "14RC_AuAu200_low.nohft/",  "14RC_AuAu200_low.nohftgeo/",
    "14RC_AuAu200_low.nohftgeo_/",  "14RC_AuAu200_mid/",  "14RC_AuHe3/",
    "15MC_pp200_minbias/",  "15MC_pp200long/",  "15RC_pAl200/",  "15RC_pAu200/",
    "15RC_pp200long/",  "15RC_pp200long.nohft/",
    "16MC_AuAu200/",  "16MC_dAu200_minbias/",  "16RC_AuAu200/",  "16RC_dAu20/",  "16RC_dAu200/",  "16RC_dAu39/",  "16RC_dAu62/",
    "17RC_54GeV/",  "17RC_AuAu54/",  "17RC_pp500/",  
    "18RC_27GeV/",  "18RC_isobar/",
    "19RC_14p5GeV/",  "19RC_19GeV/",  "19RC_31GeV_FXT/",  "19RC_4p59GeV_FXT/",  "19RC_7p7GeV/",  "19RC_AuAu200/",
    "20RC_11p5GeV/",    "20RC_19p5GeV_FXT/",  "20RC_31p2GeV_FXT/",  "20RC_7p3GeV_FXT/",  "20RC_9p2GeV/"};
  Int_t Nsets = sizeof(Sets)/sizeof(const Char_t *);
  if (i2 < i1) i2 = Nsets - 1;
  for (Int_t i = i1; i <= i2; i++) {
    Plot(Sets[i]);
    if (Ask()) return;
  }
}
