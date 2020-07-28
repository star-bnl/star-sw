#if !defined(__CINT__) || defined(__MAKECINT__)
#include <vector>
#include "Riostream.h"
#include "TSQLServer.h"
#include "TSQLResult.h"
#include "TSQLRow.h"
#include "TNtuple.h"
#include "TFile.h"
#include "TString.h"
#include "TStopwatch.h"
#include "TApplication.h"
#include "TProfile.h"
#include "TGClient.h"
#include "TGButton.h"
#include "TGListBox.h"
#include "TList.h"
#endif
using namespace std;
//
// Author: Ilka Antcheva   1/12/2006

// This macro gives an example of how to create a list box
// and how to set and use its multiple selection feature.
// To run it do either:
// .x listBox.C
// .x listBox.C++
typedef vector<TString> VecList;

class MyMainFrame : public TGMainFrame {
  
private:
  TGListBox           *fListBox;
  TGCheckButton       *fCheckMulti;
  TList               *fSelected;   
  
public:
  MyMainFrame(const TGWindow *p, UInt_t w, UInt_t h, VecList Vlist);
  virtual ~MyMainFrame();
  void DoExit();
  void DoSelect();
  void HandleButtons();
  void PrintSelected();
  
  ClassDef(MyMainFrame, 0)
};

void MyMainFrame::DoSelect()
{
  Printf("Slot DoSelect()");
}

void MyMainFrame::DoExit()
{
  Printf("Slot DoExit()");
  gApplication->Terminate(0);
}

MyMainFrame::MyMainFrame(const TGWindow *p, UInt_t w, UInt_t h, VecList Vlist) :
  TGMainFrame(p, w, h)
{
  // Create main frame
  
  fListBox = new TGListBox(this, 89);
  fSelected = new TList;
  Int_t i = 0;
  for (auto tmp : Vlist) {
    fListBox->AddEntry(tmp, i++);
  }
  fListBox->Resize(100,150);
  AddFrame(fListBox, new TGLayoutHints(kLHintsTop | kLHintsLeft |
				       kLHintsExpandX | kLHintsExpandY, 
				       5, 5, 5, 5));
  
  fCheckMulti = new TGCheckButton(this, "&Mutliple selection", 10);
  AddFrame(fCheckMulti, new TGLayoutHints(kLHintsTop | kLHintsLeft,
					  5, 5, 5, 5));
  fCheckMulti->Connect("Clicked()", "MyMainFrame", this, "HandleButtons()"); 
  // Create a horizontal frame containing button(s)
  TGHorizontalFrame *hframe = new TGHorizontalFrame(this, 150, 20, kFixedWidth);
  TGTextButton *show = new TGTextButton(hframe, "&Show");
  show->SetToolTipText("Click here to print the selection you made");
  show->Connect("Pressed()", "MyMainFrame", this, "PrintSelected()");
  hframe->AddFrame(show, new TGLayoutHints(kLHintsExpandX, 5, 5, 3, 4));
  TGTextButton *exit = new TGTextButton(hframe, "&Exit ");
  exit->Connect("Pressed()", "MyMainFrame", this, "DoExit()");
  hframe->AddFrame(exit, new TGLayoutHints(kLHintsExpandX, 5, 5, 3, 4));
  AddFrame(hframe, new TGLayoutHints(kLHintsExpandX, 2, 2, 5, 1));
  
  // Set a name to the main frame   
  SetWindowName("List Box");
  MapSubwindows();
  
  // Initialize the layout algorithm via Resize()
  Resize(GetDefaultSize());
  
  // Map main frame
  MapWindow();
  fListBox->Select(1);
}

MyMainFrame::~MyMainFrame()
{
  // Clean up main frame...
  Cleanup();
  if (fSelected) {
    fSelected->Delete();
    delete fSelected;
  }
}

void MyMainFrame::HandleButtons()
{
  // Handle check button.
  Int_t id;
  TGButton *btn = (TGButton *) gTQSender;
  id = btn->WidgetId();
  
  printf("HandleButton: id = %d\n", id);
  
  if (id == 10)  
    fListBox->SetMultipleSelections(fCheckMulti->GetState());
}


void MyMainFrame::PrintSelected()
{
  // Writes selected entries in TList if multiselection.
  
  fSelected->Clear();
  if (fListBox->GetMultipleSelections()) {
    Printf("Selected entries are:\n");
    fListBox->GetSelectedEntries(fSelected);
    fSelected->ls();
  } else {
    Printf("Selected entries is: %d\n", fListBox->GetSelected());
  }
}
#if 0
void listBox()
{
  // Popup the GUI...
  new MyMainFrame(gClient->GetRoot(), 200, 200);
}
#endif
struct Field_t {
  const Char_t *Name;
  Int_t         type; // 0 -> TString, 1 -> Int_t, 2 -> Float_t, 3 -> Bool_t
  TProfile     *prof; //
};
Field_t Fields[40] = {
  {"jobID",	0, 0},                          //  0
  {"LibLevel",	0, 0},				//  1
  {"LibTag",	0, 0},				//  2
  {"rootLevel",	0, 0},				//  3
  {"path",	0, 0},				//  4
  {"prodyear",	0, 0},				//  5
  {"logFile",	0, 0},				//  6
  {"createTime",	0, 0},			//  7
  {"chainOpt",	0, 0},				//  8
  {"jobStatus",	0, 0},				//  9
  {"crashedCode",	0, 0},			//  0
  {"errMessage",	0, 0},			// 11
  {"NoEventDone",	1, 0},			// 12
  {"memUsageF",	2, 0},				// 13
  {"memUsageL",	2, 0},				// 14
  {"CPU_per_evt_sec",	2, 0},			// 15
  {"RealTime_per_evt",	2, 0},			// 16
  {"percent_of_usable_evt",	1, 0},		// 17
  {"avg_no_tracks",	1, 0},			// 18
  {"avg_no_tracksnfit15",	1, 0},		// 19
  {"NoEventVtx",	1, 0},			// 20
  {"avgNoVtx_evt",	2, 0},			// 21
  {"avg_no_primaryT",	1, 0},			// 22
  {"avg_no_primaryT_1vtx",	1, 0},		// 23
  {"avg_no_primaryTnfit15",	1, 0},		// 24
  {"avg_no_primaryTnfit15_1vtx",	1, 0},	// 25
  {"avg_no_V0Vrt",	1, 0},			// 26
  {"avg_no_XiVrt",	1, 0},			// 27
  {"avg_no_KinkVrt",	1, 0},			// 28
  {"avgNoTrack_usbevt",	1, 0},			// 29
  {"avgNoTrackNfit15_usbevt",	1, 0},		// 20
  {"avgNoPrTrack_1vtx_usbevt",	1, 0},		// 31
  {"avgNoPrTrackNfit15_1vtx_usbevt",	1, 0},	// 32
  {"avgNoV0_usbevt",	1, 0},			// 33
  {"avgNoXi_usbevt",	1, 0},			// 34
  {"avgNoKink_usbevt",	1, 0},			// 35
  {"nodeID",	0, 0},				// 36
  {"avail",	3, 0},				// 37
  {"id",	1, 0},				// 38
  {"NoEventSkip",	1, 0}			// 39
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
  Int_t prodyear;			 //  5| prodyear                       | smallint(6)   | NO   |     | 0                   |                |
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
                                         // +--------------------------------+---------------+------+-----+---------------------+----------------+
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
void ReduceDataSet(TString &dataset, TString &year) {
  dataset.ReplaceAll("production_","");
  TString y("_"); y += year;
  dataset.ReplaceAll(year,"");
}
//________________________________________________________________________________
void SetBinName(TProfile *prof, VecList &datasets) {
  TAxisis *x = prof->GetXaxis();
  Int_t bin = 0;
  for (auto t : datasets) {
    x->SetBinLabel(++bi,t);
  }
}
//________________________________________________________________________________
void sqlJobStatus(Int_t d1 = 20200101, Int_t d2 = 0) {
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
  TFile *fout = new TFile("JobStatus.root","recreate");
  TProfile *NoEventDone = 0, *memUsageF = 0
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
    TString sql = Form("SELECT DISTINCT prodyear,Dataset,LibTag  from JobStatus where LibTag != \"n/a\" and  Dataset != \"\" and createTime >= \"%s\" order by prodyear;",
		       t1.AsSQLString());
    cout << sql << endl;
    res = db->Query(sql);
    
    int nrows = res->GetRowCount();
    if (! nrows) return;
    int nfields = res->GetFieldCount();
    printf("\nGot %d rows in result\n", nrows);
    VecList datasets;
    for (int i = 0; i < nrows; i++) {
      row = res->Next();
      A = "";
      for (int j = 0; j < nfields; j++) {
	if (A != "") A += "/";
	A += row->GetField(j);
	printf("%5i %20s : %120s\n", j,  res->GetFieldName(j), row->GetField(j));
      }
      
      cout << A.Data() << endl;
      datasets.push_back(A);
    }
#if 0
    new MyMainFrame(gClient->GetRoot(), 500, 20*nrows, datasets);
#endif
    delete res;
    Int_t nxbin = datasets.size();
    for (Int_t f = 0; f < 40; f++) {
      if (Fields[f] <= 0 || Fields[f] > 2) continue;
      Fields[f].prof = new TProfile(Fields[f].Name,From("%s versus daaset"),nxbin+1,-0.5,nxbin+0.5,"s");
      SetBinName(Fields[f].prof,datasets);
    }

  }
  JobStatus job;
  const char *sql = "select * from JobStatus limit 3";
  
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
}
