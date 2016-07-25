#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
//#include <ofstream>
#include <stdio.h>
#include <string.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TTree.h"
#include "TString.h"
#include "TFile.h"
#include "TDatime.h"
#include "TMath.h"
#include "TSQLServer.h"
#include "TSQLResult.h"
#include "TSQLRow.h"
#include "TTree.h"
#include "TStopwatch.h"
#endif
//#define DEBUG
const static Int_t tZero= 19950101;
const static TDatime t0(tZero,0);
const static Int_t timeOffSet = t0.Convert();
struct Run_t {
  Int_t Run;
  Int_t time;
  Int_t startRunTime;
  Int_t endRunTime;
};
Run_t Run;
Run_t *evRun = &Run;
struct Zdc_t {
  Int_t time;
  Double_t rs8, rs15;
};
Zdc_t Zdc;
struct TpcAv_t {
  Int_t time;
  Double_t innerCurrents, outerCurrents, innerVoltages, outerVoltages;
};
TpcAv_t TpcAv;

class tpcAnodeHVavg_t {
public:
  tpcAnodeHVavg_t() {}
  virtual ~tpcAnodeHVavg_t() {}
  UInt_t time;
  Double_t innerVoltages, outerVoltages;
};
tpcAnodeHVavg_t tpcAnodeHVavg;
tpcAnodeHVavg_t *evtpcAnodeHVavg = &tpcAnodeHVavg;

struct TpcZdc_t {
  Int_t run;
  TpcAv_t TpcAv;
  Zdc_t Zdc;
  tpcAnodeHVavg_t AnodeAv;
  Run_t R;
};
TpcZdc_t  T;


struct BPoint_t {
  Double_t day;
  Int_t    date, time;
  Double_t dsecs, dinnerCurrents, douterCurrents, sumI, sumO; //
  Double_t  innerCurrMax5,  outerCurrMax5,  innerCurrMax10,  outerCurrMax10,  innerCurrMax20,  outerCurrMax20;
  Double_t  innerCurrents5,  outerCurrents5,  innerCurrents10,  outerCurrents10,  innerCurrents20,  outerCurrents20;
  Double_t dinnerCurrents5, douterCurrents5, dinnerCurrents10, douterCurrents10, dinnerCurrents20, douterCurrents20;
  Double_t dcurrRMSI5, dcurrRMSO5, dcurrRMSI10, dcurrRMSO10, dcurrRMSI20, dcurrRMSO20;
  Double_t I5, I10, I20;
  Int_t    nskip;
  Double_t baseI, baseO;
  TpcZdc_t T;
  Int_t    run;
  Int_t    npoints;
  Double_t innerCurrAv, outerCurrAv;
};
BPoint_t BPoint, BPointO, BPoints[20], BPointR;
TFile *f = 0;
TSQLRow *row;
TSQLResult *res;
TSQLServer *db;
Int_t comp   = 1;       // by default file is compressed
Int_t split  = 99;       // by default, split Event in sub branches
Int_t branchStyle = 1; //new style by default
//if (split < 0) {branchStyle = 0; split = -1-split;}
Int_t bufsize = 64000/4;
//________________________________________________________________________________
TFile *runDescriptor() {
  TFile *fout = TFile::Open("runDescriptor.root");
  if (fout) {
    TTree *tree = (TTree *) fout->Get("RunDesc");
    if (tree) {
      cout << "Found file runDescriptor.root with runDescriptor TTree." << endl;
      return fout;
    }
    delete fout; fout = 0;
  }
  // start timer
  TStopwatch timer;
  timer.Start();
  TDatime t;
  Run_t *event = &Run;
  Int_t   *irs = (Int_t   *) &Run.Run;
  fout = new TFile("runDescriptor.root","recreate");
  TTree *tree = new TTree("RunDesc","Run information");
  tree->SetAutoSave(1000000000); // autosave when 1 Gbyte written
  tree->SetCacheSize(10000000);  //set a 10 MBytes cache (useless when writing local files)
  TTree::SetBranchStyle(branchStyle);
  TBranch *branch = tree->Branch("RunD", &event, bufsize,split);
  branch->SetAutoDelete(kFALSE);
  if(split >= 0 && branchStyle) tree->BranchRef();
  for (Int_t k = 0; k < 2; k++) {
    if (k == 0) db = TSQLServer::Connect("mysql://dbbak.starp.bnl.gov:3408/RunLog","", "");
    else        db = TSQLServer::Connect("mysql://onldb.starp.bnl.gov:3501/RunLog","", "");
    // query database and print results 30 secs average
    //                     "WHERE beginTime > '2010-04-05' limit 60";
    TString SQL("SELECT runNumber,beginTime,FROM_UNIXTIME(startRunTime), FROM_UNIXTIME(endRunTime) FROM RunLog.runDescriptor"
		" WHERE startRunTime < endRunTime and daqSetupName like 'physics' ORDER BY runNumber");
    res = db->Query(SQL);
    if (! res) continue;
    int nrows = res->GetRowCount();
    printf("\nGot %d rows in result\n", nrows);
    
    int nfields = res->GetFieldCount();
    for (int i = 0; i < nfields   ; i++) printf("%20s", res->GetFieldName(i));  printf("\n");
    for (int i = 0; i < nfields*20; i++) printf("=");                           printf("\n");
    
    for (int i = 0; i < nrows; i++) {
      row = res->Next();
      for (int j = 0; j < nfields; j++) {
	if (i < 40) printf("%20s", row->GetField(j));
	TString a(row->GetField(j));
	if (j) {
	  t.Set(a);// cout << t.AsString() << endl;
	  irs[j] = t.Convert() - timeOffSet;
	} else {
	  irs[j] = a.Atoi();
	}
      }
      if (i < 40) printf("\n");
      delete row;
      tree->Fill();
    }
    delete res;
    delete db;
  }
  
  // stop timer and print results
  timer.Stop();
  Double_t rtime = timer.RealTime();
  Double_t ctime = timer.CpuTime();
  fout->Write();
  fout->Flush();
  printf("\nRealTime=%f seconds, CpuTime=%f seconds\n", rtime, ctime);
  return fout;
}
//________________________________________________________________________________

TFile *tpcAverages() {
  TFile *fout = TFile::Open("tpcAverages.root");
  if (fout) {
    TTree *tree = (TTree *) fout->Get("tpcAverages");
    if (tree) {
      cout << "Found file tpcAverages.root with tpcAverages TTree." << endl;
      return fout;
    }
    delete fout; fout = 0;
  }
  db = TSQLServer::Connect("mysql://onldb2.starp.bnl.gov:3502/Conditions_sc","", "");
#ifdef DEBUG 
  printf("Server info: %s\n", db->ServerInfo());
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
#endif
  // start timer
  TStopwatch timer;
  timer.Start();
  TDatime t;
  TpcAv_t *event = &TpcAv;
  Double_t *ars = (Double_t *) &TpcAv.innerCurrents - 1;
  Int_t   *irs = (Int_t   *) &TpcAv.time;
  //   const char *sql = "select count(*) from Conditions_sc.tpcAverages "
  //                     "WHERE tag&(1<<2)";
  fout = new TFile("tpcAverages.root","recreate");
  TTree *tree = new TTree("tpcAverages","Anode Current information");
  tree->SetAutoSave(1000000000); // autosave when 1 Gbyte written
  tree->SetCacheSize(10000000);  //set a 10 MBytes cache (useless when writing local files)
  TTree::SetBranchStyle(branchStyle);
  TBranch *branch = tree->Branch("TpcAv", &event, bufsize,split);
  branch->SetAutoDelete(kFALSE);
  if(split >= 0 && branchStyle) tree->BranchRef();
  for (Int_t k = 0; k < 2; k++) {
    // query database and print results 30 secs average
    //                     "WHERE beginTime > '2010-04-05' limit 60";
    TString SQL("select beginTime,innerCurrents,outerCurrents,innerVoltages,outerVoltages from Conditions_sc.");
    if (k == 0) SQL += "tpcAverages_sca";
    else        SQL += "tpcAverages";
    SQL += " order by beginTime";
    res = db->Query(SQL);
    if (! res) continue;
    int nrows = res->GetRowCount();
    printf("\nGot %d rows in result\n", nrows);
    
    int nfields = res->GetFieldCount();
    for (int i = 0; i < nfields   ; i++) printf("%20s", res->GetFieldName(i));  printf("\n");
    for (int i = 0; i < nfields*20; i++) printf("=");                           printf("\n");
    
    for (int i = 0; i < nrows; i++) {
      row = res->Next();
      for (int j = 0; j < nfields; j++) {
	if (i < 40) printf("%20s", row->GetField(j));
	TString a(row->GetField(j));
	if (! j) {
	  t.Set(a);// cout << t.AsString() << endl;
	  irs[j] = t.Convert() - timeOffSet;
	} else {
	  ars[j] = a.Atof();
	}
      }
      if (i < 40) printf("\n");
      delete row;
      tree->Fill();
    }
    delete res;
  }
  delete db;
  
  // stop timer and print results
  timer.Stop();
  Double_t rtime = timer.RealTime();
  Double_t ctime = timer.CpuTime();
  fout->Write();
  fout->Flush();
  printf("\nRealTime=%f seconds, CpuTime=%f seconds\n", rtime, ctime);
  return fout;
}
//________________________________________________________________________________
TFile *ZdcC() {
  TFile *fout = TFile::Open("ZdcC.root");
  if (fout) {
    TTree *tree = (TTree *) fout->Get("ZdcC");
    if (tree) {
      cout << "Found file ZdcC.root with ZdcC TTree." << endl;
      return fout;
    }
    delete fout; fout = 0;
  }
  
  fout = new TFile("ZdcC.root","recreate");
  TTree *tree = new TTree("ZdcC","Zdc Conincidence rate");
  tree->SetAutoSave(1000000000); // autosave when 1 Gbyte written
  tree->SetCacheSize(10000000);  //set a 10 MBytes cache (useless when writing local files)
  TTree::SetBranchStyle(branchStyle);
  Zdc_t *event = &Zdc;
  TBranch *branch = tree->Branch("ZdcAv", &event, bufsize,split);
  branch->SetAutoDelete(kFALSE);
  if(split >= 0 && branchStyle) tree->BranchRef();
  TDatime t;
  
  Double_t *ars = (Double_t *) &Zdc.rs8 - 1;
  Int_t   *irs = (Int_t *) &Zdc.time;
  //   const char *sql = "select count(*) from Conditions_rich.richScalar "
  //                     "WHERE tag&(1<<2)";
  TString DB;
  // start timer
  TStopwatch timer;
  timer.Start();
  for (Int_t k = 0; k < 2; k++) {
    if (k == 0) DB = "mysql://dbbak.starp.bnl.gov:3408";
    else        DB = "mysql://onldb2.starp.bnl.gov:3502";
    DB += "/Conditions_rich";
    db = TSQLServer::Connect(DB,"", "");
#ifdef DEBUG  
    printf("Server info: %s\n", db->ServerInfo());
    // list databases available on server
    printf("\nList all databases on server %s\n", db->GetHost());
    res = db->GetDataBases();
    if (! res) continue;
    while ((row = res->Next())) {
      printf("%s\n", row->GetField(0));
      delete row;
    }
    delete res;
    // list tables in database "Conditions_rich" (the permission tables)
    printf("\nList all tables in database \"Conditions_rich\" on server %s\n",
	   db->GetHost());
    res = db->GetTables("Conditions_rich");
    while ((row = res->Next())) {
      printf("%s\n", row->GetField(0));
      delete row;
    }
    delete res;
    
    // list columns in table "richScalar" in database "mysql"
    printf("\nList all columns in table \"richScalar\" in database \"Conditions_rich\" on server %s\n",
	   db->GetHost());
    res = db->GetColumns("Conditions_rich", "richScalar");
    while ((row = res->Next())) {
      printf("%s %s\n", row->GetField(0), row->GetField(1));
      delete row;
    }
    delete res;
#endif
  
    // query database and print results 30 secs average
    const char *sql = "select rich.beginTime,rich.rs8,rich.rs15 FROM Conditions_rich.richScalar AS rich  order by beginTime";
    //                     "WHERE beginTime > '2010-04-05' limit 60";
    res = db->Query(sql);
    int nrows = res->GetRowCount();
    printf("\nGot %d rows in result\n", nrows);
  
    int nfields = res->GetFieldCount();
    for (int i = 0; i < nfields   ; i++) printf("%20s", res->GetFieldName(i));  printf("\n");
    for (int i = 0; i < nfields*20; i++) printf("=");                           printf("\n");
    
    for (int i = 0; i < nrows; i++) {
      row = res->Next();
      for (int j = 0; j < nfields; j++) {
	if (i < 40) printf("%20s", row->GetField(j));
	TString a(row->GetField(j));
	if (! j) {
	  t.Set(a);// cout << t.AsString() << endl;
	  irs[j] = t.Convert() - timeOffSet;
	} else {
	  ars[j] = a.Atof();
	}
      }
      if (i < 40) printf("\n");
      delete row;
      tree->Fill();
    }
    delete res;
    delete db;
  }  
  // stop timer and print results
  timer.Stop();
  Double_t rtime = timer.RealTime();
  Double_t ctime = timer.CpuTime();
  fout->Write();
  fout->Flush();
  printf("\nRealTime=%f seconds, CpuTime=%f seconds\n", rtime, ctime);
  return fout;
}
//________________________________________________________________________________
TFile *MergeTpcAvZdc(TFile *ftpcAv,TFile *fzdc, TFile *fanodeAv=0, TFile *fRunDesc=0) {
  TFile *fOut = TFile::Open("TpcZdcAnodeRun.root");
  if (fOut) {
    TTree *tpcZdcAv = (TTree *) fOut->Get("TpcAvZdc");
    if (tpcZdcAv) return fOut;
    delete fOut; fOut = 0;
  }
  if (! ftpcAv || ! fzdc) return 0;
  TTree *tzdc = (TTree *) fzdc->Get("ZdcC");
  if (! tzdc) return 0;
  TBranch *branch = tzdc->GetBranch("ZdcAv");
  Zdc_t *eventZdc = &Zdc;
  branch->SetAddress(&eventZdc);
  
  Int_t nZdc = (Int_t) tzdc->GetEntries();
  
  
  TTree *ttpcAv = (TTree *) ftpcAv->Get("tpcAverages");
  if (! ttpcAv) return 0;
  branch = ttpcAv->GetBranch("TpcAv");
  TpcAv_t *eventTpc = &TpcAv;
  branch->SetAddress(&eventTpc);

  Int_t nTpcAv = (Int_t) ttpcAv->GetEntries();
  //   const char *sql = "select count(*) from Conditions_rich.richScalar "
  //                     "WHERE tag&(1<<2)";
  
  Int_t nAnode = -1;
  TTree *tAnode = 0;
  if (fanodeAv) {
    tAnode = (TTree *) fanodeAv->Get("tpcAnodeHVavg");
    if (! tAnode) return 0;
    nAnode = (Int_t) tAnode->GetEntries();
    branch = tAnode->GetBranch("TpcAHV");
    branch->SetAddress(&evtpcAnodeHVavg);
  }
  Int_t nRun = -1;
  TTree *tRun = 0;
  if (fRunDesc) {
    tRun = (TTree *) fRunDesc->Get("RunDesc");
    if (! tRun) return 0;
    nRun = (Int_t) tRun->GetEntries();
    branch = tRun->GetBranch("RunD");
    branch->SetAddress(&evRun);
  }

  fOut = new TFile("TpcZdcAnodeRun.root","recreate");
  TTree *tpcZdcAv = new  TTree("TpcAvZdc","Synchronized TpcAv and Zdc data");
  tpcZdcAv->SetAutoSave(1000000000); // autosave when 1 Gbyte written
  tpcZdcAv->SetCacheSize(10000000);  //set a 10 MBytes cache (useless when writing local files)
  TTree::SetBranchStyle(branchStyle);
  TpcZdc_t *event = &T;
  branch = tpcZdcAv->Branch("TpcZdcAv", &event, bufsize,split);
  branch->SetAutoDelete(kFALSE);
  if(split >= 0 && branchStyle) tpcZdcAv->BranchRef();
  Int_t iZdc = 0;
  Int_t iTpcAv = 0;
  Int_t iAnode = 0;
  Int_t iRun = 0;
  Int_t nmatched = 0;
  while (iZdc < nZdc && iTpcAv < nTpcAv && (! tAnode || iAnode < nAnode)) {
    ttpcAv->LoadTree(iTpcAv);
    ttpcAv->GetEntry(iTpcAv);
    Int_t idif = 999999999;
    Int_t j = iZdc;
    for (; j < nZdc; j++) {
      tzdc->LoadTree(j);
      tzdc->GetEntry(j);
      Int_t diff = TMath::Abs((Int_t) TpcAv.time - (Int_t) Zdc.time);
      if (diff <= idif) {
	idif = diff;
	iZdc = j;
      } else break;
    }
    tzdc->LoadTree(iZdc);
    tzdc->GetEntry(iZdc);
    if (tAnode) {
      j = iAnode;
      idif = 999999999;
      for (; j < nAnode; j++) {
	tAnode->LoadTree(j);
	tAnode->GetEntry(j);
	tpcAnodeHVavg.time -= timeOffSet;
	Int_t time = (Int_t) tpcAnodeHVavg.time;
	Int_t diff = (Int_t) TpcAv.time - time;
	if (diff <= 0) break;
	if (diff <= idif) {
	  idif = diff;
	  iAnode = j;
	} else break;
      }
      tAnode->LoadTree(iAnode);
      tAnode->GetEntry(iAnode);
      tpcAnodeHVavg.time -= timeOffSet;
    }
    T.run = 0;
    if (tRun) {
      j = iRun;
      Int_t time = (Int_t ) TpcAv.time;
      for (; j < nRun; j++) {
	tRun->LoadTree(j);
	tRun->GetEntry(j);
	Int_t tB = Run.startRunTime;
	Int_t tE = Run.endRunTime;
	if (time >  tE) continue;
	if (time >= tB) {
	  iRun = j;
	  T.run = Run.Run;
	}
	break;
      }
      if (T.run) {
	tRun->LoadTree(iRun);
	tRun->GetEntry(iRun);
      } else {
        memset (&Run.Run, 0, sizeof(Run_t));
      }
    }
    iTpcAv++;
    T.TpcAv   = TpcAv;
    T.Zdc     = Zdc;
    T.AnodeAv = tpcAnodeHVavg;
    T.R       = Run;
    tpcZdcAv->Fill();
    if (nmatched%10000 == 0) {
      tzdc->Show(iZdc);
      ttpcAv->Show(iTpcAv);
      if (tAnode) tAnode->Show(iAnode);
      if (tRun)   tRun->Show(iRun);
      tpcZdcAv->Show(nmatched);
    }
    nmatched++;
  } 
  fOut->Write();
  fOut->Flush();
  return fOut;
}
//________________________________________________________________________________
void CreatetpcAvCurrent(BPoint_t &P) {
  TString fOut =  Form("tpcAvCurrent.%8i.%06i.C", P.date, P.time);
  const Double_t AcChargeIBefore2009 = 18.7455;
  const Double_t AcChargeOBefore2009 =  5.3879;
  ofstream out;
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_tpcAvCurrent\")) return 0;" << endl;
  out << "  tpcAvCurrent_st row = ";
  Double_t currI = P.innerCurrAv;
  Double_t currO = P.outerCurrAv;
  if (P.npoints > 2) {
    currI /= (P.npoints - 1); currI = TMath::Max(0.,currI);
    currO /= (P.npoints - 1); currO = TMath::Max(0.,currO);
  }
  out << " {" << P.run
      << ", " << currI << ", " << currO
      << ", " << P.sumI + AcChargeIBefore2009 << "," << P.sumO + AcChargeOBefore2009 << "};" << endl;
  out << "  St_tpcAvCurrent *tableSet = new St_tpcAvCurrent(\"tpcAvCurrent\",1);" << endl; 
  out << "  tableSet->AddAt(&row.run, 0);" << endl;
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
}
//________________________________________________________________________________
void DoAveraging(TFile *fMerged) {//const Char_t *DirName=".") {
  if (! fMerged) return;
  TTree *tpcZdcAv = (TTree *) fMerged->Get("TpcAvZdc");
  if (! tpcZdcAv) return;
  TBranch *branch = tpcZdcAv->GetBranch("TpcZdcAv");
  if (! branch) return;
  TpcZdc_t *eventTpcZdc = &T;
  branch->SetAddress(&eventTpcZdc);
  Int_t N = (Int_t) tpcZdcAv->GetEntries();
  
  f = new TFile("Current.root","RECREATE");
  TTree *FitP = new TTree("FitP","Anode Current");
  BPoint_t *event = &BPoint;
  FitP->SetAutoSave(1000000000); // autosave when 1 Gbyte written
  FitP->SetCacheSize(10000000);  //set a 10 MBytes cache (useless when writing local files)
  TTree::SetBranchStyle(branchStyle);
  branch = FitP->Branch("ZdcAv", &event, bufsize,split);
  branch->SetAutoDelete(kFALSE);
  if(split >= 0 && branchStyle) FitP->BranchRef();
  
  memset (&BPoint.day, 0, sizeof(BPoint_t));
  memset (&BPointO.day, 0, sizeof(BPoint_t));
  memset (&BPoints[0].day, 0, 20*sizeof(BPoint_t));
  memset (&BPointR.day, 0, sizeof(BPoint_t));
  
  Int_t mon, day, year, hour, min, sec;
  UInt_t uSaved = 0;
  TDatime dtime(20010101,0);
  UInt_t u0 = dtime.Convert();
  Int_t k = 0;
  Double_t baseI = 0;
  Double_t baseO = 0;
  Double_t baseIC = 0;
  Double_t baseOC = 0;
  Int_t idate = 0, itime = 0;
  const Double_t daysec = 24*3600;
  Double_t VI = 0; 
  Double_t VO = 0;
  Int_t ut = 0;
  Int_t nskip = 0;
  for (Int_t eve = 0; eve < N; eve++) {
    tpcZdcAv->LoadTree(eve);
    tpcZdcAv->GetEntry(eve);
    Int_t t1 = T.TpcAv.time;
    if (t1 < ut) continue;
    ut = t1;
    Int_t t2 = T.Zdc.time;
    Int_t t3 = T.AnodeAv.time;
    // Voltage
    VI = T.TpcAv.innerVoltages;
    VO = T.TpcAv.outerVoltages;
    if (VI < 100 && VO < 100) {
      VI = T.AnodeAv.innerVoltages;
      VO = T.AnodeAv.outerVoltages;
    }
    memset (&BPoint.day, 0, sizeof(BPoint_t));
    BPoint.T   = T;
    UInt_t u1 = T.TpcAv.time + timeOffSet;
    dtime.Set(u1);
    year = dtime.GetYear();
    mon  = dtime.GetMonth();
    day  = dtime.GetDay();
    hour = dtime.GetHour();
    min  = dtime.GetMinute();
    sec  = dtime.GetSecond();
    BPoint.day = ((Float_t) (u1 - u0))/daysec;
    BPoint.date = idate;
    BPoint.time = itime;
    BPoint.sumI = BPointO.sumI;
    BPoint.sumO = BPointO.sumO;
    BPoint.baseI = baseI;
    BPoint.baseO = baseO;
    // Time match
    if (TMath::Abs(t2 - t1)> 20 ||	t1 - t3 < 0             ||
	VI < 950 || VO < 1300   ||
	TMath::Abs(T.TpcAv.innerCurrents) > 200 ||
        TMath::Abs(T.TpcAv.outerCurrents) >  60 ||
	T.Zdc.rs8  < 1 || // T.Zdc.rs15 < 1 ||
	BPointO.day == 0) {
      nskip++;
      baseIC += T.TpcAv.innerCurrents;
      baseOC += T.TpcAv.outerCurrents;
      if (nskip > 5) {
	baseI = baseIC/nskip;
	baseO = baseOC/nskip;
      }
      BPoint.nskip = nskip;
    } else {
      nskip = 0;
      baseIC = baseOC = 0;
    }
    TDatime dt;
    UInt_t u = BPointO.T.TpcAv.time + timeOffSet;
    dt.Set(u);
    idate = dt.GetDate();
    itime = dt.GetTime();
    Int_t dsecs = (Int_t ) u1 - (Int_t) u;
    if (dsecs <= 0 || dsecs > 3600) {BPointO = BPoint; FitP->Fill(); continue;}
    BPoint.dsecs = dsecs;

    BPoint.dinnerCurrents = (BPoint.T.TpcAv.innerCurrents - BPointO.T.TpcAv.innerCurrents)/BPoint.dsecs;
    //    if (BPoint.dinnerCurrents > 2e4) continue;
    BPoint.douterCurrents = (BPoint.T.TpcAv.outerCurrents - BPointO.T.TpcAv.outerCurrents)/BPoint.dsecs;
    //    if (BPoint.douterCurrents > 1e4) continue;
    if (k%1000 == 0) {
      dtime.Print();
      tpcZdcAv->Show(eve);
    }
#if 0
    BPoint.sumI += 1e-6*TMath::Max(0.,BPoint.T.TpcAv.innerCurrents-baseI)*BPoint.dsecs;
    BPoint.sumO += 1e-6*TMath::Max(0.,BPoint.T.TpcAv.outerCurrents-baseO)*BPoint.dsecs;
#else
    BPoint.sumI += 1e-6*TMath::Max(0.,BPoint.T.TpcAv.innerCurrents)*BPoint.dsecs;
    BPoint.sumO += 1e-6*TMath::Max(0.,BPoint.T.TpcAv.outerCurrents)*BPoint.dsecs;
#endif
#if 1
    if (! BPoint.nskip) {
      BPoint.innerCurrMax5  = BPoint.T.TpcAv.innerCurrents;
      BPoint.outerCurrMax5  = BPoint.T.TpcAv.outerCurrents;
      BPoint.innerCurrMax10 = BPoint.T.TpcAv.innerCurrents;
      BPoint.outerCurrMax10 = BPoint.T.TpcAv.outerCurrents;
      BPoint.innerCurrMax20 = BPoint.T.TpcAv.innerCurrents;
      BPoint.outerCurrMax20 = BPoint.T.TpcAv.outerCurrents;
    }
    Double_t  innerCurrents5  = BPoint.T.TpcAv.innerCurrents;
    Double_t  outerCurrents5  = BPoint.T.TpcAv.outerCurrents;
    Double_t  innerCurrents10 = BPoint.T.TpcAv.innerCurrents;
    Double_t  outerCurrents10 = BPoint.T.TpcAv.outerCurrents;
    Double_t  innerCurrents20 = BPoint.T.TpcAv.innerCurrents;
    Double_t  outerCurrents20 = BPoint.T.TpcAv.outerCurrents;
    Double_t dinnerCurrents5  = BPoint.dinnerCurrents;
    Double_t douterCurrents5  = BPoint.douterCurrents;
    Double_t dinnerCurrents10 = BPoint.dinnerCurrents;
    Double_t douterCurrents10 = BPoint.douterCurrents;
    Double_t dinnerCurrents20 = BPoint.dinnerCurrents;
    Double_t douterCurrents20 = BPoint.douterCurrents;
    Double_t dcurrSqI5  = BPoint.dinnerCurrents*BPoint.dinnerCurrents;
    Double_t dcurrSqO5  = BPoint.douterCurrents*BPoint.douterCurrents;
    Double_t dcurrSqI10 = BPoint.dinnerCurrents*BPoint.dinnerCurrents;
    Double_t dcurrSqO10 = BPoint.douterCurrents*BPoint.douterCurrents;
    Double_t dcurrSqI20 = BPoint.dinnerCurrents*BPoint.dinnerCurrents;
    Double_t dcurrSqO20 = BPoint.douterCurrents*BPoint.douterCurrents;
    Int_t I5  = 1;
    Int_t I10 = 1;
    Int_t I20 = 1;
    Int_t ientry = k%20;
    BPoints[ientry] = BPoint;
    Int_t jentry = k - 20;
    if (jentry < 0) jentry = 0;
    for (; jentry < k; jentry++) {
      Int_t ii = jentry%20;
      if (k - jentry <  5) { 
	I5++; 
	if (! BPoints[ii].nskip ) {
	  if (BPoints[ii].T.TpcAv.innerCurrents > BPoint.innerCurrMax5) BPoint.innerCurrMax5 = BPoints[ii].T.TpcAv.innerCurrents;
	  if (BPoints[ii].T.TpcAv.outerCurrents > BPoint.outerCurrMax5) BPoint.outerCurrMax5 = BPoints[ii].T.TpcAv.outerCurrents;
	}
	innerCurrents5  += BPoints[ii].T.TpcAv.innerCurrents;
	outerCurrents5  += BPoints[ii].T.TpcAv.outerCurrents;
	dinnerCurrents5  += BPoints[ii].dinnerCurrents; 
	douterCurrents5  += BPoints[ii].douterCurrents;
	dcurrSqI5  += BPoints[ii].dinnerCurrents*BPoints[ii].dinnerCurrents; 
	dcurrSqO5  += BPoints[ii].douterCurrents*BPoints[ii].douterCurrents;
      }
      if (k - jentry <  10) { 
	I10++; 
	if (! BPoints[ii].nskip) { 
	  if (BPoints[ii].T.TpcAv.innerCurrents > BPoint.innerCurrMax10) BPoint.innerCurrMax10 = BPoints[ii].T.TpcAv.innerCurrents;
	  if (BPoints[ii].T.TpcAv.outerCurrents > BPoint.outerCurrMax10) BPoint.outerCurrMax10 = BPoints[ii].T.TpcAv.outerCurrents;
	}
	innerCurrents10 += BPoints[ii].T.TpcAv.innerCurrents;
	outerCurrents10 += BPoints[ii].T.TpcAv.outerCurrents;
	dinnerCurrents10  += BPoints[ii].dinnerCurrents; 
	douterCurrents10  += BPoints[ii].douterCurrents;
	dcurrSqI10  += BPoints[ii].dinnerCurrents*BPoints[ii].dinnerCurrents; 
	dcurrSqO10  += BPoints[ii].douterCurrents*BPoints[ii].douterCurrents;
      }
      if (k - jentry <  20) { 
	I20++; 
	if (! BPoints[ii].nskip) {
	  if (BPoints[ii].T.TpcAv.innerCurrents > BPoint.innerCurrMax20) BPoint.innerCurrMax20 = BPoints[ii].T.TpcAv.innerCurrents;
	  if (BPoints[ii].T.TpcAv.outerCurrents > BPoint.outerCurrMax20) BPoint.outerCurrMax20 = BPoints[ii].T.TpcAv.outerCurrents;
	}
	innerCurrents20 += BPoints[ii].T.TpcAv.innerCurrents;
	outerCurrents20 += BPoints[ii].T.TpcAv.outerCurrents;
	dinnerCurrents20  += BPoints[ii].dinnerCurrents; 
	douterCurrents20  += BPoints[ii].douterCurrents;
	dcurrSqI20  += BPoints[ii].dinnerCurrents*BPoints[ii].dinnerCurrents; 
	dcurrSqO20  += BPoints[ii].douterCurrents*BPoints[ii].douterCurrents;
      }
    }
    BPoint.I5 = I5;
    BPoint.innerCurrents5   =  innerCurrents5 /I5; 
    BPoint.outerCurrents5   =  outerCurrents5 /I5; 
    BPoint.dinnerCurrents5  = dinnerCurrents5 /I5; 
    Double_t RMS2 = dcurrSqI5/I5 - BPoint.dinnerCurrents5*BPoint.dinnerCurrents5;
    if (RMS2 > 0)  BPoint.dcurrRMSI5  = TMath::Sqrt(RMS2);
    else           BPoint.dcurrRMSI5  = 0;
    BPoint.douterCurrents5  = douterCurrents5 /I5; 
    RMS2 = dcurrSqO5/I5 - BPoint.douterCurrents5*BPoint.douterCurrents5;
    if (RMS2 > 0) BPoint.dcurrRMSO5  = TMath::Sqrt(RMS2);
    else          BPoint.dcurrRMSO5  = 0.;
    BPoint.I10 = I10;
    BPoint.innerCurrents10   =  innerCurrents10 /I10; 
    BPoint.outerCurrents10   =  outerCurrents10 /I10; 
    BPoint.dinnerCurrents10  = dinnerCurrents10 /I10; 
    RMS2 = dcurrSqI10/I10 - BPoint.dinnerCurrents10*BPoint.dinnerCurrents10;
    if (RMS2 > 0)  BPoint.dcurrRMSI10  = TMath::Sqrt(RMS2);
    else           BPoint.dcurrRMSI10  = 0;
    BPoint.douterCurrents10  = douterCurrents10 /I10; 
    RMS2 = dcurrSqO10/I10 - BPoint.douterCurrents10*BPoint.douterCurrents10;
    if (RMS2 > 0)  BPoint.dcurrRMSO10  = TMath::Sqrt(RMS2);
    else           BPoint.dcurrRMSO10  = 0;
    BPoint.I20 = I20;
    BPoint.innerCurrents20   =  innerCurrents20 /I20; 
    BPoint.outerCurrents20   =  outerCurrents20 /I20; 
    BPoint.dinnerCurrents20  = dinnerCurrents20 /I20; 
    RMS2 = dcurrSqI20/I20 - BPoint.dinnerCurrents20*BPoint.dinnerCurrents20;
    if (RMS2 > 0)  BPoint.dcurrRMSI20  = TMath::Sqrt(RMS2);
    else           BPoint.dcurrRMSI20  = 0;
    BPoint.douterCurrents20  = douterCurrents20 /I20; 
    RMS2 = dcurrSqO20/I20 - BPoint.douterCurrents20*BPoint.douterCurrents20;
    if (RMS2 > 0)  BPoint.dcurrRMSO20  = TMath::Sqrt(RMS2);
    else           BPoint.dcurrRMSO20  = 0;
    if (BPoint.dcurrRMSI10 > 0.2) BPoint.nskip++;
    if (BPoint.dcurrRMSO10 > 0.1) BPoint.nskip++;
    
#endif
    if (BPoint.T.R.Run) {
      TDatime tcur(BPointR.date,BPointR.time);
      UInt_t  ucur = tcur.Convert();
      if (BPoint.T.R.Run != BPointR.run || ucur - uSaved > 600) {
	if (BPointR.run != 0) {
	  uSaved = tcur.Convert():
	  CreatetpcAvCurrent(BPointR);
	} 
	BPointR = BPoint;
	BPointR.run = BPoint.T.R.Run;
	BPointR.npoints = 1;
	BPointR.innerCurrAv = BPoint.T.TpcAv.innerCurrents;	
	BPointR.outerCurrAv = BPoint.T.TpcAv.outerCurrents;      
      } else {
	BPointR.npoints++;
	if (BPointR.npoints <= 2) {// skip the 1-st measurement
	  BPointR.innerCurrAv = BPoint.T.TpcAv.innerCurrents;	
	  BPointR.outerCurrAv = BPoint.T.TpcAv.outerCurrents;      
	} else {
	  BPointR.innerCurrAv += BPoint.T.TpcAv.innerCurrents;	
	  BPointR.outerCurrAv += BPoint.T.TpcAv.outerCurrents;      
	}
      }
    }
    FitP->Fill();
    k++;
    BPointO = BPoint;
  }
  f->Write();
  f->Flush();

}
//________________________________________________________________________________
void tpcAvCurrent() {
  TFile *ftpcAv = tpcAverages();
  TFile *fzdc   = ZdcC();
  TFile *fanodeAv = new TFile("tpcAnodeHVC.root"); // to get it run Db2NttpcAnodeHVavg.C
  if (! ftpcAv || !fzdc) return; 
  TFile *fRunDesc = runDescriptor();
  TFile *fMerged = MergeTpcAvZdc(ftpcAv,fzdc,fanodeAv,fRunDesc);
  delete ftpcAv;
  delete fzdc;
  delete fanodeAv;
  delete fRunDesc;
#if 1
  DoAveraging(fMerged);
#endif
}
