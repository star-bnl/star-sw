// Convert Db -> Cint/Root structure
#ifndef __CINT__
#include "iostream.h"
#include "Rtypes.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TClassTable.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "StBFChain.h"
#include "StIOMaker.h"
void bfc (const Int_t Last, 
	  const Char_t *Chain,
	  const Char_t *infile, 
	  const Char_t *outfile, 
	  const Char_t *TreeFile);
//R__EXTERN StBFChain *chain;
#else
#define BIT(n)       (1 << (n))
#define SETBIT(n,i)  ((n) |= (1 << i))
#define CLRBIT(n,i)  ((n) &= ~(1 << i))
#define TESTBIT(n,i) ((Bool_t)(((n) & (1 << i)) != 0))

class StBFChain;
class TTree;
StBFChain *chain;
class StIOMaker;
#endif
class St_db_Maker;
St_db_Maker *dbMk = 0;
class TTable;
TTable *table = 0;
//________________________________________________________________________________
void Db2C(const Char_t *dirname  =  "Calibrations/tpc",
	  //"Geometry/svt", // "Calibrations/tpc",//"VMCGeometry",//Geometry/ssd", //"
	  Int_t date = 20060421, Int_t time = 101) { // 2001-03-10
  TString separator("/");
  if (dbMk == 0) {
    // Baseline shared libraries
    gSystem->Load("libTable");
    gSystem->Load("St_base"); 
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    gSystem->Load("St_Tables.so");
    if ( gClassTable->GetID("TGiant3") < 0) // ! root4star
      gSystem->Load("libmysqlclient");
    gSystem->Load("StDbLib.so");
    gSystem->Load("StDbBroker.so"); 
    gSystem->Load("St_db_Maker.so");
    dbMk = new St_db_Maker("db","MySQL:StarDb");
    //    dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
    dbMk->SetDebug(1);
    //  dbMk->SetFlavor("ofl+sim");
    //  dbMk->SetFlavor("simu","svtWafersPosition"); 
    dbMk->Init();
  }
  dbMk->SetDateTime(date,time); 
  TDatime newTime(date,time); 
  // to browse 1 database, use this one
  TDataSet *set = dbMk->GetDataBase(dirname);
  if (! set) {cout << "Dataset : " << dirname << " has not bee found" << endl; return;}
  TDataSetIter next(set,0);
  TString ts,dir,Name;
  TDataSet *set = 0;
  while ((set =  next())) {
    if (! set->HasData()) continue;
    dir = set->Path(); cout << dir;
    dir = gSystem->DirName(dir.Data());
    dir = gSystem->DirName(dir.Data());
    dir.ReplaceAll("bfc/.make/","");
    dir.ReplaceAll("db/.const/","");
    if (gSystem->AccessPathName(dir)) {
      Int_t iok = gSystem->mkdir(dir,kTRUE);
      if (iok > -1) cout << "Make directory " << dir << " done " << endl;
      else         {cout << "Make directory " << dir << " failed with " << iok << endl;}
    }
    table = (TTable *) set;
    TDatime t[2];
    dbMk->GetValidity(table,t);
    Char_t *name = table->GetName();
    if (name && name[0] == '.') continue;
    cout << name << "\tValidity:" << t[0].GetDate() << "/" << t[0].GetTime()
	 << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endl;
#if 0
    Int_t Nrows = table->GetNRows();
    if (Nrows > 10) Nrows = 10;
    table->Print(0,Nrows);
#endif
    ts = dir;
    ts += Form("/%s.%08d.%06d.C",name,t[0].GetDate(),t[0].GetTime());
    ofstream out;
    out.open(ts.Data());       cout << "Create " << ts << endl;
    table->SavePrimitive(out,""); 
    out.close();
  }
}
