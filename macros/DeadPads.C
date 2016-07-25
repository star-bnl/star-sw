#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TSystem.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TList.h"
#include "TIterator.h"
#include "tables/St_tpcGain_Table.h"
#include "St_db_Maker/St_db_Maker.h"
#else
class  St_tpcGain;
class St_db_Maker;
#endif 
St_db_Maker *dbMk = 0;
class St_tpcGain;
St_tpcGain *table = 0;
void Load() {
#if defined(__CINT__) && ! defined(__MAKECINT__)
  gSystem->Load("libTable");
  gSystem->Load("libStDb_Tables");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("St_Tables.so");
  gSystem->Load("StDbLib.so");
  gSystem->Load("StDbBroker.so"); 
  gSystem->Load("St_db_Maker.so");
#endif
}
void DeadPads(Int_t date = 20030525, Int_t time = 1){
  const Char_t *tabNam  = "Calibrations/tpc/tpcGain"; 
  // Baseline shared libraries
  Load();
  dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  dbMk->SetDebug(1);
  dbMk->SetFlavor("ofl");
  
  dbMk->Init();

  // Make reaquests for data
  // choose timestamp 

  //  dbMk->SetDateTime(20020101,10000);
  dbMk->SetDateTime(date,time); 
  //  dbMk->InitRun(2305001); 


  // to browse 1 database, use this one
  TDataSet *set = dbMk->GetDataBase(gSystem->DirName(tabNam));
  table = (St_tpcGain *) set->Find(gSystem->BaseName(tabNam));
  if (table) {
    TDatime t[2];
    dbMk->GetValidity(table,t);
    cout << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
	 << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endl;
    //    table->Print(0,5);
    tpcGain_st *g = table->GetTable();
    const Int_t Npads[45]  = { 
      88, 96, 104, 112, 118, 126, 134, 142, 150, 
	158, 166, 174, 182,
	98, 100, 102, 104, 106, 106, 108, 110, 112,
	112, 114, 116, 118, 120, 122, 122, 124, 126, 
	128, 128, 130, 132, 134, 136, 138, 138, 140, 
	142, 144, 144, 144, 144 };// !number of pads in row
    
    Int_t NkilledPads = 0;
    Int_t NalivePads  = 0;
    Int_t Ntotal      = 0;
    for (int sector=0;sector<24; sector++) {
      for (int row = 0; row<45; row++) {
	for (int pad = 2; pad < Npads[row]-2; pad++) {
	  Ntotal++;
	  if (g->Gain[row][pad] <= 0) continue;
	  NalivePads++;
	  for (int p = pad - 2; p <= pad + 2; p++) {
	    if (p == pad) continue;
	    if (g->Gain[row][p] <= 0) {NkilledPads++; break;}
	  }
	  if (Ntotal%1000 == 1) 
	    cout << "Total " << Ntotal << "\talivePads " << NalivePads << "\tkilledPads " << NkilledPads << endl;
	}
      }
    }
    cout << "Total " << Ntotal << "\talivePads " << NalivePads << "\tkilledPads " << NkilledPads << endl;
  }
  else cout << "Table:" << tabNam << " has not been found" << endl;
  
}







