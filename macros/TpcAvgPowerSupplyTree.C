/*
  root.exe lDb.C TpcAvgPowerSupplyTree.C+
 */
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TDatime.h"
#include "StEvtHddr.h"
#include "StChain.h"
#include "StDetectorDbMaker/St_TpcAvgPowerSupplyC.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StChain/StEvtHddr.h"
#endif
TpcAvgPowerSupply_st *event = 0;
Int_t comp   = 1;       // by default file is compressed
Int_t split  = 99;       // by default, split Event in sub branches
Int_t branchStyle = 1; //new style by default
//if (split < 0) {branchStyle = 0; split = -1-split;}
Int_t bufsize = 64000/4;
static TFile *fout = 0;
static TTree *tree = 0;
class St_db_Maker;
St_db_Maker *dbMk = 0;
Int_t run = 100000;
//________________________________________________________________________________
TFile *MakeTree() {
  if (fout) return fout;
  fout = new TFile("TpcAvgPowerSupplyTree.root","recreate");
  tree = new TTree("T","Anode Volatage information");
  tree->SetAutoSave(1000000000); // autosave when 1 Gbyte written
  tree->SetCacheSize(10000000);  //set a 10 MBytes cache (useless when writing local files)
  TTree::SetBranchStyle(branchStyle);
  TBranch *branch = tree->Branch("TpcAvgPowerSupply_st", &event, bufsize,split);
  branch->SetAutoDelete(kFALSE);
  if(split >= 0 && branchStyle) tree->BranchRef();
  return fout;
}
//________________________________________________________________________________
void TpcAvgPowerSupplyTree(Int_t date = 20190301, Int_t time = 1, Int_t dateEnd = 20200101) {
  if (! dbMk) dbMk = (St_db_Maker *) StChain::GetChain();
  StMaker::GetChain()->Init();
  MakeTree();
  TDatime t(date,time);
  TDatime tt[2];
  t.Print();
  StEvtHddr* hddr = StChain::GetChain()->GetEvtHddr();
  Int_t n = 0;
  while(1) {
    hddr->SetRunNumber(++run);
    hddr->SetDateTime(t);
    dbMk->Make();
    St_TpcAvgPowerSupplyC *tableC = St_TpcAvgPowerSupplyC::instance();
    St_TpcAvgPowerSupply *table =  (St_TpcAvgPowerSupply *)  tableC->Table();
    UInt_t u = t.Convert();
    dbMk->GetValidity(table,tt);
    cout << "got " << table->GetName() << "\tu " << u << endl;
    cout << "Validity:" << tt[0].GetDate() << "/" << tt[0].GetTime() << "\tu " << tt[0].Convert()	
	 << " -----   " << tt[1].GetDate() << "/" << tt[1].GetTime() << "\tu " << tt[1].Convert()   
	 << endl;
    if (n < 5) {
      n++;
      tableC->PrintC();
    }
    event =  table->GetTable();
    tree->Fill();
    t = tt[1];
    Int_t d = t.GetDate();
    if (d > dateEnd) return;
  }
}
