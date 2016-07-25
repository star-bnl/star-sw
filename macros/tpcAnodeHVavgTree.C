#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TDatime.h"
#include "StEvtHddr.h"
#include "StChain.h"
#include "StDetectorDbMaker/St_tpcAnodeHVavgC.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StChain/StEvtHddr.h"
#endif
class tpcAnodeHVavg_t {
public:
  tpcAnodeHVavg_t() {}
  virtual ~tpcAnodeHVavg_t() {}
  UInt_t time;
  Double_t innerVoltages, outerVoltages;
};
tpcAnodeHVavg_t tpcAnodeHVavg;
tpcAnodeHVavg_t *event = &tpcAnodeHVavg;
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
TFile *tpcAnodeHVavgTree() {
  if (fout) return fout;
  fout = new TFile("tpcAnodeHVC.root","recreate");
  tree = new TTree("tpcAnodeHVavg","Anode Volatage information");
  tree->SetAutoSave(1000000000); // autosave when 1 Gbyte written
  tree->SetCacheSize(10000000);  //set a 10 MBytes cache (useless when writing local files)
  TTree::SetBranchStyle(branchStyle);
  TBranch *branch = tree->Branch("TpcAHV", &event, bufsize,split);
  branch->SetAutoDelete(kFALSE);
  if(split >= 0 && branchStyle) tree->BranchRef();
  return fout;
}
//________________________________________________________________________________
void tpcAnodeHVavgC(UInt_t &u) {
  if (! dbMk) dbMk = (St_db_Maker *) StChain::GetChain();
  TDatime t;
  TDatime tt[2];
  t.Set(u); t.Print();
  StEvtHddr* hddr = StChain::GetChain()->GetEvtHddr();
  hddr->SetRunNumber(++run);
  hddr->SetDateTime(t);
  dbMk->Make();
  St_tpcAnodeHVavgC *tableC = St_tpcAnodeHVavgC::instance();
  Double_t innerSectorAnodeVoltage = 0, outerSectorAnodeVoltage = 0;
  Int_t nAliveInner = 0;
  Int_t nAliveOuter = 0;
  
  St_tpcAnodeHVavg *table =  (St_tpcAnodeHVavg *)  tableC->Table();
  dbMk->GetValidity(table,tt);
  cout << "got " << table->GetName() << "\tu " << u << endl;
  cout << "Validity:" << tt[0].GetDate() << "/" << tt[0].GetTime() << "\tu " << tt[0].Convert()	
       << " -----   " << tt[1].GetDate() << "/" << tt[1].GetTime() << "\tu " << tt[1].Convert()   
       << endl;
  for (Int_t sec = 1; sec <= 24; sec++) {
    for (Int_t row = 1; row <= 45; row++) {
      if (tableC->livePadrow(sec,row)) {
	if (row <= 13) {
	  nAliveInner++;
	  innerSectorAnodeVoltage += tableC->voltagePadrow(sec,row);
	} else {
	  nAliveOuter++;
	  outerSectorAnodeVoltage += tableC->voltagePadrow(sec,row);
	}
      }
    }
  }
  if (nAliveInner > 1) innerSectorAnodeVoltage /= nAliveInner;
  if (nAliveOuter > 1) outerSectorAnodeVoltage /= nAliveOuter;
  cout << "innerSectorAnodeVoltage = " << innerSectorAnodeVoltage << "\touterSectorAnodeVoltage = " << outerSectorAnodeVoltage << endl;
  tpcAnodeHVavg.time = u;
  tpcAnodeHVavg.innerVoltages = innerSectorAnodeVoltage;
  tpcAnodeHVavg.outerVoltages = outerSectorAnodeVoltage;
  tree->Fill();
  u = tt[1].Convert();
}
