#include "TFile.h"
#include "Riostream.h"
#include "StiUtilities/StiDebug.h"
#include "TNtuple.h"
#include "StMaker.h"
#include "TDataSet.h"
TNtuple *tuple = 0;
TFile *fOut = 0;
//________________________________________________________________________________
void Init() {
  fOut = new TFile("StiAux.root","recreate");
  Char_t *names ="xnl:ynl:znl:xhl:yhl:zhl:xul:yul:zul:ca:nYY:nZZ:hYY:hZZ:uYY:uZZ:xng:yng:zng:xhg:yhg:zhg:psi:dip:rho:chi2";
  tuple = new TNtuple("StiAux","the smoother infomation",names);
}
//________________________________________________________________________________
void stiAux(Int_t Nevent=100) {
  if (! fOut) Init();
  for (Int_t ev = 0; ev < Nevent; ev++) {
    if (StMaker::GetChain()->Make()) break;
    TDataSet* pEvent = (TDataSet*) StMaker::GetChain()->GetInputDS("StEvent");
    if (! pEvent) return;
    StiAux *myAux = (StiAux*)pEvent->Find("StiAux");
    if (! myAux) return;
    //  e->Clear("C");
    StiAux_t *myRes = 0;
    Int_t jhit = 1;
    while ((myRes = myAux->Get(jhit))) {
      jhit++;
      //    cout << myRes->xnl[0] << "\t" << myRes->xnl[1] << "\t" << myRes->xnl[2] << endl; 
      //    e->AddHit(myRes);
      tuple->Fill(&(myRes->xnl[0]));
    }
    StMaker::GetChain()->Clear();
  }
  //  tree->Fill(); 
}
