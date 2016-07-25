#include "TFile.h"
#include "TObjArray.h"
#include "TStopwatch.h"

void ew()
{

  int MHITS = 500000;      
  int MHITZ = 100;
  int MTRKS = MHITS/MHITZ;
  char cbuf[100];
  TObjArray *evt = new TObjArray(10);
  TObjArray *hits = new TObjArray(MHITS);
  hits->SetName("Hits");
  TObjArray *trks = new TObjArray(MTRKS);
  TObjArray *vtx  = new TObjArray(MTRKS);
  trks->SetName("Trks");
  vtx->SetName("Vtx");
   evt->Add(hits);
   evt->Add(trks);
   evt->Add(vtx);

  int ih;
  printf("*** Hits started\n");
  for (ih=0;ih<MHITS;ih++) {
    sprintf(cbuf,"hit%d",ih);
    TNamed *hit = new TNamed(cbuf,"");
    hits->AddAt(hit,ih);
  }
  printf("*** Hits finished\n");
  
  int it;
  for (it=0;it<MTRKS;it++) {
    TObjArray *trk = new TObjArray(MHITZ);
    trks->Add(trk);
    vtx->Add(trk);
    for (ih = 0; ih <MHITZ; ih++) {
      trk->Add(hits->At(it*MHITZ+ih));
    }
  }
  printf("*** Traks finished\n");
   
   TFile ff("e.root","recreate","",0);
   printf("*** Write started\n");
   TStopwatch www;
   evt->Write("evt",TObject::kSingleKey);  
   www.Print();
   printf("*** Write finished\n");

}   
