{
  gSystem->Load("lib/St_base.so");
  gSystem->Load("lib/St_Tables.so");
  gSystem->Load("lib/StChain.so");

#ifndef __CINT__
#include "Rtypes.h"
#include "St_XDFFile.h"
#include "St_DataSet.h"
#include "St_Module.h"
#include "St_Table.h"
#endif
//gSystem.Exec("rm *.log");
  Char_t *filename = "/star/mds/data/SD98/auau200/bfc/central/hijing/set0001/regular/tss/auau_ce_b0-2_0001_0020.xdf";
  St_XDFFile xdffile_in(filename,"r");

// Create the main chain object
  StChain chain("StChain");

//  Create the makers to be called by the current chain
  St_xdfin_Maker xdfin("Xdfin");
  St_evg_Maker evg("evg","event");

  chain.PrintInfo();
  chain.SetInputXDFile(&xdffile_in);

// Init the mai chain and all its makers
  chain.Init();
// Prepare TCanvas to show some histograms created by makers
  TCanvas *histCanvas = new TCanvas("histCanvas");
  if (histCanvas) {
    // Create the list of the titles of the histograms we want to plot during this session
    Char_t *titles[]={"Particle pt versus eta"};
    Int_t listsize  = 1;
    TIter nexthist(evg.Histograms());
    TH1 *hist=0;

    while (hist = (TH1 *) nexthist()) {
      Int_t i = 0;
      Char_t *name = hist->GetTitle();
      for (i=0;i<listsize;i++) if (strcmp(name,titles[i++])==0) hist->Draw();
    }
  }
for (i=0;i<2;i++){
  chain.Make(i);
  histCanvas->Modified();
  histCanvas->Update();
}
  TBrowser b;
}
