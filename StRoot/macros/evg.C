{ gROOT->Reset();
//  gSystem->Load("libEG.so");
  gSystem->Load("lib/St_base.so");
  gSystem->Load("lib/St_Tables.so");
  gSystem->Load("lib/StChain.so");

#ifndef __CINT__
#include "Rtypes.h"
#include "St_XDFFile.h"
#include "St_DataSet.h"
#include "St_Module.h"
#include "St_Table.h"
#include "StChain.h"
#include "StMaker.h"
#endif
  Char_t *filename = "/star/mds/data/SD98/auau200/bfc/central/hijing/set0001/regular/tss/auau_ce_b0-2_0001_0020.xdf";
  St_XDFFile xdffile_in(filename,"r");

// Create the main chain object
  StChain chain("StChain");

//  Create the makers to be called by the current chain
  St_xdfin_Maker xdfin("Xdfin","event");
  St_evg_Maker evg("evg","event");

  chain.PrintInfo();
  chain.SetInputXDFile(&xdffile_in);

// Init the mai chain and all its makers
  chain.Init();
  xdffile_in.CloseXDF();
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
  gBenchmark->Start("evg");
  char *dir_name = "/star/mds/data/SD98/auau200/evg/central/hijing/set0001/regular";
  printf ("%s \n",dir_name);
  void *dir = 0;
  if (dir = gSystem->OpenDirectory(dir_name)){ 
    char *file = 0;
    printf ("Directory %s is opened\n", dir_name);
    Long_t id, size, flags, modtime;
    Int_t  iok;
    Int_t nfiles = 0;
    while (file = gSystem.GetDirEntry(dir)) {
      //      flags = id = size = modtime = 0;
      iok = gSystem->GetPathInfo(file, &id, &size, &flags, &modtime);
      printf ("file = %s iok = %i\n", file, iok);
      if (iok == 1) { 
        printf("Open %s \n",file);
        if (!(xdffile_in.OpenXDF(gSystem->ConcatFileName(dir_name,file)))){
          nfiles++;
          Int_t i=0;
          for (i=0;i<200;i++){
            chain.Make(i);
            histCanvas->Modified();
            histCanvas->Update();
            chain.Clear(); 
	  }
        }
        xdffile_in.CloseXDF();
      }
      if (nfiles>2) break;
    }
  }
  gBenchmark->Stop("evg");
  gBenchmark->Print("evg");
  TBrowser b;
}
