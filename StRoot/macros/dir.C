{
  gROOT->Reset();
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
  gBenchmark->Start("evg");
//  char *dir_name = "/star/mds/data/SD98/auau200/evg/central/hijing/set0001/regular";
  char *dir_name = "/star/mds/data/SD98/auau200/bfc/central/hijing/set0001/regular/tss";
  printf ("%s \n",dir_name);
  void *dir = 0;
  if (dir = gSystem->OpenDirectory(dir_name)){ 
    char *n = 0;
    printf ("Directory %s is opened\n", dir_name);
    Long_t id, size, flags, modtime;
    Int_t  iok;
    while (n = gSystem.GetDirEntry(dir)) {
      //      flags = id = size = modtime = 0;
      iok = gSystem->GetPathInfo(n, &id, &size, &flags, &modtime);
      printf ("file = %s iok = %i\n", n, iok);
      if (iok == 1){
      //    if (iok && flags > 3 && !xdfile_in.OpenXDF(n)){
      printf("Open %s \n",n);
      //      for (i=0;i<200;i++){
      //        chain.Make(i);
      //        histCanvas->Modified();
      //        histCanvas->Update();
      //      }
      //      xdfile_in.CloseXDF();
      }
  }
  gBenchmark->Stop("evg");
  gBenchmark->Print("evg");
  }
}
