// $Id: clone.C,v 1.1 1999/01/23 18:38:50 fisyak Exp $
// $Log: clone.C,v $
// Revision 1.1  1999/01/23 18:38:50  fisyak
// Cleanup for SL98l
//
// Revision 1.1  1998/11/01 16:42:29  fisyak
// dst analysis
//
{
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("global");
  gSystem->Load("St_global");
  gSystem->Load("St_dst_Maker");
  const Int_t Nevents=10;
  const Char_t *FileOut = "clone.root";
  TFile       *root_out  = 0; 
  if (FileOut) root_out  =  new TFile(FileOut,"RECREATE");

  StChain *chain = new StChain("bfc");

  St_dst_Maker  *dst= new St_dst_Maker("dst","event/data/global/dst");
  dst->Save();
  chain->PrintInfo();
// Init the mai chain and all its makers
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  gBenchmark->Start("dst");
  Int_t i=0;
  chain->MakeTree("dst_Tree","Proba 1");
  for (Int_t i =1; i <= Nevents; i++){
    if (chain->Make(i)) break;
    root_out->cd();
    chain->FillClone();
    if (i != Nevents) chain->Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }
  if (Nevents > 1) {
    chain->Finish();
    gBenchmark->Print("dst");
  }
    if (root_out){
      root_out->Write();   
      root_out->Close();   
      delete root_out;
    }

  else TBrowser b;
#if 0
#endif
    //  delete dst;
    //  delete chain;
}
