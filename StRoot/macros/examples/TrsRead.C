//
// $Id: TrsRead.C,v 1.1 1999/11/09 19:10:47 calderon Exp $
//
// Description:
// Read in a .trs file created with TrsWrite.C
//
///////////////////////////////////////////////////////
// $Log: TrsRead.C,v $
// Revision 1.1  1999/11/09 19:10:47  calderon
// Initial Commit
// Example macro to read .trs file.
//
//
////////////////////////////////////////////////////////
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("libm");
  gSystem->Load("StUtilities");
  gSystem->Load("StChain");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTrsMaker");
}
void TrsRead(const Int_t Nevents=1)
{
  if (gClassTable->GetID("StChain") < 0) Load();
  chain = new StChain("trs");
  chain->SetDebug();

  //  Create the makers to be called by the current chain
  StTrsMaker    *tpc_raw = new StTrsMaker("Trs");

  // Tell TRS to read the file.
  tpc_raw->readFile("test.trs");

  // Init the main chain and all its makers
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  gBenchmark->Start("trs");
  
  for (Int_t i =1; i <= Nevents; i++){
    if (chain->Make(i)) break;
    
    if (i != Nevents) chain->Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }
  if (Nevents > 1) {
    chain->Finish();
    gBenchmark->Print("trs");
  }
  else b = new TBrowser;

}
