// $Id: g2r.C,v 1.4 1999/02/16 18:15:49 fisyak Exp $
// $Log: g2r.C,v $
// Revision 1.4  1999/02/16 18:15:49  fisyak
// Check in the latest updates to fix them
//
// Revision 1.3  1999/01/28 00:11:55  fisyak
// add g2r
//
// Revision 1.2  1999/01/26 16:04:15  fine
// GetEvent.C print out of the current input file name
//
// Revision 1.1  1999/01/21 00:54:27  fisyak
// Cleanup
//

TBrowser *b = 0;
class StChain;
StChain  *chain=0;

void Load(){
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("xdf2root");
    gSystem->Load("St_Tables");
    gSystem->Load("St_xdfin_Maker");
    gSystem->Load("St_io_Maker");
    gSystem->Load("St_TLA_Maker");
}
g2r(const Int_t   Nevents=10,
    const Char_t *fileinp = "/afs/rhic/star/data/samples/hijet-g2t.xdf",
    const Char_t *FileOut = "/afs/rhic/star/data/samples/hijet-g2t.root")
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
  St_XDFFile   *xdf_in   = 0;
  if (fileinp)  xdf_in   = new St_XDFFile(fileinp,"r");
  TFile       *root_out  = 0; 
  if (FileOut) root_out  =  new TFile(FileOut,"RECREATE");
  //  if (FileOut) root_out  =  new TFile(FileOut,"UPDATE");
  // Create the main chain object
  if (chain) delete chain;
  chain = new StChain("bfc");
  if (xdf_in) {
    St_xdfin_Maker *xdfin = new St_xdfin_Maker("xdfin");
    chain->SetInputXDFile(xdf_in);
  }
  St_TLA_Maker    *geant = new St_TLA_Maker("geant","event/geant/Event");
  St_io_Maker *out  = new St_io_Maker("Output","all");
  chain->PrintInfo();
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
//  chain->MakeTree("StChainTree","Transform g2t tables into Root file");
  gBenchmark->Start("bfc");
  out->Add(geant->GetName(),"geant_branch_name.root");
  if (root_out) {chain->Write();}
  Int_t i=0;
  for (Int_t i =1; i <= Nevents; i++){
    if (chain->Make(i)) break;
    St_DataSet *dst = chain->DataSet("dst");
    if (root_out){
      gBenchmark->Start("root i/o");
 //     root_out->cd();
//      cout << "============================ bytes written =" << chain->FillTree() << endl;
//      chain->Tree()->ls();
      gBenchmark->Stop("root i/o");
    }

    if (i != Nevents) chain->Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }
  if (Nevents > 1) {
    chain->Finish();
    if (root_out){ 
//      chain->Tree()->ls();
//      chain->Tree()->Write();
      root_out->Write();
      root_out->Close();   
      delete root_out;
      gBenchmark->Print("root i/o");
    }
    gBenchmark->Print("bfc");
  }
  else {if (!b)   b = new TBrowser;}
}
