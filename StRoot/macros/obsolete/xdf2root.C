// $Id: xdf2root.C,v 1.3 2000/01/25 16:06:38 fisyak Exp $
// $Log: xdf2root.C,v $
// Revision 1.3  2000/01/25 16:06:38  fisyak
// g2r -> g2t
//
// Revision 1.2  1999/05/21 15:34:02  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
// Revision 1.1  1999/02/17 16:55:25  kathy
// Proper name
//
// Revision 1.4  1999/02/16 18:15:49  fisyak
// Check in the latest updates to fix them
//
// Revision 1.3  1999/01/28 00:11:55  fisyak
// add g2t
//
// Revision 1.2  1999/01/26 16:04:15  fine
// GetEvent.C print out of the current input file name
//
// Revision 1.1  1999/01/21 00:54:27  fisyak
// Cleanup
//
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================

// This is test macro to read in xdf file with g2t tables 
// and write out the corresponding root files 

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
xdf2root(const Int_t   Nevents=10,
    const Char_t *fileinp = "/afs/rhic/star/data/samples/hijet-g2t.xdf",
    const Char_t *FileOut = "/afs/rhic/star/data/samples/hijet-g2t.root")
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();

  // set up files
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

  // do initialization
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");

//  chain->MakeTree("StChainTree","Transform g2t tables into Root file");
  gBenchmark->Start("bfc");
  out->Add(geant->GetName(),"geant_branch_name.root");
  if (root_out) {chain->Write();}

  // process events
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

  // at end of job:
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
