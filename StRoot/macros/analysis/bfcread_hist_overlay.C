// $Id: bfcread_hist_overlay.C,v 1.1 2000/01/31 18:48:41 kathy Exp $
// $Log: bfcread_hist_overlay.C,v $
// Revision 1.1  2000/01/31 18:48:41  kathy
// new macro to show how to use the Overlay methods that Curtis wrote - just by themselves
//
//======================================================================
// owner:  Kathy Turner / Curtis Lansdell
// what it does:  see below
//=======================================================================
// bfcread_hist_overlay.C
//
// what it does: reads the *.hist.root file produced from a chain 
//               (such as bfc) and then uses the Overlay* methods
//               from StHistUtil class to overlay 2 histograms
//               (this does a 1dim and 2dim example)
//
// inputs: MainFile - *.hist.root file from bfc output
//         MakerHistDir - directory name of Maker that you want histograms 
//                   from (this will be first input when you did constructor)
//             -- see standard Maker names note below!
//         TopDirTree - top level directory tree in your input hist file
//                (this is 3rd argument of constructor for StTreeMaker that
//                 you probably used to write the *.hist.root file)
//           NOTE: if you ran bfc, then the TopDirTree = bfcTree !!
//         PageTitle - title at top of each page - if it's "", then it's
//                set to MainFile by default
//
//======================================================================

class StChain;
StChain *chain;

class StIOMaker;
StIOMaker *IOMk=0;

//------------------------------------------------------------------------

void bfcread_hist_overlay(
  const Char_t *MainFile=
     "/afs/rhic/star/data/samples/gstar.hist.root",
  const Char_t *MakerHistDir="QA",
  const Char_t *TopDirTree="bfcTree",
  const Char_t *PageTitle="")
{             

  cout << "bfcread_hist_overlay.C, input hist file = " 
       << MainFile << endl;
  cout << "bfcread_hist_overlay.C, directory name for hist = " 
       << MakerHistDir << endl;
  cout << "bfcread_hist_overlay.C, top level directory in hist file = " 
       << TopDirTree << endl;
  cout << "bfcread_hist_overlay.C, hist page title " << 
     PageTitle  << endl;

//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StUtilities");
    gSystem->Load("StAnalysisUtilities");


// setup chain with IOMaker - can read in .dst.root, .dst.xdf files
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,TopDirTree);
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
//  IOMk->SetBranch("tpc_tracks",0,"r"); //activate tpc_tracks Branch
//  IOMk->SetBranch("geantBranch",0,"r"); //activate geant Branch
  IOMk->SetBranch("histBranch",0,"r"); //activate dst Branch


// constructor for other maker (not used in chain)
   StHistUtil   *HU  = new StHistUtil;

// now must set pointer to StMaker so HistUtil can find histograms
//  with StHistUtil methods
// -- input any maker pointer but must cast as type StMaker
   HU->SetPntrToMaker((StMaker *)IOMk);

// ONLY use StIOMaker in chain 
// --- now execute chain member functions - 1 event (histograms) only
  IOMk->Init();
  IOMk->Clear();
  IOMk->Make();

  cout <<" bfcread_hist_overlay.C, have read histograms" << endl;

  if (PageTitle=="") PageTitle=MainFile;
  HU->SetGlobalTitle(PageTitle);

// 1Dim. overlay
  Int_t result = HU->Overlay1D(MakerHistDir,
                   "TabQaGtrkRT","TabQaPtrkR");
  if (result == kStErr)
    cout << " !!! There was an error in Overlay1D !!!" << endl;  

// 2Dim. overlay
  result = HU->Overlay2D(MakerHistDir,
                   "TabQaGtrkLengthVEtaT","TabQaPtrkLengthVEta");
  if (result == kStErr)
    cout << " !!! There was an error in Overlay2D !!!" << endl;  
   

  cout <<" bfcread_hist_overlay.C, end of macro" << endl;

}












