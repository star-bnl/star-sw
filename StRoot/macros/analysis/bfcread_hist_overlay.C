// $Id: bfcread_hist_overlay.C,v 1.7 2006/08/15 21:42:41 jeromel Exp $
// $Log: bfcread_hist_overlay.C,v $
// Revision 1.7  2006/08/15 21:42:41  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.6  2002/01/29 20:03:08  genevb
// Switched default dir from QA to EventQA
//
// Revision 1.5  2000/07/26 19:53:45  lansdell
// made changes for creating new QA histograms
//
// Revision 1.4  2000/04/12 15:06:53  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
//
// Revision 1.3  2000/03/20 17:32:55  kathy
// setbranches in all macros so that they will work with softlinks - for StIOMaker
//
// Revision 1.2  2000/02/14 20:30:41  kathy
// removing unneeded macros; updating documentation in bfcread macros
//
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
//
// Documentation on StHistUtil class is at:
//   http://duvall.star.bnl.gov/STARAFS/comp/pkg/dev/StRoot/StAnalysisUtilities/doc/
//
//======================================================================

class StChain;
StChain *chain;

class StIOMaker;
StIOMaker *IOMk=0;

//------------------------------------------------------------------------

void bfcread_hist_overlay(
  const Char_t *MainFile=
     "/afs/rhic.bnl.gov/star/data/samples/gstar.hist.root",
  const Char_t *MakerHistDir="EventQA",
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
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StUtilities");
    gSystem->Load("StAnalysisUtilities");


// setup chain with IOMaker - can read in .dst.root, .dst.xdf files
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,TopDirTree);
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
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
                   "TabQaGtrkRT","TabQaPtrkRT");
  if (result == kStErr)
    cout << " !!! There was an error in Overlay1D !!!" << endl;  

// 2Dim. overlay
  result = HU->Overlay2D(MakerHistDir,
                   "TabQaGtrkLengthVEtaT","TabQaPtrkLengthVEtaT");
  if (result == kStErr)
    cout << " !!! There was an error in Overlay2D !!!" << endl;  
   

  cout <<" bfcread_hist_overlay.C, end of macro" << endl;

}












