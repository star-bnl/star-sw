// $Id: bfcread_hist_to_ps.C,v 1.12 1999/12/01 21:30:11 kathy Exp $ 
// $Log: bfcread_hist_to_ps.C,v $
// Revision 1.12  1999/12/01 21:30:11  kathy
// added input TopDirTree to bfcread_hist* macros in order to tell which top level directory hist file has since sometimes its not bfcTree; cleaned up print statements in bfcread_dst*hist.C macros; two new macros bfcread_dst_*QA_outhistfile.C added which read dst file and book and fill histograms and write out a new *.hist.root file, instead of just sending hist to postscript - this new *.hist.root file can then be read into bfcread_hist*.C to look at it --- note that topdirtree is different!
//
// Revision 1.11  1999/11/30 19:23:06  kathy
// changed bfcread_dst*.C so that MakerHist is hardwired in instead of being input; wrote better documentation in bfcread_hist*.C so that it explains where top level directory is set
//
// Revision 1.10  1999/11/23 20:42:16  genevb
// Re-arranged load order
//
// Revision 1.9  1999/11/19 20:13:22  kathy
// cleaned up macros to remove uneccessary lines; also added info about new tables to QA* macros
//
// Revision 1.8  1999/11/05 22:50:40  kathy
// now input global title for all output pages in the macro for printing & drawing histograms
//
// Revision 1.7  1999/11/05 16:30:16  kathy
// minor changes to documentation in macro
//
// Revision 1.6  1999/11/03 21:35:35  kathy
// small fixes for use of StIOMaker - had it wrong before
//
// Revision 1.5  1999/11/03 19:02:55  kathy
// changes to default input files and output file names - needed by perl script for testing
//
// Revision 1.4  1999/11/03 17:13:02  kathy
// fixed macros so they use StIOMaker instead of StTreeMaker
//
// Revision 1.3  1999/09/21 15:07:03  kathy
// change to have notes on input values at top of each macro, also clean up notes on usage and remove the usage of method St_QA_Maker::SetPntrToHistUtil which is not going to be used now that I made St_QA_Maker totally independent of the histogram printing
//
// Revision 1.2  1999/09/20 20:28:12  kathy
// fix to give macro correct name
//
// Revision 1.1  1999/09/20 20:09:02  kathy
// bfcread_hist_list_all now lists all histograms in hist.root file; bfcread_hist_list now only lists those that are in the Maker that is input; bfcread_hist_to_ps prints and draws the hist that are in the input Maker, bfcread_dst_QAhist.C reads .dst.root file - runs QA_Maker and prints and draws the QA histograms
//
//
//======================================================================
// owner:  Kathy Turner
// what it does:  see below
//=======================================================================
// bfcread_hist_to_ps.C 
//
// what it does: reads the *.hist.root file produced from a chain 
//               (such as bfc) and
//               then draws & sends to ps file the 
//                histograms from given input Maker
//
// inputs: MainFile - *.hist.root file from bfc output
//         MakerHistDir - directory name of Maker that you want histograms 
//                   from (this will be first input when you did constructor)
//             -- see standard Maker names note below!
//         TopDirTree - top level directory tree in your input hist file
//                (this is 3rd argument of constructor for StTreeMaker that
//                 you probably used to write the *.hist.root file)
//           NOTE: if you ran bfc, then the TopDirTree = bfcTree !!
//         psFile - output postscript file name
//         PageTitle - title at top of each page - if it's "", then it's
//                set to MainFile by default
// 
//
// standard Maker names in bfc ==>
//   (but if you run your own Maker here, then use whatever name you give it)
//  are listed at 
// http://duvall.star.bnl.gov/STARAFS/comp/pkg/dev/StRoot/St_QA_Maker/doc/
//
//
//======================================================================

class StChain;
StChain *chain;

class StIOMaker;
StIOMaker *IOMk=0;

//------------------------------------------------------------------------

void bfcread_hist_to_ps(
  const Char_t *MainFile=
     "/star/rcf/test/dev/tfs_Linux/Tue/year_1b/set0352_01_35evts.hist.root",
  const Char_t *MakerHistDir="QA",
  const Char_t *TopDirTree="bfcTree",
  const Char_t *psFile="QA_hist.ps",
  const Char_t *PageTitle="")
{             

  cout << "bfcread_hist_to_ps.C, input hist file = " 
       << MainFile << endl;
  cout << "bfcread_hist_to_ps.C, directory name for hist = " 
       << MakerHistDir << endl;
  cout << "bfcread_hist_to_ps.C, top level directory in hist file = " 
       << TopDirTree << endl;
  cout << "bfcread_hist_to_ps.C, output ps file  = " 
       << psFile << endl;
  cout << "bfcread_hist_to_ps.C, page title for histograms = " 
       << PageTitle << endl;

//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("St_QA_Maker");

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
//   - not St_QA_Maker
//   - I'm just using methods from St_QA_Maker...
// --- now execute chain member functions - 1 event (histograms) only
  IOMk->Init();
  IOMk->Clear();
  IOMk->Make();

// method to print out list of histograms
// - can do this anytime after they're booked
// - default is to print out QA hist branch
   Int_t NoHist=0;
   NoHist = HU->ListHists(MakerHistDir);
   cout << " in bfcread_hist_list: Num of Hist = " << NoHist << endl;
      
// Set the default canvas style to plain (so it won't print out grey!)
    gROOT->SetStyle("Plain");
//    gStyle->SetOptStat(111111);

    HU->SetHistsNamesDraw("*","*");
    HU->SetPostScriptFile(psFile);
    HU->SetZones(2,3);
    HU->SetPaperSize();
    HU->SetDefaultLogYList(MakerHistDir);
      if (PageTitle=="") PageTitle=MainFile;
    HU->SetGlobalTitle(PageTitle);

   Int_t numLog = 0;
   numLog = HU->ExamineLogYList();
   cout <<" bfcread_hist_to_ps.C, Number hist to plot with log scale = " << numLog << endl;

//  Now draw the actual histograms to canvas and to ps file
    HU->DrawHists(MakerHistDir);
   
   cout <<" bfcread_hist_to_ps.C, end of macro" << endl;

}












