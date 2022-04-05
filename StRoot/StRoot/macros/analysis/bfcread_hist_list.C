// $Id: bfcread_hist_list.C,v 1.22 2006/08/15 21:42:39 jeromel Exp $ 
// $Log: bfcread_hist_list.C,v $
// Revision 1.22  2006/08/15 21:42:39  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.21  2002/01/29 20:03:08  genevb
// Switched default dir from QA to EventQA
//
// Revision 1.20  2000/06/13 18:41:59  kathy
// had to move order of library loading for some unknown reason...
//
// Revision 1.19  2000/06/13 00:58:59  lansdell
// added libglobal_Tables to resolve crashes
//
// Revision 1.18  2000/04/12 15:06:52  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
//
// Revision 1.17  2000/03/20 17:32:55  kathy
// setbranches in all macros so that they will work with softlinks - for StIOMaker
//
// Revision 1.16  2000/02/14 20:30:40  kathy
// removing unneeded macros; updating documentation in bfcread macros
//
// Revision 1.15  2000/01/19 16:29:51  kathy
// update macros to use default input files in /afs/rhic.bnl.gov/star/data/samples
//
// Revision 1.14  2000/01/18 16:38:05  kathy
// add loading of StUtilities and StAnalysisUtilities so that StHistUtil class can now be picked up from StAnalysisUtilities library
//
// Revision 1.13  2000/01/13 17:18:04  kathy
// updated bfcread_hist* macros so that they can now use the new *PrintList methods from StHistUtil to only print a subset (given a list of names) of histograms from the given Maker Directory; also updated documentation
//
// Revision 1.12  2000/01/05 22:12:03  kathy
// changed input file to current one
//
// Revision 1.11  1999/12/01 21:30:11  kathy
// added input TopDirTree to bfcread_hist* macros in order to tell which top level directory hist file has since sometimes its not bfcTree; cleaned up print statements in bfcread_dst*hist.C macros; two new macros bfcread_dst_*QA_outhistfile.C added which read dst file and book and fill histograms and write out a new *.hist.root file, instead of just sending hist to postscript - this new *.hist.root file can then be read into bfcread_hist*.C to look at it --- note that topdirtree is different!
//
// Revision 1.10  1999/11/30 19:23:05  kathy
// changed bfcread_dst*.C so that MakerHist is hardwired in instead of being input; wrote better documentation in bfcread_hist*.C so that it explains where top level directory is set
//
// Revision 1.9  1999/11/23 20:40:48  genevb
// Re-arranged load order
//
// Revision 1.8  1999/11/19 20:13:21  kathy
// cleaned up macros to remove uneccessary lines; also added info about new tables to QA* macros
//
// Revision 1.7  1999/11/05 16:30:16  kathy
// minor changes to documentation in macro
//
// Revision 1.6  1999/11/03 21:35:35  kathy
// small fixes for use of StIOMaker - had it wrong before
//
// Revision 1.5  1999/11/03 19:02:54  kathy
// changes to default input files and output file names - needed by perl script for testing
//
// Revision 1.4  1999/11/03 17:13:00  kathy
// fixed macros so they use StIOMaker instead of StTreeMaker
//
// Revision 1.3  1999/09/21 15:07:02  kathy
// change to have notes on input values at top of each macro, also clean up notes on usage and remove the usage of method St_QA_Maker::SetPntrToHistUtil which is not going to be used now that I made St_QA_Maker totally independent of the histogram printing
//
// Revision 1.2  1999/09/20 20:09:01  kathy
// bfcread_hist_list_all now lists all histograms in hist.root file; bfcread_hist_list now only lists those that are in the Maker that is input; bfcread_hist_to_ps prints and draws the hist that are in the input Maker, bfcread_dst_QAhist.C reads .dst.root file - runs QA_Maker and prints and draws the QA histograms
//
//
//======================================================================
// owner:  Kathy Turner
// what it does:  see below
//=======================================================================
// bfcread_hist_list.C 
//
// what it does: reads the *.hist.root file produced from a chain 
//               (such as bfc) and
//               then prints list of histograms from given input Maker
//
// inputs: MainFile - *.hist.root file from bfc output
//         MakerHistDir - directory name of Maker that you want histograms 
//                   from (this will be first input when you did constructor)
//             -- see standard Maker names note below!
//         TopDirTree - top level directory tree in your input hist file
//                (this is 3rd argument of constructor for StTreeMaker that
//                 you probably used to write the *.hist.root file)
//            NOTE: if you ran bfc, then the TopDirTree = bfcTree !!
//
// standard Maker names in bfc 
//   (but if you run your own Maker here, then use whatever name you give it)
//  are listed at 
//  http://www.star.bnl.gov/STAR/html/comp_l/train/tut/bfc_maker_names.html
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

void bfcread_hist_list(
  const Char_t *MainFile=
    "/afs/rhic.bnl.gov/star/data/samples/gstar.hist.root",
  const Char_t *MakerHistDir="EventQA",
  const Char_t *TopDirTree="bfcTree")
{

  cout << "bfcread_hist_list.C, input hist file = " 
       << MainFile << endl;
  cout << "bfcread_hist_list.C, directory name for hist = " 
       << MakerHistDir << endl;
  cout << "bfcread_hist_list.C, top level directory in hist file = " 
       << TopDirTree << endl;

//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StUtilities");
    gSystem->Load("StAnalysisUtilities");
    gSystem->Load("libglobal_Tables");

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

// method to print out list of histograms
// - can do this anytime after they're booked
// - default is to print out QA hist branch
   Int_t NoHist=0;
   NoHist = HU->ListHists(MakerHistDir);
   cout << " in bfcread_hist_list: Num of Hist = " << NoHist << endl;
      
}
 






