// $Id: bfcread_dst_QA_outhistfile.C,v 1.1 1999/12/01 21:30:11 kathy Exp $
// $Log: bfcread_dst_QA_outhistfile.C,v $
// Revision 1.1  1999/12/01 21:30:11  kathy
// added input TopDirTree to bfcread_hist* macros in order to tell which top level directory hist file has since sometimes its not bfcTree; cleaned up print statements in bfcread_dst*hist.C macros; two new macros bfcread_dst_*QA_outhistfile.C added which read dst file and book and fill histograms and write out a new *.hist.root file, instead of just sending hist to postscript - this new *.hist.root file can then be read into bfcread_hist*.C to look at it --- note that topdirtree is different!
//
//
//======================================================================
// owner:  Kathy Turner
// what it does:  see below
//=======================================================================
// bfcread_dst_QA_outhistfile.C 
//
// Kathy's notes (12/1/99):
//   - reads from .dst.root or .xdf.root file produced from bfc.C 
//   - opens output histogram file
//   - run maker (St_QA_Maker) to book,fill histograms
//   - writes out histograms to *.hist.root file
//
//
// inputs: 
//         nevents - # events to process
//         MainFile - input dst file from bfc output
//         outHistFile - output hist file name --> outHistFile.hist.root
//         TopDirTree - top level directory tree in your input hist file
//                (this is 3rd argument of constructor for StTreeMaker that
//                 you probably used to write the *.hist.root file)
//           NOTE: if you ran bfc, then the TopDirTree = bfcTree !!
//         MakerHistDir - directory name of Maker that you want histograms 
//                   from (this will be first input when you did constructor)
//             -- see standard Maker names note below!
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

void bfcread_dst_QA_outhistfile(
     Int_t nevents=2, 
     const Char_t *MainFile=
     "/star/rcf/test/dev/tfs_Solaris/Fri/year_1b/set0352_01_35evts.dst.root",
     const Char_t *outHistFile="QAMaker",
     const Char_t *TopDirTree="QAtree",
     const Char_t *MakerHistDir="QA")
{
//
  cout << "bfcread_dst_QA_outhistfile.C, num events to process " 
       << nevents  << endl;
  cout << "bfcread_dst_QA_outhistfile.C, input file name       " 
       << MainFile << endl;
  cout << "bfcread_dst_QA_outhistfile.C, Maker directory name  " 
       << MakerHistDir<< endl;
  cout << "bfcread_dst_QA_outhistfile.C, output hist name      " 
       << outHistFile<< endl;
  cout << "bfcread_dst_QA_outhistfile.C, output top level directory name " 
       << TopDirTree<< endl;

  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_QA_Maker");
  gSystem->Load("StTreeMaker");

//  Setup top part of chain
  chain = new StChain("bfc");
  chain->SetDebug();
   
// Input File Maker
    StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");

// constructor for other class  (not a Maker so not used in chain)
   StHistUtil   *HU  = new StHistUtil;

// now must set pointer to StMaker so HistUtil can find histograms
//  with StHistUtil methods
// -- input any maker pointer but must cast as type StMaker
   HU->SetPntrToMaker((StMaker *)IOMk);

//  add other makers to chain:
  St_QA_Maker  *QA  = new St_QA_Maker(MakerHistDir,"title-notused");

// output hist.root file:
   StTreeMaker* treeMk = new StTreeMaker("tree",outHistFile,TopDirTree);
      treeMk->SetIOMode("w");
      treeMk->SetBranch("histBranch");

// --- now execute chain member functions
  chain->Init();
 
// method to print out list of histograms - can do this anytime after they're booked
  Int_t NoHist=0;
  NoHist = HU->ListHists(MakerHistDir);
  cout << " bfcread_dst_QA_outhistfile.C, No. of Hist we have == " << NoHist << endl;

// loop over events:
  int iev=0,iret=0, evnum=0;
 EventLoop: if (iev<nevents && !iret) {  // goto loop code
   evnum=iev+1;
   cout <<  " bfcread_dst_QA_outhistfile.C, processing event !!! " << evnum << endl ;
   chain->Clear();
   iret = chain->Make();
   iev++;                                // goto loop code
   goto EventLoop;                       // goto loop code
 }

  cout <<  " bfcread_dst_QA_outhistfile.C, passed chain->Make !!!" << endl ;

  chain->Finish();
    cout <<  "bfcread_dst_QA_outhistfile.C, passed chain->Finish" << endl ; 
   
}
 














