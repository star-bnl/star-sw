// $Id: bfcread_dst_QA_outhistfile.C,v 1.7 2000/01/18 16:38:05 kathy Exp $
// $Log: bfcread_dst_QA_outhistfile.C,v $
// Revision 1.7  2000/01/18 16:38:05  kathy
// add loading of StUtilities and StAnalysisUtilities so that StHistUtil class can now be picked up from StAnalysisUtilities library
//
// Revision 1.6  2000/01/13 16:55:11  kathy
// updating bfcread_dst*.C macros to use the new methods in StHistUtil which allow printing from a list; also make sure all libraries needed are loaded in the ones running St_QA_Maker; also update documentation
//
// Revision 1.5  2000/01/12 23:16:37  kathy
// add all libraries that are now needed to load for St_QA_Maker; add code for using new print methods - can't yet print from list though....
//
// Revision 1.4  2000/01/11 16:31:02  kathy
// change to current input file in Root2XDF.C and bfcread_dst_EventQA*.C; load St_global library in bfcread_dst_QA_outhistfile.C which is now needed when using St_QA_Maker class
//
// Revision 1.3  2000/01/10 21:57:16  kathy
// must now load St_global when running St_QA_Maker
//
// Revision 1.2  2000/01/05 22:12:03  kathy
// changed input file to current one
//
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
//
// standard Maker names in bfc 
//   (but if you run your own Maker here, then use whatever name you give it)
//  are listed at 
//  http://www.star.bnl.gov/STAR/html/comp_l/train/tut/bfc_maker_names.html
//
//======================================================================

class StChain;

StChain *chain;

void bfcread_dst_QA_outhistfile(
     Int_t nevents=10, 
     const Char_t *MainFile=
     "/star/rcf/test/dev/tfs_Linux/Mon/year_1b/hc_lowdensity/gstar.dst.root",
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
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_QA_Maker");
  gSystem->Load("tls");
  gSystem->Load("St_tpc");
  gSystem->Load("St_svt");
  gSystem->Load("St_global");
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
 














