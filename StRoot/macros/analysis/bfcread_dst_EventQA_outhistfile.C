// $Id: bfcread_dst_EventQA_outhistfile.C,v 1.9 2000/03/20 17:32:54 kathy Exp $
// $Log: bfcread_dst_EventQA_outhistfile.C,v $
// Revision 1.9  2000/03/20 17:32:54  kathy
// setbranches in all macros so that they will work with softlinks - for StIOMaker
//
// Revision 1.8  2000/03/17 23:10:05  kathy
// make sure the dst branch is explicitly set in the macros using dst.root files as input - otherwise they don't work properly with soft links
//
// Revision 1.7  2000/02/14 20:30:40  kathy
// removing unneeded macros; updating documentation in bfcread macros
//
// Revision 1.6  2000/01/19 16:29:50  kathy
// update macros to use default input files in /afs/rhic/star/data/samples
//
// Revision 1.5  2000/01/18 16:38:05  kathy
// add loading of StUtilities and StAnalysisUtilities so that StHistUtil class can now be picked up from StAnalysisUtilities library
//
// Revision 1.4  2000/01/18 15:09:58  kathy
// setbranch runco so this branch of file will also be opened
//
// Revision 1.3  2000/01/13 16:55:11  kathy
// updating bfcread_dst*.C macros to use the new methods in StHistUtil which allow printing from a list; also make sure all libraries needed are loaded in the ones running St_QA_Maker; also update documentation
//
// Revision 1.2  2000/01/11 16:31:02  kathy
// change to current input file in Root2XDF.C and bfcread_dst_EventQA*.C; load St_global library in bfcread_dst_QA_outhistfile.C which is now needed when using St_QA_Maker class
//
// Revision 1.1  1999/12/01 21:30:10  kathy
// added input TopDirTree to bfcread_hist* macros in order to tell which top level directory hist file has since sometimes its not bfcTree; cleaned up print statements in bfcread_dst*hist.C macros; two new macros bfcread_dst_*QA_outhistfile.C added which read dst file and book and fill histograms and write out a new *.hist.root file, instead of just sending hist to postscript - this new *.hist.root file can then be read into bfcread_hist*.C to look at it --- note that topdirtree is different!
//
//
//======================================================================
// owner:  Curtis Lansdell, Kathy Turner
// what it does: 
//    - reads a *.dst.root OR .dst.xdf file from bfc.C 
//    - sets dst & runco branches (uses both)
//    - runs StEventMaker (loads StEvent) 
//    - runs StEventQAMaker (books & fills hist)
//    - opens & writes histograms to output hist file 
//
//=======================================================================
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
// standard Maker names in bfc,doEvents
//   (but if you run your own Maker here, then use whatever name you give it)
//  are listed at 
//  http://www.star.bnl.gov/STAR/html/comp_l/train/tut/bfc_maker_names.html
//                                                     doEvents_maker_names.html
//
// Documentation on StEventQAMaker class is at:
//   http://duvall.star.bnl.gov/STARAFS/comp/pkg/dev/StRoot/St_QA_Maker/doc/
//
// Documentation on StHistUtil class is at:
//   http://duvall.star.bnl.gov/STARAFS/comp/pkg/dev/StRoot/StAnalysisUtilities/doc/
//
//======================================================================

class StChain;
StChain *chain;

void bfcread_dst_EventQA_outhistfile(
     Int_t nevents=10, 
     const Char_t *MainFile=
     "/afs/rhic/star/data/samples/gstar.dst.root",
     const Char_t *outHistFile="StEQAMaker",
     const Char_t *TopDirTree="StEQAtree",
     const Char_t *MakerHistDir="StEQA")
{
//
  cout << "bfcread_dst_EventQA_outhistfile.C, num events to process " 
       << nevents  << endl;
  cout << "bfcread_dst_EventQA_outhistfile.C, input file name       " 
       << MainFile << endl;
  cout << "bfcread_dst_EventQA_outhistfile.C, Maker directory name  " 
       << MakerHistDir<< endl;
  cout << "bfcread_dst_EventQA_outhistfile.C, output hist name      " 
       << outHistFile<< endl;
  cout << "bfcread_dst_EventQA_outhistfile.C, output top level directory name " 
       << TopDirTree<< endl;

  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_QA_Maker"); 
  gSystem->Load("StTreeMaker");

  gSystem->Load("StEvent");
  gSystem->Load("StEventMaker");


//  Setup top part of chain
  chain = new StChain("MyChain");
  chain->SetDebug();
   
// Input File Maker
    StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
    IOMk->SetIOMode("r");
    IOMk->SetBranch("*",0,"0");                 //deactivate all branches
    IOMk->SetBranch("dstBranch",0,"r");
    IOMk->SetBranch("runcoBranch",0,"r");

// constructor for other maker (not used in chain)
   StHistUtil   *HU  = new StHistUtil;

// now must set pointer to StMaker so HistUtil can find histograms
//  with StHistUtil methods
// -- input any maker pointer but must cast as type StMaker
   HU->SetPntrToMaker((StMaker *)IOMk);

//  add other makers to chain:
  StEventMaker *eventMaker = new StEventMaker("events","title-notused");
  StEventQAMaker *EventQA = new StEventQAMaker(MakerHistDir,"StEvent/QA-notused");

// output hist.root file:
   StTreeMaker* treeMk = new StTreeMaker("tree",outHistFile,TopDirTree);
      treeMk->SetIOMode("w");
      treeMk->SetBranch("histBranch");


// --- now execute chain member functions --> Init
    Int_t iInit = chain->Init();
    if (iInit) chain->Fatal(iInit,"on init");
    chain->PrintInfo();
 
// method to print out list of histograms - 
//can do this anytime after they're booked
  Int_t NoHist=0;
  NoHist = HU->ListHists(MakerHistDir);
  cout << " !!! bfcread_dst_QAhist.C, No. of Hist we have == " << NoHist << endl;

 
// loop over events:
  int iev=0,iret=0, evnum=0;
 EventLoop: if (iev<nevents && !iret) {  // goto loop code
   evnum=iev+1;
   cout <<  " !!! bfcread_dst_EventQA_outhistfile.C, processing event !!! " << evnum << endl ;
   chain->Clear();
   iret = chain->Make();
   iev++;                                // goto loop code
   goto EventLoop;                       // goto loop code
 }

  cout <<  " !!! bfcread_dst_EventQA_outhistfile.C, passed chain->Make !!!" << endl ;

  chain->Finish();
  cout <<  "bfcread_dst_EventQA_outhistfile.C, passed chain->Finish" << endl ; 
   
}
 














