//  
//  
//
//======================================================================
// owner:  Curtis Lansdell, Kathy Turner
// what it does: 
//    - reads a *.event.root file from bfc.C 
//    - sets event branch
//    - runs StEventQAMaker (books & fills hist)
//    - opens & writes histograms to output hist file 
//
//=======================================================================
//
// inputs: 
//         nevents - # events to process
//         MainFile - input event file from bfc output
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

void bfcread_event_QA_outhistfile(
     Int_t nevents=2, 
     const Char_t *MainFile=
     //"/afs/rhic/star/data/samples/gstar.dst.root",
"/star/rcf/test/dev/tfs_redhat61/Tue/year_1h/hc_standard/hc_standard.40_evts.event.root",
     const Char_t *outHistFile="StEQAMaker",
     const Char_t *TopDirTree="StEQAtree",
     const Char_t *MakerHistDir="StEQA")
{
//
  cout << "bfcread_event_QA_outhistfile.C, num events to process " 
       << nevents  << endl;
  cout << "bfcread_event_QA_outhistfile.C, input file name       " 
       << MainFile << endl;
  cout << "bfcread_event_QA_outhistfile.C, Maker directory name  " 
       << MakerHistDir<< endl;
  cout << "bfcread_event_QA_outhistfile.C, output hist name      " 
       << outHistFile<< endl;
  cout << "bfcread_event_QA_outhistfile.C, output top level directory name " 
       << TopDirTree<< endl;

  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_QA_Maker"); 
  gSystem->Load("StTreeMaker");
  gSystem->Load("StEvent");

//  Setup top part of chain
  chain = new StChain("MyChain");
  chain->SetDebug();
   
// Input File Maker
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetIOMode("r");
  IOMk->SetBranch("event",0,"r");

// constructor for other maker (not used in chain)
   StHistUtil   *HU  = new StHistUtil;

// now must set pointer to StMaker so HistUtil can find histograms
//  with StHistUtil methods
// -- input any maker pointer but must cast as type StMaker
   HU->SetPntrToMaker((StMaker *)IOMk);

//  add other makers to chain:
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
   cout <<  " !!! bfcread_event_QA_outhistfile.C, processing event !!! " << evnum << endl ;
   chain->Clear();
   iret = chain->Make();
   iev++;                                // goto loop code
   goto EventLoop;                       // goto loop code
 }

  cout <<  " !!! bfcread_event_QA_outhistfile.C, passed chain->Make !!!" << endl ;

  chain->Finish();
  cout <<  "bfcread_event_QA_outhistfile.C, passed chain->Finish" << endl ; 
   
}
 














