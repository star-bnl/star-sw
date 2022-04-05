// $Id: bfcread_event_QA_outhistfile.C,v 1.19 2015/04/02 19:54:41 genevb Exp $ 
// $Log: bfcread_event_QA_outhistfile.C,v $
// Revision 1.19  2015/04/02 19:54:41  genevb
// Bichsel now needed for TPC dE/dx QA
//
// Revision 1.18  2013/11/22 16:31:20  genevb
// Restore parallelism with bfcread_event_QAhist.C
//
// Revision 1.17  2013/03/14 17:28:31  genevb
// StTpcDb.so now depends on StEvent.so
//
// Revision 1.16  2011/03/09 22:17:10  genevb
// Update dependence on StEEmcUtil
//
// Revision 1.15  2011/01/19 20:03:03  fisyak
// switch order DbUtil and TpcDb shared libraries loading
//
// Revision 1.14  2010/02/22 20:05:16  genevb
// Using StTpcDbMaker now requires StMagFMaker
//
// Revision 1.13  2008/03/07 19:26:11  genevb
// Ye olde loading of StdetectorDbMaker library change
//
// Revision 1.12  2006/08/15 21:42:36  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.11  2005/01/27 05:28:58  genevb
// open runcoBranch
//
// Revision 1.10  2001/07/17 03:26:51  genevb
// Modify input file for year 2001
//
// Revision 1.9  2001/07/17 03:24:45  genevb
// Modify TPC DB info for year 2001
//
// Revision 1.8  2001/05/24 20:10:37  lansdell
// changed DB maker SetDateTime option to year_2b
//
// Revision 1.7  2001/05/16 20:53:37  lansdell
// added StMcEvent to chain
//
// Revision 1.6  2001/04/28 21:45:19  genevb
// include libs for EMC
//
// Revision 1.5  2000/07/26 19:53:45  lansdell
// made changes for creating new QA histograms
//
// Revision 1.4  2000/06/13 00:58:59  lansdell
// added libglobal_Tables to resolve crashes
//
// Revision 1.3  2000/06/02 20:26:03  lansdell
// added check on Make() return codes
//
// Revision 1.2  2000/05/15 20:24:00  kathy
// correct Log,Id so they get written out
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
     //"/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root",
"/star/rcf/test/dev/trs_redhat61/Tue/year_2001/hc_standard/hc_standard.40_evts.event.root",
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
  gSystem->Load("St_Tables");
  gSystem->Load("libglobal_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libgen_Tables");
  gSystem->Load("libgeometry_Tables");

  gSystem->Load("StUtilities");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StBichsel");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StEvent");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StarMagField");
  gSystem->Load("StMagF");
  gSystem->Load("StPmdUtil");
  gSystem->Load("St_QA_Maker"); 
  gSystem->Load("StTreeMaker");

//  Setup top part of chain
  chain = new StChain("MyChain");
  chain->SetDebug();
   
// Input File Maker
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetIOMode("r");
  IOMk->SetBranch("event",0,"r");
  IOMk->SetBranch("geantBranch",0,"r");
  IOMk->SetBranch("runcoBranch",0,"r");

// database stuff
  const char* calibDB = "MySQL:StarDb";
  const char* calibDB2 = "$STAR/StarDb";
  St_db_Maker* calibMk = new St_db_Maker("StarDb",calibDB,calibDB2);
  StMagFMaker* magfMk = new StMagFMaker; // now required for StTpcDbMaker
  //calibMk->SetDateTime("year_2b");
  calibMk->SetDebug();  
  StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");

// constructor for other maker (not used in chain)
  StHistUtil   *HU  = new StHistUtil;

// now must set pointer to StMaker so HistUtil can find histograms
//  with StHistUtil methods
// -- input any maker pointer but must cast as type StMaker
  HU->SetPntrToMaker((StMaker *)IOMk);

//  add other makers to chain:
  StMcEventMaker *mcEvent = new StMcEventMaker;
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
 EventLoop: if (iev<nevents && iret!=2) {  // goto loop code
   evnum=iev+1;
   cout <<  " !!! bfcread_event_QA_outhistfile.C, processing event !!! " << evnum << endl ;
   chain->Clear();
   switch (iret = chain->Make()) {
     case 0: break;
     case 2: { gMessMgr->Info("Last event from input."); break; }
     case 3: { gMessMgr->Error() << "Event " << evnum << " had error " <<
	       iret << ". Now skipping event."; gMessMgr->Print(); break; }
     default: { gMessMgr->Warning() << "Event " << evnum << " returned status "
	        << iret << ". Continuing."; gMessMgr->Print(); }
   }
   iev++;                                // goto loop code
   goto EventLoop;                       // goto loop code
 }

  cout <<  " !!! bfcread_event_QA_outhistfile.C, passed chain->Make !!!" << endl ;

  chain->Finish();
  cout <<  "bfcread_event_QA_outhistfile.C, passed chain->Finish" << endl ; 
   
}
 














