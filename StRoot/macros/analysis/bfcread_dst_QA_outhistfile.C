// $Id: bfcread_dst_QA_outhistfile.C,v 1.18 2000/06/05 17:25:03 lansdell Exp $
// $Log: bfcread_dst_QA_outhistfile.C,v $
// Revision 1.18  2000/06/05 17:25:03  lansdell
// StTpcDb no longer loaded
//
// Revision 1.17  2000/06/02 20:25:28  lansdell
// added check on Make() return codes
//
// Revision 1.16  2000/05/09 19:38:03  kathy
// update to use standard default input files and only process few events by default - to make it easy to run in automatic macro testing script
//
// Revision 1.15  2000/04/13 21:46:34  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.14  2000/04/12 15:06:52  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
//
// Revision 1.13  2000/03/20 17:32:55  kathy
// setbranches in all macros so that they will work with softlinks - for StIOMaker
//
// Revision 1.12  2000/03/20 15:45:06  kathy
// add libraries to load in bfcread_dst_QA*.C so it will work on linux; worked on solaris without adding them - I have no idea why
//
// Revision 1.11  2000/03/17 23:10:06  kathy
// make sure the dst branch is explicitly set in the macros using dst.root files as input - otherwise they don't work properly with soft links
//
// Revision 1.10  2000/02/14 20:30:40  kathy
// removing unneeded macros; updating documentation in bfcread macros
//
// Revision 1.9  2000/02/07 19:46:35  kathy
// add read from geant Branch so that geant table histograms can be filled in St_QA_Maker
//
// Revision 1.8  2000/01/19 16:29:50  kathy
// update macros to use default input files in /afs/rhic/star/data/samples
//
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
//    - sets dst & geant branches (uses both)
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
// Documentation on St_QA_Maker class is at:
//   http://duvall.star.bnl.gov/STARAFS/comp/pkg/dev/StRoot/St_QA_Maker/doc/
//
// Documentation on StHistUtil class is at:
//   http://duvall.star.bnl.gov/STARAFS/comp/pkg/dev/StRoot/StAnalysisUtilities/doc/
//
//
//======================================================================

class StChain;

StChain *chain;

void bfcread_dst_QA_outhistfile(
     Int_t nevents=2, 
     const Char_t *MainFile=
     "/afs/rhic/star/data/samples/gstar.dst.root",
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

  gSystem->Load("libgen_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libglobal_Tables");


  gSystem->Load("StUtilities");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StDbUtilities");

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
//  - turn geant Branch on - dstBranch already on from input file
     IOMk->SetDebug();
     IOMk->SetIOMode("r");
     IOMk->SetBranch("*",0,"0");                 //deactivate all branches
     IOMk->SetBranch("dstBranch",0,"r"); //activate dst Branch
     IOMk->SetBranch("geantBranch",0,"r"); //activate geant Branch

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
 EventLoop: if (iev<nevents && iret!=2) {  // goto loop code
   evnum=iev+1;
   cout <<  " bfcread_dst_QA_outhistfile.C, processing event !!! " << evnum << endl ;
   chain->Clear();
   switch (iret = chain->Make()) {
     case 0: break;
     case 2: { gMessMgr->Info("Last event from input."); break; }
     case 3: { gMessMgr->Error() << "Event " << evnum << " had error " <<
	       iret << ". Now skipping event."; gMessMgr->Print(); break; }
     default: { gMessMgr->Warning() << "Event " << evnum << " returned status "
	        << iret << ". Continuing."; gMessMgr->Print(); }
   }
   iev++;
   goto EventLoop;                       // goto loop code
 }

  cout <<  " bfcread_dst_QA_outhistfile.C, passed chain->Make !!!" << endl ;

  chain->Finish();
    cout <<  "bfcread_dst_QA_outhistfile.C, passed chain->Finish" << endl ; 
   
}
 














