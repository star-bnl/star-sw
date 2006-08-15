// $Id: StMcEventReadMacro.C,v 1.24 2006/08/15 21:43:04 jeromel Exp $
// $Log: StMcEventReadMacro.C,v $
// Revision 1.24  2006/08/15 21:43:04  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.23  2005/10/06 20:25:21  fisyak
// Don't need db
//
// Revision 1.22  2005/09/29 00:46:19  fisyak
// Persistent McEvent
//
// Revision 1.21  2005/07/07 19:43:42  calderon
// Load EEmcUtil.
// Add switch for Igt.
//
// Revision 1.20  2005/05/11 21:02:57  calderon
// Add switch for SSD.
//
// Revision 1.19  2005/04/18 20:13:59  calderon
// Add switches to load/or not the Fgt and Fst hits.
//
// Revision 1.18  2002/08/15 14:49:27  calderon
// Go back to loading St_Tables instead of loading each table individually...
// (as was the case since revision 1.12 back in April 2000!)
//
// Revision 1.17  2001/04/09 17:47:15  calderon
// load StEmcUtil instead of St_emc_Maker
//
// Revision 1.16  2000/06/22 23:54:30  calderon
// add gSystemLoad->("libglobal_Tables"); because it is needed for year2a
//
// Revision 1.15  2000/06/06 03:03:30  calderon
// Use with new Emc classes.
//
// Revision 1.14  2000/04/20 17:02:42  calderon
// Modified macros to continue looping when status = 3
// Pick up maker with name "StMcAnalysisMaker" instead of "McAnalysis"
// in StAssociator.C
//
// Revision 1.13  2000/04/13 22:00:24  calderon
// set dst branch and appropriate tables as per Kathy
//
// Revision 1.12  2000/04/12 17:39:02  kathy
// change to only load table libraries needed: lib*_Tables instead of all tables: St_Tables
//
// Revision 1.11  2000/01/19 21:00:40  kathy
// update macros to use standard default xdf files in /afs/rhic.bnl.gov/star/data/samples
//
// Revision 1.10  2000/01/12 20:29:15  calderon
// Changed default file to the one produced weekly by Lidia in
// /star/rcf/test/dev/tfs_Linux/Tue/year_2a/hc_standard/
//
// Revision 1.9  1999/12/14 18:18:01  calderon
// using new StMcEvent, StEvent & StAssociationMaker
//
// Revision 1.8  1999/12/03 01:01:33  calderon
// Updated for new StMcEvent 2.0 and StMcEventMaker.
// Uses StTpcDb to get the geometry info (calib has some problems still).
//
// Revision 1.7  1999/11/03 22:47:33  calderon
// Changed default file.  Previous one no longer existed.
//
// Revision 1.6  1999/07/28 21:29:34  calderon
// Modified event loop: use 'if' and 'goto' to avoid using 'for'
//
// Revision 1.5  1999/07/28 20:27:46  calderon
// Version with SL99f libraries
//
// Revision 1.4  1999/07/23 19:57:26  calderon
// Load StarClassLibrary before loading StMcEvent
//
// Revision 1.3  1999/07/23 14:35:43  calderon
// Updated names of default files and of packages
//
// Revision 1.2  1999/07/23 10:53:52  kathy
// put in header info in Manuel's macros
//
//
//////////////////////////////////////////////////////////////////////
// owner: Manuel Calderon de la Barca Sanchez
//
// what it does: reads .geant.root file, 
//               loads StMcEvent by putting StMcEventMaker in chain
//
// note: for more info on StMcEvent and StAssociationMaker, do a 
//      cvs checkout and say "make" in the doc/tex directory - you'll
//      get a ps file with user guide and reference manual.
//////////////////////////////////////////////////////////////////////
// 
//
//======================================================================
class StChain;
class St_DataSet;
St_DataSet *Event;
TBrowser *brow=0;


// The acual file to be used is passed as an argument to the macro, or a default can be set

void StMcEventReadMacro(Int_t nevents=1,
			const char *MainFile= "/star/rcf/test/new/trs_sl302.ittf/year_2005/cucu200_minbias/rcf1216_05_200evts.geant.root") {
  gROOT->LoadMacro("bfc.C");
  bfc(-1,"in,McEvent,nodefault",MainFile);
  // IO Maker
  StIOMaker *IOMk = (StIOMaker *) chain->Maker("inputStream");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("geantBranch",0,"r");
  IOMk->SetBranch("dstBranch",0,"r");
  // StMcEvent
  StMcEventMaker  *mcEventReader  = (StMcEventMaker  *) chain->Maker("StMcEventMaker");
  mcEventReader->doPrintEventInfo = true;
  mcEventReader->doPrintMemoryInfo = false;
  mcEventReader->doUseTpc = true;
  mcEventReader->doUseSvt = true;
  mcEventReader->doUseSsd = true;
  mcEventReader->doUseFtpc = true;
  mcEventReader->doUseRich = true;
  mcEventReader->doUseBemc = true;
  mcEventReader->doUseBsmd = true;
  mcEventReader->doUseIst  = true; 
  mcEventReader->doUseFst  = true; 
  mcEventReader->doUseFgt  = true;
  mcEventReader->doUseIgt  = true;
    
  chain->PrintInfo();
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) chain->Fatal(initStat, "during Init()");
  chain->EventLoop(1,nevents);
}
