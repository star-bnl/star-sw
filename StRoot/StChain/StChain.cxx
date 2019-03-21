 /*!
  StChain                                                              

 Main class to control the StChain program.                           
                                                                      
 This class was done on the base of <a href="http://root.cern.ch/root/Atlfast.html"> ATLFAST </a> 
 C++ class library           
 This class is a general framework for programs that needs to:       
    - Initialise some parameters                                      
    - Loop on events                                                  
    - Print results and save histograms, etc                          
                                                                      
 The event processor StChain::Make loops on a list of Makers          
 where each maker performs some task on the event data and generates  
 results.                                                             
 New Makers can be inserted by a user without modifying this class.   
 Note that the order in which the Makers are called is the order      
 of insertion in the list of Makers.                                  
                                                                      
 Makers must derive from the base class StMaker.                      
 StMaker provides a common interface to all Makers.                   
 Each Maker is responsible for defining its own parameters and        
 histograms.                                                          
 Each Maker has its own list of histograms.                           
 The pointer supporting the created object(s) is defined in StMaker   
   GetDataSet() points to a DataSet owned by the Maker                
                                                                      
 The function StChain::Maketree must be called after the creation     
 of the StChain object to create a Root Tree (not yet implemented).   
 
 The eventloop method can terminated earlier with the external TERM (-15) signal
 (for example, Condor sends this signal to notifye the job aboyt the its intent to evict the job.
                                                                      
*/
#define   STAR_TRACKING 1


#include <stdlib.h>
#include <stdio.h>
#include "TROOT.h"
#include "TError.h"
#include "TBrowser.h"
#include "TBenchmark.h"
#include "TSystem.h"
#include "StChain.h"
#include "StEvtHddr.h"
#include "StMessMgr.h"
#include "StMemStat.h"
#include "StCloseFileOnTerminate.h"
#include "TApplication.h"
ClassImp(StChain)

//_____________________________________________________________________________
StChain::StChain(const char *name,  const Bool_t UseOwnHeader):
StMaker(name),m_EvtHddr(0),mChainOpt(0)
{
   m_Version     = 100;       //StChain  version number and release date
   m_VersionDate = 180698;
   mNTotal = 0; mNFailed = 0;
   if ( UseOwnHeader || !(dynamic_cast<StEvtHddr*>(GetDataSet("EvtHddr"))))  
			  m_EvtHddr = new StEvtHddr(m_ConstSet); 

}

//_____________________________________________________________________________
StChain::~StChain()
{
}
//_____________________________________________________________________________
void StChain::Streamer(TBuffer &)
{ Error("Streamer"," attempt to write %s\n ",GetName());
  assert(0);
}
//_____________________________________________________________________________
void StChain::Clear(Option_t *option)
{
 // StartTimer();
 StMaker::Clear(option);
 // TCollection::EmptyGarbageCollection();
 // StopTimer();
}
//_____________________________________________________________________________
Int_t StChain::Finish(){
 // StartTimer();
 if (TestBIT(kFiniEnd)){ 
   Warning("Finish","chain %s.%s Finished twice, Ignore it"
           ,GetName(),ClassName());
   return 1;
 }
 TCollection::StartGarbageCollection();
 Int_t res = StMaker::Finish();
 // TCollection::EmptyGarbageCollection();
 SetBIT  (kFiniEnd);
 // StopTimer();
 PrintTotalTime();
 // delete gMessMgr; gMessMgr = 0;
 return res;
}
//_____________________________________________________________________________
Int_t StChain::Init()
{
 // StartTimer();
 Int_t res = StMaker::Init();
 // StopTimer();
 return res;
}
//_____________________________________________________________________________
Int_t StChain::Make() {
 // StartTimer();
  if (m_EvtHddr) m_EvtHddr->SetProdDateTime();
 Int_t res = StMaker::Make();
 // StopTimer();
 return res;
}
//_____________________________________________________________________________
Int_t StChain::MakeEvent() 
{
  // Make next event from the TBrowser TContextMenu
  Clear();
  return StMaker::IMake(GetNumber()+1);
}
//_____________________________________________________________________________
const StChainOpt *StChain::GetChainOpt()    const
{
  if (mChainOpt) return mChainOpt;
  return StMaker::GetChainOpt();
}
//_____________________________________________________________________________
Int_t StChain::EventLoop(Int_t jBeg,Int_t jEnd, StMaker *outMk) 
{
  TBenchmark evnt;
  int jCur=0,iMake=0;
#ifdef STAR_TRACKING 
#ifdef OLDTRACKING    
// Add a record to MySQL tracking Db     
  LOG_QA << "Events="       << mNTotal
         << ",Failed="      << mNFailed
         << ",StepEventId=" << "'Start'"
         << ",StepContext=" << "'MemUsed',"  << "MessageId='='"
         << ",ProgrammMessage='" << int(StMemStat::Used())
         << "'" << endm;
         
  LOG_QA << "Events="       << mNTotal
         << ",Failed="      << mNFailed
         << ",StepEventId=" << "'Start'"
         << ",StepContext=" << "'ProgSize',"  << "MessageId='='"
         << ",ProgrammMessage='" << int(StMemStat::ProgSize())
         << "'" << endm;
#else
// Add a record to MySQL tracking Db     
// LOG_QA << "SequenceValue="<< mNTotal
   LOG_UCM 
         << "StageID=" << "'1'"
         << ",MessageKey=" << "'MemUsed'" 
         << ",MessageValue='" << int(StMemStat::Used())
         << "'" << endm;
         
// LOG_QA << "SequenceValue="<<mNTotal 
   LOG_UCM 
         << "StageID=" << "'1'"
         << ",MessageKey=" << "'ProgSize'"
         << ",MessageValue='" << int(StMemStat::ProgSize())
         << "'" << endm;
#endif         
#endif                
  if (jBeg > 1) Skip(jBeg-1);
  // End of the event loop as soon as the application receives TERM (-15) system signal
  class teminator : public  StTerminateNotified {
    Bool_t fEnd_of_time;
  public: 
    teminator() : StTerminateNotified(), fEnd_of_time(kFALSE) {}
    Bool_t Notify() {return ! fEnd_of_time;}
    void SetNotifiedCallBack() { 
      fEnd_of_time = true; 
      fgStChain->Error(__FUNCTION__," Job will be terminated soon by the external signal . . . . "); 
      if (GetTopChain()) {
	fgStChain->Error(__FUNCTION__," Forced Finish . . . . "); 
	GetTopChain()->Finish();
      }
      fgStChain->Error(__FUNCTION__,"Terminating  . . . . ");
      gApplication->Terminate(15);
    }
  } endOfTime;
  for (jCur=jBeg; jCur<=jEnd; jCur++) {
     evnt.Reset(); evnt.Start("QAInfo:");

     Clear();
     iMake = Make(jCur);

     if (outMk && iMake == kStErr) {/*outMk->IMake(jCur);*/ mNFailed++;}
     if (iMake%10 == kStEOF || iMake%10==kStFatal)	break;
     mNTotal++;
     evnt.Stop("QAInfo:");
     //  evnt.Show("QAInfo:");
     
     //
     // ATTENTION - please DO NOT change the format of the next line,
     //   they are used by our parsers to detect a generation 
     //   was succesful and thereafter Catalog the produced files (or
     //   add useful info to our trackijg DB). Thank you.
     //
     LOG_QA << Form
     /*printf */ ("QAInfo: Done with Event [no. %d/run %d/evt. %d/Date.Time %d.%d/sta %d] Real Time = %10.2f seconds Cpu Time =  %10.2f seconds",
	jCur,GetRunNumber(),GetEventNumber(),GetDate(), GetTime(),
	     iMake,evnt.GetRealTime("QAInfo:"),evnt.GetCpuTime("QAInfo:")) 
     << endm;

#ifdef STAR_TRACKING 
#ifdef OLDTRACKING    
  // Add a record to MySQL tracking Db     
  LOG_QA << "Events="       << mNTotal
         << ",Failed="      << mNFailed
         << ",StepEventId=" << "'EventFinish'"
         << ",StepContext=" << "'Cpu',"  << "MessageId='='"
         << ",ProgrammMessage='" << evnt.GetCpuTime("QAInfo:")
         << "'" << endm;

  LOG_QA << "Events="       << mNTotal
         << ",Failed="      << mNFailed
         << ",StepEventId=" << "'EventFinish'"
         << ",StepContext=" << "'RealTime',"  << "MessageId='='"
         << ",ProgrammMessage='" << evnt.GetRealTime("QAInfo:")
         << "'" << endm;
#else
// Add a record to MySQL tracking Db     
//  LOG_QA << "SequenceValue="       << mNTotal
  // LOG_UCM
//          << "StageID=" << "'3'"
//          << ",MessageKey=" << "'Cpu'"
//          << ",MessageValue='" << evnt.GetCpuTime("QAInfo:")
//          << "'" << endm;

//  LOG_QA << "SequenceValue="       << mNFailed
  // LOG_UCM 
//          << "StageID=" << "'3'"
//          << ",MessageKey=" << "'RealTime'" 
//          << ",MessageValue='" << evnt.GetRealTime("QAInfo:")
//          << "'" << endm;
#endif
#endif                
  }

  LOG_QA << Form
 /*printf */ ("QAInfo:EventLoop completed code %d",iMake)
  << endm;
  gSystem->Exec("date");
  TDatime t;
  LOG_QA << Form
 /* printf */ ("QAInfo:Run is finished at Date/Time %i/%i; Total events processed :%i and not completed: %i",
	    t.GetDate(),t.GetTime(),mNTotal,mNFailed)
        << endm;

#ifdef STAR_TRACKING     
// Add a record to MySQL tracking Db     
#ifdef OLDTRACKING
  LOG_QA << "Events="       << mNTotal
         << ",Failed="      << mNFailed
         << ",StepEventId=" << "'Finish'"
         << ",StepContext=" << "'MemUsed',"  << "MessageId='='"
         << ",ProgrammMessage='" << int(StMemStat::Used())
         << "'" << endm;

  LOG_QA << "Events="       << mNTotal
         << ",Failed="      << mNFailed
         << ",StepEventId=" << "'Finish'"
         << ",StepContext=" << "'ProgSize',"  << "MessageId='='"
         << ",ProgrammMessage='" << int(StMemStat::ProgSize())
         << "'" << endm;
#else
// Add a record to MySQL tracking Db     

//   LOG_QA << "SequenceValue="       << mNTotal
  // LOG_UCM 
//          << "StageID=" << "'3'"
//          << ",MessageKey=" << "'MemUsed'"  
//          << ",MessageValue='" << int(StMemStat::Used())
//          << "'" << endm;

//   LOG_QA << "SequenceValue="       << mNFailed
  // LOG_UCM 
//          << "StageID=" << "'3'"
//          << ",MessageKey=" << "'ProgSize'"
//          << ",MessageValue='" << int(StMemStat::ProgSize())
//          << "'" << endm;
#endif         
   if (GetLogger()) GetLogger()->Close();

#endif                
  fflush(stdout);
  return iMake;
}


// $Id: StChain.cxx,v 1.83 2019/03/21 18:56:46 jeromel Exp $
// $Log: StChain.cxx,v $
// Revision 1.83  2019/03/21 18:56:46  jeromel
// Added ATTENTION message
//
// Revision 1.82  2016/05/26 15:27:11  jeromel
// Missing init added
//
// Revision 1.81  2013/07/18 14:05:25  fisyak
// Open garbage can at Finish
//
// Revision 1.80  2011/10/13 20:06:53  perev
// Put removed UCM messages back(req JL)
//
// Revision 1.79  2011/10/11 16:01:48  perev
// Remove redundant printouts
//
// Revision 1.78  2011/06/20 15:13:50  fisyak
// Force to call Finish with SIGTERM signal obtained from condor_vacate_job after time limit reached
//
// Revision 1.77  2010/04/27 21:31:44  fine
// remove the logger destruction side effect
//
// Revision 1.76  2010/04/23 22:40:08  fine
// RT #1911. Close the local logger at Finish
//
// Revision 1.75  2010/03/02 23:09:24  fine
// Simplify Close/Terminate interface
//
// Revision 1.74  2010/03/01 23:37:41  fine
// Terminate StChain::EventLoop with the extrenal TERM 15 signal
//
// Revision 1.73  2009/06/23 19:37:33  fine
// replace QA logger with the dedicated UCM one
//
// Revision 1.72  2009/03/17 20:03:36  perev
// Back to StMemSet version
//
// Revision 1.70  2009/01/26 14:32:33  fisyak
// rename TMemStat => StMemStat due clash with ROOT class
//
// Revision 1.69  2008/06/03 22:33:14  fisyak
// Add geometries for y2005g, y2006g and y2007g; use ROOT convention for variable definitions
//
// Revision 1.68  2008/03/05 00:01:51  fisyak
// Move Skip method in base class
//
// Revision 1.67  2007/10/19 16:18:32  fine
// new Db schema from TxCorp
//
// Revision 1.66  2007/10/17 18:54:04  fine
// new Db tracking schema from TxCorp
//
// Revision 1.65  2007/09/18 20:42:35  fine
// Fix the message typo
//
// Revision 1.64  2007/04/26 20:36:49  perev
// Some ChainOpt fixes
//
// Revision 1.63  2006/07/03 04:13:38  fine
// new Job tracking Db activated
//
// Revision 1.62  2006/07/01 01:19:16  fine
// Add new jiob tracking option code
//
// Revision 1.61  2006/06/05 00:20:59  fine
// class the new StMessMgr method to flush the logger buffers
//
// Revision 1.60  2006/06/04 22:59:00  fine
// Change the wonr event tracking code : Finish with the proper EventFinish
//
// Revision 1.59  2006/05/24 17:33:43  fine
// remove the redundant Db fields
//
// Revision 1.58  2006/05/16 18:54:23  fine
// fix StChain and MySql
//
// Revision 1.57  2006/05/12 18:48:48  fine
// reshape jobn tracking. remove the redundand table columns
//
// Revision 1.56  2006/05/12 18:08:14  fine
// fix the MySQLAppender problem and re-shape the trakDb messages
//
// Revision 1.55  2006/05/09 23:31:20  fine
// Reshape the job tracking Db tables and add a few LOQ_QA message to record it with the Job tracking Db
//
// Revision 1.54  2006/03/28 02:09:19  fine
// Add SIMS_USER SUMS_AUTHENTICATED_USER SUMS_JOBNAME
//
// Revision 1.53  2006/02/05 01:41:23  fine
// Add the tracking information from the STAR chain
//
// Revision 1.52  2005/08/29 21:42:20  fisyak
// switch from fBits to fStatus for StMaker control bits
//
// Revision 1.51  2005/08/12 21:27:31  perev
// Remove call output in the case or read error
//
// Revision 1.50  2004/11/04 22:26:38  fine
// populate the package with save/restore the logger and edit some messages
//
// Revision 1.49  2004/08/03 17:18:46  perev
// EventLoop corrected according to current policy
//
// Revision 1.48  2002/11/26 02:16:39  perev
// EventLoop added
//
// Revision 1.47  2002/03/12 21:19:00  fisyak
// Set only one StEvtHddr as default option (due to Embedding)
//
// Revision 1.46  2002/02/02 23:31:13  jeromel
// doxygenized. Added some text for the Make() method.
//
// Revision 1.45  2001/04/10 21:38:49  perev
// Maki(int) --> IMake(int)
//
// Revision 1.44  2000/11/27 13:31:23  fisyak
// Add Production time set
//
// Revision 1.43  2000/03/23 00:15:21  fine
// Adjusted to libSTAR for ROOT 2.24
//
// Revision 1.42  1999/12/06 01:57:29  fine
// Time statistic fixed
//
// Revision 1.41  1999/12/03 01:24:39  fine
// Advanced timer has been introduced
//
// Revision 1.40  1999/07/14 15:26:18  fine
// Context menu MakeEvent method has been introduced
//
// Revision 1.39  1999/07/13 02:19:33  perev
// GetCVS,StEvtHddr,etc...
//
// Revision 1.38  1999/07/11 20:40:35  perev
// Move Clear from StChain to StMaker
//
// Revision 1.37  1999/06/11 17:45:56  perev
// assert StMaker::Streamer to forbid to write it
//
// Revision 1.36  1999/03/19 20:30:48  perev
// GetCVSTag introduced
//
// Revision 1.35  1999/03/11 01:23:58  perev
// new schema StChain
//
// Revision 1.23  1998/12/21 19:42:50  fisyak
// Move ROOT includes to non system
//
// Revision 1.22  1998/11/29 20:01:09  fisyak
// Fix typo with Run/run
//
// Revision 1.21  1998/11/25 21:58:21  fisyak
// Cleanup
//
// Revision 1.20  1998/11/22 18:28:05  fisyak
// Add name of tag
//
// Revision 1.19  1998/11/19 01:23:56  fine
// StChain::MakeDoc has been introduced, StChain::MakeDoc has been fixed (see macros/bfc_doc.C macro
//
// Revision 1.18  1998/10/31 00:21:30  fisyak
// Makers take care about branches
//
// Revision 1.17  1998/10/07 18:43:57  perev
// Add Spy classes for Farm Monitor
//
// Revision 1.16  1998/10/06 18:00:26  perev
// cleanup
//
// Revision 1.15  1998/09/23 20:22:51  fisyak
// Prerelease SL98h
//
// Revision 1.14  1998/09/16 14:29:33  love
// St_DataSetIter.h added
//
// Revision 1.13  1998/08/26 12:15:08  fisyak
// Remove asu & dsl libraries
//
// Revision 1.12  1998/08/18 14:05:01  fisyak
// Add to bfc dst
//
// Revision 1.11  1998/08/07 19:34:53  fisyak
// Add St_run_Maker
//
// Revision 1.10  1998/07/23 21:03:30  fisyak
// Add more comments
//
// Revision 1.9  1998/07/23 11:32:11  fisyak
// Add comments
//
// Revision 1.8  1998/07/20 15:08:08  fisyak
// Add tcl and tpt
//
// Revision 1.7  1998/07/19 21:16:29  fisyak
// add log information
//
// Revision 1.6  1998/07/19 21:14:48  fisyak
// add log information
//
