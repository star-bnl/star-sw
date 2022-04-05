//*-- Author : Dominik Flierl
//             Christof Struck
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// This Maker reads l3 data as they come with the raw data stream       //
// from the experiment and fills them into StEvent                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
//
//  $Id: Stl3RawReaderMaker.cxx,v 1.21 2017/04/26 21:15:47 perev Exp $
//
//  $Log: Stl3RawReaderMaker.cxx,v $
//  Revision 1.21  2017/04/26 21:15:47  perev
//  Hide m_Debug
//
//  Revision 1.20  2008/01/23 19:10:33  fine
//  Fix the lost L3_Reader class definition
//
//  Revision 1.19  2007/04/28 17:56:24  perev
//  Redundant StChain.h removed
//
//  Revision 1.18  2004/08/09 18:25:21  kollegge
//  Added tpc detector id when setting the number of points since StEvent can now store the number of points for different detectors.
//
//  Revision 1.17  2004/03/26 11:25:23  kollegge
//  Added another quality check of raw data quality to prevent inconsistent information in StL3EventSummary, fixes bug http://www.star.bnl.gov/rt2/Ticket/Display.html?id=359
//
//  Revision 1.16  2004/03/26 00:31:59  dietel
//  Check L3_SUMD, fixes http://www.star.bnl.gov/rt2/Ticket/Display.html?id=357
//
//  Revision 1.15  2004/01/23 23:14:06  kollegge
//  Crashs of the l3 online analysis code so far stopped the chain. Changed this to a warning message.
//
//  Revision 1.14  2002/05/16 02:39:13  struck
//  switch reco/embedding mode (m_Mode=0/1).
//  Embedding mode skips L3 biased events (return kStErr).
//  Reco mode fills StEvent as before.
//
//  Revision 1.13  2002/02/27 20:17:58  struck
//  adding globalTrack->setEncodedMethod() to mark tracks as l3 tracks in StEvent
//
//  Revision 1.12  2002/02/26 21:40:38  struck
//  move GetDataSet("StDAQReader") from Init() to Make(), solves a seg. fault in current dev release
//
//  Revision 1.11  2002/02/13 22:36:32  struck
//  major code clean-up for Stl3RawReaderMaker, first version of Stl3CounterMaker
//
//  Revision 1.10  2001/11/14 23:30:56  struck
//  major update: set 'unbiased'-flag, correct bugs in StGlobalTrack-filling
//
//  Revision 1.9  2001/09/27 03:49:52  struck
//  actual no. of gl3s handled flexible, max no. of gl3s and algorithms now global define-statements
//
//  Revision 1.8  2001/09/25 01:42:51  struck
//  cs: l3 vertex now put into StL3Trigger
//
//  Revision 1.7  2001/08/29 20:30:40  struck
//  and don't forget to delete your array ;-)
//
//  Revision 1.6  2001/08/29 20:24:49  struck
//  makes Solaris compiler happy
//
//  Revision 1.5  2001/08/20 22:32:00  struck
//  first version filling L3 counters and algorithm info into StEvent
//
//

#include "Stl3RawReaderMaker.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"
#include "StDAQMaker/StDAQReader.h"
#include "StDaqLib/L3/L3_Reader.hh"
#include "tables/St_l3RunSummary_Table.h"
#include "tables/St_l3AlgorithmInfo_Table.h"
#include "StEventTypes.h"
#include "StEnumerations.h"
#include "Rtypes.h"
#include "TMath.h"

#include "St_l3_Coordinate_Transformer.h"
#include "St_l3_Coordinates.h"


#define gufld   gufld_
extern "C" {void gufld(Float_t *, Float_t *);}


ClassImp(Stl3RawReaderMaker)

//_____________________________________________________________________________
Stl3RawReaderMaker::Stl3RawReaderMaker(const char *name):StMaker(name){
 //  l3RawReader constructor
}

//_____________________________________________________________________________
Stl3RawReaderMaker::~Stl3RawReaderMaker(){
}


//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::Init(){
  //  Init - is a first method the top level StChain calls to initialize all its makers

  mL3On = kFALSE;

  //SetDebug(1);

  // reset database pointer
  mDbSet = 0;

  mNumberOfGl3Nodes = 0;
  mNumberOfAlgorithms = 0;
  mEventCounter = 0;

  // reset counter
  for (int i=0; i<MaxNumberOfGl3Nodes; i++) {
        mGlobalCounter[i].nProcessed = 0;
	mGlobalCounter[i].nReconstructed = 0;
	for (int j=0; j<MaxNumberOfAlgorithms; j++) {
	      mAlgorithmCounter[i][j].algId = 0;
	      mAlgorithmCounter[i][j].nProcessed = 0;
	      mAlgorithmCounter[i][j].nAccept = 0;
	      mAlgorithmCounter[i][j].nBuild = 0;
	}
  }

  // return
  return StMaker::Init();
}


//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::Make()
{
    //
    //  Make - this method is called in loop for each event
    //
       
    // here we start
    gMessMgr->Info("Stl3RawReaderMaker: Now we start l3RawReader Maker.");
    

    // get the l3 daqreader
    // Make Connection to raw data
    DAQReaderSet = GetDataSet("StDAQReader");
    if (!DAQReaderSet) {
          gMessMgr->Error() << "Stl3RawReaderMaker::Make():  no DaqReader found!" << endm;
	  return kStWarn;
    }

    StDAQReader *daqReader = (StDAQReader*)(DAQReaderSet->GetObject()) ;
    if (daqReader) { 
          ml3reader = daqReader->getL3Reader();

	  if (ml3reader) {

	        // set flag L3 ON
	        mL3On = kTRUE;

		// debug output
	        if (Debug()) {
		      int sec = 23;
		      if (ml3reader->getGlobalTrackReader())
			    gMessMgr->Info() << ml3reader->getGlobalTrackReader()->getNumberOfTracks()
					     << " global tracks found." << endm;
		      if (ml3reader->getSl3ClusterReader(sec))
			    gMessMgr->Info() << ml3reader->getSl3ClusterReader(sec)->getNumberOfClusters() 
					     << " sl3 clusters found in sec " << sec << endm;
		      if (ml3reader->getSl3TrackReader(sec))
			    gMessMgr->Info() << ml3reader->getSl3TrackReader(sec)->getNumberOfTracks()
					     << " sl3 tracks found in sec " << sec << endm;
		      if (ml3reader->getI960ClusterReader(sec))
			    gMessMgr->Info() << ml3reader->getI960ClusterReader(sec)->getNumberOfClusters()
					     << " i960 clusters found in sec " << sec << endm;
		}

		switch (m_Mode) {
		case 0:
		      // standard mode: fill StEvent
		      gMessMgr->Info("Stl3RawReaderMaker: Fill StEvent.");
		      if (fillStEvent() != 0) {
			    gMessMgr->Error("Stl3RawReaderMaker: problems filling l3 into StEvent.");
			    return kStErr;
		      }
		      break;
		case 1:
		      // embedding mode: skip biased events,
                      //                 return kStErr (or better: kStSkip)
		      gMessMgr->Info("Stl3RawReaderMaker: Embedding Mode, check L3 bias.");
		      return checkL3Bias();
		      break;
		default:
		      // unknown mode: do nothing!
		      gMessMgr->Error("Stl3RawReaderMaker: Unknown mode, return kStWarn");
		      return kStWarn;
		}

	  } // if (ml3reader)

	  else {
	        // if L3 is on for this run but no data found
	        // warning: should happen only if we crashed during the run.
	        // new since FY04 run: DAQ doesn't send us all events
	        // so this is normal/expected behaviour
	        if (mL3On) {
		      gMessMgr->Warning("Stl3RawReaderMaker: L3 is ON, but no l3 data found in this event. Either DAQ has not send us this event or we crashed during online analysis.");
		      return kStWarn;
		}
		// if L3 is off so far
		// it may be switched off for this run
		// so don't crash the chain
		else {
		      gMessMgr->Warning("Stl3RawReaderMaker: no l3 data found.");
		      // standard mode: return warning
		      if (m_Mode!=1) return kStWarn;
		      // embedding mode: return ok, since there is obviously no bias
		      else return kStOk;
		}
	  } 

    } // if (daqReader)

    // go home
    return kStOk;
}

//_____________________________________________________________________________

Int_t Stl3RawReaderMaker::checkL3Bias() 
{

  // get Gl3AlgorithmReader
  Gl3AlgorithmReader* gl3Reader = ml3reader->getGl3AlgorithmReader();
  if (!gl3Reader) {
        gMessMgr->Error("Stl3RawReaderMaker: L3 is ON, but L3 summary data is missing!");
	gMessMgr->Error("Stl3RawReaderMaker: ==> This event is bad, skip it!");
	return kStErr;
  }

  // check data
  // L3_summary.on==0 indicates that the event crashed in
  // L3 online and raw data contains no valid information for us
  if (ml3reader->getL3_Summary()->on == 0) {
        gMessMgr->Warning("Stl3RawReaderMaker: L3 crashed online on this event, no usefull information.");
	gMessMgr->Warning("Stl3RawReaderMaker: ==> This event is WEIRD, but definitely unbiased!");
	return kStOk;
  }

  // number of algorithms
  mNumberOfAlgorithms = gl3Reader->getNumberofAlgorithms();

  Algorithm_Data* algData = gl3Reader->getAlgorithmData();

  // default return value: kStErr == skip event
  // should be kStSkip
  int returnValue = kStErr; 

  //
  // check Pass-Thru Algorithm for hadronic triggers
  // that is either TWReject && triggerWord=0x1...
  // or TRUE && triggerWord=0x1...
  //
  // __STATUS as of NOV. 2001__
  //
  for (int i=0; i<mNumberOfAlgorithms; i++) {
	if ( (algData[i].algId==10 || algData[i].algId==1)
	     && ml3reader->getL3_P()->trg_word>0x0fff
	     && ml3reader->getL3_P()->trg_word<0x2000
	     && algData[i].build==1 ) {

	      if (Debug()) {
		    printf("L0 trigger word: %x\n", ml3reader->getL3_P()->trg_word); 
		    printf("pass-thru algorithm Id: %i\n", algData[i].algId);
	      }
	      // this event is unbiased ==>> take it!
	      returnValue = kStOk;
	}
  }

  return returnValue;
}

//_____________________________________________________________________________

Int_t Stl3RawReaderMaker::fillStEvent() 
{

  // get StEvent if not return
  mStEvent = (StEvent *) GetInputDS("StEvent");
  if (!mStEvent) {
        gMessMgr->Error("Stl3RawReaderMaker: No StEvent");
	return 1;
  }
  
  // create StL3Trigger and connect it
  myStL3Trigger = new StL3Trigger() ;
  if (!myStL3Trigger) {
        gMessMgr->Error("Stl3RawReaderMaker: No Stl3Trigger.");
	return 1;
  }
  mStEvent->setL3Trigger(myStL3Trigger);


  // check data
  if (!ml3reader->getL3_SUMD()) {
    gMessMgr->Warning("Stl3RawReaderMaker: No L3_SUMD bank.");
    myStL3Trigger->setL3EventSummary(NULL);
    return 0;
  }

  if (!ml3reader->getL3_Summary()) {
    gMessMgr->Warning("Stl3RawReaderMaker: No l3 summary.");
    myStL3Trigger->setL3EventSummary(NULL);
    return 0;
  }

  // L3_summary.on==0 indicates that the event crashed in
  // L3 online and raw data contains no valid information for us
  if (ml3reader->getL3_Summary()->on == 0) {
        gMessMgr->Warning("Stl3RawReaderMaker: L3 crashed online on this event, no usefull information.");
	myStL3Trigger->setL3EventSummary(NULL);
	return 0;
  }

  // create StL3EventSummary
  StL3EventSummary* myEventSummary = new StL3EventSummary(ml3reader->getL3_SUMD());
  if (!myEventSummary) {
    gMessMgr->Error("Stl3RawReaderMaker: No Stl3EventSummary.");
    return 1;
  } 

  // connect StL3EventSummary to StL3Trigger
  myStL3Trigger->setL3EventSummary(myEventSummary);


  // get Gl3AlgorithmReader
  Gl3AlgorithmReader* gl3Reader = ml3reader->getGl3AlgorithmReader();
  if (!gl3Reader) {
        gMessMgr->Error("Stl3RawReaderMaker: L3 is ON, but L3 summary data is missing!");
	return 1;
  }

  // get database info
  mDbSet = GetDataBase("RunLog/l3");
  // if we didn't get the db use maximum values as default
  if (mDbSet) {
        // get number of gl3 nodes
        TTable* l3runSummaryTable = (TTable* )mDbSet->Find("l3RunSummary");
	// database can be there, but now tables :-(
	if (l3runSummaryTable) {
	      l3RunSummary_st* data = (l3RunSummary_st* )l3runSummaryTable->GetArray();
	      mNumberOfGl3Nodes = data->nGl3Nodes;
	      if (Debug()) {
		    gMessMgr->Info() << "database: runNumber = " << data->runNumber << endm;
		    gMessMgr->Info() << "database: nGl3Nodes = " << data->nGl3Nodes << endm;
	      }
	      // check limit
	      if (mNumberOfGl3Nodes>MaxNumberOfGl3Nodes) {
		    gMessMgr->Error() << "Stl3RawReaderMaker: number of gl3 nodes too high: db -> " << mNumberOfGl3Nodes
				      << "  max -> " << MaxNumberOfGl3Nodes << endm;
		    return 1;
	      }
	}
	else {
	      mNumberOfGl3Nodes = MaxNumberOfGl3Nodes;
	      gMessMgr->Warning("Stl3RawReaderMaker: no table entry for this run in 'l3RunSummary' found!");
	      gMessMgr->Warning("Stl3RawReaderMaker: using default values.");
	}
  }
  else {
        mNumberOfGl3Nodes = MaxNumberOfGl3Nodes;
	gMessMgr->Warning("Stl3RawReaderMaker: no database 'Runlog/l3' found!");
	gMessMgr->Warning("Stl3RawReaderMaker: using default values.");
  }

  // gl3 node which built this event
  int gl3Id = ml3reader->getGl3Id();

  // number of algorithms
  mNumberOfAlgorithms = gl3Reader->getNumberofAlgorithms();
  
  // check number of algorithms and gl3 node id
  if (mNumberOfAlgorithms >= MaxNumberOfAlgorithms) {
        gMessMgr->Error() << "Stl3RawReaderMaker: number of algorithms exceeds limit: found "
			  << mNumberOfAlgorithms << ", limit " << MaxNumberOfAlgorithms
			  << endm;
	return 1;
  }
  if (gl3Id<=0 || gl3Id>= MaxNumberOfGl3Nodes) {
        gMessMgr->Error() << "Stl3RawReaderMaker: gl3 id exceeds limit: found "
			  << gl3Id << ", limit " << MaxNumberOfGl3Nodes
			  << endm;
	return 1;
  }

  //--------------------------
  // now do the bookkeeping
  //--------------------------

  // increase the event counter
  mEventCounter++;

  // first: global counter
  mGlobalCounter[gl3Id-1].nProcessed     = gl3Reader->getNumberOfProcessedEvents();
  mGlobalCounter[gl3Id-1].nReconstructed = gl3Reader->getNumberOfReconstructedEvents();
  // second: algorithm counter
  Algorithm_Data* algData = gl3Reader->getAlgorithmData();
  for (int i=0; i<mNumberOfAlgorithms; i++) {
        mAlgorithmCounter[gl3Id-1][i].algId      = algData[i].algId;
	mAlgorithmCounter[gl3Id-1][i].nProcessed = algData[i].nProcessed;
	mAlgorithmCounter[gl3Id-1][i].nAccept    = algData[i].nAccept;
	mAlgorithmCounter[gl3Id-1][i].nBuild     = algData[i].nBuild;
  }

  // get total counters
  GlobalCounter totalCounter;
  // make solaris happy
  const int maxAlg = mNumberOfAlgorithms;
  AlgorithmCounter* totalAlgCounter = new AlgorithmCounter[maxAlg];
  //AlgorithmCounter totalAlgCounter[maxAlg];


  totalCounter.nProcessed     = 0;
  totalCounter.nReconstructed = 0;
  for (int i=0; i<mNumberOfAlgorithms; i++) {
        totalAlgCounter[i].algId      = 0;
	totalAlgCounter[i].nProcessed = 0;
	totalAlgCounter[i].nAccept    = 0;
	totalAlgCounter[i].nBuild     = 0;
  }
  
  for (int i=0; i<mNumberOfGl3Nodes; i++) {
        // if one gl3 node wasn't seen so far,
        // the counters are still undefined
        if (mGlobalCounter[i].nProcessed==0 && mEventCounter<(2*mNumberOfGl3Nodes)) {
	      totalCounter.nProcessed     = -1;
	      totalCounter.nReconstructed = -1;
	      for (int j=0; j<mNumberOfAlgorithms; j++) {
		    totalAlgCounter[j].algId      = mAlgorithmCounter[gl3Id-1][j].algId;
		    totalAlgCounter[j].nProcessed = -1;
		    totalAlgCounter[j].nAccept    = -1;
		    totalAlgCounter[j].nBuild     = -1;
	      }
	      break;
	}
	// summ-up the counters of all gl3 nodes
	// we have seen so far after #(mNumberOfGl3Nodes*2) events
	totalCounter.nProcessed     += mGlobalCounter[i].nProcessed;
	totalCounter.nReconstructed += mGlobalCounter[i].nReconstructed;
	for (int k=0; k<mNumberOfAlgorithms; k++) {
	      totalAlgCounter[k].algId       = mAlgorithmCounter[gl3Id-1][k].algId;
	      totalAlgCounter[k].nProcessed += mAlgorithmCounter[i][k].nProcessed;
	      totalAlgCounter[k].nAccept    += mAlgorithmCounter[i][k].nAccept;
	      totalAlgCounter[k].nBuild     += mAlgorithmCounter[i][k].nBuild;
	}
  }

  //--------------------------
  // fill StL3EventSummary
  //--------------------------

  // fill counters and number of tracks
  myEventSummary->setCounters(totalCounter.nProcessed, totalCounter.nReconstructed);
  myEventSummary->setNumberOfTracks(ml3reader->getL3_Summary()->nTracks);
  myEventSummary->setL0TriggerWord(ml3reader->getL3_P()->trg_word);

  //--------------------------
  // fill StL3AlgorithmInfo
  //--------------------------
  // get database info if available
  l3AlgorithmInfo_st* dbAlgInfo = 0;
  if (mDbSet) {
        TTable* l3algorithmInfoTable = (TTable* )mDbSet->Find("l3AlgorithmInfo");
	if (l3algorithmInfoTable) {
	      dbAlgInfo = (l3AlgorithmInfo_st* )l3algorithmInfoTable->GetArray();
	      if (Debug()) {
		    for (int i=0; i<l3algorithmInfoTable->GetNRows(); i++) {
		          cout << "  run \tidxAlg\talgId\tpreScale\tpostScale" << endl;
		          cout << dbAlgInfo[i].runNumber 
			       << "\t" << dbAlgInfo[i].idxAlg
			       << "\t" << dbAlgInfo[i].algId
			       << "\t" << dbAlgInfo[i].preScale
			       << "\t" << dbAlgInfo[i].postScale << endl;
			  cout <<"  GI: " << dbAlgInfo[i].GI1 << " " << dbAlgInfo[i].GI2 << " " << dbAlgInfo[i].GI3
			       << " " << dbAlgInfo[i].GI4 << " " << dbAlgInfo[i].GI5 << endl;
			  cout << "  GF: " << dbAlgInfo[i].GF1 << " " << dbAlgInfo[i].GF2 << " " << dbAlgInfo[i].GF3
			       << " " << dbAlgInfo[i].GF4 << " " << dbAlgInfo[i].GF5 << endl;
			  cout << "------------" << endl; 
		    }
	      }
	      // check entries, just to be _really_ safe (paranoia;-)
	      if (l3algorithmInfoTable->GetNRows()!=mNumberOfAlgorithms) {
		    gMessMgr->Warning() << "Stl3RawReaderMaker: Warning: database entries don't match raw data!" << endm;
		    gMessMgr->Warning() << "Stl3RawReaderMaker: db nAlgorithms: " << l3algorithmInfoTable->GetNRows()
					<< ", raw data: " << mNumberOfAlgorithms
					<< endm;
		    gMessMgr->Warning() << "Stl3RawReaderMaker: Skip database info for this event." << endm;
		    dbAlgInfo = 0;
	      }
	}
	else {
	      gMessMgr->Warning("Stl3RawReaderMaker: No entry for this run found in table 'l3algorithmInfo'.");
	}
  }
  // now fill algorithm info in StEvent
  for (int i=0; i<mNumberOfAlgorithms; i++) {
        StL3AlgorithmInfo* myL3AlgorithmInfo = new StL3AlgorithmInfo(&algData[i]);
	if (!myL3AlgorithmInfo) {
	      gMessMgr->Error("Stl3RawReaderMaker: No StL3AlgorithmInfo.");
	      return 1;
	}
	myL3AlgorithmInfo->setCounters(totalAlgCounter[i].nProcessed,
				       totalAlgCounter[i].nAccept,
				       totalAlgCounter[i].nBuild);

	//
	// check Pass-Thru Algorithm for hadronic triggers
	// that is either TWReject && triggerWord=0x1...
	// or TRUE && triggerWord=0x1...
	// __status as of Nov. 2001__
	//
	int passThruId = 0;
	if ( (algData[i].algId==10 || algData[i].algId==1)
	     && ml3reader->getL3_P()->trg_word>0x0fff
	     && ml3reader->getL3_P()->trg_word<0x2000
	     && algData[i].build==1 ) {

	      passThruId = algData[i].algId;
	      if (Debug()) {
		    cout << "pass-thru algorithm Id: " << passThruId << endl;
	      }

	      // set unbiased flag in event summary
	      myEventSummary->setUnbiasedTrigger();
	}

	// get algorithm info from database
	// and check algId (paranoia again?)
	if (dbAlgInfo && dbAlgInfo[i].algId==totalAlgCounter[i].algId) {
	      myL3AlgorithmInfo->setPreScale(dbAlgInfo[i].preScale);
	      myL3AlgorithmInfo->setPostScale(dbAlgInfo[i].postScale);
	      int intPara[IntParameterSize] = {dbAlgInfo[i].GI1, dbAlgInfo[i].GI2,
					       dbAlgInfo[i].GI3, dbAlgInfo[i].GI4,
					       dbAlgInfo[i].GI5};
	      float floatPara[FloatParameterSize] = {dbAlgInfo[i].GF1, dbAlgInfo[i].GF2,
						     dbAlgInfo[i].GF3, dbAlgInfo[i].GF4,
						     dbAlgInfo[i].GF5};
	      myL3AlgorithmInfo->setParameters(intPara, floatPara);

	      if (dbAlgInfo[i].algId==passThruId)
		    myEventSummary->setUnbiasedTriggerPreScale(dbAlgInfo[i].preScale);

	} // if dbAlgInfo
	myEventSummary->addAlgorithm(myL3AlgorithmInfo);

  } // for mNumberOfAlgorithms

  // delete counter array
  delete [] totalAlgCounter;


  // call filling routines
  // global tracks and vertex
  if (ml3reader->getGlobalTrackReader()) {

	// vertex position
	StPrimaryVertex* myL3Vertex = new StPrimaryVertex;
	vertex vert = ml3reader->getGlobalTrackReader()->getVertex();
	StThreeVectorF* pos = new StThreeVectorF(vert.x, vert.y, vert.z);
	myL3Vertex->setPosition(*pos);
	myStL3Trigger->addPrimaryVertex(myL3Vertex);
 
       if (fillStEventWithL3GlobalTracks()!=0) return 1;

  }

  // i960 hits
  //if (  ml3reader->getI960ClusterReader(1) )
  //  { if (  fillStEventWithi960Hits() != 0 ) return 1; } ;
  

  // all right go home
  return 0 ;  
}


//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::fillStEventWithL3GlobalTracks() 
{ 
  // get track nodes
  StSPtrVecTrackNode& myTrackNodeVector = myStL3Trigger->trackNodes();
  StSPtrVecTrackDetectorInfo& myTrackDetectorInfoVector = myStL3Trigger->trackDetectorInfo();
  
  // loop over rawdata tracks and fill them into StEvent
  int numberOfTracks = ml3reader->getGlobalTrackReader()->getNumberOfTracks();
  gMessMgr->Info() << "Stl3RawReaderMaker: Try to fill " << numberOfTracks << " tracks into StEvent." << endm;

  // get L3 raw tracks
  globalTrack* globalL3Tracks = ml3reader->getGlobalTrackReader()->getTrackList();

  // get magnetic field
  Float_t xval[3] = {0.,0.,0.};
  Float_t bval[3];
  gufld(xval,bval);
  int signOfField = bval[2] < 0 ? -1 : 1;

  for (int trackindex=0; trackindex<numberOfTracks; trackindex++) {
        StTrackDetectorInfo* detectorInfo = new StTrackDetectorInfo();
	detectorInfo->setNumberOfPoints(globalL3Tracks[trackindex].nHits,kTpcId);
	myTrackDetectorInfoVector.push_back(detectorInfo);
		
	// StTrackGeometry
	StThreeVectorD* origin = new StThreeVectorD(globalL3Tracks[trackindex].r0 * TMath::Cos(globalL3Tracks[trackindex].phi0),
						    globalL3Tracks[trackindex].r0 * TMath::Sin(globalL3Tracks[trackindex].phi0),
						    globalL3Tracks[trackindex].z0);
	StThreeVectorD* momentum = new StThreeVectorD(globalL3Tracks[trackindex].pt * TMath::Cos(globalL3Tracks[trackindex].psi),
						      globalL3Tracks[trackindex].pt * TMath::Sin(globalL3Tracks[trackindex].psi),
						      globalL3Tracks[trackindex].pt * globalL3Tracks[trackindex].tanl );
	StHelixModel* helixModel = new StHelixModel( globalL3Tracks[trackindex].q,
						     globalL3Tracks[trackindex].psi,
						     0.0,
						     atan(globalL3Tracks[trackindex].tanl), 
						     *origin, 
						     *momentum, -1 );		
      // StGlobalTrack
      StGlobalTrack* globalTrack = new StGlobalTrack();
      globalTrack->setFlag(globalL3Tracks[trackindex].flag);
      globalTrack->setLength(globalL3Tracks[trackindex].length);
      globalTrack->setDetectorInfo(detectorInfo);
      globalTrack->setGeometry(helixModel);
      // set finder/fitting method
      int l3word = kL3FitId + (1<<l3Standard);
      //cout << l3word << endl; // should print 16390
      globalTrack->setEncodedMethod(l3word); 

      // helicity h = -sign(q*B)
      short h = globalTrack->geometry()->charge()*signOfField > 0 ? -1 : 1;
      globalTrack->geometry()->setHelicity(h);

      // curvature
      double curv = fabs(0.0003 * bval[2] / globalL3Tracks[trackindex].pt);
      globalTrack->geometry()->setCurvature(curv);

      // add dE/dx information
      if (globalL3Tracks[trackindex].dedx > 0) {
	    StDedxPidTraits* myStDedxPidTraits =  new StDedxPidTraits(kTpcId, kTruncatedMeanId,
								      globalL3Tracks[trackindex].ndedx,
								      globalL3Tracks[trackindex].dedx*1e6, 0);
	    globalTrack->addPidTraits(myStDedxPidTraits);
      }

      // StTrackNode
      StTrackNode* trackNode = new StTrackNode();
      trackNode->addTrack(globalTrack);
      myTrackNodeVector.push_back(trackNode);


      //==================================
      // check dca
      //double kapa = fabs(0.0003 * bval[2] / globalL3Tracks[trackindex].pt);
      //double lambda = atan(globalL3Tracks[trackindex].tanl);
      //double phase = globalL3Tracks[trackindex].psi - h * TMath::Pi()/2;
      //StHelixD* track = new StHelixD(kapa, lambda, phase, *origin, h);
      //StThreeVectorD vertex(0,0,myStL3Trigger->primaryVertex()->position().z());
      //if (globalL3Tracks[trackindex].nHits>20 && globalL3Tracks[trackindex].q<0)
      //cout << " dca = " << track->distance(vertex) << ", bval = " << bval[2] << endl;
      //==================================


    }
  // ok
  return 0 ;
}


//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::fillStEventWithi960Hits() 
{   
  // create StTpcHitCollection and connect it to StEvent
  StTpcHitCollection* mHitCollection = new StTpcHitCollection() ; 
  myStL3Trigger->setTpcHitCollection(mHitCollection);

  // prepare transformation from pad,time,padrow to x,y,z
  St_l3_Coordinate_Transformer transformer ;
  transformer.Print_parameters() ;
  St_l3_xyz_Coordinate XYZ(0,0,0) ;
  St_l3_ptrs_Coordinate PTRS(0,0,0,0) ;

  // loop over clusters and fill them into StEvent
  Int_t totalcluster = 0 ;
  for ( Int_t secindex=1 ; secindex<=24 ; secindex+=2 )
    {
      if ( ml3reader->getI960ClusterReader(secindex)->getClusterList() )
	{
	  gMessMgr->Info() << "Stl3RawReaderMaker: Found some i960 clusters in sector:" << secindex <<endm ;
	  L3_Cluster* myl3cluster = ml3reader->getI960ClusterReader(secindex)->getClusterList() ;
	  Int_t numOfClusters = ml3reader->getI960ClusterReader(secindex)->getNumberOfClusters() ;
	  
	  for (Int_t clindex=0 ;  clindex < numOfClusters ; clindex++)
	    {
	      totalcluster++ ;
	      // pad,time,row,q,flag
	      Double_t pad  = ((Double_t)(myl3cluster[clindex].pad)) / 64 ;
	      Double_t time = ((Double_t)(myl3cluster[clindex].time)) / 64 ;
	      Int_t row  = myl3cluster[clindex].padrow ;
	      Int_t q    = myl3cluster[clindex].charge ;
	      Char_t flag = myl3cluster[clindex].flags  ;

	      // upper 4 bits are RB, lower 4 bits are MZ
	      Int_t RBMZ = myl3cluster[clindex].RB_MZ  ;
	      Int_t rb   = RBMZ >> 4 ;
	      Int_t mz   = RBMZ & 15 ;
	      
	      // determine sector
	      Int_t sector = 0 ;
	      if (rb<=6) { sector = secindex; } else { sector = secindex+1; }      
	      
	      // coordinate transformation
	      PTRS.Setptrs((Double_t) pad, (Double_t) time,(Double_t) row, (Double_t) sector) ;
	      transformer.raw_to_global(PTRS,XYZ) ;
	      
	      // some output
	      if (clindex%500==0)
		{
		  cout << XYZ.Getx() <<"\t" << XYZ.Gety() <<"\t" << XYZ.Getz() <<"\t";
		  cout << row  <<"\t" << sector << "\t" << rb  << "\t" << mz  << "\t" << q << "\t";
		  cout << ((Double_t)(myl3cluster[clindex].pad)) / 64 <<"\t" ;
		  cout << ((Double_t)(myl3cluster[clindex].time)) / 64 <<"\t" ;
		  cout << (Int_t)(myl3cluster[clindex].padrow)  <<"\n" ;
		}
		  
	      // Fill it
	      // position and error
	      StThreeVectorF* pos = new StThreeVectorF(XYZ.Getx(),XYZ.Gety(),XYZ.Getz()) ;
	      StThreeVectorF* poserror = new StThreeVectorF(0,0,0) ;
	      // pack sec and row : bits 4-8 = sector[1,24] and bits 9-14 = padrow[1-45]
	      ULong_t hw = 0 ;
	      ULong_t hrow = 0 ;
	      ULong_t hsec = 0 ;
	      if ( row >=1 && row <=45 )  { hrow = row << 9 ; } else { hrow=0 ; } 
	      if ( sector >=1 && sector <=24 )  { hsec = sector << 4 ; } else { hsec=0 ; } 
	      hw = hw | hrow ;
	      hw = hw | hsec ;
	      // track reference counter set always to 0
	      UChar_t c = 0 ;
	      // create hit
	      StTpcHit* tpcHit = new StTpcHit(*pos,*poserror,hw,q,c) ;
	      tpcHit->setFlag(flag) ;
	      // add to hit collection
	      if (tpcHit) { mHitCollection->addHit(tpcHit) ;} else { delete tpcHit; return 1;}				 
	    } // clusters
	} // if ...
    } // sectors
  cout << "total found clusters " << totalcluster << endl;
  // ok
  return 0 ;
}


