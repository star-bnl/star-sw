//*-- Author : Dominik Flierl
//             Christof Struck
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// This Maker reads l3 data as they come with the raw data stream       //
// from the experiment and fills them into StEvent or into a            //
// TTree Structure which can then be used as a l3 Mini DST              //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
//
//  $Id: Stl3RawReaderMaker.cxx,v 1.10 2001/11/14 23:30:56 struck Exp $
//
//  $Log: Stl3RawReaderMaker.cxx,v $
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
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StDAQMaker/StDAQReader.h"
#include "tables/St_l3RunSummary_Table.h"
#include "tables/St_l3AlgorithmInfo_Table.h"
#include "StEventTypes.h"
#include "StEnumerations.h"
#include "TTree.h"
#include "Stl3MiniEvent.h"
#include "Rtypes.h"
#include "TMath.h"
#include "TH1.h"
#include "TF1.h"

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

  // Make Connection to raw data
  DAQReaderSet = GetDataSet("StDAQReader");
 
  // set switches
  mWriteMiniEvent = kFALSE;
  mWriteStEvent   = kTRUE;
  mCalculateVertex = 0; // =1 or =2 for vertex finding routines
  mL3On = kFALSE;

  //SetDebug(1);

  // reset database pointer
  mDbSet = 0;

  // allocate memory
  //mGlobalCounter = new GlobalCounter[mMaxNumberOfGl3Nodes];
  //mAlgorithmCounter = new AlgorithmCounter[mMaxNumberOfGl3Nodes][mMaxNumberOfAlgorithms];

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

  // create TTree and Branch
  if (mWriteMiniEvent) {
        mGlobalTrackTree = new TTree("L3GTracks","L3Globaltracks") ;
	mL3Event = new Stl3MiniEvent() ;
	mGlobalTrackTree->Branch("L3Event","Stl3MiniEvent",&mL3Event,128000,1);
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
    cout << "Now we start l3RawReader Maker. \n" ;
    

    // get the l3 daqreader
    StDAQReader *daqReader = (StDAQReader*)(DAQReaderSet->GetObject()) ;
    if (daqReader) { 
          ml3reader = daqReader->getL3Reader();

	  if (ml3reader) {

	        // set flag L3 ON
	        mL3On = kTRUE;

		// debug output
	        if (m_DebugLevel) {
		      int sec = 23;
		      if (ml3reader->getGlobalTrackReader())
			    cout << ml3reader->getGlobalTrackReader()->getNumberOfTracks()
				 << " global tracks found.\n";
		      if (ml3reader->getSl3ClusterReader(sec))
			    cout << ml3reader->getSl3ClusterReader(sec)->getNumberOfClusters() 
				 << " sl3 clusters found in sec " << sec <<" .\n";
		      if (ml3reader->getSl3TrackReader(sec))
			    cout << ml3reader->getSl3TrackReader(sec)->getNumberOfTracks()
				 << " sl3 tracks found in sec " << sec <<" .\n";
		      if (ml3reader->getI960ClusterReader(sec))
			    cout << ml3reader->getI960ClusterReader(sec)->getNumberOfClusters()
				 << " i960 clusters found in sec " << sec <<" .\n";
		}

		// fill StEvent
		if (mWriteStEvent) {
		      if (fillStEvent() != 0) {
			    cout << "ERROR: problems filling l3 into StEvent." << endl;
			    return kStErr;
		      }
		}

		// fill tree
		if (mWriteMiniEvent) {
		      if ( fillTree() != 0 ) {
			    cout << "problems filling l3 tree.\n" ;
			    return kStWarn ;
		      }
		}

	  } // if (ml3reader)

	  else {
	        // if L3 is on for this run
	        // crash chain
	        if (mL3On) {
		  cout << "ERROR: L3 is ON, but no l3 data found in this event." << endl;
		      return kStErr;
		}
		// if L3 is off so far
		// it may be switched off for this run
		// so don't crash the chain
		else {
		      cout << "WARNING: no l3 data found." << endl;
		      return kStWarn;
		}
	  } 

    } // if (daqReader)

    // go home
    return kStOk;
}


//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::fillTree()
{
  // global tracks
  if (  ml3reader->getGlobalTrackReader()->getTrackList() )
    { if ( fillMiniEventWithL3GlobalTracks() !=0 ) return 1; }

  // i960 hits
  if (  ml3reader->getI960ClusterReader(1) )
    { if ( fillMiniEventWithi960Clusters() != 0 ) return 1; }

  // sl3Tracks and sl3Hits may be added here

  
  //////
  // Fill Tree
  //////
  // tracks and possibly hits are already in 
  mL3Event->SetNTracks(ml3reader->getGlobalTrackReader()->getNumberOfTracks()) ;
  mL3Event->SetNHits(ml3reader->getGlobalTrackReader()->getNumberOfHits()) ;
  mL3Event->SetVertex(ml3reader->getGlobalTrackReader()->getVertex().x,
		      ml3reader->getGlobalTrackReader()->getVertex().y,
 		      ml3reader->getGlobalTrackReader()->getVertex().z ) ;

  mGlobalTrackTree->Fill();

  // all right go home
  return 0 ;  
}


//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::fillMiniEventWithL3GlobalTracks()
{
  // get l3tracks out of .daq file
  globalTrack* globalL3Tracks = ml3reader->getGlobalTrackReader()->getTrackList() ;

  // get TClonesArray 
  TClonesArray& trackArray = *(mL3Event->GetTrackArray()) ;
  trackArray.Clear() ;

  // loop over tracks and fill them into TClonesArray
  Int_t numTracks = ml3reader->getGlobalTrackReader()->getNumberOfTracks() ;
  cout << numTracks <<" global Tracks expected.\n" ;
  for(Int_t trackid = 0 ; trackid < numTracks ; trackid++)
    {
      if (trackid%1000 ==0 ) cout << trackid << "hea" << endl ;
      new(trackArray[trackid]) Stl3Track(
					 globalL3Tracks[trackid].nHits ,
					 globalL3Tracks[trackid].q ,
					 globalL3Tracks[trackid].flag ,
					 globalL3Tracks[trackid].innerMostRow , 
					 globalL3Tracks[trackid].outerMostRow ,
					 globalL3Tracks[trackid].pt ,
					 globalL3Tracks[trackid].psi ,
					 globalL3Tracks[trackid].tanl ,
					 globalL3Tracks[trackid].z0 ,
					 globalL3Tracks[trackid].phi0 ,
					 globalL3Tracks[trackid].r0 ,
					 globalL3Tracks[trackid].length
					 ) ; 
	  ml3reader->getGlobalTrackReader()->getTrackList() ;
	  ml3reader->getGlobalTrackReader()->getNumberOfTracks() ;
    }
  //ok
  return 0;
}


//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::fillMiniEventWithi960Clusters()
{
  // get TClonesArray 
  TClonesArray& hitArray = *(mL3Event->GetHitArray()) ;
  hitArray.Clear() ;

  //loop over sectors
  Int_t hitArrayIndex  = 0 ;
  for(Int_t sec=1 ;sec <=24 ; sec +=2 )
    {
      // get clusters for this sector 
      L3_Cluster* i960cluster = ml3reader->getI960ClusterReader(sec)->getClusterList() ;    
      
      // loop over clusters and fill them into TClonesArray
      Int_t a = ml3reader->getI960ClusterReader(sec)->getNumberOfClusters(); 
      cout << a <<  "  clusters expected in sec  " << sec << endl ; 
      for(Int_t clusindex = 0 ; clusindex < a  ;clusindex++)
	    {
	      //if (clusindex%10000 ==0 ) cout << clusindex <<endl ;
	      new(hitArray[hitArrayIndex]) Stl3Hit(
						   i960cluster[clusindex].pad ,
						   i960cluster[clusindex].time ,
						   i960cluster[clusindex].padrow ,
						   i960cluster[clusindex].charge 
						   ) ;
	      ml3reader->getI960ClusterReader(sec)->getNumberOfClusters() ;
	      hitArrayIndex++ ; 
	    }
        }
  // ok
  cout << hitArrayIndex << " i960 clusters found.\n" ;
  return 0;
}


//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

Int_t Stl3RawReaderMaker::fillStEvent() 
{

  // get StEvent if not return
  mStEvent = (StEvent *) GetInputDS("StEvent");
  if (!mStEvent) {
        cout << "No StEvent" << endl;
	return 1;
  }
  
  // create StL3Trigger and connect it
  myStL3Trigger = new StL3Trigger() ;
  if (!myStL3Trigger) {
        cout << "No Stl3Trigger." << endl;
	return 1;
  }
  mStEvent->setL3Trigger(myStL3Trigger);

  // create StL3EventSummary
  StL3EventSummary* myEventSummary = new StL3EventSummary(ml3reader->getL3_SUMD());
  if (!myEventSummary) {
        cout << "No Stl3EventSummary." << endl;
	return 1;
  }
  // connect StL3EventSummary to StL3Trigger
  myStL3Trigger->setL3EventSummary(myEventSummary);


  // get Gl3AlgorithmReader
  Gl3AlgorithmReader* gl3Reader = ml3reader->getGl3AlgorithmReader();
  if (!gl3Reader) {
        cout << "ERROR: L3 is ON, but L3 summary data is missing!" << endl;
	return 1;
  }

  // check data
  // L3_summary.on==0 indicates that the event crashed in
  // L3 online and raw data contains no valid information for us
  if (ml3reader->getL3_Summary()->on == 0) {
        cout << "Warning: L3 crashed online on this event, no usefull information."
	     << endl;
	myEventSummary->setCounters(-1, -1);
	return 0;
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
	      if (m_DebugLevel) {
		    cout << "database: runNumber = " << data->runNumber << endl;
		    cout << "database: nGl3Nodes = " << data->nGl3Nodes << endl;
	      }
	      // check limit
	      if (mNumberOfGl3Nodes>MaxNumberOfGl3Nodes) {
		    cout << " ERROR: number of gl3 nodes too high: db -> " << mNumberOfGl3Nodes
			 << "  max -> " << MaxNumberOfGl3Nodes << endl;
		    return 1;
	      }
	}
	else {
	      mNumberOfGl3Nodes = MaxNumberOfGl3Nodes;
	      cout << "  no table entry for this run in 'l3RunSummary' found!" << endl;
	      cout << "  using default values." << endl;
	}
  }
  else {
        mNumberOfGl3Nodes = MaxNumberOfGl3Nodes;
	cout << "  no database 'Runlog/l3' found!" << endl;
	cout << "  using default values." << endl;
  }

  // gl3 node which built this event
  int gl3Id = ml3reader->getGl3Id();

  // number of algorithms
  mNumberOfAlgorithms = gl3Reader->getNumberofAlgorithms();
  
  // check number of algorithms and gl3 node id
  if (mNumberOfAlgorithms >= MaxNumberOfAlgorithms) {
        cout << "ERROR: number of algorithms exceeds limit: found "
	     << mNumberOfAlgorithms << ", limit " << MaxNumberOfAlgorithms
	     << endl;
	return 1;
  }
  if (gl3Id<=0 || gl3Id>= MaxNumberOfGl3Nodes) {
        cout << "ERROR: gl3 id exceeds limit: found "
	     << gl3Id << ", limit " << MaxNumberOfGl3Nodes
	     << endl;
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
	      totalAlgCounter[k].algId       = mAlgorithmCounter[i][k].algId;
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
	      if (m_DebugLevel) {
		    for (int i=0; i<l3algorithmInfoTable->GetNRows(); i++) {
		          cout << "  run \tidxAlg\talgId\tpreScale\tpostScale" << endl;
			  cout << dbAlgInfo[i].runNumber 
			       << "\t" << dbAlgInfo[i].idxAlg
			       << "\t" << dbAlgInfo[i].algId
			       << "\t" << dbAlgInfo[i].preScale
			       << "\t" << dbAlgInfo[i].postScale << endl;
			  cout << "  GI: " << dbAlgInfo[i].GI1 << " " << dbAlgInfo[i].GI2 << " " << dbAlgInfo[i].GI3
			       << " " << dbAlgInfo[i].GI4 << " " << dbAlgInfo[i].GI5 << endl;
			  cout << "  GF: " << dbAlgInfo[i].GF1 << " " << dbAlgInfo[i].GF2 << " " << dbAlgInfo[i].GF3
			       << " " << dbAlgInfo[i].GF4 << " " << dbAlgInfo[i].GF5 << endl;
			  cout << "------------" << endl;
		    }
	      }
	      // check entries, just to be _really_ safe (paranoia;-)
	      if (l3algorithmInfoTable->GetNRows()!=mNumberOfAlgorithms) {
		    cout << "Warning: database entries don't match raw data!" << endl;
		    cout << " db nAlgorithms: " << l3algorithmInfoTable->GetNRows()
			 << ", raw data: " << mNumberOfAlgorithms
			 << endl;
		    cout << " Skip database info for this event." << endl;
		    dbAlgInfo = 0;
	      }
	}
	else {
	      cout << "No entry for this run found in table 'l3algorithmInfo'." << endl;
	}
  }
  // now fill algorithm info in StEvent
  for (int i=0; i<mNumberOfAlgorithms; i++) {
        StL3AlgorithmInfo* myL3AlgorithmInfo = new StL3AlgorithmInfo(&algData[i]);
	if (!myL3AlgorithmInfo) {
	      cout << "No StL3AlgorithmInfo." << endl;
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
	      if (m_DebugLevel) {
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
  

  // calculate vertex offline and fill into StEvent
//   if (  ml3reader->getGlobalTrackReader()->getTrackList() )
//       { 
// 	  StPrimaryVertex* mVertex1 = new StPrimaryVertex() ;
// 	  if ( findVertexMethod1(*mVertex1) !=0 ) 
// 	      {
// 		  delete mVertex1 ;
// 		  return 1 ; 
// 	      }
// 	  else
// 	      {
// 		  myStL3Trigger-> addPrimaryVertex(mVertex1) ;
// 		  cout << "mVertex straight lines :  " << mVertex1->position().x() << "\t" ;
// 		  cout << mVertex1->position().y() << "\t" << mVertex1->position().z() << endl ;
// 	      }


// 	  StPrimaryVertex* mVertex2 = new StPrimaryVertex() ;
// 	  if ( findVertexMethod2(*mVertex2) !=0 ) 
// 	      {
// 		  delete mVertex2 ;
// 		  return 1 ; 
// 	      }
// 	  else
// 	      {
// 		  myStL3Trigger-> addPrimaryVertex(mVertex2) ;
// 		  cout << "mVertex helixes :  " << mVertex2->position().x() << "\t" ;
// 		  cout << mVertex2->position().y() << "\t" << mVertex2->position().z() << endl ;
// 	      }
//     } ;


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
  cout << "Try to fill " << numberOfTracks << " tracks into StEvent." << endl;

  // get L3 raw tracks
  globalTrack* globalL3Tracks = ml3reader->getGlobalTrackReader()->getTrackList();

  // get magnetic field
  Float_t xval[3] = {0.,0.,0.};
  Float_t bval[3];
  gufld(xval,bval);
  int signOfField = bval[2] < 0 ? -1 : 1;

  for (int trackindex=0; trackindex<numberOfTracks; trackindex++) {
        StTrackDetectorInfo* detectorInfo = new StTrackDetectorInfo();
	detectorInfo->setNumberOfPoints(globalL3Tracks[trackindex].nHits);
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
      globalTrack->setFlag(globalL3Tracks[trackindex].flag) ;
      globalTrack->setLength(globalL3Tracks[trackindex].length) ;
      globalTrack->setDetectorInfo(detectorInfo) ;
      globalTrack->setGeometry(helixModel) ;

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
	  cout << "Found some i960 clusters in sector:" << secindex <<endl ;
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
  cout <<"total found clusters " << totalcluster << endl ;
 // ok
  return 0 ;
}


//_____________________________________________________________________________
Int_t Stl3RawReaderMaker::findVertexMethod1(StPrimaryVertex& mvertex)
{
    // get L3 raw tracks
    globalTrack* globalL3Tracks = ml3reader->getGlobalTrackReader()->getTrackList() ;

    // loop over rawdata tracks and fill them into StEvent
    Double_t b = 0 ;
    Int_t countz = 0 ;
    Double_t vertexZ = 9999 ;
    TH1D* vertexZdis = new TH1D("vz","vz",800,-200,200) ;
    vertexZdis->Reset();
    Int_t numberOfTracks = ml3reader->getGlobalTrackReader()->getNumberOfTracks() ;
    cout << "Try to calculate vertex with " << numberOfTracks << " tracks and straight line approximation .\n" ; 
    for(Int_t trackid = 0 ; trackid < numberOfTracks ;  trackid++)
	{
	    if ( globalL3Tracks[trackid].nHits>14  && globalL3Tracks[trackid].pt >1.0)
	    	{
		    double psi = globalL3Tracks[trackid].psi ;
		    double phi = globalL3Tracks[trackid].phi0 ;
		    b = globalL3Tracks[trackid].z0  - globalL3Tracks[trackid].r0 * cos(psi-phi) * globalL3Tracks[trackid].tanl ;
		    
		    if (b<200 && b>-200)
			{
			  //if (countz%1000 ==0) {     cout << " b: \t" << b ; } ;
			    vertexZ += b;
			    countz++;
			    vertexZdis->Fill(b) ;
			}
		}
  	}
 
    if (countz !=0)
	{
	  Int_t maxbin = vertexZdis->GetMaximumBin() ;
	  Double_t maxval = vertexZdis->GetBinCenter(maxbin) ;
	  
	  TF1 mygaus("mygaus","[0]*exp(-0.5*( (x-[1])/[2])^2)", maxval-10 ,maxval+10 );
	  mygaus.SetParNames("Constant","Mean_value","Sigma");
	  mygaus.SetParameter(0,20);
	  mygaus.SetParameter(1,maxval);
	  mygaus.SetParameter(2,2);
	  vertexZdis->Fit("mygaus","Q0R");
	  
	  //cout << "fit :" << mygaus->GetParameter(1)  << endl ;
	  // vertexZ = vertexZ/countz ;
	  vertexZ = mygaus.GetParameter(1);
	  //cout << endl << "vertexZ: " << vertexZ << "\t";
	  //cout << "  count: " << countz << endl ; 
	  
 	}
    else
	{
	    vertexZ = 9999 ;
	}

   
    // fill it
    StThreeVectorF* pos = new StThreeVectorF(0.0,0.0,vertexZ);
    mvertex.setPosition(*pos);

    // clean up
    delete vertexZdis ;

    // ok go home
    return 0;
}


//________________________________________________________________________
Int_t Stl3RawReaderMaker::findVertexMethod2(StPrimaryVertex& mvertex)
{
    // get L3 raw tracks
    globalTrack* globalL3Tracks = ml3reader->getGlobalTrackReader()->getTrackList() ;

    // some preparation
    Int_t numberOfTracks = ml3reader->getGlobalTrackReader()->getNumberOfTracks() ;
    cout << "Try to calculate vertex with  " << numberOfTracks << " tracks and helix extrapolation.\n" ; 
    StHelixD hel;
    
    /////
    // find vertex z position
    /////
    Double_t B = 0.25 * 0.01 ; // this is important ! B-field in right dimension : * 0.01 
    TH1D* vertexZdis = new TH1D("vz2","vz2",800,-200,200) ;
    vertexZdis->Reset();
    {for(Int_t trackid = 0 ; trackid < numberOfTracks ;  trackid++)
	{
	  if ( globalL3Tracks[trackid].nHits>14  && globalL3Tracks[trackid].pt > 0.2  )
	    { 
	      // make a StHelix out of l3 track
	      Double_t c     = fabs(0.3 * globalL3Tracks[trackid].q * B / (globalL3Tracks[trackid].pt)) ;
	      Double_t dip   = atan(globalL3Tracks[trackid].tanl) ;
	      Double_t h     = -((globalL3Tracks[trackid].q*B)/fabs(globalL3Tracks[trackid].q*B));
	      Double_t phase = globalL3Tracks[trackid].psi-h*TMath::Pi()/2 ;
	      StThreeVectorD orig(globalL3Tracks[trackid].r0*cos(globalL3Tracks[trackid].phi0),
				  globalL3Tracks[trackid].r0*sin(globalL3Tracks[trackid].phi0),
				  globalL3Tracks[trackid].z0);
	     
	     
	  	     
	      hel.setParameters( c , dip , phase , orig ,  h ) ;

	      Double_t ver = hel.z(hel.pathLength(0,0)) ;
	      //cout << ver << "\t" ;
	      vertexZdis->Fill(ver) ;
	    }
	}}
    // fit z vertex
    Double_t vertexZ = 9999 ;
    TF1 *mygaus = new TF1("mygaus","[0]*exp(-0.5*( (x-[1])/[2])^2)", -10 ,10 );
    if (vertexZdis->GetEntries() > 3)
      {
	Int_t maxbin = vertexZdis->GetMaximumBin() ;
	Double_t maxval = vertexZdis->GetBinCenter(maxbin) ;
	
	mygaus->SetRange(maxval-10 ,maxval+10 );
	mygaus->SetParNames("Constant","Mean_value","Sigma");
	mygaus->SetParameter(0,20);
	mygaus->SetParameter(1,maxval);
	mygaus->SetParameter(2,2);
	vertexZdis->Fit("mygaus","Q0R");
	
	//cout << "fit :" << mygaus->GetParameter(1)  << endl ;
	vertexZ = mygaus->GetParameter(1) ; 	
      }
    else
      {
	vertexZ = 9999 ;
      }

    ////
    // now calculate the x-y position 
    ////
    StThreeVectorD* normale = new   StThreeVectorD(0,0,1) ;
    StThreeVectorD* center  = new   StThreeVectorD(0,0,vertexZ) ;
    TH1D* vertexX = new TH1D("vertexX","vertexX",100,-5,5);
    TH1D* vertexY = new TH1D("vertexY","vertexY",100,-5,5);

    {for(Int_t trackid = 0 ; trackid < numberOfTracks ;  trackid++)
      {
	if ( globalL3Tracks[trackid].nHits>14  && globalL3Tracks[trackid].pt >0.2 )
	  { 
	    // make a StHelix out of l3 track
	    Double_t c     = fabs(0.3 * globalL3Tracks[trackid].q * B / (globalL3Tracks[trackid].pt)) ;
	    Double_t dip   = atan(globalL3Tracks[trackid].tanl) ;
	    Double_t h     = -((globalL3Tracks[trackid].q*B)/fabs(globalL3Tracks[trackid].q*B));
	    Double_t phase = globalL3Tracks[trackid].psi-h*TMath::Pi()/2 ;
	    StThreeVectorD orig(globalL3Tracks[trackid].r0*cos(globalL3Tracks[trackid].phi0),
				globalL3Tracks[trackid].r0*sin(globalL3Tracks[trackid].phi0),
				globalL3Tracks[trackid].z0);
	    
	    
	    hel.setParameters( c , dip , phase , orig ,  h ) ;
	    
	    
	    Double_t vertexXp = hel.x(hel.pathLength(*center, *normale)) ;
	    Double_t vertexYp = hel.y(hel.pathLength(*center, *normale)) ;
	    //cout << vertexXp << "\t" ;
	    //cout << vertexYp << endl ;
	    vertexX->Fill(vertexXp) ;
	    vertexY->Fill(vertexYp) ;
	  }
      }}
     
    /////
    // fit xy vertex
    /////
    Double_t vertexXpos ;
    Double_t vertexYpos ; 
    if (vertexX->GetEntries() > 3 && vertexY->GetEntries() > 3 )
      {
	//x
	Int_t maxbin = vertexX->GetMaximumBin() ;
	Double_t maxval = vertexX->GetBinCenter(maxbin) ;
	
	mygaus->SetRange(maxval-2 ,maxval+2 );
	mygaus->SetParameter(0,20);
	mygaus->SetParameter(1,maxval);
	mygaus->SetParameter(2,1);
	vertexX->Fit("mygaus","Q0R");
	
	//cout << "vertex x position fit :" << mygaus->GetParameter(1)  << endl ;
	vertexXpos = mygaus->GetParameter(1) ;  
	if ( fabs(vertexXpos) >100 ) vertexXpos = 100 ;

	//y
	maxbin = vertexY->GetMaximumBin() ;
	maxval = vertexY->GetBinCenter(maxbin) ;
	mygaus->SetRange(maxval-2 ,maxval+2 );
	mygaus->SetParameter(0,20);
	mygaus->SetParameter(1,maxval);
	mygaus->SetParameter(2,1);
	vertexY->Fit("mygaus","Q0R");
	
	//cout << "vertex y position fit :" << mygaus->GetParameter(1)  << endl ;
	vertexYpos = mygaus->GetParameter(1) ;  
	if ( fabs(vertexYpos) >100 ) vertexYpos = 100 ;
      }
    else
	{
	    vertexXpos = vertexYpos = 9999 ;
	}

    // fill it
    StThreeVectorF* pos = new StThreeVectorF(vertexXpos,vertexYpos,vertexZ);
    mvertex.setPosition(*pos);

    // clean up 
    delete  mygaus ;
    delete  vertexX ;
    delete  vertexY ;
    delete  normale ;
    delete  center ;
    delete  vertexZdis ;

    // ok
    return 0 ;
}

