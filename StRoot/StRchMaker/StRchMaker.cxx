/***************************************************************************
 *
 * $Id: StRchMaker.cxx,v 1.15 2000/04/05 21:25:18 lasiuk Exp $
 *
 * Author:  bl
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              StRchMaker.cxx - ROOT/STAR Maker for offline chain.
 *              Incorporation of cluster finder here
 ***************************************************************************
 *
 * $Log: StRchMaker.cxx,v $
 * Revision 1.15  2000/04/05 21:25:18  lasiuk
 * with CF
 *
 * Revision 1.17  2000/05/18 21:57:19  lasiuk
 * dev patch
 *
 * Revision 1.16  2000/05/18 11:42:20  lasiuk
 * mods for pre StEvent writing
 *
 * Revision 1.15  2000/04/05 21:25:18  lasiuk
 * with CF
 *
 * Revision 1.14  2000/03/12 23:49:26  lasiuk
 * order of arguments for the reader
 *
 * Revision 1.13  2000/02/21 23:20:10  lasiuk
 * debug output formatting and reduce output to screen
 *
 * Revision 1.12  2000/02/14 20:50:29  lasiuk
 * use DAQ/sim interface with a switch settable at the c'tor
 *
 * Revision 1.11  2000/01/12 16:52:58  lasiuk
 * comment out assert statement
 *
 * Revision 1.10  2000/01/11 21:18:04  lasiuk
 * Fills new dst_rch_pixel;
 * debug macros;
 * used in first DAQ data
 *
 * Revision 1.9  1999/09/24 01:23:22  fisyak
 * Reduced Include Path
 **************************************************************************/

#include <iostream.h>

#include "StRchMaker.h"
#include "StChain.h"

#include "StEventTypes.h"
#include "StEvent/StRichMCHit.h"
// Interfaces
//
// DAQ Libraries

#include "StDaqLib/RICH/RICH_Reader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "StRrsMaker/StRichPadPlane.h"
#include "StRrsMaker/StRichSingleMCPixel.h"
// Database
#ifdef RCH_WITH_PAD_MONITOR
#include "StRichHit.h"

#include "StRichSingleHitCollection.h"
#endif

// Internal Rch
#include "StRichSimpleHit.h"
#include "StRichClusterAndHitFinder.h"
#include "StRichSimpleHitCollection.h"

//
// dst tables in $STAR/include/tables/
#include "tables/St_g2t_rch_hit_Table.h"
#include "tables/St_dst_rch_pixel_Table.h"

//
// line items of the table
// in $STAR/include
//idl in $STAR/include/../pams/global/idl/dst_rch_pixel.idl
#include "dst_rch_pixel.h"
ClassImp(StRchMaker) // macro
StRchMaker::StRchMaker(const char *name, int daq, int matrix, int sa)
    : StMaker(name), mDaq(daq), mUseMatrix(matrix), mWithDstPixels(sa)

    StRchMaker::StRchMaker(const char *name, int daq, int matrix, int cf)
	: StMaker(name), mDaq(daq), mUseMatrix(matrix), mCfOnly(cf)
{


#ifdef RCH_HISTOGRAM   // in the .h file
    mRchNTupleFile = 0;
    mPadPlane = 0;
    
#endif
    drawinit=kFALSE;
}

//-----------------------------------------------------------------

StRchMaker::~StRchMaker() {}

//-----------------------------------------------------------------

Int_t StRchMaker::Init() {
    cout << "In StRchMaker::init()" << endl;
    //
    // either DAQ or SIM data.  MACRO switchable!
    //

    //
    // Read in parameters from the dataBase!
    // For now we can hard-code it!
    mGeometryDb = StRichGeometryDb::getDb();
    
    mPads =  mGeometryDb->numberOfPadsInARow(); //160;
    PR(mPads);
    mRows = mGeometryDb->numberOfRowsInAColumn(); //96;
    PR(mRows);

    mNumberOfPads = mGeometryDb->numberOfPads();//mRows*mPads;
    PR(mNumberOfPads);
    mEventNumber = 0;

    mSingleHitCollection = new  StRichSingleHitCollection();
    AddConst(new St_ObjectSet("richHits",mSingleHitCollection));
    mClusterFinder = new StRichClusterAndHitFinder();

    //
    // the hit Collection
    mSimpleHitCollection = new  StRichSimpleHitCollection();
    AddConst(new St_ObjectSet("richHits",mSimpleHitCollection));
    
#ifdef RCH_HISTOGRAM
    mRchNTupleFile = new TFile("RchData.root","RECREATE","Rch Ntuples");
    mPadPlane      = new TNtuple("rawNtuple", "raw data", "row:pad:adc:evt");
    mClusters      = new TNtuple("clusters", "cluster data","q:max:rms2:pads:evt");
    mHits          = new TNtuple("hits", "hit data", "q:max:evt");
    mcc            = new TH1F("ccharge","Cluster Charge",50,0,500);
    mmc            = new TH1F("cmaxadc","Cluster max ADC",50,0,500);
    mrms           = new TH1F("crms2","Cluster RMS2",50,0,1000);
    mpad           = new TH1F("cpads","Cluster pads",15,0,15);
    mqpad          = new TH1F("caveq","Cluster q/pad",20,0,150);
    mcratio        = new TH1F("cq2max","Cluster q/maxadc",50,0,5);

    mhc            = new TH1F("hcharge","Hit Charge",50,0,500);
    mhmc           = new TH1F("hmaxadc","Hit max ADC",50,0,500);

	
   }
    
    return StMaker::Init();
}


int StRchMaker::adcDecoder(unsigned long code,
			   unsigned long* pad, unsigned long* row, unsigned long* adc)
{
    *pad = ( code        & 0xff);
    *row = ((code >> 8)  & 0xff);
    *adc = ((code >> 16) & 0x3ff);

    return 0;
}

//-----------------------------------------------------------------
    ofstream raw("./sa.txt");
Int_t StRchMaker::Make() {
    ofstream raw("./rchMaker.txt");
    cout << "RchMaker::Make()" << endl;
    //
    mTheRichCollection = 0;
    //
    //cout << "  # " << mEventNumber << endl;
    //
    vector<StRichSinglePixel*> pixelStore;
    pixelStore.clear();
    
//     cout << "\n **************************\n" << endl;
//     cout << "Try get StEvent " << endl;
//      do {
    StEvent* myEvent = (StEvent *) GetInputDS("StEvent");
    StEvent& ev = *myEvent;
    //
    // should be a data member?
    StRichPixelCollection* theCollection;
    int pixelsInHand = 0;
    //
    if (myEvent) {
	//cout << "StEvent structure Exists! " << endl;
	theCollection = ev.richPixelCollection();
	cout << " *** Try make one yourself" << endl;
	if(theCollection) {
	    cout << "RICH Pixels in StEvent\n";
	    pixelsInHand = 1;
	}
	mEvent = new StEvent();
    if(!pixelsInHand) {
	//cout << "RICH Pixels not in StEvent\n";
	//cout << "\tTRY GET THE DATA SETS!" << endl;
    //
	    //cout << "Sim" << endl;
	    St_ObjectSet *rrsEvent = (St_ObjectSet*)GetDataSet("Rrs/.const/richPixels");
	    cout << "Sim Mode" << endl;
	    St_ObjectSet *rrsEvent =
		(St_ObjectSet*)GetDataSet("Rrs/.const/richPixels");
	    if(!rrsEvent) {
		cout << "\tDataSet: rrsEvent not there\n";
		cout << "\tSkip this event\n" << endl;
	    StRichPadPlane* myRichSimData = (StRichPadPlane*)(rrsEvent->GetObject());
	    if(!myRichSimData) {
	    StRichPadPlane* theRichSimData =
		(StRichPadPlane*)(rrsEvent->GetObject());
	    if(!theRichSimData) {
		cout << "\tRichSimData: not there\n";
		cout << "\tSkip this event\n" << endl;
	    mTheRichReader = new StRrsReader(myRichSimData, -9);
		return kStWarn;
	    }
	    //cout << "DAQ" << endl;
	}
		return kStWarn;
		clearPadMonitor();
		return kStWarn;
	
	//
	// If the StRichPixelCollection hasn't been retrieved from StEvent
	// the dataSet must be produced.
	//
	// Create the table...right now with the maximum number of entries
	// this is cleaned up immediately
	//

	//cout << "Try allocalte richPixelTable" << endl;
	St_dst_rch_pixel* richPixelTable =
	    new St_dst_rch_pixel("dst_rch_pixel",mNumberOfPads);
	
	if(mTheRichReader) { // no reader
	    //
	    // Write to the DST!
	    //
	    // this is used in order to index the table entry!
	    int currentTableRow = 0;
	    }
	    for(int iPad=0; iPad<mPads; iPad++) {  //x--> 160
		for(int iRow=0; iRow<mRows; iRow++) { // y -> 96
		
		    unsigned long theADCValue =
			mTheRichReader->GetADCFromCoord(iPad,iRow);
		    
		    //pack adc/row/pad into a single long.  Use:
		    // the first 8 bits for the Pad (0-159)
		    // the next  8 bits for the Row (0-96)
		    // the next 11 bits for the ADC (0-1023)

		    if(theADCValue) {
			unsigned long codedValue = 0;
			codedValue = (theADCValue << 16) | (iRow << 8) | iPad;

			//
			// fill the pixel store
			pixelStore.push_back(new StRichSinglePixel(iPad,
								   iRow,
								   theADCValue));

			//
			// fill the dst structure
			dst_rch_pixel_st* aPixel = new dst_rch_pixel_st;
			aPixel->codedData = codedValue;
		    
			//
			// add to the table
			//
			richPixelTable->AddAt(aPixel, currentTableRow);
			currentTableRow++;
			    
#ifdef RCH_DEBUG
			if (theADCValue > 0) {
			    raw << "pad: "  << iPad
				<< " row: " << iRow
				<< " adc: " << theADCValue
				<< " code " << codedValue << endl;
			    
			    unsigned long decodepad;
			    unsigned long decoderow;
			    unsigned long decodeadc;
			    if(adcDecoder(codedValue,&decodepad,&decoderow,&decodeadc)) {
				raw << decodepad << "/"
				    << decoderow << "/"
				    << decodeadc << endl;
			    }
			}
#endif	    
#ifdef RCH_HISTOGRAM
			mRawData[0] = iRow;
			mRawData[1] = iPad;
			mRawData[2] = theADCValue;
			mRawData[3] = mEventNumber;
			mPadPlane->Fill(mRawData);
#endif
		    } // if (theADCvalue) ?fill the dst structure 
		} // loop over rows
	    } // loop over pads

	    //
	    // Add the data to the dataset!
	    //
	    AddData(richPixelTable);

		
	} // if I couldn't get the reader!
	if(mTheRichReader) {
	    cout << "Error::StRchMaker::Make()\n";
	    cout << "\tCannot get the mTheRichReader\n";
	else {
	    cout << "\tCould not get a Reader\n";
	    cout << "\tSkip Event" << endl;
	    
    } // RICH pixels not in hand...not in STEVENT
    else {
	// the pixels are in StEvent
	// get the pixels
	cout << "Load the pixels from StEvent" << endl;
	for(unsigned int ii=0; ii< theCollection->size(); ii++) {
	    StRichPixel aPixel = theCollection->pixel(ii);
	    pixelStore.push_back(new StRichSinglePixel(aPixel.pad(),
						       aPixel.row(),
						       aPixel.adc()));
	}
		} // if (theADCvalue) ?fill the dst structure 

    // StEvent Structure does NOT exist
    if(!pixelsInHand && !mDaq) {
	// delete the reader each time!
	delete mTheRichReader;
	mTheRichReader = 0;
    }
	}
    }
//     }
    // Start the cluster finder
    // write it into StEvent if possible
    // If there are pixels in the pixelStore
    // Start the cluster finder...
    // ...then, write it into StEvent...if possible
    //
    
    // the cluster finder stuff
    mClusterFinder->loadPixels(pixelStore);
    cout << "At the cluster finder" << endl;
    mClusterFinder->clearAndDestroyAll();
#ifdef RCH_WITH_PAD_MONITOR
    mClusterFinder->setBorderFlags();
    StRichPadMonitor* thePadMonitor = StRichPadMonitor::getInstance(mGeometryDb);   
#endif
    
#ifdef RCH_WITH_PAD_MONITOR
    if(!mTheRichReader) {
	for(unsigned int jj=0; jj<pixelStore.size(); jj++) {
	    thePadMonitor->drawPad(*pixelStore[jj]);
	}
    }
	cerr << "\n In drawing Pixels\n";
    }
    thePadMonitor->update();
#endif
    
    
#ifdef RCH_DEBUG
    mClusterFinder->printList(raw);
#endif
    
    mClusterFinder->makeTheClustersAndFilter();
    
#ifdef RCH_DEBUG
    
    if(mUseMatrix) {
	cout << "==> USE MATRIX" << endl;
	if(!mClusterFinder->makeHitsFromPixelMatrix()) {
	    cout << "==> hits from pixel matrix failed!" << endl;
	}
    }
    else {
	cout << "==> USE SIMPLE HITS" << endl;
	if(!mClusterFinder->simpleHitsFromClusters()) {
	    cout << "==> simple hits from clusters failed!" << endl;
	}
    cout << "==> USE SIMPLE HITS" << endl;
    if(!mClusterFinder->simpleHitsFromClusters()) {
      cout << "==> simple hits from clusters failed!" << endl;
    
    mClusterFinder->calculateHitsInLocalCoordinates();
    mClusterFinder->calculateHitsInGlobalCoordinates();
    
#ifdef RCH_DEBUG
    cout << "Dump Hit Info==>size: " << mClusterFinder->getHits().size() << endl;
    cout << "Try get the hits" << endl;
    HitVector theHits = mClusterFinder->getHits();
    cout << "got the hits" << endl;

#ifdef RCH_WITH_PAD_MONITOR
    for(unsigned int jj=0; jj<theHits.size(); jj++) {
	//cout << "StRchMaker::drawHit() " << *theHits[jj] << endl;
	thePadMonitor->drawHit(theHits[jj]);
    for(unsigned int jj=0; jj<mTheHits.size(); jj++) {
	//cout << "StRchMaker::drawHit() " << *mTheHits[jj] << endl;
	thePadMonitor->drawHit(mTheHits[jj]);
    }
    thePadMonitor->update();
#endif
   // cluster
   
#ifdef RCH_HISTOGRAM
    // cluster
    ClusterVector myClusters = mClusterFinder->getClusters();

    unsigned int kk;
    for(kk=0; kk<myClusters.size(); kk++) {
	mcc->Fill(myClusters[kk]->amplitudeSum());
	mmc->Fill(myClusters[kk]->minimumAmplitudeOfLocalMax());
	mrms->Fill(myClusters[kk]->rms2());
	mpad->Fill(myClusters[kk]->numberOfPads());
	mqpad->Fill(myClusters[kk]->amplitudeSum()/myClusters[kk]->numberOfPads());
	mcratio->Fill(myClusters[kk]->amplitudeSum()/myClusters[kk]->minimumAmplitudeOfLocalMax());
	
	mCluster[0] = myClusters[kk]->amplitudeSum();
	mCluster[1] = myClusters[kk]->minimumAmplitudeOfLocalMax();
	mCluster[2] = myClusters[kk]->rms2();
	mCluster[3] = myClusters[kk]->numberOfPads();
	mCluster[4] = mEventNumber;
	mClusters->Fill(mCluster);
    for(kk=0; kk<theHits.size(); kk++) {
	mhc->Fill(theHits[kk]->charge());
	mhmc->Fill(theHits[kk]->maxAmplitude());
	mhc2m->Fill(theHits[kk]->charge()/theHits[kk]->maxAmplitude());
	mhc->Fill(mTheHits[kk]->charge());
	mHit[0] = theHits[kk]->charge();
	mHit[1] = theHits[kk]->maxAmplitude();
	
	mHit[0] = mTheHits[kk]->charge();
	mHit[1] = mTheHits[kk]->maxAmplitude();
	mHit[2] = mEventNumber;
	mHits->Fill(mHit);
   

#endif
    mSingleHitCollection->mTheHits = theHits;
    PR(mSingleHitCollection->mTheHits.size());
    PR(theHits.size());
//     for(int zz=0; zz<mSingleHitCollection->mTheHits.size(); zz++) {
// 	PR(mSingleHitCollection->mTheHits[zz]);
	}
//     for(int zz=0; zz<mSimpleHitCollection->mTheHits.size(); zz++) {
// 	PR(mSimpleHitCollection->mTheHits[zz]);
//     }
	mTheRichReader = 0;
    }

	}
    AddData(new St_ObjectSet("StRichEvent", richCollection));
  printf("* $Id: StRchMaker.cxx,v 1.15 2000/04/05 21:25:18 lasiuk Exp $\n");
}
//-----------------------------------------------------------------
  printf("* $Id: StRchMaker.cxx,v 1.15 2000/04/05 21:25:18 lasiuk Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
    printf("* $Id: StRchMaker.cxx,v 1.15 2000/04/05 21:25:18 lasiuk Exp $\n");
    printf("**************************************************************\n");
    if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------


Int_t StRchMaker::Finish() {

    cout << "StRchMaker::Finish()" << endl;

    cout << "Delete the cluster finder" << endl;
    delete mClusterFinder;
    mClusterFinder = 0;
	
#ifdef RCH_HISTOGRAM
    cout << "close the Histogram files!!!!!!" << endl;
    mRchNTupleFile->Write();
    mRchNTupleFile->Close();
}


 * Revision 1.10  2000/01/11 21:18:04  lasiuk
 * Fills new dst_rch_pixel;
 * debug macros;
 * used in first DAQ data
 *
 * Revision 1.9  1999/09/24 01:23:22  fisyak
 * Reduced Include Path
 **************************************************************************/
