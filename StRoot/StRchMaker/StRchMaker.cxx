/***************************************************************************
 *
 * $Id: StRchMaker.cxx,v 1.24 2000/06/16 20:34:11 dunlop Exp $
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
 * Revision 1.24  2000/06/16 20:34:11  dunlop
 * Fixed segfault again.  Was stomped last checkin
 *
 * Revision 1.24  2000/06/16 20:34:11  dunlop
 * Fixed segfault again.  Was stomped last checkin
 *
 * Revision 1.23  2000/06/16 02:05:38  lasiuk
 * include paths; drawing cleanup macros
 *
 * Revision 1.21  2000/06/13 18:13:59  dunlop
 * Commented out verbosity under real conditions
 *
 * Revision 1.20  2000/06/01 21:10:40  dunlop
 * filled cluster piece not in ctor
 *
 * Revision 1.19  2000/05/31 19:26:15  dunlop
 * Filling non-ctor entries in persistent hits + support for this
 *
 * Revision 1.18  2000/05/23 16:49:51  lasiuk
 * writing to StEvent/StRichCollection
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
#include "St_DataSetIter.h"

#include "StGlobals.hh"
#include "StThreeVectorF.hh"

// StEvent
#include "StEvent/StEvent.h"
#include "StEvent/StContainers.h"
#include "StEvent/StRichCollection.h"
#include "StEvent/StRichHit.h"
#include "StEvent/StRichMCHit.h"
#include "StEvent/StRichPixel.h"
#include "StEvent/StRichMCPixel.h"

//
// Interfaces
//
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/RICH/RICH_Reader.hh"
#include "StDAQMaker/StDAQReader.h"
// Simulation
#include "StRrsMaker/StRrsReader.h"
#include "StRrsMaker/StRichPadPlane.h"
#include "StRrsMaker/StRichSinglePixel.h"
#include "StRrsMaker/StRichSingleMCPixel.h"
// Database
#ifdef RCH_WITH_PAD_MONITOR
#include "StRrsMaker/StRichPadMonitor.h"

#ifdef RICH_WITH_PAD_MONITOR
#include "StRichDisplayMaker/StRichPadMonitor.h"
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
StRchMaker::StRchMaker(const char *name, int daq, int matrix, int cf)
    : StMaker(name), mDaq(daq), mUseMatrix(matrix), mCfOnly(cf)

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

    //
    // cluster finder
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

Int_t StRchMaker::Make() {
#ifdef RCH_DEBUG
    ofstream raw("./rchMaker.txt");
#endif
    cout << "RchMaker::Make()" << endl;

    //
    // Initialize Flags
    //
    mRichCollectionPresent    = 0;
    mPixelCollectionPresent   = 0;
    mClusterCollectionPresent = 0;
    mHitCollectionPresent     = 0;


    //
    // ptr initialization
    //
    mTheRichCollection = 0;
    
    //
    // increase event counter
    //
    mEventNumber++;
    //PR(mEventNumber);

    //
    // Load the pixels into a container for
//      do {
//        if(getchar()) break;
//      } while (true);

    //
    // Try get StEvent Structure
    //
    mEvent = (StEvent *) GetInputDS("StEvent");
    //StEvent& ev = *mEvent;

    //
    // Interogate StEvent structure
    //
    
    if (mEvent) {
	mTheRichCollection = mEvent->richCollection();
	if(mTheRichCollection) {
	    cout << "StEvent richCollection Exists" << endl;
	    mRichCollectionPresent = 1;
	    if(mTheRichCollection->pixelsPresent()) {
		cout << "StEvent RichPixelCollection Exists" << endl;
		mPixelCollectionPresent = 1;
	    }
	    else {
		cout << "** StEvent RichPixelCollection DOES NOT Exist" << endl;
	    }
	    if(mTheRichCollection->clustersPresent()) {
		cout << "StEvent RichClusters Exists" << endl;
		mClusterCollectionPresent = 1;
	    }
	    else {
		cout << "** StEvent RichClusters DOES NOT Exist" << endl;
	     cout << "** StEvent richCollection DOES NOT Exist" << endl;
	     cout << "** StEvent RichPixelCollection DOES NOT Exist" << endl;
	     cout << "** StEvent RichClusterCollection DOES NOT Exist" << endl;
	     cout << "** StEvent RichHitCollection DOES NOT Exist" << endl;
	    cout << "** StEvent RichPixelCollection DOES NOT Exist" << endl;
	    cout << "** StEvent RichClusterCollection DOES NOT Exist" << endl;
	    cout << "** StEvent RichHitCollection DOES NOT Exist" << endl;
	}
    }
    else {
	cout << " *****WARNING IN STRCHMAKER*****" << endl;
	cout << " *****StEvent structure does not exist" << endl;
	cout << " *** Try make one yourself" << endl;
	mEvent = new StEvent();
	//AddData(new St_ObjectSet("StRichEvent", mEvent));

    }

    //
    // If there are no pixels, get the interface access to the data
    //
    if(!mPixelCollectionPresent) {
	cout << " No Pixel Collection!!!  -->  TRY GET THE DATA SETS!" << endl;
	if(!mDaq) {
	    cout << "Sim Mode" << endl;
	    St_ObjectSet *rrsEvent =
		(St_ObjectSet*)GetDataSet("Rrs/.const/richPixels");
	    if(!rrsEvent) {
		cout << "\tDataSet: rrsEvent not there\n";
		cout << "\tSkip this event\n" << endl;
		clearPadMonitor();
		return kStWarn;
	    }

	    StRichPadPlane* theRichSimData =
		(StRichPadPlane*)(rrsEvent->GetObject());
	    if(!theRichSimData) {
		cout << "\tRichSimData: not there\n";
		cout << "\tSkip this event\n" << endl;
		clearPadMonitor();
		return kStWarn;
	    }
	    mTheRichReader = new StRrsReader(theRichSimData, -9);
	}
		return kStWarn;
	    }

	    mTheDataReader = (StDAQReader*)(mTheRichData->GetObject());
	    if(!mTheDataReader) {
		cout << "\tStDAQReader*: not there\n";
		cout << "\tSkip this event\n" << endl;
		clearPadMonitor();
		return kStWarn;
	    }
	    if (!(mTheDataReader->RICHPresent())) {
		cout << "\tRICH not in datastream\n";
		cout << "\tSkip this event\n" << endl;
		clearPadMonitor();
		return kStWarn;
	    }
	    mTheRichReader = mTheDataReader->getRICHReader();
	}

	if(mTheRichReader) {
	    cout << "Got the Reader " << endl;
	}
	else {
	    cout << "\tCould not get a Reader\n";
	    cout << "\tSkip Event" << endl;
		} // if (theADCvalue) ?fill the dst structure 
	    } // loop over rows
	} // loop over pads
    }
    
    //
    // If the StRichPixelCollection hasn't been retrieved from StEvent

    for(int iPad=0; iPad<mPads; iPad++) {  //x--> 160
	for(int iRow=0; iRow<mRows; iRow++) { // y -> 96
	    
	    unsigned long theADCValue =
		mTheRichReader->GetADCFromCoord(iPad,iRow);
	    if (theADCValue) {
	    //pack adc/row/pad into a single long.  Use:
	    // the first 8 bits for the Pad (0-159)
	    // the next  8 bits for the Row (0-96)
	    // the next 11 bits for the ADC (0-1023)
		
	    if(theADCValue) {
		unsigned long codedValue = 0;
		codedValue = (theADCValue << 16) | (iRow << 8) | iPad;
		
		//
		// fill the pixel store
		//
		if(dynamic_cast<StRrsReader*>(mTheRichReader)) {
		    // ...from simulation
		    anIDList mcInfo =
			dynamic_cast<StRrsReader*>(mTheRichReader)->GetMCDetectorInfo(iPad, iRow);
		    mPixelStore.push_back(new StRichSingleMCPixel(iPad,
								  iRow,
								  theADCValue,
			    );
		    }
		    mPixelStore.push_back(new StRichSingleMCPixel(iPad,iRow,theADCValue,
		    // ...from data
								  mcInfo));
		}
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
	
	}
    }
    

    //
    // If we only want to run the cluster finder...load the pixels from
    // StEvent...
    //
//     if(mCfOnly) {
// 	// Load the pixels from StEvent
// 	// get the pixels
// 	cout << "WARNING:\n";
// 	cout << "\tLoad the *CODED* pixels from StEvent\n";
// 	cout << "\tNo MC info is available"  << endl;
// 	for(unsigned int ii=0; ii< theCodedCollection->size(); ii++) {
// 	    StRichPixel aPixel = theCodedCollection->pixel(ii);
// 	    mPixelStore.push_back(new StRichSinglePixel(aPixel.pad(),
// 						       aPixel.row(),
// 						       aPixel.adc()));
// 	}
//     }
    
    //
    // If there are pixels in the pixelStore
    // Start the cluster finder...
    // ...then, write it into StEvent...if possible
    //
    
    // the cluster finder stuff
    //
    cout << "At the cluster finder" << endl;
    mClusterFinder->clearAndDestroyAll();
#ifdef RCH_WITH_PAD_MONITOR
    mClusterFinder->setBorderFlags();
    
#ifdef RICH_WITH_PAD_MONITOR
    cout << "Try get the pad monitor" << endl;   
    StRichPadMonitor* thePadMonitor = StRichPadMonitor::getInstance(mGeometryDb);
    //
    // Clears The Old Data.Must be done here and RRS maker
    
    thePadMonitor->clearHits();
    thePadMonitor->clearTracks();
    cout << "mDaq = " << mDaq << endl;
    if(mDaq) {
	thePadMonitor->clearPads();
	for(unsigned int jj=0; jj<mPixelStore.size(); jj++)
	    thePadMonitor->drawPad(*mPixelStore[jj]);
	
	cerr << "\n In drawing Pixels\n";
    }
    thePadMonitor->update();
#endif
    
    
#ifdef RCH_DEBUG
    mClusterFinder->printList(raw);
#endif
    
    mClusterFinder->makeTheClustersAndFilter();
    
#ifdef RCH_DEBUG
    mClusterFinder->printList(raw);
    mClusterFinder->dumpClusterInformation(raw);
#endif

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
    }
    
    mClusterFinder->calculateHitsInLocalCoordinates();
    mClusterFinder->calculateHitsInGlobalCoordinates();
    
#ifdef RCH_DEBUG
    cout << "Dump Hit Info==>size: " << mClusterFinder->getHits().size() << endl;
    mClusterFinder->dumpHitInformation(raw);
#endif

    //
    // get the hits for 2 reasons:
    // - pass via the data set
    // - histogram and padMonitor
    //

#ifdef RCH_WITH_PAD_MONITOR
    mTheHits = mClusterFinder->getHits();

#ifdef RICH_WITH_PAD_MONITOR
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
    }
    
    // hits
    for(kk=0; kk<mTheHits.size(); kk++) {
	mhc->Fill(mTheHits[kk]->charge());
	mhmc->Fill(mTheHits[kk]->maxAmplitude());
	mhc2m->Fill(mTheHits[kk]->charge()/mTheHits[kk]->maxAmplitude());
	
	mHit[0] = mTheHits[kk]->charge();
	mHit[1] = mTheHits[kk]->maxAmplitude();
	mHit[2] = mEventNumber;
	mHits->Fill(mHit);
	
    }
#endif
    
    
    //
    // Here is where the Hit Collection should be written out
    // ...pass the hits via the data set
    //
    PR(mTheHits.size());
    mSimpleHitCollection->mTheHits = mTheHits;
    PR(mSimpleHitCollection->mTheHits.size());
    
	//delete mTheRichCollection;
	mTheRichCollection = 0;
    }
    if(mEvent) {
	this->fillStEvent();
    }


    // Cleanup the reader if using daq
    if(!mDaq) {
	// delete the reader each time!
	if(mTheRichReader) {
	    delete mTheRichReader;
	}
	mTheRichReader = 0;
    }

    return kStOK;
}

void StRchMaker::fillStEvent()
{
    //
    // This function means there is a dependency on the
    // StEvent classes
    //
    //
    // Write At Least the Hits?
    //
    cout << "\nStRchMaker::fillStEvent()" << endl;
    StRichCollection *richCollection;

    if(!mTheRichCollection) {
	cout << " StRchMaker::Make a new collection" << endl;
	richCollection = new StRichCollection();

	//AddData(new St_ObjectSet("RichCollection", richCollection));
    }
    else {
	cout << " StRchMaker::use the already existing collection" << endl;
	richCollection = mTheRichCollection;
    }

    if(!(richCollection->pixelsPresent())) {
    //
//    if(!(richCollection->pixelsPresent())) {
	cout << " StRchMaker::fill the pixels" << endl;
	StRichSinglePixelCollection thePixels = mClusterFinder->getPixels();
	PR(thePixels.size());
	for(size_t ii=0; ii<thePixels.size(); ii++) {
 	    unsigned long codedValue = 0;
	    //PR(dynamic_cast<StRichSingleMCPixel*>(thePixels[ii]));
 	    unsigned long adc = static_cast<unsigned long>(thePixels[ii]->charge());
 	    unsigned long row = thePixels[ii]->row();
 	    unsigned long pad = thePixels[ii]->pad();
	    
 	    codedValue = (adc << 16) | (row << 8) | pad;

  	    if(dynamic_cast<StRichSingleMCPixel*>(thePixels[ii])) {
  		//cout << " ::fillStEvent() -> MC pixel" << endl;
		StRichMCPixel* persistentPixel = new StRichMCPixel(codedValue);
	        anIDList mcInfo = dynamic_cast<StRichSingleMCPixel*>(thePixels[ii])->MCInfo();
		//
		for(size_t jj=0; jj<mcInfo.size(); jj++) {
 		    persistentPixel->addInfo(new StRichMCInfo(mcInfo[jj].mHitID,
							      mcInfo[jj].mG_ID,
							      mcInfo[jj].mTrackp,
							      mcInfo[jj].mCharge,
							      static_cast<unsigned short>(mcInfo[jj].mSignalType)));
		}

		richCollection->addPixel(persistentPixel);
	    }
 	    else {
 		//cout << " ::fillStEvent() -> plain pixel" << endl;
 		richCollection->addPixel(new StRichPixel(codedValue));
 	    }
  	}
    }
    
    //
    if(!richCollection->clustersPresent()) {
    //PR(richCollection->clustersPresent());
//    if(!richCollection->clustersPresent()) {
    if(1) {
	cout << " StRchMaker::fill the clusters" << endl;
	ClusterVector theClusters = mClusterFinder->getClusters();
	PR(theClusters.size());
	for(size_t ii=0; ii<theClusters.size(); ii++) {
	    StRichCluster* thePersistentCluster = new StRichCluster(theClusters[ii]->numberOfPads(),
								    theClusters[ii]->numberOfLocalMax(),
								    theClusters[ii]->firstPad(),
								    theClusters[ii]->amplitudeSum(),
								    theClusters[ii]->amplitude2Sum(),
								    theClusters[ii]->rms2());
	    thePersistentCluster->setMinimumAmplitudeOfLocalMax(theClusters[ii]->minimumAmplitudeOfLocalMax());
	    
	    richCollection->addCluster(thePersistentCluster);
	}
        
    }

    //
    if(!richCollection->hitsPresent()) {
    //
//    if(!richCollection->hitsPresent()) {
    if(1) {
	cout << " StRchMaker::fill the hits" << endl;
	PR(mTheHits.size());
	for(size_t ii=0; ii<mTheHits.size(); ii++) {
	    if(dynamic_cast<StRichSimpleMCHit*>(mTheHits[ii])) {
		//cout << "mchit ";
		StRichMCHit* thePersistentHit = new StRichMCHit(StThreeVectorF(mTheHits[ii]->global().x(),
									       mTheHits[ii]->global().y(),
									       mTheHits[ii]->global().z()),
								StThreeVectorF(mTheHits[ii]->localError().x(),
									       mTheHits[ii]->localError().y(),
									       mTheHits[ii]->localError().z()),
								1,
								mTheHits[ii]->charge(),
								mTheHits[ii]->maxAmplitude(),
								static_cast<unsigned char>(0));
		
		//Set the single MCInfo
		StRichID theID = (dynamic_cast<StRichSimpleMCHit*>(mTheHits[ii]))->getMCInfo();
		
		thePersistentHit->setMCInfo(
		    StRichMCInfo(
			theID.mHitID,
			theID.mG_ID,
			theID.mTrackp,
			theID.mCharge,
			static_cast<unsigned short>(theID.mSignalType)
			)
		    );
		
		// Add it
		richCollection->addHit(thePersistentHit);
	    }
	    else {
		//cout << "hit ";
		StRichHit* thePersistentHit = new StRichHit(StThreeVectorF(mTheHits[ii]->global().x(),
									   mTheHits[ii]->global().y(),
									   mTheHits[ii]->global().z()),
							    StThreeVectorF(mTheHits[ii]->localError().x(),
									   mTheHits[ii]->localError().y(),
									   mTheHits[ii]->localError().z()),
							    1,
							    mTheHits[ii]->charge(),
							    mTheHits[ii]->maxAmplitude(),
							    static_cast<unsigned char>(0));
		// Add it
		richCollection->addHit(thePersistentHit);
	    }
	    // Fill in the stuff missing in the constructor
	    richCollection->getRichHits().back()->setClusterNumber(mTheHits[ii]->clusterNumber());
	    
	    richCollection->getRichHits().back()->local() 
		= StThreeVectorF(mTheHits[ii]->local().x(),
				 mTheHits[ii]->local().y(),
				 mTheHits[ii]->local().z());
	    
	    richCollection->getRichHits().back()->internal() 
		= StThreeVectorF( mTheHits[ii]->internal().x(),
				  mTheHits[ii]->internal().y(),
				  mTheHits[ii]->internal().z());
	}
        
    }
    //
    //cout << endl;
    //
    //
    // Store the rich collection into StEvent
    cout << "Write to StEvent" << endl;
    //PR(richCollection);
    //PR(mEvent);
    mEvent->setRichCollection(richCollection);
    //PR(mEvent->richCollection());
    //
    //cout << "Pass the collection via the data set now as well" << endl;
    AddData(new St_ObjectSet("StRichEvent", richCollection));
    
}
//-----------------------------------------------------------------
  printf("* $Id: StRchMaker.cxx,v 1.24 2000/06/16 20:34:11 dunlop Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
    printf("* $Id: StRchMaker.cxx,v 1.24 2000/06/16 20:34:11 dunlop Exp $\n");
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
