 /***************************************************************************
 *
 * $Id: StRchMaker.cxx,v 1.14 2000/03/12 23:49:26 lasiuk Exp $
 *
 * Author: Jon Gans
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              StRchMaker.cxx - ROOT/STAR Maker for offline chain.
 ***************************************************************************
 *
 * $Log: StRchMaker.cxx,v $
 * Revision 1.14  2000/03/12 23:49:26  lasiuk
 * order of arguments for the reader
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
 * debug macros;
 * used in first DAQ data
 *
 * Revision 1.9  1999/09/24 01:23:22  fisyak

#include "StRchMaker.h"
#include "StChain.h"

#include "StEventTypes.h"
#include "StEvent/StRichMCHit.h"

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

StRchMaker::StRchMaker(const char *name, int daq)
    : StMaker(name), mDaq(daq)
// in $STAR/include
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

    mRows = 96;
    mPads = 160;
    mNumberOfPads = mRows*mPads;
    mGeometryDb = StRichGeometryDb::getDb();
    
    PR(mRows);



    mSingleHitCollection = new  StRichSingleHitCollection();
    AddConst(new St_ObjectSet("richHits",mSingleHitCollection));
    mClusterFinder = new StRichClusterAndHitFinder();
    mmc            = new TH1F("cmaxadc","Cluster max ADC",50,0,500);
    mrms           = new TH1F("crms2","Cluster RMS2",50,0,1000);
    mpad           = new TH1F("cpads","Cluster pads",15,0,15);
    mqpad          = new TH1F("caveq","Cluster q/pad",20,0,150);
    mcratio        = new TH1F("cq2max","Cluster q/maxadc",50,0,5);

			   unsigned long* pad, unsigned long* row, unsigned long* adc)
{
    *pad = ( code        & 0xff);
}

//-----------------------------------------------------------------
    ofstream raw("./sa.txt");
Int_t StRchMaker::Make() {
    ofstream raw("./rchMaker.txt");
    //
    //StDAQReader *theDataReader;
    
    if(!mDaq) {
	St_ObjectSet *rrsEvent = (St_ObjectSet*)GetDataSet("Rrs/.const/richPixels");
	//mTheRichData   = (StRichPadPlane*)(rrsEvent->GetObject());
	mTheRichReader = new StRrsReader( (StRichPadPlane*)(rrsEvent->GetObject()), -9);
	theCollection = ev.richPixelCollection();
    else {
	mTheRichData   = GetDataSet("StDAQReader");
	mTheDataReader = (StDAQReader*)(mTheRichData->GetObject());
	mTheRichReader = mTheDataReader->getRICHReader();
    }


    //assert(theRichData);
    
    //
    //
    // Create the table...right now with the maximum number of entries
    // this is cleaned up immediately
    //
    //cout << "Try allocalte richPixelTable" << endl;
    St_dst_rch_pixel* richPixelTable =
	new St_dst_rch_pixel("dst_rch_pixel",mNumberOfPads);

    //
    // get the Reader from the DAQ (mike's) Library
    if(mTheRichReader) {
	    //cout << "DAQ" << endl;
	// Loop over the data:
	//The TIC data was 96x24 => 2304
	//The RICH data is 160x96 (pad x row)
	//                           960x16 (channelxsequencer)
		clearPadMonitor();
	// Write to the DST!

	// this is used in order to index the table entry!
	//
	int currentTableRow = 0;
	for(int iPad=0; iPad<mPads; iPad++) {  //x--> 160
	    for(int iRow=0; iRow<mRows; iRow++) { // y -> 96
		
		unsigned long theADCValue =
		    mTheRichReader->GetADCFromCoord(iPad,iRow);
	// If the StRichPixelCollection hasn't been retrieved from StEvent
		//pack adc/row/pad into a single long.  Use:
		// the first 8 bits for the Pad (0-159)
		// the next  8 bits for the Row (0-96)
		// the next 10 bits for the ADC (0-1023)
		if(theADCValue) {
	if(mTheRichReader) { // no reader
		    unsigned long codedValue = 0;
		    codedValue = (theADCValue << 16) | (iRow << 8) | iPad;

		    
		    //
		    // fill the dst structure
		    //
		    dst_rch_pixel_st* aPixel = new dst_rch_pixel_st;
		    aPixel->codedData = codedValue;
	    }
		    //
		    // add to the table
		    //
		    richPixelTable->AddAt(aPixel, currentTableRow);
		    currentTableRow++;
								   theADCValue));
			//
		    if (theADCValue > 10) {
			cout << "pad: "  << iPad
			     << " row: " << iRow
			     << " adc: " << theADCValue
			     << " code " << codedValue << endl;
			
			short decodepad = (codedValue         & 0xff);
			short decoderow = ((codedValue >> 8)  & 0xff);
			short decodeadc = ((codedValue >> 16) & 0x3ff);
			cout << decodepad << "/"
			     << decoderow << "/"
			     << decodeadc << endl;
		    }
#ifdef RCH_HISTOGRAM
		    
		    
		    
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
    } // if I couldn't get the reader!
	    cout << "\tCannot get the mTheRichReader\n";
    	cout << "Error::StRchMaker::Make() cannot get the Rich Reader" << endl;
	cout << "Skip Event" << endl;
	return kStWarn;
	for(unsigned int ii=0; ii< theCollection->size(); ii++) {
	    StRichPixel aPixel = theCollection->pixel(ii);
    
    if(!mDaq) {
						       aPixel.adc()));
	}
		} // if (theADCvalue) ?fill the dst structure 

	}
    }
    // Add the data to the dataset!
    AddData(richPixelTable);
    // ...then, write it into StEvent...if possible
//     for(int zz=0; zz<mSingleHitCollection->mTheHits.size(); zz++) {
// 	PR(mSingleHitCollection->mTheHits[zz]);
	}
//     for(int zz=0; zz<mSimpleHitCollection->mTheHits.size(); zz++) {
// 	PR(mSimpleHitCollection->mTheHits[zz]);
//     }
	mTheRichReader = 0;
  printf("**************************************************************\n");
  printf("* $Id: StRchMaker.cxx,v 1.14 2000/03/12 23:49:26 lasiuk Exp $\n");
	}
    AddData(new St_ObjectSet("StRichEvent", richCollection));
  printf("* $Id: StRchMaker.cxx,v 1.14 2000/03/12 23:49:26 lasiuk Exp $\n");
}
//-----------------------------------------------------------------
  printf("* $Id: StRchMaker.cxx,v 1.14 2000/03/12 23:49:26 lasiuk Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
    printf("* $Id: StRchMaker.cxx,v 1.14 2000/03/12 23:49:26 lasiuk Exp $\n");
    printf("**************************************************************\n");
    if (Debug()) StMaker::PrintInfo();


    cout << "close the files!!!!!!" << endl;

    cout << "StRchMaker::Finish()" << endl;

    cout << "Delete the cluster finder" << endl;
    delete mClusterFinder;

//-----------------------------------------------------------------
















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
