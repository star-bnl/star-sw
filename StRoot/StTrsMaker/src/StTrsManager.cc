/***************************************************************************
 *
 * $Id: StTrsManager.cc,v 1.1 1998/11/10 17:12:25 fisyak Exp $
 *
 * Author: brian May 18, 1998
 *
 ***************************************************************************
 *
 * Description: Overall Simulation manager.  Stores global parameters and
 *              sector information during simulation
 *
 ***************************************************************************
 *
 * $Log: StTrsManager.cc,v $
 * Revision 1.1  1998/11/10 17:12:25  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/11/10 17:12:25  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.7  1998/11/01 13:55:14  lasiuk
 * coordinate transform rename
 *
 * Revision 1.6  1998/10/22 00:24:24  lasiuk
 * Oct 22
 *
 * Revision 1.5  1998/06/30 22:49:54  lasiuk
 * add signal processors; processEvent() continues to grow
 *
 * Revision 1.4  1998/06/23 22:28:06  ullrich
 * readEvent() now directly called in processEvent().
 *
 * Revision 1.3  1998/06/04 23:23:13  lasiuk
 * add addDb() functions;
 * define processEvent() **Must add processSector()??**
 *
 * Revision 1.2  1998/05/21 21:27:59  lasiuk
 * Initial revision
 *
 * Revision 1.1.1.1  1998/05/19 22:33:44  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include "StTrsManager.hh"
#include "StTpcCoordinateTransform.hh"
#include "StCoordinates.hh"

StTrsManager::StTrsManager(int subsegments)
{
    mNumberOfSubSegments = subsegments;
}

StTrsManager::~StTrsManager() {/* noopt */}

void StTrsManager::addReader(StTrsReader* reader)
{
    mReader = reader;
}

void StTrsManager::addWriter(StTrsWriter* writer)
{
    mWriter = writer;
}

void StTrsManager::addSlowControlDb(StTpcSlowControl* SCDb)
{
    mSlowControlDb = SCDb;
}

void StTrsManager::addTpcGeometryDb(StTpcGeometry* geoDb)
{
    mTpcGeometryDb = geoDb;
}

void StTrsManager::addMagneticFieldDb(StMagneticField* magDb)
{
    mMagneticFieldDb = magDb;
}

void StTrsManager::addGasDb(StTrsDeDx* gasDb)
{
    mGasDb = gasDb;
}

void StTrsManager::addAnalogSignalGenerator(StTrsAnalogSignalGenerator* siggen)
{
    mAnalogSignalGenerator = siggen;
}

void StTrsManager::addDigitalSignalGenerator(StTrsDigitalSignalGenerator* siggen)
{
    mDigitalSignalGenerator = siggen;
}

void StTrsManager::addSector(StTrsSector* sec)
{
    mSector = sec;
}

void StTrsManager::addChargeTransporter(StTrsChargeTransporter* transpt)
{
    mChargeTransporter = transpt;
}

bool StTrsManager::readEvent()
{
    mData.clear();
    // must also clear wire histogram
    return mReader->getData(mData);
}
    
bool StTrsManager::processEvent()
{
    int ii,jj,kk; // counters
    //
    // Read one event
    //
    if (!readEvent()) return false;
    
    //loop over all segments
    cout << "Loop over all segments " << mData.size() << endl;

    for(ii=0; ii<mData.size(); ii++) {

	cout << "mData[" << ii << "].position() " << mData[ii].position() << endl;
	cout << "**mData[ii].dE " << mData[ii].dE() << endl;
	cout << "**mData[ii].ds " << mData[ii].ds() << endl;
	double segmentLength = mData[ii].ds()/static_cast<double>(mNumberOfSubSegments);
	PR(segmentLength);
	
	// for each segment:
	mData[ii].rotate(mTpcGeometryDb, mSlowControlDb);

#ifndef ST_NO_TEMPLATE_DEF_ARGS
	list<StTrsMiniChargeSegment> splitSegment;
#else
	list<StTrsMiniChargeSegment, allocator<StTrsMiniChargeSegment> > splitSegment;
#endif
	mData[ii].split(mGasDb, mMagneticFieldDb,
			mNumberOfSubSegments, segmentLength, &splitSegment);

	//
	// Loop over list of MiniSegments
	//
	
	cout << "number of MiniSegments: " << splitSegment.size() << endl;
#ifndef ST_NO_TEMPLATE_DEF_ARGS
	list<StTrsMiniChargeSegment>::iterator iter;
#else
	list<StTrsMiniChargeSegment, allocator<StTrsMiniChargeSegment> >::iterator iter;
#endif
	for(iter=splitSegment.begin(); iter!=splitSegment.end(); iter++) {
#ifndef __sun
	  PR(iter->position());
#else
	  PR((*iter).position());    // but with ospace
#endif
	    //
	    //   transportToWire (alters a miniChargeSegment)
	    //
	    mChargeTransporter->transportToWire(*iter);

	    //if no charge delete it
#ifndef __sun
	    if(!iter->charge()) {
#else
	    if( !((*iter).charge()) ) {    // bug with ospace
#endif
		splitSegment.erase(iter);
		continue;
	    }
	    
	    // create a wireHistogram bin and add the entry
#ifndef __sun
	    StTrsWireBinEntry aBin(iter->position(), iter->charge());
#else
	    StTrsWireBinEntry aBin((*iter).position(), (*iter).charge());  // bug with ospace
#endif
	    mWireHistogram->addEntry(aBin);
	}
    } // Loop over all segments

    // process the wires
//     cout << "\n*****************************\nprocess the hits on the wires" << endl;
//     StTpcCoordinateTransform transform(mTpcGeometryDb, mSlowControlDb);
//     int pad,row;
//     cout << "Wire Histogram size " << mWireHistogram->getWireHistogram().size() << endl;
//     for(ii=0; ii<mWireHistogram->getWireHistogram().size(); ii++) {
	
// 	cout << " mWireHistogram->getWire(" << ii << ").size() " << mWireHistogram->getWire(ii).size() << endl;
// 	for(jj=0; jj<mWireHistogram->getWire(ii).size(); jj++) {
// 	    double electrons =
// 		mWireHistogram->getWire(ii)[jj].numberOfElectrons()*mAnalogSignalGenerator->simpleAvalanche();
// 	    cout << "electrons after gain: " << mWireHistogram->getWire(ii)[jj].numberOfElectrons()
// 		 << "-->  " << electrons << endl;

// 	    StThreeVector<> chargePosition = mWireHistogram->getWire(ii)[jj].position();
// 	    cout << "chargePosition: " << (chargePosition) << endl;

// 	    //
// 	    // Pad coordinates:
// 	    //
// 	    StTpcLocalCoordinate myCoord(mWireHistogram->getWire(ii)[jj].position());
// 	    StThreeVector<double> padPosition = transform.padCentroid(myCoord, &pad, &row);
// 	    cout << "centerOfPad " << padPosition << endl;
// 	    PR(pad);PR(row);

// 	    for(int irow=-3; irow<4; irow++) {
// 		for(int ipad =-8; ipad<8; ipad++) {
		    
// 		    //which pad/inner/outer sector
// 		    double padWidth, padLength;
// 		    if(row<13) {
// 			padWidth  = mTpcGeometryDb->innerSectorPadWidth();
// 			padLength = mTpcGeometryDb->innerSectorPadLength();
// 		    }
// 		    else {
// 			padWidth  = mTpcGeometryDb->outerSectorPadWidth();
// 			padLength = mTpcGeometryDb->outerSectorPadLength();
// 		    }	
// 		    // Integration Limits
// 		    double xLower = padPosition.x() - padWidth/2;
// 		    double xUpper = xLower + padWidth;

// 		    double yLower = padPosition.y()-padLength/2;
// 		    double yUpper = yLower+padLength;
		    
// 	    // should also give length of charge (dl) --> calc prf from lambda
// 	    // for now assume point charge

// 		    // return enough for sample
// 		    double charge =
// 			mAnalogSignalGenerator->imageCharge(chargePosition.x(),chargePosition.y(),
// 							    padPosition.x(),   padPosition.y(),
// 							    xLower,            xUpper,
// 							    yLower,            yUpper);
// 		} // pads
// 	    }     // rows
// 	    mAnalogSignalGenerator->sampleAnalogSignal(); // fills sector
// 	}
//     }

//     //}
//     cout << "clear wire histogram" << endl;
    mSector->clear();

    //
    // Loop over all the entries in sector
    // digitize
    cout << "digitize here\n  loop over pad rows " << mSector->rows().size() << endl;
    // for(ii=0; ii<mSector->rows().size(); ii++) { // pad row
// 	cout << "pads: " << mSector->pads(ii).size() << endl;
// 	for(jj=0; jj<mSector->pads(ii).size(); jj++) { // pads
// 	    for(kk=0; kk<mSector->timeBins(ii,jj).size(); kk++) { // timebins
// 		mDigitalSignalGenerator->digitizeSignal();
// 	    }
// 	}
//     }

    mWriter->writeAscii();
    
    //} Loop over all sectors (separate member function)!
    

    return true;
}

void StTrsManager::addWireHistogram(StTrsWireHistogram* histo)
{
    mWireHistogram = histo;
}
