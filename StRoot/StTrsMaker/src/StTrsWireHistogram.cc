/***************************************************************************
 *
 * $Id: StTrsWireHistogram.cc,v 1.15 1999/10/04 16:17:33 long Exp $
 *
 * Author: brian, May 1998 
 ***************************************************************************
 *
 * Description: Collection of all StTrsMiniChargeSegment transported to
 *              the pad-plane
 *
 ***************************************************************************
 *
 * $Log: StTrsWireHistogram.cc,v $
 * Revision 1.15  1999/10/04 16:17:33  long
 * wireCoordinate(kk)--> bin.position().y()
 *
 * Revision 1.14  1999/07/19 21:39:19  lasiuk
 * - addEntry() distributes charge on a (user) settable range of wires
 * - setRangeOfWiresForChargeDistribution(int) added (default is 0)
 *
 * Revision 1.13  1999/07/09 03:47:04  lasiuk
 * add switch for singleElectron multiplication, gaussian random
 * number generator
 *
 * Revision 1.12  1999/04/20 20:06:04  ward
 * Protection against pointer error in StTrsWireHistogram::clear
 *
 * Revision 1.11  1999/04/07 00:49:05  lasiuk
 * use the z offset for driftLength
 *
 * Revision 1.10  1999/02/16 23:40:32  lasiuk
 * check clear/add entry
 *
 * Revision 1.9  1999/02/14 20:44:32  lasiuk
 * gas gain settable via member function
 * escape if min()<0
 *
 * Revision 1.8  1999/02/12 01:26:38  lasiuk
 * Limit debug output
 *
 * Revision 1.7  1999/02/10 18:03:42  lasiuk
 * gas gain manual setting
 * debug output
 *
 * Revision 1.6  1999/02/10 04:28:29  lasiuk
 * comment debug
 *
 * Revision 1.5  1999/01/18 20:59:39  lasiuk
 * change gas gain 10^4
 *
 * Revision 1.4  1999/01/18 10:17:07  lasiuk
 * distanceToWire
 *
 * Revision 1.3  1998/11/16 14:47:25  lasiuk
 * use wireIndex to clarify name (not wireNumber)
 * remove mLastWire, mLastEntry
 *
 * Revision 1.2  1998/11/13 21:32:38  lasiuk
 * gains
 *
 * Revision 1.1  1998/11/10 17:12:28  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.5  1998/11/08 17:08:02  lasiuk
 * change from boolean macros
 * use resize() for LINUX/ allocators for SUN
 * add typedefs for vector<> types
 *
 * Revision 1.4  1998/11/03 17:31:57  lasiuk
 * incorporate gas gain/fluctuations
 * add time delay of collection depending on charge position wrt wire position
 * rename wire()
 *
 * Revision 1.3  1998/10/22 00:24:28  lasiuk
 * Oct 22
 *
 * Revision 1.2  1998/06/30 22:48:58  lasiuk
 * use db numbers; typecast yposition to wire position
 *
 * Revision 1.1  1998/06/04 23:31:58  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include <unistd.h> // sleep

#include "SystemOfUnits.h"
#include "StTrsWireHistogram.hh"

StTrsWireHistogram* StTrsWireHistogram::mInstance = 0; // static data member
HepJamesRandom  StTrsWireHistogram::mEngine;
RandGauss       StTrsWireHistogram::mGaussianDistribution(mEngine);
RandExponential StTrsWireHistogram::mExponentialDistribution(mEngine);

StTrsWireHistogram::StTrsWireHistogram(StTpcGeometry* geoDb, StTpcSlowControl* scDb, StTrsDeDx* gasDb)
    : mMin(-1),
      mMax(-1),
      mGeomDb(geoDb),
      mSCDb(scDb),
      mGasDb(gasDb),
      mDoGasGain(true),
      mDoGasGainFluctuations(false),
      mGasGainCalculationDone(false),
      mDoSingleElectronMultiplication(false),
      mDoTimeDelay(false),
      mRangeOfWiresForChargeDistribution(0)
{
    mNumberOfInnerSectorAnodeWires =
	mGeomDb->numberOfInnerSectorAnodeWires();
    mNumberOfOuterSectorAnodeWires =
	mGeomDb->numberOfOuterSectorAnodeWires();

    mTotalNumberOfAnodeWires =
	mNumberOfInnerSectorAnodeWires + mNumberOfOuterSectorAnodeWires;
    mSectorWires.resize(mTotalNumberOfAnodeWires);
//     PR(mTotalNumberOfAnodeWires);

    gasGainCalculation();
    mGasGainCalculationDone = true;
}

StTrsWireHistogram::~StTrsWireHistogram() {/* nopt */}

StTrsWireHistogram* StTrsWireHistogram::instance(StTpcGeometry* geoDb, StTpcSlowControl* scDb, StTrsDeDx* gasDb)
{
    if(!mInstance) {
	mInstance = new StTrsWireHistogram(geoDb, scDb, gasDb);
    }
    else { // do nothing
	cerr << "Cannot make a second instance of StTrsWireHistogram() " << endl;
	cerr << "Continuing..." << endl;
    }
    return mInstance;
}

void StTrsWireHistogram::addEntry(StTrsWireBinEntry& bin)
{
    // also needs db information to create coordinate->wire map
    // Find closes wire to: bin.position().y()

    double yCoordinateOfHit = bin.position().y();
    //PR(yCoordinateOfHit);
    double tmpWire;
    int    offSet;
    int    wireLimit;
    double innerSectorBoundary =
	mGeomDb->outerSectorEdge() - mGeomDb->ioSectorSpacing();
    
    if(yCoordinateOfHit < innerSectorBoundary) { // in inner part of sector
	tmpWire =
	    (yCoordinateOfHit - mGeomDb->firstInnerSectorAnodeWire())/mGeomDb->anodeWirePitch() + .5;
	offSet = 0;
	wireLimit = mGeomDb->numberOfInnerSectorAnodeWires() - 1;
    }
    else { // in outer part of sector
	tmpWire =
	    (yCoordinateOfHit - mGeomDb->firstOuterSectorAnodeWire())/mGeomDb->anodeWirePitch() + .5;
	offSet = mGeomDb->numberOfInnerSectorAnodeWires();
	wireLimit = mGeomDb->numberOfInnerSectorAnodeWires() +
	    mGeomDb->numberOfOuterSectorAnodeWires() - 1;
    }

    //
    // CAREFUL at the i/o sector boundaries
    // let boundary wires catch more than +/- pitch/2
    //
    int    wireIndex = static_cast<int>(tmpWire) + offSet; 
    if(wireIndex < offSet)
	wireIndex = offSet;
    if(wireIndex > wireLimit)
	wireIndex = wireLimit;

    //PR(offSet);
    //PR(wireLimit);
    //PR(wireIndex);
    //PR(wireCoordinate(wireIndex));

    //
    // Check Wire Index before doing any further calculations:
    //
    if (wireIndex >= 0 &&
	wireIndex < mTotalNumberOfAnodeWires) {

	//
	// Distribute Charge on Multiple Wires!
	//
	double chargeFraction;
	int lowestWireToCollectCharge =
	    max(0,(wireIndex-mRangeOfWiresForChargeDistribution));
	int highestWireToCollectCharge =
	    min((mTotalNumberOfAnodeWires-1),(wireIndex+mRangeOfWiresForChargeDistribution));

	double delta = mGeomDb->anodeWirePitch()/2.;
	double sigma =
	    mGasDb->transverseDiffusionCoefficient()*sqrt(bin.position().z());

	for(int kk=lowestWireToCollectCharge;
	    kk<=highestWireToCollectCharge; kk++) {

	    // the y coordinate of the wire:
	    double yOfMiniSegment = wireCoordinate(kk);
	    //
	    // Make a new StTrsWireBinEntry
	    //
	    //    StThreeVector<double>
	    //	position(bin.position().x(),
	    //		 wireCoordinate(kk),
	    //		 bin.position().z()); 
       StThreeVector<double>	
                position(bin.position().x(),
                   	 bin.position().y(),
	    		 bin.position().z());//HL,8/31/99
             
	    StTrsWireBinEntry
		theNewSegment(position,0);
	     
	    //
	    // Establish integration Limits:
	    // Take care of charge conservatione (Integral to +/- infinity)
	    double yLower = (kk == lowestWireToCollectCharge) ?
		-9.e99 : (wireCoordinate(kk) - delta);
	    double yUpper = (kk == highestWireToCollectCharge) ?
		+9.e99 : wireCoordinate(kk) + delta;

	    double arg1 = (yUpper-yCoordinateOfHit)/(M_SQRT2*sigma);
	    double arg2 = (yLower-yCoordinateOfHit)/(M_SQRT2*sigma);

	    chargeFraction = 0.5*(erf(arg1)-erf(arg2));
            chargeFraction=1.0;//HL,8/31/99

	    //
	    // Gas Gain
	    double avalancheFactor = bin.numberOfElectrons()*chargeFraction;
	    if(mDoGasGain) {
		if(mDoSingleElectronMultiplication) {
		    avalancheFactor =
			exponentialAvalanche(kk, bin.numberOfElectrons()*chargeFraction);
		}
		else {  // Do Gaussian scaling
		    avalancheFactor =
			gaussianAvalanche(kk, bin.numberOfElectrons()*chargeFraction);
		}
	    }  // end of gas gain
           
	    
	    theNewSegment.setNumberOfElectrons(avalancheFactor);	
	  
	    // Time Delay
	    // Increase DriftLength Proportional to the Distance from the Wire
	    // Keeping in mind a 2mm shift is approximately .1 timebins
	    // This means 500 um is 2 mm!
	    // Currently a linear function is used, but a better profile
	    // can probably be found with some study.

	    if(mDoTimeDelay) {
		double distanceToWire =
		    fabs(yCoordinateOfHit - wireCoordinate(wireIndex));
		{
#ifndef ST_NO_NAMESPACES
		    using namespace units;
#endif
		    double increase = 250*micrometer/millimeter*distanceToWire;

		    double oldDriftLength = bin.position().z();
		    theNewSegment.position().setZ((oldDriftLength+increase));
		}
	    }

	    //
	    // You had better convince yourself
	    // that this is really right!
	    //
	    // Change coordinate of the wire
	    // due to diffusion.  Remember that this
	    // is not needed in the single electron
	    // limit!  That is the reason for the
	    // strange scale factor!  Must be done
	    // before the gas gain is calculated or
	    // the factor is ~1
	    //
// 	    double ne = bin.numberOfElectrons()*chargeFraction;
// 	    double scaledSigma = sqrt(fabs((ne-1)/ne)*sigma);
// 	    double xPosition =
// 		mGaussianDistribution.shoot(theNewSegment.position().x(),
// 					    (scaledSigma));
// 	    theNewSegment.position().setX(xPosition);

	    mSectorWires[kk].push_back(theNewSegment);

	    //
	    // Set the limits for the wire Histogram
	    if (mMin<0) mMin = kk;
	    if (mMax<0) mMax = kk;
	    if (mMin > kk) mMin = kk;
	    if (mMax < kk) mMax = kk;
	    
	} // loop over kk wires
    }
    else {
	cout << "wire " << wireIndex << " Out Of Wire Grid..."<< endl;
    }
}

void StTrsWireHistogram::clear()
{
    for(int ii=mMin; ii<= mMax; ii++) {
        if(ii<0) continue; // Iwona and Herb, April 20 1999.
	mSectorWires[ii].clear();
    }
    mMin = -1;
    mMax = -1;

}

double StTrsWireHistogram::wireCoordinate(int index) const
{
    double wireY = (index < mNumberOfInnerSectorAnodeWires) ?
	mGeomDb->firstInnerSectorAnodeWire() + (index)*mGeomDb->anodeWirePitch() :
	mGeomDb->firstOuterSectorAnodeWire() + (index-mGeomDb->numberOfInnerSectorAnodeWires())*mGeomDb->anodeWirePitch();
    return wireY;
}

//vector<StTrsWireBinEntry>&
aTpcWire& StTrsWireHistogram::getWire(int num)
{
    if(num>=0 && num<mTotalNumberOfAnodeWires) {
	return mSectorWires[num];
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw range_error("Bounds error in StTrsWireHistogram::wire()");
#else
	cerr << "Bounds error...only " << mTotalNumberOfAnodeWires << " wires." << endl;
	cerr << "Exitting..." << endl;
	return mSectorWires[0];  // This is a sad/sad workaround for no exceptions!
#endif
    }
}

//vector<vector<StTrsWireBinEntry> >&
aTpcWirePlane& StTrsWireHistogram::getWireHistogram()
{
    return mSectorWires;
}

// void StTrsWireHistogram::putWire(int wNumber, vector<StTrsWireBinEntry>& wire)
// {
//     if(wNumber>=0 && wNumber<mTotalNumberOfAnodeWires)
// 	mSectorWires[wNumber] = wire;
//     else {
// #ifndef ST_NO_EXCEPTIONS
// 	throw range_error("StTrsWireHistogram::putWire() index error!");
// #else
// 	cerr << "StTrsWireHistogram::putWire() index error!" << endl;
// #endif
//     }
// }

void StTrsWireHistogram::setDoGasGainFluctuations(bool doIt)
{
    // Make sure if you turn "on" fluctuations, that gas gain is on!
    if(doIt && !mDoGasGain)
	mDoGasGain = true;

    mDoGasGainFluctuations = doIt;
}

// These two could be inline, but are only called once
// so probably doesn't matter performance wise
void StTrsWireHistogram::setGasGainInnerSector(double v)
{
    mInnerSectorGasGain = v;
    cout << "Gas gain IS: " << mInnerSectorGasGain << endl;
}

void StTrsWireHistogram::setGasGainOuterSector(double v)
{
    mOuterSectorGasGain = v;
    cout << "Gas gain OS: " << mOuterSectorGasGain << endl;
}

double StTrsWireHistogram::exponentialAvalanche(int iWire, double numElec)
{
    double gasGainFactor;
    double electrons = 0;
    if(mDoGasGainFluctuations) {
	int ctr = 0;
	do {
	    gasGainFactor = (iWire < mNumberOfInnerSectorAnodeWires) ?
		mExponentialDistribution.shoot(mInnerSectorGasGain) :
		mExponentialDistribution.shoot(mOuterSectorGasGain);
	    electrons += gasGainFactor;
	    ctr++;
	} while(ctr<numElec);
    }
    else {
	electrons = (iWire < mNumberOfInnerSectorAnodeWires) ?
	    numElec*mInnerSectorGasGain :
	    numElec*mOuterSectorGasGain;
    }
    return electrons;
}

double StTrsWireHistogram::gaussianAvalanche(int iWire, double numElec)
{
    //
    // The fluctuation values (13%) should be set via the
    // db or some macro call.  For now we will keep them
    // here
    double innerFluc;
    double outerFluc;
    if(mDoGasGainFluctuations) {
	innerFluc = .13;
	outerFluc = .13;
    }
    else {
	innerFluc = 0.;
	outerFluc = 0.;
        return  (iWire < mNumberOfInnerSectorAnodeWires) ?//HL
	  mInnerSectorGasGain*numElec ://HL
	  mOuterSectorGasGain*numElec ;//HL
    }

    return (iWire < mNumberOfInnerSectorAnodeWires) ?
	mGaussianDistribution.shoot(mInnerSectorGasGain,innerFluc*mInnerSectorGasGain)*numElec :
	mGaussianDistribution.shoot(mOuterSectorGasGain,outerFluc*mOuterSectorGasGain)*numElec;
}


double StTrsWireHistogram::noFluctuations(int wireIndex) const
{
    return (wireIndex<mNumberOfInnerSectorAnodeWires) ?
	mInnerSectorGasGain : mOuterSectorGasGain;
}

void StTrsWireHistogram::gasGainCalculation()
{
    // do calculation here and use values from db
    // For now use values from SN263
    //     mInnerSectorGasGain =
    // 	exp(scDb->innerSectorGasGainb*(innerSectorAnodeVoltage-innerSectorGasGainVzero));
    //     mOuterSectorGasGain =
    // 	exp(scDb->outerSectorGasGainb*(outerSectorAnodeVoltage-outerSectorGasGainVzero));
}
