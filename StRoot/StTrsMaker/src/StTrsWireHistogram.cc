/***************************************************************************
 *
 * $Id: StTrsWireHistogram.cc,v 1.7 1999/02/10 18:03:42 lasiuk Exp $
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
 * Revision 1.7  1999/02/10 18:03:42  lasiuk
 * gas gain manual setting
 * debug output
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

 **************************************************************************/
HepJamesRandom StTrsWireHistogram::mEngine;
#include <unistd.h> // sleep
StTrsWireHistogram* StTrsWireHistogram::mInstance = 0; // static data member
HepJamesRandom  StTrsWireHistogram::mEngine;
RandGauss       StTrsWireHistogram::mGaussianDistribution(mEngine);
RandExponential StTrsWireHistogram::mExponentialDistribution(mEngine);

StTrsWireHistogram::StTrsWireHistogram(StTpcGeometry* geoDb, StTpcSlowControl* scDb)
    : mMin(-1), mMax(-1)
{
    mGeomDb = geoDb;
    mSCDb   = scDb;
    mNumberOfInnerSectorAnodeWires =
	mGeomDb->numberOfInnerSectorAnodeWires();
    PR(mSectorWires.size());
    mNumberOfOuterSectorAnodeWires =
	mGeomDb->numberOfOuterSectorAnodeWires();

    mTotalNumberOfAnodeWires =
	mNumberOfInnerSectorAnodeWires + mNumberOfOuterSectorAnodeWires;
    //mSectorWires.assign(mTotalNumberOfAnodeWires);
    mDoGasGain              = true;
    mDoGasGainFluctuations  = false;
    mGasGainCalculationDone = false;

    mDoGasGainFluctuations          = false;
    mGasGainCalculationDone         = false;
    mDoSingleElectronMultiplication = false;
    
    //Time Delay At Collection
    mDoTimeDelay = false;
}

StTrsWireHistogram::~StTrsWireHistogram() {/* nopt */}

StTrsWireHistogram* StTrsWireHistogram::instance(StTpcGeometry* geoDb, StTpcSlowControl* scDb)
{
    if(!mInstance) {
	mInstance = new StTrsWireHistogram(geoDb, scDb);
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
    //PR(innerSectorBoundary);
    
    if(yCoordinateOfHit < innerSectorBoundary) { // in inner part of sector
	//PR(mGeomDb->firstInnerSectorAnodeWire());
	tmpWire =
	    (yCoordinateOfHit - mGeomDb->firstInnerSectorAnodeWire())/mGeomDb->anodeWirePitch() + .5;
	offSet = 0;
	wireLimit = mGeomDb->numberOfInnerSectorAnodeWires() - 1;
    }
    else { // in outer part of sector
	//PR(mGeomDb->firstOuterSectorAnodeWire());
	tmpWire =
	    (yCoordinateOfHit - mGeomDb->firstOuterSectorAnodeWire())/mGeomDb->anodeWirePitch() + .5;
	offSet = mGeomDb->numberOfInnerSectorAnodeWires();
	wireLimit = mGeomDb->numberOfInnerSectorAnodeWires() +
	    mGeomDb->numberOfOuterSectorAnodeWires() - 1;
    }

    // CAREFUL at the i/o sector boundaries
    // let boundary wires catch more than +/- pitch/2
    //if (tmpWire<0) tmpWire -=.5;
    int    wireIndex = static_cast<int>(tmpWire) + offSet; 
    if(wireIndex < offSet)
	wireIndex = offSet;
    if(wireIndex > wireLimit)
	wireIndex = wireLimit;

    //PR(offSet);
    //PR(wireLimit);
//     PR(wireIndex);

    //
    // Check Wire Index before doing any further calculations:
// 	    PR(avalancheFactor);
	PR(bin.numberOfElectrons());
	if(mDoGasGain) {
	    double avalancheFactor = avalanche(wireIndex);
// 	PR(bin.numberOfElectrons());
	    bin.scaleNumberOfElectrons(avalancheFactor);
	    //bin.scaleNumberOfElectrons(avalanche(wireIndex));
	else {  // Do Gaussian scaling
	}
	
      }  // end of gas gain
      
//  	PR(bin.numberOfElectrons());

	// Time Delay
	    // Increase DriftLength Proportional to the Distance from the Wire
	    // Keeping in mind a 2mm shift is approximately .1 timebins
	    // This means 500 um is 2 mm!
	    // Currently a linear function is used, but a better profile
	    // can probably be found with some study.

	if(mDoTimeDelay) {
	    double distanceToWire = fabs(yCoordinateOfHit - wireCoordinate(wireIndex));
	    {
#ifndef ST_NO_NAMESPACES
		using namespace units;
#endif
		double increase = 250*micrometer/millimeter*distanceToWire;
		//PR(increase);
        cout << "add at wire # " << wireIndex <<endl;
		bin.position().setZ((oldDriftLength+increase));
	    }
//         cout << "add at wire # " << wireIndex <<endl;

	//
	// Change coordinate of the wire!
	//
	//PR(bin.position());
	bin.position().setY(wireCoordinate(wireIndex));
	//PR(bin.position());
//          cout << "add at wire # " << wireIndex <<endl;
	mSectorWires[wireIndex].push_back(bin);
	//PR(bin);

	if (mMin<0) mMin = wireIndex;
	if (mMax<0) mMax = wireIndex;
	if (mMin > wireIndex) mMin = wireIndex;
	if (mMax < wireIndex) mMax = wireIndex;
	    
    }
	else {
	    cout << "wire " << wireIndex << " Out Of Wire Grid..."<< endl;
	}
}
void StTrsWireHistogram::clear()
{
    for(int ii=mMin; ii<= mMax; ii++) {
    mMax = -1;	
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

// These two could be inline, but are only called once so probably doesn't matter
void StTrsWireHistogram::setGasGainInnerSector(double v)
    mInnerSectorGasGain = v;
    cout << "Gas gain IS: " << mInnerSectorGasGain << endl;
}

void StTrsWireHistogram::setGasGainOuterSector(double v)
{
    mOuterSectorGasGain = v;
    cout << "Gas gain OS: " << mOuterSectorGasGain << endl;
}

double StTrsWireHistogram::avalanche(int iWire)
{
    
    double gasGainFactor = (mDoGasGainFluctuations) ?

    double gasGainFactor;
      gasGainFactor = (mDoGasGainFluctuations) ?
	gaussianMultiplication(iWire) :
	noFluctuations(iWire);
	mGaussianDistribution.shoot(mInnerSectorGasGain,.13*mInnerSectorGasGain) :
	mGaussianDistribution.shoot(mOuterSectorGasGain,.13*mOuterSectorGasGain);
}


double StTrsWireHistogram::exponentialFluctuations(int wireIndex) const
{
    return (wireIndex<mNumberOfInnerSectorAnodeWires) ?
	mExponentialDistribution.shoot(mInnerSectorGasGain) :
	mExponentialDistribution.shoot(mOuterSectorGasGain);
}

double StTrsWireHistogram::noFluctuations(int wireIndex) const
{
    return (wireIndex<mNumberOfInnerSectorAnodeWires) ?
	mInnerSectorGasGain : mOuterSectorGasGain;

    // Take gain 10^4
    mInnerSectorGasGain = 10000.;
    mOuterSectorGasGain = 10000.;
}
    // For now use values from SN247
    //mInnerSectorGasGain = 1315.;
    //mOuterSectorGasGain = 615.;
//     mInnerSectorGasGain =
// 	exp(scDb->innerSectorGasGainb*(innerSectorAnodeVoltage-innerSectorGasGainVzero));
//     mOuterSectorGasGain =
// 	exp(scDb->outerSectorGasGainb*(outerSectorAnodeVoltage-outerSectorGasGainVzero));
}
