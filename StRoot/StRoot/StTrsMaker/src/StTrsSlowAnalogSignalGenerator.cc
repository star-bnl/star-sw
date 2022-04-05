/***************************************************************************
 *
 * $Id: StTrsSlowAnalogSignalGenerator.cc,v 1.30 2009/11/03 14:34:19 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTrsSlowAnalogSignalGenerator.cc,v $
 * Revision 1.30  2009/11/03 14:34:19  fisyak
 * Remove default in zFromTB
 *
 * Revision 1.29  2008/10/13 19:56:12  fisyak
 * Account that Z-offset is sector dependent
 *
 * Revision 1.28  2008/06/20 15:01:20  fisyak
 * move from StTrsData to StTpcRawData
 *
 * Revision 1.27  2007/07/12 20:25:05  fisyak
 * Use StarLogger, use time of flight, fix cluster shape
 *
 * Revision 1.26  2003/09/02 17:59:19  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.25  2000/06/23 00:12:41  snelling
 * Removed dependence on local files now pointed to StDbUtilities
 *
 * Revision 1.24  2000/06/07 02:03:12  lasiuk
 * exit/abort ultimatum
 *
 * Revision 1.23  2000/02/10 01:21:51  calderon
 * Switch to use StTpcDb.
 * Coordinates checked for consistency.
 * Fixed problems with StTrsIstream & StTrsOstream.
 *
 * Revision 1.22  1999/12/08 02:10:42  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.21  1999/10/25 18:38:49  calderon
 * changed mPos and pos() to mPosition and position() to
 * be compatible with StEvent/StMcEvent.
 *
 * Revision 1.20  1999/10/22 15:51:48  calderon
 * Remove ifdefs for erf.  Problem is solved by loading libm at the
 * macro level.
 *
 * Revision 1.19  1999/10/22 00:00:15  calderon
 * -added macro to use Erf instead of erf if we have HP and Root together.
 * -constructor with char* for StTrsDedx so solaris doesn't complain
 * -remove mZeros from StTrsDigitalSector.  This causes several files to
 *  be modified to conform to the new data format, so only mData remains,
 *  access functions change and digitization procedure is different.
 *
 * Revision 1.18  1999/07/19 21:41:22  lasiuk
 * - debug check and cleanup.  No changes
 *
 * Revision 1.17  1999/04/27 18:33:08  lasiuk
 * change position of time shift in sampleAnalogSignal()
 *
 * Revision 1.16  1999/04/27 15:05:04  lasiuk
 * time shift in ns
 *
 * Revision 1.15  1999/04/23 19:19:50  lasiuk
 * add delay to centroid of signal:
 * Calculated in constructor (mTimeShiftOfSignalCentroid)
 * and applied in in signalsampler()
 *
 * Revision 1.14  1999/02/28 20:12:50  lasiuk
 * threshold/noise additions
 *
 * Revision 1.13  1999/02/26 18:26:09  lasiuk
 * to offset must be used uniformly.  Correction to
 * "timeOfSignal" should be merged with coordinate transform
 *
 * Revision 1.12  1999/02/19 10:35:04  lasiuk
 * Function prototype for StTpcCoordinateTransform
 *
 * Revision 1.11  1999/02/16 23:34:33  lasiuk
 * inline 2 functions
 * merge operations for speed up (after profiler0
 *
 * Revision 1.10  1999/02/15 03:38:16  lasiuk
 * protection if min()<0
 *
 * Revision 1.9  1999/02/14 20:46:09  lasiuk
 * debug info
 *
 * Revision 1.8  1999/02/12 01:27:18  lasiuk
 * Limit Debug output
 *
 * Revision 1.7  1999/02/10 20:55:16  lasiuk
 * Feb 10,1999
 *
 * Revision 1.6  1999/01/22 23:38:28  lasiuk
 * set defaults for signal sampling and induced charge
 *
 * Revision 1.5  1999/01/18 21:01:30  lasiuk
 * use fractionSampled(); enumerated types for function selection
 *
 * Revision 1.4  1999/01/18 10:21:10  lasiuk
 * use integral to deposit total charge in time bin
 *
 * Revision 1.3  1998/11/16 14:48:19  lasiuk
 * use wire index instead of wireNumber
 * comment signal threshold
 * add deltaResponse()
 *
 * Revision 1.2  1998/11/13 21:32:16  lasiuk
 * adjust charge generated
 *
 * Revision 1.1  1998/11/10 17:12:27  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.5  1998/11/08 17:30:26  lasiuk
 * allocators for SUN
 *
 * Revision 1.4  1998/11/04 18:47:12  lasiuk
 * signal sampler machinery
 *
 * Revision 1.3  1998/10/22 14:58:27  lasiuk
 * image charge returns double and uses PRF integral
 *
 * Revision 1.2  1998/10/22 00:24:27  lasiuk
 * Oct 22
 *
 * Revision 1.1  1998/06/30 22:46:50  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include <unistd.h>
#include <math.h>
#include <algorithm>

#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StTpcPadCoordinate.hh"

#include "StTrsSlowAnalogSignalGenerator.hh"
#include "TMath.h"
//static const double symGausAppFactor  = (M_SQRT1_2*M_2_SQRTPI/(2.*mSigma1));
//static const double tau1              = mSigma1;
//static const double tau2              = 2.*mSigma1;
//static const double asymGausAppFactor = (M_2_SQRTPI/M_SQRT2)/(tau1+tau2);
//static const double asymGausUnRFactor = (M_2_SQRTPI/M_SQRT2)/(mSigma1+mSigma2);
// This should really come from the gas database...
// Hmmm...actually why should the electronics have to know about this?
static const double sigmaL = .05*centimeter/TMath::Sqrt(centimeter);

StTrsAnalogSignalGenerator* StTrsSlowAnalogSignalGenerator::mInstance = 0; // static data member

//StTrsSlowAnalogSignalGenerator::StTrsSlowAnalogSignalGenerator() {/* nopt */}

StTrsSlowAnalogSignalGenerator::StTrsSlowAnalogSignalGenerator(StTpcGeometry* geo, StTpcSlowControl* sc, StTpcElectronics* el, StTrsSector* sec)
    : StTrsAnalogSignalGenerator(geo, sc, el, sec)
{
  // Set Defaults for the functional forms
  mChargeDistribution = endo;
  mSampler            = symmetricGaussianApproximation;

  //
  // Define here instead of calculating...
  //
  
  mDriftVelocity      = mSCDb->driftVelocity(13);
  mSamplingFrequency  = mElectronicsDb->samplingFrequency();
  
  mTimeBinWidth         = 1./mSamplingFrequency;
  mTau                  = mSigma1;
  mTau1                 = mSigma1;
  mTau2                 = 2.*mSigma2;
  mSymGausApproxFactor  = (M_SQRT1_2*M_2_SQRTPI/(2.*mSigma1));
  mAsymGausApproxFactor = (M_2_SQRTPI/M_SQRT2)/(mTau1+mTau2);
  mAsymGausUnRestFactor = (M_2_SQRTPI/M_SQRT2)/(mSigma1+mSigma2);

//   PR(mDriftVelocity/(centimeter/(1.e-6*second)));
//   PR(mSamplingFrequency/MHz);
//   PR(mTimeBinWidth/nanosecond);
//   PR(mTau/nanosecond);
//   PR(mTau1/nanosecond);
//   PR(mTau2/nanosecond);
//   PR(mSigma1/nanosecond);
//   PR(mSigma2/nanosecond);
//   PR(mSymGausApproxFactor);
//   PR(mAsymGausApproxFactor);
//   PR(mAsymGausUnRestFactor);
}

StTrsSlowAnalogSignalGenerator::~StTrsSlowAnalogSignalGenerator() {/* missing */}

StTrsAnalogSignalGenerator*
StTrsSlowAnalogSignalGenerator::instance()
{
    if (!mInstance) {
#ifndef ST_NO_EXCEPTIONS
	throw domain_error("StTrsSlowAnalogSignalGenerator::instance() Invalid Arguments");
#else
	cerr << "StTrsSlowAnalogSignalGenerator::instance() Invalid Arguments" << endl;
	cerr << "Cannot create Instance" << endl;
	cerr << "Aborting..." << endl;
	abort();
#endif
    }
    return mInstance;
}

StTrsAnalogSignalGenerator*
StTrsSlowAnalogSignalGenerator::instance(StTpcGeometry* geo, StTpcSlowControl* sc, StTpcElectronics* el, StTrsSector* sec)
{
    if (!mInstance) {
	mInstance = new StTrsSlowAnalogSignalGenerator(geo, sc, el, sec);
    }
    // else do nothing
    return mInstance;
}

double StTrsSlowAnalogSignalGenerator::endoChargeIntegral(double xo, double yo, double xl, double xu, double yl, double yu)
{
#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif
	
    double L = (yo < mGeomDb->lastInnerSectorAnodeWire()) ?
	mGeomDb->innerSectorAnodeWirePadPlaneSeparation() :
	mGeomDb->outerSectorAnodeWirePadPlaneSeparation();
    //PR(L);
    double t3 = 1/L;
    double t17 = -TMath::ATan(TMath::Exp(pi*(xu-xo)*t3/2))+TMath::ATan(TMath::Exp(pi*(xl-xo)*t3/2));
    double t19 = pi*pi;
    double t20 = 1/t19;
    double t29 = 
	-4.0*TMath::ATan(TMath::Exp(-pi*(-yu+yo)*t3/2))*t17*t20
	+4.0*TMath::ATan(TMath::Exp(-pi*(-yl+yo)*t3/2))*t17*t20;

    return t29;
}

//double StTrsSlowAnalogSignalGenerator::gattiChargeIntegral(double xo, double yo, double xl, double xu, double yl, double yu)
double StTrsSlowAnalogSignalGenerator::imageChargeIntegral(double xo, double yo, double xl, double xu, double yl, double yu)
{
#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif
//     cout << "StTrsSlowAnalogSignalGenerator::imageChargeIntegral" << endl;
    // Arguments
    //  double xo, double yo,  //charge location 
    //	double xp, double yp,  //pad centroid
    //	double xl, double xu,  //integration limits
    //	double yl, double yu   // "
    
	//
	// Integrate a charge distribution sigma:
	//
	//                                          q d
	//            sigma := 1/2 ----------------------------------
	//                                     2           2    2 3/2
	//                         Pi ((x - xo)  + (y - yo)  + d )
	//
	// position of charge        (xo,yo)
	// wire/pad-plane separation  d
	//
	//              yu   xu
	//             /    /
	//            |    |                       q d
	//      G :=  |    |   1/2 ---------------------------------- dx dy
	//            |    |                   2           2    2 3/2
	//           /    /        Pi ((x - xo)  + (y - yo)  + d )
	//           yl   xl
	//

    
	double q = 1;
	double d = 4*millimeter;

	//double d = (yo < mGeomDb->firstOuterSectorPadRow()) ?
	//   mGeomDb->innerSectorAnodeWirePadPlaneSeparation() :
	//   mGeomDb->outerSectorAnodeWirePadPlaneSeparation();
	double t1 = xu*xu;
	double t2 = xo*xo;
	double t3 = xo*xu;
	double t4 = t1+t2-2.0*t3;
	double t5 = yo-yu;
	double t8 = TMath::Power(-xu+xo,2.0);
	double t9 = TMath::Sqrt(t8);
	double t10 = 1/t9;
	double t11 = yu*yu;
	double t12 = yo*yu;
	double t13 = yo*yo;
	double t15 = 5*TMath::Sqrt(t1+t2+t11+(d*d)/100.-2.*t12-2.*t3+t13);
	double t19 = TMath::ATan((50./d)*t4*t5*t10/t15);
	double t22 = TMath::Power(-xl+xo,2.0);
	double t23 = TMath::Sqrt(t22);
	double t27 = xl*xl;
	double t28 = xo*xl;
	double t29 = t27+t2-2.0*t28;
	double t31 = 1/t23;
	double t33 = 5*TMath::Sqrt(t27+t2+t11+(d*d)/100-2.*t12-2.*t28+t13);
	double t37 = TMath::ATan((50./d)*t29*t5*t31/t33);
	double t44 = t10*t31;
	double t46 = yo-yl;
	double t48 = yl*yl;
	double t49 = yo*yl;
	double t51 = 5*TMath::Sqrt(t1+t2+t48+(d*d)/100-2.*t49-2.*t3+t13);
	double t55 = TMath::ATan((50./d)*t4*t46*t10/t51);
	double t62 = 5*TMath::Sqrt(t27+t2+t48+(d*d)/100-2.*t49-2.*t28+t13);
	double t66 = TMath::ATan((50./d)*t29*t46*t31/t62);
	double t74 = 0.1591549431*q*(-t19*xu*t23+t19*xo*t23+t37*xl*t9-t37*xo*t9)*t44
	    -0.1591549431*q*(-t55*xu*t23+t55*xo*t23+t66*xl*t9-t66*xo*t9)*t44;
	
	return t74;
}

void StTrsSlowAnalogSignalGenerator::setChargeDistribution(StDistribution v)
{
    if(v == endo  ||
       v == gatti ||
       v == dipole  )
	mChargeDistribution = v;
    else {
	cerr << "Cannot Determine Charge Distribution Requested" << endl;
	abort();
    }
}

// This is now an inline function.  See the .hh file
// double StTrsSlowAnalogSignalGenerator::signalOnPad(double xo, double yo, double xl, double xu, double yl, double yu)
// {

// //     cout << "StTrsSlowAnalogSignalGenerator::signalOnPad()" << endl;
//      switch(mChargeDistribution)
// 	{
// 	case endo:
// // 	    cout << "********************Endo" << endl;
// 	    return endoChargeIntegral(xo,yo,xl,xu,yl,yu);
// 	    break;
//  	case gatti:
// //  	    cout << "********************GATTI" << endl;
// // 	    return gattiChargeIntegral(xo,yo,xl,xu,yl,yu);
// 	    cout << "Gatti Distribution Not Implemented Yet!" << endl;
// 	    exit(0);
//  	    break;
// 	case dipole:
// // 	    cout << "********************DIPOLE" << endl;
// 	    return imageChargeIntegral(xo,yo,xl,xu,yl,yu);
// 	    break;
// 	default:
// 	    cerr << "Default Function Selected. ERROR!" << endl;
// 	    exit(0);
// 	    break;
// 	}
// }

void StTrsSlowAnalogSignalGenerator::inducedChargeOnPad(StTrsWireHistogram* wireHistogram, Int_t sector)
{
    //
    // coordinate transform is now a data member
    // 
    //StTpcCoordinateTransform transformer(mGeomDb, mSCDb, mElectronicsDb);
#if 0
    PR(wireHistogram->minWire());
    PR(wireHistogram->maxWire());
#endif
    if(wireHistogram->minWire()<0) {
	cerr << "Wire Plane is empty" << endl;
	return;
    }
    mDriftVelocity      = mSCDb->driftVelocity(sector);
    for(int jj=wireHistogram->minWire(); jj<=wireHistogram->maxWire(); jj++) {

	// StTrsWireHistogram defines typedefs:
	// ABOVE: typedef vector<StTrsWireBinEntry> aTpcWire
	aTpcWire currentWire = wireHistogram->getWire(jj);
	aTpcWire::iterator iter;

	for(iter  = currentWire.begin();
	    iter != currentWire.end();
	    iter++) {
	    // What is the location of the avalanche
	    // center of Pad that is being processed?
	    // the y coordinate is the position of the wire

  	    //PR(*iter);
	    
	    StTpcPadCoordinate    tpcRaw;
	    //StTpcLocalCoordinate  xyCoord(iter->position()); // mVolumeId seems to be invalid??
	    StTpcLocalSectorCoordinate  xyCoord(iter->position(),12);
	    //
	    // THIS IS IMPORTANT TO REALIZE!!!
	    //
	    // xyCoord is now:
	    //   x position of hit
	    //   y position of wire
	    //   z drift length
// 	    PR(iter->position());
//  	    PR(xyCoord);


// 	    cout << "**Transform2Raw" << endl;
	    transformer(xyCoord,tpcRaw);
//  	    PR(tpcRaw);
	    int centralPad = (Int_t) tpcRaw.pad();
	    int centralRow = tpcRaw.row();
//  	    PR(centralRow);
//   	    PR(mDeltaRow);
//  	    PR(centralPad);
//    	    PR(mDeltaPad);

	    // Calculate the row/pad limits
	    mRowLimits.first  = (centralRow > mDeltaRow) ?
		centralRow - mDeltaRow : centralRow;
	    mRowLimits.second = (centralRow < (mGeomDb->numberOfRows()-mDeltaRow)) ?
		centralRow + mDeltaRow : centralRow;

	    // Careful No inner/outer sector coupling!!
	    if(xyCoord.position().y() < mGeomDb->outerSectorEdge()) {
		mRowLimits.second = min(mRowLimits.second, mGeomDb->numberOfInnerRows());
	    }
	    else {
		mRowLimits.first  = max(mRowLimits.first, (mGeomDb->numberOfInnerRows()+1));
		mRowLimits.second = min(mRowLimits.second,(mGeomDb->numberOfRows()));
	    }
// 	    PR(mRowLimits.first);
// 	    PR(mRowLimits.second);

	    mPadLimits.first  = (centralPad > mDeltaPad) ?
		centralPad - mDeltaPad : centralPad;

	    //
	    // Loop over the cross coupled rows(irow)/pads(ipad)
	    //
	    for(int irow=mRowLimits.first; irow<=mRowLimits.second; irow++) {
		mPadLimits.second =
		    (centralPad < (mGeomDb->numberOfPadsAtRow(irow) - mDeltaPad)) ?
		    centralPad + mDeltaPad : mGeomDb->numberOfPadsAtRow(irow);
//  		PR(mPadLimits.first);
//  		PR(mPadLimits.second);
		for(int ipad=mPadLimits.first; ipad<=mPadLimits.second; ipad++) {
// 		    cout << " row: " << irow << " pad: " << ipad << endl;
#ifdef ST_SECTOR_BOUNDS_CHECK
		    if( !(ipad>0 && ipad<mGeomDb->numberOfPadsAtRow(irow)) )
			continue;
#endif
		    double padWidth, padLength;
		    if(irow > mGeomDb->numberOfInnerRows()) {  // pad in Outer Sector
			padWidth  = mGeomDb->outerSectorPadWidth();
			padLength = mGeomDb->outerSectorPadLength();
		    }
		    else {
			padWidth  = mGeomDb->innerSectorPadWidth();
			padLength = mGeomDb->innerSectorPadLength();			
		    }
		    tpcRaw.setPad(ipad);
		    tpcRaw.setRow(irow);
		    transformer(tpcRaw,xyCoord);
// 		    PR(tpcRaw);
// 		    PR(xyCoord);
		    // Integral limits for nearest pad
		    double xl = xyCoord.position().x() - padWidth/2;
		    double xu = xyCoord.position().x() + padWidth/2;
		    double yl = xyCoord.position().y() - padLength/2;
		    double yu = xyCoord.position().y() + padLength/2;

		    // charge location:  iter->position().x(), ycoord
		    // pad centroid:     xyCoord  // used to calculate integral limits

		    // signalOnPad calculates charge fraction!
		    // ie-->assumes charge=1, then the total charge is scaled
		    double chargeOfSignal =
		        signalOnPad(iter->position().x(),
				    iter->position().y(),  // charge location
				    xl, xu, yl, yu);       // integral limits
//    		    PR(chargeOfSignal);
//    		    PR(iter->numberOfElectrons());
		    chargeOfSignal *= iter->numberOfElectrons();
//    		    PR(chargeOfSignal);
		    //
		    // This should really be from the Coordinate transform!
		    // otherwise code has to be changed twice!
		    //
		    double timeOfSignal =
			(iter->position().z() + mElectronicsDb->tZero()*mDriftVelocity)/mDriftVelocity;
		    // OH-OH OFFSET (replaced!...)
		    timeOfSignal =
			iter->position().z()/mDriftVelocity;


		    // Check the threshold before you
		    // make and store an analog signal
		    //if() continue;
		    StTrsAnalogSignal padSignal(timeOfSignal, chargeOfSignal);

		    //
		    // DIAGNOSTIC: Print out all the signals on the pad
// 		    cout << "padSignal "
// 			 << padSignal.time()/nanosecond << " ns\t"
// 			 << padSignal.amplitude() << '\t'
// 			 << irow << "," << ipad <<endl;
		    mSector->addEntry(irow,ipad,padSignal);
		} // pad limits

	    } // row limits

	} // (iterator) Loop over all wires

    } // (jj)Loop over the wires of the Histogram

}

/*******************************************************************/
// Signal Sampling
// For all functions below: 
// tbin = 0,1,2,3...
// t    = 20,150,250,350...

double StTrsSlowAnalogSignalGenerator::deltaResponse(double tbin, StTrsAnalogSignal& s)
{
    double value=0;

    // Calculate pulse Height at bin Centroid
    double to = tbin*(1./mSamplingFrequency);
    double deltaT = (1./mSamplingFrequency)/2;

    value =  ((s.time() < to+deltaT) && (s.time() > to-deltaT)) ?
	mGain*s.amplitude() : 0;

    value *= mFractionSampled;

    //    cout << "gain/volt " << (s.amplitude()*mGain/(.001*volt)) << " mV" << endl;
    return value;
}

double StTrsSlowAnalogSignalGenerator::symmetricGaussianApproximateResponse(double tbin, StTrsAnalogSignal& s)
{
    //                                                2
    //                1                       (t - to)
    //    F :=   ------------------   exp(-  ---------- )
    //                  1/2                           2
    //           (2  Pi)    sigma             2  sigma

    // Take the value of the function at the mid-point of the
    // time bin, and multiply it by the width of the bin!
    // charge = F(t) dt
    double value=0;

    // Now defined as static const double mSymGausApproxFactor
    //double factor = 1/(TMath::Sqrt(2*pi)*mSigma1);
    //double factor = (M_SQRT1_2*M_2_SQRTPI/(2.*mSigma1));

    // Calculate at bin Centroid (from static const double)
    double t = mTimeBinWidth*(tbin+.5);

    value =  mGain*s.amplitude()*mSymGausApproxFactor*TMath::Exp(-sqr(t - s.time())/(2*sqr(mSigma1)));
    
    value *= mFractionSampled*mTimeBinWidth;
//     PR(value/(.001*volt));

    return value;

}
double StTrsSlowAnalogSignalGenerator::symmetricGaussianExactResponse(double tbin, StTrsAnalogSignal& s)
{
    //                                                2
    //                1                       (t - to)
    //    F :=   ------------------   exp(-  ---------- )
    //                  1/2                           2
    //           (2  Pi)    sigma             2  sigma

    double value=0;

    // Calculate at bin Centroid
    //double t = mTimeBinWidth*(tbin+.5);
    double lowerBound = tbin*mTimeBinWidth; // used to be --> 100.*nanosecond;
    double upperBound = lowerBound + mTimeBinWidth; // 100.*nanosecond;
//     PR(lowerBound);
//     PR(upperBound);
    
    value =  (mGain*s.amplitude()/2.)*
	(TMath::Erf((upperBound-s.time())/(M_SQRT2*mSigma1)) -
	 TMath::Erf((lowerBound-s.time())/(M_SQRT2*mSigma1)));

    value *= mFractionSampled;
    
    return value;
}

double StTrsSlowAnalogSignalGenerator::asymmetricGaussianApproximateResponse(double tbin, StTrsAnalogSignal& s)
{
    // Take the value of the function at the mid-point of the
    // time bin, and multiply it by the width of the bin!
    // charge = F(t) dt
    double value=0;

    // Assigned as "mTau"
    //double tau1 = mSigma1;
    //double tau2 = 2.*mSigma1;

    // Replaced by: mAsymGausApproxFactor
    //double factor = (M_2_SQRTPI/M_SQRT2)/(tau1+tau2);

    double t = mTimeBinWidth*(tbin+.5);
    
    if(t<s.time())
 	value =  mGain*s.amplitude()*mAsymGausApproxFactor*TMath::Exp(-sqr(t-s.time())/(2*sqr(mTau1)));
    else
 	value =  mGain*s.amplitude()*mAsymGausApproxFactor*TMath::Exp(-sqr(t-s.time())/(2*sqr(mTau2)));

    value *= (mFractionSampled*mTimeBinWidth);

    //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    // Amount of Charge in the bin
    //value *= 1./mSamplingFrequency;

    //    cout << "gain/volt " << (s.amplitude()*mGain/(.001*volt)) << " mV" << endl;
    return value;
}

double StTrsSlowAnalogSignalGenerator::realShaperResponse(double tbin, StTrsAnalogSignal& sig)
{
    //
    // Take the value of the function at the mid-point of the
    // time bin, and multiply it by the width of the bin!
    // charge = F(t) dt
    double value=0;

    // Use mDriftVelocity...
    //double driftVelocity = mSCDb->driftVelocity();

    // use mTau
    //double tau = mSigma1;

    // Oh darn! Why do we need gas parmeters here...it is because
    // we convolute the response of the electronics with the diffusion
    // DON'T DO THAT!!!!
    //double sigmaL = .05*centimeter/TMath::Sqrt(centimeter);
    double t = mTimeBinWidth*(tbin+.5);

    // Remember centroid is at 2tau+10ns
    // should be calculated via the extremem...but this
    // can only be found numerically...There is a routine
    // that will do this, but I won't incorporate it yet.
    // For now use the approximation:
    double tzero = sig.time() - 2.*mTau+10.*nanosecond;
    
    //double K = sigmaL*TMath::Sqrt(sig.time())/(tau*TMath::Sqrt(driftVelocity));
    double K = sigmaL/mTau*TMath::Sqrt(tzero/mDriftVelocity);

    //double lambda =  (sig.time() - t)/(K*tau) + K;
    double lambda =  (tzero - t)/(K*mTau) + K;

    // Corrected From SN197
    value = 1./(2.*mTau)*sqr(K)*TMath::Exp(K*(lambda-.5*K))*
	( .5*(1+sqr(lambda))*(1-TMath::Erf(lambda/M_SQRT2)) -
	  lambda/(TMath::Sqrt(2*M_PI))*TMath::Exp(-sqr(lambda)/2));

    value *= mFractionSampled*mGain*sig.amplitude();

    // Amount of Charge in the bin
    value *= mTimeBinWidth;
    
    //    cout << "gain/volt " << (s.amplitude()*mGain/(.001*volt)) << " mV" << endl;
    return value;

}

double StTrsSlowAnalogSignalGenerator::oneOverT(double t, double to)
{
    // CAUTION:  The following routine can be used to add an
    // undershoot or unrestored baseline to the signal profile.
    // Make sure you think you know what you are doing...
    double value;

    if(t == to) {
	value = 0;
    }
    else {
	value = 1./(t-to);     // Unrestored
	//value = -1./(t-to);  // Undershoot
    }
    return value;
}

// double StTrsSlowAnalogSignalGenerator::asymmetricGaussianResponseWithUnrestoredBaseline(double t, StTrsAnalogSignal& sig)
// {
//     double value=0;

//     // These are taken from DB at Constructor
// //     double mSigma1 = 1;
// //     double mSigma2 = TMath::Sqrt(8.);

// ---->  use     mAsymGausUnRestFactor
//     double factor = TMath::Sqrt(2/pi)/(mSigma1+mSigma2);
//     if(t<sig.time())
// 	value =  sig.amplitude()*mAsymGausUnRestFactor*exp(-sqr(t-sig.time())/(2*sqr(mSigma1)));
//     else {
// 	if ((t-sig.time())<mSigma2) {  // if inside 1*sigma
// 	    value =  sig.amplitude()*mAsymGausUnRestFactor*exp(-sqr(t-sig.time())/(2*sqr(mSigma2)));
// 	}
// 	else {  // generate a fraction of undershoot/unrestored
// 	    value =
// 		sig.amplitude()*mAsymGausUnRestFactor*exp(-sqr(t-sig.time())/(2*sqr(mSigma2))) +
//                              oneOverT(t,sig.time());
// 	}	    
//     }
//
//     value *= mFractionSampled;
//     return (mGain*value);
// }

void StTrsSlowAnalogSignalGenerator::setElectronicSampler(StSignal t)
{
    if(t == delta                           ||
       t == symmetricGaussianApproximation  ||
       t == symmetricGaussianExact          ||
       t == asymmetricGaussianApproximation ||
       t == realShaper )
	mSampler = t;
    else {
	cerr << "Cannot determine Electronics Response specified" << endl;
	// this would be a good place to throw an exception
	abort();
    }
}

// This is now an inline function.  See the .hh file
// inline double StTrsSlowAnalogSignalGenerator::signalSampler(double t, StTrsAnalogSignal& sig)
// {
//     //
//     // This is where the function for the Signal Sampling is selected
//     // Add a function that returns the amplitude of a signal at
//     // a time 't' given the position in time and amplitude of all
//     // the other signals (contained in the StTrsAnalogSignal 'sig'
//     // -- symmetricGaussianResponse
//     // -- asymmetricGaussianResponse
//     // -- endoResponse
//     if(mSampler == (StTrsSlowAnalogSignalGenerator::undefined)) {
// 	cerr << "ERROR: no function selected" << endl;
// 	exit(0);
//     }
//     switch(mSampler)
// 	{
// 	case symmetricGaussianApproximation:
// 	    return symmetricGaussianApproximateResponse(t, sig);
// 	    break;
// 	case delta:
// 	    return deltaResponse(t, sig);
// 	    break;
// 	case symmetricGaussianExact:
// 	    return symmetricGaussianExactResponse(t, sig);
// 	    break;
// 	case asymmetricGaussianApproximation:
// 	    return asymmetricGaussianApproximateResponse(t, sig);
// 	    break;
// 	case realShaper:
// 	    return realShaperResponse(t,sig);
// 	    break;
// 	    //case StSignal::asymmetricGaussianResponseWithUnRestoredBaseline:
// 	    //return asymmetricGaussianResponseWithUnRestoredBaseline(t, sig);
// 	    //break;
// 	default:
// 	    cerr << "Default Function Selected. ERROR!" << endl;
// 	    exit(0);
// 	    break;
// 	}
// }

void StTrsSlowAnalogSignalGenerator::sampleAnalogSignal()
{
     cout << "StTrsSlowAnalogSignalGenerator::sampleAnalogSignal()" << endl;
    
    // operates on mSector 

    // I have the centroid IN TIME (make sure!!!!) of each hit!
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StTrsAnalogSignal> continuousAnalogTimeSequence;
#else
    vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> > continuousAnalogTimeSequence;
#endif
    //double freq = mElectronicsDb->samplingFrequency();
    //PR(freq);
    //PR(mSector->numberOfRows());
    for(int irow=1; irow<=mSector->numberOfRows(); irow++) {
	
	//PR(irow);
	for(int ipad=1; ipad<=mSector->numberOfPadsInRow(irow); ipad++) {
	    //PR(ipad);
	    continuousAnalogTimeSequence = mSector->timeBinsOfRowAndPad(irow,ipad);

	    mDiscreteAnalogTimeSequence.clear();

	    // Make sure it is not empty
	    //PR(continuousAnalogTimeSequence.size());
	    if(!continuousAnalogTimeSequence.size()) continue;
	    for(mTimeSequenceIterator  = continuousAnalogTimeSequence.begin();
		mTimeSequenceIterator != continuousAnalogTimeSequence.end();
		mTimeSequenceIterator ++) {
//   		    PR(mTimeSequenceIterator->time());
//   		    PR(mTimeShiftOfSignalCentroid);
		    double tmpTime =
			mTimeSequenceIterator->time() +
			mTimeShiftOfSignalCentroid;
		    mTimeSequenceIterator->setTime(tmpTime);
//   		PR(mTimeSequenceIterator->time());
//   		PR(mTimeSequenceIterator->time()/nanosecond);
	    }
//  	    cout << "row/pad " << irow << '/' << ipad << ' ' << continuousAnalogTimeSequence.size() << endl;
	    
	    // Calculate the analog signal at the centroid of the time bin
	    // Loop over all the time bins:
//
//   	    cout << "How many signals? " << endl;
//   	    PR(continuousAnalogTimeSequence.size());
//   	    for(int bbb=0; bbb<continuousAnalogTimeSequence.size(); bbb++)
//   		cout << " " << bbb << " " << continuousAnalogTimeSequence[bbb] << endl;
//   	    cout << "row: " << irow << " pad: " << ipad << " timeBin: " << endl;

	    double timeBinT;
	    for(int itbin=0; itbin<mGeomDb->numberOfTimeBuckets(); itbin++) {
//   		cout << itbin << ", ";
		timeBinT = itbin*mTimeBinWidth;
//  		PR(timeBinT);
// 		PR(timeBinT/nanosecond);
		
		double pulseHeight = 0;
		for(mTimeSequenceIterator =continuousAnalogTimeSequence.begin();
		    mTimeSequenceIterator!=continuousAnalogTimeSequence.end();
		    mTimeSequenceIterator++) {

		    //
		    // The current time bin will be filled with
		    // charge from any signal that is within
		    // 10 time bins.  This should be a settable
		    // parameter.
		    //
		    if( fabs(timeBinT-mTimeSequenceIterator->time()) > 10.*mTimeBinWidth)
			continue;
//    		    cout << " tb " << itbin << " "
// 			 << mTimeSequenceIterator->time()/nanosecond << " " << (*mTimeSequenceIterator) << endl;
		    pulseHeight +=
			signalSampler(itbin, *mTimeSequenceIterator);
		}
		
		//
		// DIAGNOSTIC
		// Print out the pulse height in each time bin
//   		cout << itbin << " pulse Height: " << pulseHeight << '\t' << (pulseHeight/(.001*volt)) << " mV" << endl;

		//
		// Add noise here 
		//
		// : Everywhere
		if(!mAddNoiseUnderSignalOnly && mAddNoise) {
		    pulseHeight += generateNoise(); // noise;
		}

		//Do not store analog Signal if it is not above a
		// minimal threshold (should read value from database)
// 	        *********************************************************
  		//if(pulseHeight < mSignalThreshold) continue;
// 	        *********************************************************
		//
		// : Only Under Signal
		if(mAddNoiseUnderSignalOnly && mAddNoise) {
		    //double noise = generateNoise();
		    pulseHeight += generateNoise(); //noise;
		}
// 		cout << itbin << " pulse Height: " << pulseHeight << '\t' << (pulseHeight/(.001*volt)) << endl;

		mElectronicSignal.setTime(itbin);
		mElectronicSignal.setAmplitude(pulseHeight);
//   		if(mElectronicSignal.amplitude() !=0 ) {
// 		if(irow == 14 && (ipad == 14 || ipad == 52)) {
//  		    cout << "mElectronicSignal " << mElectronicSignal
//  			 << '\t' << (mElectronicSignal.amplitude()/(.001*volt)) << endl;
//  		}
		mDiscreteAnalogTimeSequence.push_back(mElectronicSignal);

	    } // loop over time bins

	    mSector->assignTimeBins(irow,ipad,mDiscreteAnalogTimeSequence);
	    
	} // loop over pads

    } // loop over rows

}
