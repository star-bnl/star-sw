/***************************************************************************
 *
 * $Id: StRichCerenkovHistogram.cxx,v 1.8 2002/05/21 22:52:55 lasiuk Exp $
 *
 * Author:  bl Mar 2, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              for use in Cerenkov Ray Tracing Algorithm
 * old version ~/test
 ***************************************************************************
 *
 * $Log: StRichCerenkovHistogram.cxx,v $
 * Revision 1.8  2002/05/21 22:52:55  lasiuk
 * attempt 2
 *
 * Revision 1.6  2002/02/19 04:26:49  lasiuk
 * addition of filling StEvent for inclusion in chain
 *
 * Revision 1.5  2002/02/01 17:45:56  lasiuk
 * Mods for gcc(7.2)
 * outer helix usage
 * histo mods
 *
 * Revision 1.4  2002/01/12 00:10:23  lasiuk
 * debin addition; quartz cerenkov angle, tuple modification, shift
 * to 183 nm for ray tracing, no temperature effect yet
 *
 * Revision 1.3  2001/12/19 20:18:38  lasiuk
 * Changeover in algorithm of isolating the Cherenkov angle
 *
 * Revision 1.2  2001/11/21 20:36:06  lasiuk
 * azimuth angle calculation, trace and retracing algorithms, rotation
 * matrices, clean up intersection calculation.  Addition of quick
 * rings for graphics, change definition of ntuples, and
 * removal of old PID method
 *
 * Revision 1.1  2001/08/21 17:58:33  lasiuk
 * for 2000 analysis
 *
 **************************************************************************/

#include "StRichCerenkovHistogram.h"

#include "StGlobals.hh"
#include "SystemOfUnits.h"

#include <numeric>
#include <algorithm>

#ifndef ST_NO_NAMESPACES
using namespace units;
using std::accumulate;
using std::max_element;
#endif

StRichCerenkovHistogram::StRichCerenkovHistogram()
{
    cout << "StRichCerenkovHistogram::StRichCerenkovHistogram()\n" << endl;
    this->init();
}

// ----------------------------------------------------
StRichCerenkovHistogram::~StRichCerenkovHistogram() {/*nopt*/}

// ----------------------------------------------------
void StRichCerenkovHistogram::init()
{
    cout << "StRichCerenkovHistogram::init()" << endl;

    //
    // 1 time only settings:
    //
    mDoPhiCut   = false;
    mInitialWindowSize = 40.*milliradian;

    this->clearData();
}

// ----------------------------------------------------
bool StRichCerenkovHistogram::setCerenkovQuantities(pair<double,double> quant)
{
    //cout << "StRichCerenkovHistogram::setCerenkovQuantities()" << endl;
    mCerenkovQuantities.push_back(quant);

//     PR(mCerenkovQuantities.back().first/degree);
//     PR(mCerenkovQuantities.back().second); // #photon fraction
    return true;
}

// ----------------------------------------------------
bool StRichCerenkovHistogram::addEntry(StRichCerenkovPhoton gamma)
{
//      cout << "StRichCerenkovHistogram::addEntry()" << endl;
//      PR(gamma);
//     PR(gamma.theta()/degree);
//     PR(gamma.phi()/degree);

    if(gamma.theta()>mLargestTheta)  mLargestTheta = gamma.theta();
    if(gamma.theta()<mSmallestTheta) mSmallestTheta = gamma.theta();
    mHistogram.push_back(gamma);

//     PR(mSmallestTheta/degree);
//     PR(mLargestTheta/degree);
    return true;
}

// ----------------------------------------------------
void StRichCerenkovHistogram::clearData()
{
//     cout << "StRichCerenkovHistogram::clearData()" << endl;
    mHistogram.clear();

    mCerenkovQuantities.clear();

    mResults.clear();
    
    mIteration1.clear();

    //
    // Cerenkov angle ranges
    //
    mSmallestTheta = 90.*degree;
    mLargestTheta = 0;

    //
    // results
    //
    mCerenkovAngle = 0;
    mNumberOfPhotons = 0;

    //
    // flags
    //
    mCalculationDone = false;
    //mFlag = 1;  initialized in ::evaluate();

    //
    // Cut Values and Parameters
    mPhiCut     = 0.*degree;
    mWindowSize = mInitialWindowSize;
    mPeakBin    = StRichWindowBin();
}

// ----------------------------------------------------
bool StRichCerenkovHistogram::checkHypothesis() {

//     cout << "StRichCerenkovHistogram::checkHypothesis()" << endl;

    //
    // loop over the hypothesis
    // first is the angle
    // second is the fraction
    //

    //
    // Printing out the "expected values"
    //
//     for(size_t ii=0; ii<mCerenkovQuantities.size(); ii++) {
// 	double angle = mCerenkovQuantities[ii].first;
// 	cout << ii << "\tangle="
// 	     << angle/degree << "\tfraction="
// 	     << mCerenkovQuantities[ii].second <<endl;
//     }

    return true;
}

// ----------------------------------------------------
double StRichCerenkovHistogram::peakAngle(short* numberOfPhotons) {

    //
    // the maximum Cerenkov angle here is;
    // 38.9 => that is saturated
    //

    *numberOfPhotons = mPeakBin.mNumber;
    return mPeakBin.mAngle;
}


// ----------------------------------------------------
StRichWindowBin
StRichCerenkovHistogram::calculateBinStatistics(double lowerBound, double upperBound) {

//     cout << "StRichCerenkovHistogram::calculateBinStatistics()" << endl;
//     PR(lowerBound/degree);
//     PR(upperBound/degree);

    double binValue = 0.;
    double binSigma = 0.;
    double binSymmetry = 0.;
    
    double moment1=0;
    double moment2=0;
    double sumWeight=0;

    vector<double> binAngles;
    for(size_t ii=0; ii< mHistogram.size(); ii++) {

	double theta = mHistogram[ii].theta();

// 	PR(theta/degree);
	if((theta>=lowerBound) && (theta<upperBound) ) {
// 	    cout << "add->" << theta/degree << endl;
	    binAngles.push_back(theta);
	    double weight = this->weight(theta);
	    moment1 += weight*theta;
	    moment2 += weight*sqr(theta);
	    sumWeight += weight;
		
	}
    } // for loop

    //
    // put it in...
//     PR(sumWeight);
//     PR(moment1);
//     PR(moment2);
//     PR(binAngles.size());

    if(binAngles.size()>0) {
	binValue = moment1/sumWeight;
	binSigma = sqrt(fabs((moment2/sumWeight) - sqr(binValue)));

	for(size_t jj=0; jj<binAngles.size(); jj++) {
	    binSymmetry += (binValue-binAngles[jj])*this->weight(binAngles[jj]);
	}
	binSymmetry /= sumWeight;
    }
    else {
	binValue = (lowerBound+upperBound)/2.;
	binSymmetry = 999;
    }
//     PR(sumWeight);
//     PR(binValue/degree);
//     PR(binSigma/degree);
//     PR(binSymmetry/degree);
    
    return StRichWindowBin(binValue,sumWeight,binSigma,binSymmetry,binAngles.size());
}


// ----------------------------------------------------
void StRichCerenkovHistogram::calculateSlidingWindowHistogram() {

//     cout << "StRichCerenkovHistogram::calculateSlidingWindowHistogram()" << endl;
//     PR(mWindowSize/milliradian);
    
    double lowerBound = mSmallestTheta;
    mResults.setWindowSize(mWindowSize);
    mResults.setPhiCut(mPhiCut);

    
    do {
	//
	// Initialization for each bin calculation
	//
	double upperBound = lowerBound+mWindowSize;
// 	PR(lowerBound/degree);
// 	PR(upperBound/degree);

	//mResult.addEntry(this->calculateBinStatistics(lowerBound,upperBound));

	StRichWindowBin bin = this->calculateBinStatistics(lowerBound,upperBound);
// 	PR(bin);
	mResults.addEntry(bin);

	//
	// Increase to the next bin
	//
	lowerBound += mWindowSize;
// 	PR(lowerBound/degree);

    } while (lowerBound<mLargestTheta);

//     PR(mResults.size());
}


// ----------------------------------------------------
void StRichCerenkovHistogram::evaluate() {

    //
    // The binning effect must be removed before
    // the Cherenkov angle can be quoted.
    // First sort the Histogram bins for further processeing
    //
    
//     cout << "StRichCerenkovHistogram::evaluate()" << endl;
    size_t ii;

//     PR(mIteration1.size());
    mIteration1.clear();
//     PR(mWindowSize/radian);
    double it1WindowSize = .75*mWindowSize;
    //double it1WindowSize = mWindowSize/4;
//     PR(it1WindowSize/radian);

    mIteration1.setWindowSize(it1WindowSize);
    mIteration1.setPhiCut(mPhiCut);

    StRichWindowBin newBin;
    for(ii=0; ii<mResults.size(); ii++) {
	StRichWindowBin binUnderEvaluation = mResults.bin(ii);

	double centralValue = 0;
	double currentValue = binUnderEvaluation.mAngle;
 	//if(currentValue/degree >41.5) break;
	short maxCnt = 20;
	short ctr = 0;

	do {
	    centralValue = currentValue;
// 	    PR(centralValue/degree);
	    double lowerBound = centralValue - it1WindowSize/2.;
	    double upperBound = centralValue + it1WindowSize/2.;
// 	    PR(lowerBound/degree);
// 	    PR(upperBound/degree);
	    
	    newBin =
		this->calculateBinStatistics(lowerBound,upperBound);
	    currentValue = newBin.mAngle;
// 	    PR(newBin);
// 	    PR(currentValue/degree);
	    ctr++;
	    if(ctr>maxCnt) break;
	} while (currentValue != centralValue);
	
// 	PR(newBin);
	
	mIteration1.addEntry(newBin);

// 	PR(mIteration1.size());
	
    } // loop over all bins
    
    //
    //
    // mIteration1 contains the "debinned" angles
    // Resort them to make sure that in the recalculation
    // the bin value hasn't changed.
    //
    // If the extracted Cherenkov angle is within
    // several degrees of the "expected" hypothesis,
    // return a flag value of 0
    //

    mIteration1.process();
//     mIteration1.status();
    
    double deltaTheta = 2.*degree;
    double maxAngle = 41.*degree;
    StRichWindowBin theResult;
    mFlag = 1;
//     cout << "maxAngle" << maxAngle << endl;

    for(size_t ii=0; ii< mIteration1.size(); ii++) {
	theResult = mIteration1.bin(ii);
// 	PR(theResult);
// 	cout << "theResult.mAngle " << theResult.mAngle << endl;

	if(theResult.mAngle > maxAngle) {
// 	    cout << "Exceeds max (" << theResult.mAngle << ")" << endl;
	    continue;
	}
	
	if( (theResult.mAngle > (mCerenkovQuantities[0].first-deltaTheta) &&
	     theResult.mAngle < (mCerenkovQuantities[0].first+deltaTheta)) ||
	    (theResult.mAngle > (mCerenkovQuantities[1].first-deltaTheta) &&
	     theResult.mAngle < (mCerenkovQuantities[1].first+deltaTheta)) ||
	    (theResult.mAngle > (mCerenkovQuantities[2].first-deltaTheta) &&
	     theResult.mAngle < (mCerenkovQuantities[2].first+deltaTheta)) ) {

	    if(ii==0)
		mFlag = 0;
	    
	}
	break;
    }
    
    //
    // Assign the Cherenkov angle
    //

//     cout << "This is the result that will be quoted" << endl;
    PR(theResult);
    mCerenkovAngle = theResult.mAngle;
    mCerenkovSigma = theResult.mSigma;
    mNumberOfPhotons = theResult.mNumber;
    
//      PR(mCerenkovAngle/degree);
//      PR(mCerenkovSigma/degree);
//      PR(mNumberOfPhotons);

}

// ----------------------------------------------------
void StRichCerenkovHistogram::findPeakAngle() {

//     cout << "StRichCherenkovHistogram::findPeakAngle()" << endl;

    mResults.process();
//     mResults.status();

//     cout << "Peak angle is: " << endl;
    int numberOfPhotons = 0;
    StRichWindowBin bin;
    for(size_t ii=0; ii<mResults.size(); ii++) {

	if(mResults.bin(ii).mNumber > numberOfPhotons) {
	    bin = mResults.bin(ii);
	    numberOfPhotons = mResults.bin(ii).mNumber;
	}
	
    }
    mPeakBin = bin;
//     PR(mPeakBin);
}

// ----------------------------------------------------
void StRichCerenkovHistogram::calculate() {

//     cout << "StRichCerenkovHistogram::calculate()\n";
    
//     PR(mSmallestTheta/degree);
//     PR(mLargestTheta/degree);

    //mInitialWindowSize = 20.*milliradian;
    this->setWindowSize(mInitialWindowSize);
//     PR(mWindowSize/milliradian);

//     PR(mResults.size());
    this->calculateSlidingWindowHistogram();
//     PR(mResults.size());
    
//     cout << "StRichCerenkovHistogram  Sliding window done" << endl;

    this->findPeakAngle();
    //
    // Assign a Cherenkov angle based on the
    // collection of Rich Cherekov Windows
    //
    this->evaluate();

    //
    // secondary check
    //
    this->setWindowSize(mInitialWindowSize);
    this->checkHypothesis();


    mCalculationDone = true;
}

// ----------------------------------------------------
double StRichCerenkovHistogram::cerenkovAngle(unsigned short *flag) {

    *flag = mFlag;
    return mCerenkovAngle;
}

// ---------------------------------------------------
double StRichCerenkovHistogram::cerenkovSigma() const
{
    return mCerenkovSigma;
}

// ----------------------------------------------------
void StRichCerenkovHistogram::status() {

    if(!mCalculationDone) {
	cout << "StRichCerenkovHistogram::status()" << endl;
	cout << "Must Call calculate() first" << endl;
	return;
    }
//     cout << "Smallest Angle: " << (mSmallestTheta/degree) << endl;
//     cout << "Largest Angle:  " << (mLargestTheta/degree) << endl;
    cout << "-------------------------------------------------------" << endl;
    cout << "Angle  #photons  Weight  Sigma  Symmetry"   << endl;
    cout << "-------------------------------------------------------" << endl;

    if(mIteration1.size()) {
	cout << (mIteration1.bin(0)) << endl;
	cout << (mPeakBin) << endl;
    }
    else {
	cout << "undefined value mFlag=(" << mFlag << ")" << endl;
    }
    cout << "===========================================" << endl;
}
