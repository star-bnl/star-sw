/***************************************************************************
 *
 * $Id: StRichCerenkovHistogram.cxx,v 1.3 2001/12/19 20:18:38 lasiuk Exp $
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

    this->clearData();
    
    //
    // constant angle default
    //
    mPhiCut     = 0.*degree;
    mWindowSize = 40.*milliradian;
    
    mDoPhiCut   = false;
}

// ----------------------------------------------------
bool StRichCerenkovHistogram::setCerenkovQuantities(pair<double,double> quant)
{
    cout << "StRichCerenkovHistogram::setCerenkovQuantities()" << endl;
    PR(mCerenkovQuantities.size());
    mCerenkovQuantities.push_back(quant);

    PR(mCerenkovQuantities.back().first/degree);
    PR(mCerenkovQuantities.back().second);
    return true;
}

// ----------------------------------------------------
bool StRichCerenkovHistogram::addEntry(StRichCerenkovPhoton gamma)
{
    cout << "StRichCerenkovHistogram::addEntry()" << endl;
    PR(gamma);
//     PR(gamma.theta()/degree);
//     PR(gamma.phi()/degree);

    if(gamma.theta()>mLargestTheta)  mLargestTheta = gamma.theta();
    if(gamma.theta()<mSmallestTheta) mSmallestTheta = gamma.theta();
    mHistogram.push_back(gamma);

    PR(mSmallestTheta/degree);
    PR(mLargestTheta/degree);
    return true;
}

// ----------------------------------------------------
void StRichCerenkovHistogram::clearData()
{
    cout << "StRichCerenkovHistogram::clearData()" << endl;
    PR(mHistogram.size());
    mHistogram.clear();

    PR(mCerenkovQuantities.size());
    mCerenkovQuantities.clear();

    PR(mResults.size());
    mResults.clear();
    

    mSmallestTheta = 90.*degree;
    mLargestTheta = 0;

    mCerenkovAngle = 0;
    mNumberOfPhotons = 0;
    
    mCalculationDone = false;
}






// ----------------------------------------------------
bool StRichCerenkovHistogram::checkHypothesis() {

    cout << "StRichCerenkovHistogram::checkHypothesis()" << endl;
    PR(mCerenkovAngle/degree);
    //
    // loop over the hypothesis
    // first is the angle
    // second is the fraction
    //

    vector<double> counts;
    counts.resize(3); counts[0] = 0; counts[1] = 0; counts[2]=0;
    for(size_t ii=0; ii<mCerenkovQuantities.size(); ii++) {
	double angle = mCerenkovQuantities[ii].first;
	cout << ii << "\tangle=" << angle/degree << endl;
	if(angle > 1) {
	    cout << "StRichCerenkovHistogram::checkHypothesis() BAD (" << ii << ")" << endl;
	    continue;
	}

	//
	//
	// Calculate 1,2,3 sigma effects
	//
	//
	
// 	for(size_t jj=0; jj<mHistogram.size(); jj++) {

// 	    double theta = mHistogram[jj].theta();
// 	    PR(theta/degree);
// 	    PR(windowSize/degree);
// 	    if( (angle>(theta-mWindowSize)) && (angle<(theta+mWindowSize)) ) {
// 		counts[ii] += this->weight(theta);
// 	    }
	    
// 	}
    }

    //
    //
    // Reassigne if it is not compatible
    //
    //
	
    return true;
}

// ----------------------------------------------------
double StRichCerenkovHistogram::mean() const {

    double sum = 0;
    double numberOfEntries = 0;
    double mean = 0;

    numberOfEntries = mHistogram.size();
    for(size_t ii=0; ii<numberOfEntries; ii++) {

	sum += mHistogram[ii].theta();

    }

    if(numberOfEntries) {
	mean = (sum/numberOfEntries);
    }

    return mean;
}

// ----------------------------------------------------
double StRichCerenkovHistogram::secondMoment() const {

    double sum = 0;
    double numberOfEntries = mHistogram.size();
    double secondMoment = 0;
    for(size_t ii=0; ii<mHistogram.size(); ii++) {

	sum += sqr(mHistogram[ii].theta());

    }

    if(numberOfEntries) {
	secondMoment = (sum/numberOfEntries);
    }

    return secondMoment;
}

// ----------------------------------------------------
double StRichCerenkovHistogram::sigma() const {

    return ( sqrt(this->rms()) );
}

// ----------------------------------------------------
double StRichCerenkovHistogram::rms() const {

    return (this->secondMoment() - sqr(this->mean()) );
}

// ----------------------------------------------------
double StRichCerenkovHistogram::bestAngle() {

    //
    // the maximum Cerenkov angle here is;
    // 38.9 => that is saturated
    //

    return 0.0;
}





// ----------------------------------------------------
void StRichCerenkovHistogram::calculateSlidingWindow() {

    cout << "StRichCerenkovHistogram::calculateSlidingWindow()" << endl;
    PR(mWindowSize/milliradian);
    
    double lowerBound = (mSmallestTheta-mWindowSize/2);
    StRichWindowHistogram result(mWindowSize, mPhiCut);

    
    do { //
	 // Initialization for each bin calculation
	 //
 	double binValue  = 0;
 	double binSigma  = 0;

	double upperBound = lowerBound+mWindowSize;
	PR(lowerBound/degree);
	PR(upperBound/degree);

	double moment1=0;
	double moment2=0;
	double sumWeight=0;

	for(size_t ii=0; ii< mHistogram.size(); ii++) {

	    double theta = mHistogram[ii].theta();

	    PR(theta/degree);
	    if((theta>=lowerBound) && (theta<upperBound) ) {
		cout << "add->" << theta/degree << endl;
		double weight = this->weight(theta);
		moment1 += weight*theta;
		moment2 += weight*sqr(theta);
		sumWeight += weight;
		
	    }
	} // for loop

	//
	// put it in...
	PR(sumWeight);
	PR(moment1);
	PR(moment2);

	if(sumWeight) {
	    binValue = moment1/sumWeight;
	    binSigma = (moment2/sumWeight) - sqr(binValue);
	}
	else {
	    binValue = lowerBound+mWindowSize/2.;
	}
	
	result.addEntry(StRichWindowBin(binValue,sumWeight,binSigma));
 	PR(binValue/degree);
	//
	//
	cout << "increase" << endl;
	lowerBound += mWindowSize;
	PR(lowerBound/degree);

    } while (lowerBound<mLargestTheta);

    PR(mResults.size());
    mResults.push_back(result);
    PR(mResults.size());
}


// ----------------------------------------------------
void StRichCerenkovHistogram::evaluate() {

    cout << "StRichCerenkovHistogram::evaluate()" << endl;
    size_t ii;
    for(ii=0; ii<mResults.size(); ii++) {
	mResults[ii].process();
    }

    //
    // Assign the Cherenkov angle
    //
    //
    // compare phiCut and noPhiCut values
    //
    PR(mResults[0].maxBin());
    mCerenkovAngle = mResults[0].maxBin()->mAngle;
    mCerenkovSigma = mResults[0].maxBin()->mSigma;
    mNumberOfPhotons = mResults[0].maxBin()->mWeight;
    
    PR(mCerenkovAngle/degree);
    PR(mCerenkovSigma/degree);
    PR(mNumberOfPhotons);
}

// ----------------------------------------------------
void StRichCerenkovHistogram::calculate() {

    cout << "StRichCerenkovHistogram::calculate()\n";
    
    PR(mSmallestTheta/degree);
    PR(mLargestTheta/degree);

    double initialWindowSize = 40.*milliradian;
    this->setWindowSize(initialWindowSize);
    PR(mWindowSize/milliradian);

    PR(mResults.size());
    this->calculateSlidingWindow();
    PR(mResults.size());
    
    cout << "StRichCerenkovHistogram  Sliding window done" << endl;

    //
    // Assign a Cherenkov angle based on the
    // collection of Rich Cherekov Windows
    //
    this->evaluate();

    //
    // secondary check
    //
    this->setWindowSize(initialWindowSize);
    this->checkHypothesis();


    mCalculationDone = true;
}

// ----------------------------------------------------
double StRichCerenkovHistogram::cerenkovAngle(unsigned short *flag) {

    ///
    // should be in init
//     double mfraction = .50;
//     double mdeviation = 2.*degree;
   
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
    cout << "-------------------------------------------------------" << endl;
    cout << "Bin Value   All Counts    Phi Cut"   << endl;
    cout << "-------------------------------------------------------" << endl;

    cout.width(8);
    cout.precision(4);
    for(size_t ii=0; ii<mResults.size(); ii++) {
	mResults[ii].status();
    }
    cout.precision(6);
    cout.width(0);
    cout << "===========================================" << endl;
}
