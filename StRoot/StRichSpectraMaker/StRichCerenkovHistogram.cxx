/***************************************************************************
 *
 * $Id: StRichCerenkovHistogram.cxx,v 1.2 2001/11/21 20:36:06 lasiuk Exp $
 *
 * Author:  bl Mar 2, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              for use in Cerenkov Ray Tracing Algorithm
 ***************************************************************************
 *
 * $Log: StRichCerenkovHistogram.cxx,v $
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

StRichCerenkovHistogram::StRichCerenkovHistogram(int numbBins)
    : mNumberOfBins(numbBins)
{
    cout << "StRichCerenkovHistogram::StRichCerenkovHistogram(int)\n";
    this->init();
}

// ----------------------------------------------------
StRichCerenkovHistogram::~StRichCerenkovHistogram() {/*nopt*/}

// ----------------------------------------------------
void StRichCerenkovHistogram::init()
{
    cout << "StRichCerenkovHistogram::init()" << endl;
    mHistogram.clear();
    mHistogram.resize(mNumberOfBins);
    PR(mHistogram.size());

    mResults.resize(eNumberOfHistoCuts);
    mAngleRange = 50.*degree;
    
    mBinSize    = mAngleRange/mHistogram.size();


    mCalculationDone = false;
    
    //
    // constant angle default
    //
    mPhi        = 0.*degree;
    mDoPhiCut   = false;

    mThreshold      = 0;
    mDoThresholdCut = false;
}

// ----------------------------------------------------
bool StRichCerenkovHistogram::addEntry(StRichCerenkovPhoton gamma)
{
//      cout << "StRichCerenkovHistogram::addEntry()" << endl;
//     PR(gamma);
//      PR(gamma.theta()/degree);
//      PR(gamma.phi()/degree)
//     PR(mBinSize);
//     PR(mBinSize/degree);
    
    size_t bin = this->whichBin(gamma.theta());
//     PR(bin);
    if( bin< mHistogram.size() ) {
	mHistogram[bin].push_back(gamma);
	return true;
    }
    return false;
}

// ----------------------------------------------------
void StRichCerenkovHistogram::clearData()
{
    cout << "StRichCerenkovHistogram::clearData()" << endl;
    PR(mHistogram.size());
    size_t ii;
    for(ii=0; ii<mHistogram.size(); ii++) {
	mHistogram[ii].clear();
    }

    cout << "try results" << endl;
    PR(mResults.size());
    for(ii=0; ii<mResults.size(); ii++) {
	mResults[ii].clear();
    }
    cout << "done" << endl;
    mCalculationDone = false;
}

// ----------------------------------------------------
int StRichCerenkovHistogram::rawEntries(int thresh) const {

    int sum = 0;

    for(size_t ii=0; ii< mHistogram.size(); ii++) {
	
	int entries = mHistogram[ii].size();
	entries = max(0, entries-thresh);
	sum += entries;
    }
    return sum;
}
// ----------------------------------------------------
int StRichCerenkovHistogram::binEntries(size_t bin) const {

    //cout << "StRichCerenkovHistogram::binEntries()" << endl;
    int entries = 0;
    if(bin>=0 && bin <mHistogram.size()) {

	entries = (mDoPhiCut) ? this->countPhi(bin) : mHistogram[bin].size();

	if(mDoThresholdCut) {
	    entries = max(0, (entries-mThreshold));
	}
    }
    else {
	cout << "StRichCerenkovHistogram::binEntries()\n";
	cout << "\tWARNING (" << bin << ") not a valid bin" << endl;
    }
    
    return entries;
}

// ----------------------------------------------------
int StRichCerenkovHistogram::numberOfEntries() const {

    int sum = 0;
    
    for(size_t ii=0; ii<mHistogram.size(); ii++) {
	sum += this->binEntries(ii);
    }

    return sum;
}

// ----------------------------------------------------
double StRichCerenkovHistogram::binValue(size_t i) const {

    double value = 0;
    if(i<mHistogram.size()) {
	value = (mBinSize * (.5+i));
    }
    else {
	cout << "StRichCerenkovHistogram::binValue()\n";
	cout << "\tWARNING: Bad Bin (" << i << ")" << endl;
    }
    
    return value;
}

// ----------------------------------------------------
int StRichCerenkovHistogram::countPhi(int bin) const {

    //cout << "StRichCerenkovHistogram::countPhi()" << endl;

    // Assume that bin is checked already
    int entries = 0;
    for(size_t ii=0; ii<mHistogram[bin].size(); ii++) {

	if( fabs(mHistogram[bin][ii].phi()) > mPhi) {
	    entries += 1;
	}

    }
    return entries;
}

// ----------------------------------------------------
size_t StRichCerenkovHistogram::whichBin(double value) const {

    // assume value is in radians already
    //PR(bin);
    return static_cast<size_t>(floor( (value)/mBinSize ));

}

// ----------------------------------------------------
double StRichCerenkovHistogram::pedestal() const {

    double pedestal = 0;
    double sum = 0.;
    double numberOfBins = 0.;
    for(size_t ii=0; ii<mHistogram.size(); ii++) {

	int entries = this->binEntries(ii);

	if(entries) {
	    sum += entries;
	    numberOfBins +=1.;
	}

    }
    
    if(numberOfBins) {
	pedestal = (sum/numberOfBins);
    }
    
    return pedestal;
}

// ----------------------------------------------------
double StRichCerenkovHistogram::pedestalSigma() const {

    double sum = 0.;
    double numberOfBins = 0.;
    for(size_t ii=0; ii<mHistogram.size(); ii++) {

	int entries = this->binEntries(ii);

	if(entries) {
	    sum += sqr(entries);
	    numberOfBins +=1.;
	}

    }

    return ( (sum/numberOfBins) - sqr(this->pedestal()));
}


// ----------------------------------------------------
double StRichCerenkovHistogram::mean() const {

    double sum = 0;
    double numberOfEntries = 0;
    double mean = 0;
    
    for(size_t ii=0; ii<mHistogram.size(); ii++) {

	double entries = this->binEntries(ii);

	numberOfEntries += entries;
	sum += (entries)*(this->binValue(ii));

    }

    if(numberOfEntries) {
	mean = (sum/numberOfEntries);
    }

    return mean;
}

// ----------------------------------------------------
double StRichCerenkovHistogram::secondMoment() const {

    double sum = 0;
    double numberOfEntries = 0;
    double secondMoment = 0;
    
    for(size_t ii=0; ii<mHistogram.size(); ii++) {

	double entries = this->binEntries(ii);
	numberOfEntries += entries;
	sum += (entries)*sqr(this->binValue(ii));

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
double StRichCerenkovHistogram::mostProbable() const {

    double maxEntries = 0;
    double maxBin  = 0;

    for(size_t ii=0; ii< mHistogram.size(); ii++) {

	double numberOfEntries = this->binEntries(ii);
	if(numberOfEntries >= maxEntries) {
	    maxEntries = numberOfEntries;
	    maxBin = ii;
	}
	
    }
    return (this->binValue(maxBin));
}

// ----------------------------------------------------
double StRichCerenkovHistogram::bestAngle() {

    //
    // the maximum Cerenkov angle here is;
    // 38.9 => that is saturated
    //

    bool oldThresholdCut = mDoThresholdCut;
    bool oldPhiCut = mDoPhiCut;
    
    double pedestal = floor(this->pedestal());

    this->setThreshold(static_cast<int>(pedestal));
    this->doThresholdCut(true);

    double theBestAngle = this->mean(); 
    this->doCuts(oldThresholdCut, oldPhiCut);

    return theBestAngle;
}

// ----------------------------------------------------
double StRichCerenkovHistogram::truncatedAngle() {

//     PR(this->bestAngle());
//     PR(mBinSize);

    int centralBin = this->whichBin(this->bestAngle());
    //cout << "centralBin= " << centralBin;
    double width = floor( (this->sigma())/(mBinSize) );
    
    double sum = 0;
    double numberOfEntries = 0;
    double truncatedAngle = 0.;
    for(size_t ii=static_cast<size_t>(centralBin-width);
	ii<=static_cast<size_t>(centralBin+width); ii++) {
	
	double entries = this->binEntries(ii);
	
	numberOfEntries += entries;
	sum += entries*(this->binValue(ii));

    }

    if(numberOfEntries) {
	truncatedAngle = sum/numberOfEntries;
    }

    return truncatedAngle;
}

// ----------------------------------------------------
void StRichCerenkovHistogram::calculate() {

    bool oldThresholdCut = mDoThresholdCut;
    bool oldPhiCut       = mDoPhiCut;

    //doCuts(theshold, phi);

    //
    // NO CUTS
    //

    this->doCuts(false, false);
//     cout << "thold= "     << mThreshold       << "(" << mDoThresholdCut << ")"
// 	 << " phi= "      << (mPhi/degree)    << "(" << mDoPhiCut       << ")"
// 	 << " Pedestal= " << this->pedestal() << "\n";
    mResults[eNoCuts].push_back(this->mean());
    mResults[eNoCuts].push_back(this->sigma());
    mResults[eNoCuts].push_back(this->mostProbable());
    mResults[eNoCuts].push_back(this->bestAngle());
    mResults[eNoCuts].push_back(this->truncatedAngle());
    mResults[eNoCuts].push_back(this->pedestal());
    mResults[eNoCuts].push_back(static_cast<double>(this->numberOfEntries()));

    //
    // THRESHOLD ONLY
    //

    this->doCuts(true, false);
//     cout << "thold= "     << mThreshold       << "(" << mDoThresholdCut << ")"
// 	 << " phi= "      << (mPhi/degree)    << "(" << mDoPhiCut       << ")"
// 	 << " Pedestal= " << this->pedestal() << "\n";
    mResults[eTHOnly].push_back(this->mean());
    mResults[eTHOnly].push_back(this->sigma());
    mResults[eTHOnly].push_back(this->mostProbable());
    mResults[eTHOnly].push_back(this->bestAngle());
    mResults[eTHOnly].push_back(this->truncatedAngle());
    mResults[eTHOnly].push_back(this->pedestal());
    mResults[eTHOnly].push_back(this->numberOfEntries());
    
    //
    // PHI ONLY
    //

    this->doCuts(false, true);
//     cout << "thold= "     << mThreshold       << "(" << mDoThresholdCut << ")"
// 	 << " phi= "      << (mPhi/degree)    << "(" << mDoPhiCut       << ")"
// 	 << " Pedestal= " << this->pedestal() << "\n";
    mResults[ePhiOnly].push_back(this->mean());
    mResults[ePhiOnly].push_back(this->sigma());
    mResults[ePhiOnly].push_back(this->mostProbable());
    mResults[ePhiOnly].push_back(this->bestAngle());
    mResults[ePhiOnly].push_back(this->truncatedAngle());
    mResults[ePhiOnly].push_back(this->pedestal());
    mResults[ePhiOnly].push_back(this->numberOfEntries());
    
    //
    // TH and PHI
    //

    this->doCuts(true, true);
//     cout << "thold= "     << mThreshold       << "(" << mDoThresholdCut << ")"
// 	 << " phi= "      << (mPhi/degree)    << "(" << mDoPhiCut       << ")"
// 	 << " Pedestal= " << this->pedestal() << "\n";
    mResults[eTHPhi].push_back(this->mean());
    mResults[eTHPhi].push_back(this->sigma());
    mResults[eTHPhi].push_back(this->mostProbable());
    mResults[eTHPhi].push_back(this->bestAngle());
    mResults[eTHPhi].push_back(this->truncatedAngle());
    mResults[eTHPhi].push_back(this->pedestal());
    mResults[eTHPhi].push_back(this->numberOfEntries());
    
    this->doCuts(oldThresholdCut, oldPhiCut);
    mCalculationDone = true;
}

// ----------------------------------------------------
double StRichCerenkovHistogram::cerenkovAngle(unsigned short *flag) {

    if(!mCalculationDone) this->calculate();

    double cerenkovAngle = 0.;

    ///
    // should be in init
    double mfraction = .50;
    double mdeviation = 2.*degree;
   
    //
    // Phi cut no threshold:
    double fraction =
	(mResults[ePhiOnly][eNumberOfEntries])/(mResults[eNoCuts][eNumberOfEntries]);
    if(fraction < mfraction) {
	cout << "StRichCerenkovHistogram::cerenkovAngle\n";
	cout << "BAD FRACTION" << endl;
	*flag = 1;
    }

    if( mResults[eNoCuts][eSigmaAngle] <  mResults[ePhiOnly][eSigmaAngle]) {
    	cout << "StRichCerenkovHistogram::cerenkovAngle\n";
	cout << "BAD SIGMA" << endl;
	*flag = 1;
    }

    double deviation = 
	fabs(mResults[ePhiOnly][eMostProbableAngle] - mResults[ePhiOnly][eBestAngle]);

    if(deviation>mdeviation) {
    	cout << "StRichCerenkovHistogram::cerenkovAngle\n";
	cout << "BAD DEVIATION" << endl;
	*flag = 1;
    }
    
    cerenkovAngle = mResults[ePhiOnly][eBestAngle];
    return cerenkovAngle;
}

// ----------------------------------------------------
double StRichCerenkovHistogram::cerenkovSigma() const
{
    return mResults[ePhiOnly][eSigmaAngle];
}

// ----------------------------------------------------
double StRichCerenkovHistogram::cerenkovMostProbable() const
{
    return mResults[ePhiOnly][eMostProbableAngle];
}

// ----------------------------------------------------
void StRichCerenkovHistogram::status() {

    if(!mCalculationDone) this->calculate();
    
    bool oldThresholdCut = mDoThresholdCut;
    bool oldPhiCut = mDoPhiCut;

    cout << "StRichCerenkovHistogram::status()" << endl;
    cout << "===============================================" << endl;
    cout << "Bin    value  count    phi<" << (this->phi()/degree) << endl;
    cout << "===============================================" << endl;
    for(size_t ii=0; ii<mHistogram.size(); ii++) {
	this->doCuts(false,false);
	cout << ii << "\t"
	     << (this->binValue(ii)/degree) << "\t"
	     << (this->binEntries(ii)) << "\t";
	this->doCuts(false,true);
	cout << (this->binEntries(ii)) << endl;
    }
    cout << "===========================================" << endl;
    this->doCuts(false,false);
    cout << "\t\t" << (this->numberOfEntries()) << "\t";
    this->doCuts(false,true);
    cout << this->numberOfEntries() << endl;
    this->doCuts(oldThresholdCut, oldPhiCut);
    cout << "-------------------------------------------------------" << endl;
    cout << "                Most       Best   Truncated" << endl;
    cout << "Mean    Sigma  Probable    Angle    Angle      Entries"   << endl;
    cout << "-------------------------------------------------------" << endl;

    cout.width(8);
    cout.precision(4);
    //
    // NO CUTS
    //
    cout << "T/H= "      << mThreshold       << "(0) "
	 << "phi= "      << (mPhi/degree)    << "(0) "
	 << "Pedestal= " << mResults[eNoCuts][ePedestal] << "\n";
    cout << (mResults[eNoCuts][eMeanAngle]/degree) << "\t"
	 << (mResults[eNoCuts][eSigmaAngle]/degree) << "\t"
	 << (mResults[eNoCuts][eMostProbableAngle]/degree) << "\t"
	 << (mResults[eNoCuts][eBestAngle]/degree) << "\t"
	 << (mResults[eNoCuts][eTruncatedAngle]/degree) << "\t"
	 << (mResults[eNoCuts][eNumberOfEntries]) << endl;

    //
    // THRESHOLD CUT ONLY
    //
    cout << "T/H= "     << mThreshold       << "(1) "
	 << "phi= "     << (mPhi/degree)    << "(0) "
	 << "Pedestal= " << mResults[eTHOnly][ePedestal] << "\n";
    cout << (mResults[eTHOnly][eMeanAngle]/degree) << "\t"
	 << (mResults[eTHOnly][eSigmaAngle]/degree) << "\t"
	 << (mResults[eTHOnly][eMostProbableAngle]/degree) << "\t"
	 << (mResults[eTHOnly][eBestAngle]/degree) << "\t"
	 << (mResults[eTHOnly][eTruncatedAngle]/degree) << "\t"
	 << (mResults[eTHOnly][eNumberOfEntries]) << endl;
    
    //
    // PHI CUT ONLY
    //
    cout << "T/H= "      << mThreshold       << "(0)"
	 << "phi= "      << (mPhi/degree)    << "(1)"
	 << "Pedestal= " << mResults[ePhiOnly][ePedestal] << "\n";
    cout << (mResults[ePhiOnly][eMeanAngle]/degree) << "\t"
	 << (mResults[ePhiOnly][eSigmaAngle]/degree) << "\t"
	 << (mResults[ePhiOnly][eMostProbableAngle]/degree) << "\t"
	 << (mResults[ePhiOnly][eBestAngle]/degree) << "\t"
	 << (mResults[ePhiOnly][eTruncatedAngle]/degree) << "\t"
	 << (mResults[ePhiOnly][eNumberOfEntries]) << endl;

    //
    // PHI & THRESHOLD CUT ONLY
    //
    cout << "T/H= "       << mThreshold       << "(1)"
	 << " phi= "      << (mPhi/degree)    << "(1)"
	 << " Pedestal= " << mResults[eTHPhi][ePedestal] << "\n";
    cout << (mResults[eTHPhi][eMeanAngle]/degree) << "\t"
	 << (mResults[eTHPhi][eSigmaAngle]/degree) << "\t"
	 << (mResults[eTHPhi][eMostProbableAngle]/degree) << "\t"
	 << (mResults[eTHPhi][eBestAngle]/degree) << "\t"
	 << (mResults[eTHPhi][eTruncatedAngle]/degree) << "\t"
	 << (mResults[eTHPhi][eNumberOfEntries]) << endl;


    cout.precision(6);
    cout.width(0);
    cout << "===========================================" << endl;
}
