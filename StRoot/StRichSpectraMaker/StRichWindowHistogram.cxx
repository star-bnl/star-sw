/***************************************************************************
 *
 * $Id: StRichWindowHistogram.cxx,v 1.2 2002/01/12 00:10:24 lasiuk Exp $
 *
 * Author:  bl Nov 2, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              for use in Cerenkov Ray Tracing Algorithm
 ***************************************************************************
 *
 * $Log: StRichWindowHistogram.cxx,v $
 * Revision 1.2  2002/01/12 00:10:24  lasiuk
 * debin addition; quartz cerenkov angle, tuple modification, shift
 * to 183 nm for ray tracing, no temperature effect yet
 *
 * Revision 1.1  2001/12/19 20:18:38  lasiuk
 * Changeover in algorithm of isolating the Cherenkov angle
 *
 **************************************************************************/
#include "StRichWindowHistogram.h"

#include "StGlobals.hh"
#include "SystemOfUnits.h"

#include <numeric>
#include <algorithm>

#ifndef ST_NO_NAMESPACES
using namespace units;
using std::accumulate;
using std::max_element;
#endif

ostream& operator<<(ostream& os, const StRichWindowBin& win) {
    return (os << (win.mAngle/degree) << '\t' << (win.mNumber) << '\t' << win.mWeight << '\t' << (win.mSigma/degree) << '\t' << (win.mSymmetry/degree));
}

StRichWindowHistogram::StRichWindowHistogram(double binSize, double phiCut)
    : mBinSize(binSize), mPhiCut(phiCut)
{
    this->init();
}

// ----------------------------------------------------
StRichWindowHistogram::~StRichWindowHistogram()
{
    this->clear();
}

// ----------------------------------------------------
void StRichWindowHistogram::init()
{
//     cout << "StRichWindowHistogram::init()" << endl;

    this->clear();    
}

// ----------------------------------------------------
void StRichWindowHistogram::clear()
{
//     cout << "StRichWindowHistogram::clear()" << endl;
    mMaxBin=0;
    
    mWindowHistogram.clear();
}


// ----------------------------------------------------
bool StRichWindowHistogram::addEntry(StRichWindowBin entry)
{
//     cout << "StRichWindowHistogram::addEntry()" << endl;
    mWindowHistogram.push_back(entry);

    return true;
}

// ----------------------------------------------------
int StRichWindowHistogram::numberOfEntries(int threshold) const
{
    cout << "StRichWindowHistogram::numberOfEntries()" << endl;

    int count = 0;
    
    for(size_t ii=0; ii<mWindowHistogram.size(); ii++) {

	if(mWindowHistogram[ii].mWeight >= threshold)
	    count++;
    }
    return count;
}

// ----------------------------------------------------
void StRichWindowHistogram::process() {

    //
    // The values of the bins are available here
    // First: sort them
    //
    cout << "StRichWindowHistogram::process() " << endl;
    size_t ii;
    
    sort(mWindowHistogram.begin(),mWindowHistogram.end(),weight());
    for(ii=0; ii<mWindowHistogram.size(); ii++) {
	// is angle consistent with D, prediction and momentum
	mMaxBin = &mWindowHistogram[ii];
	break;
    }
//     PR(mMaxBin);
//     PR(*mMaxBin);
//     PR(this->maxBin());
}

// ----------------------------------------------------
void StRichWindowHistogram::status() {

    cout << "========StRichWindowHistogram::status()==========" << endl;
    cout << "Bin Size= " << (this->binSize()/milliradian)
	 << "(" << this->binSize()/degree << ")"
	 << " mrad  PhiCut= " << (this->phiCut()/degree) << endl;
    cout << "-------------------------------------------------" << endl;
    cout << "Angle  Number  Weight   Sigma    Symmetry" << endl;
    cout << "-------------------------------------------------" << endl;
//     cout.width(8);
    cout.precision(4);
    for(size_t ii=0; ii<mWindowHistogram.size(); ii++) {
	cout << mWindowHistogram[ii] << endl;
    }

    if(mMaxBin)
	cout << "\tAll: " << (this->maxBin()->mAngle/degree) << endl;

    cout.precision(6);
    cout.width(0);
    cout << "===========================================" << endl;
}
