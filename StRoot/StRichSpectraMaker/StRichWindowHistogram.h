/***************************************************************************
 *
 * $Id: StRichWindowHistogram.h,v 1.1 2001/12/19 20:18:38 lasiuk Exp $
 *
 * Author:  bl Nov 2, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              for use in Cerenkov Ray Tracing Algorithm
 ***************************************************************************
 *
 * $Log: StRichWindowHistogram.h,v $
 * Revision 1.1  2001/12/19 20:18:38  lasiuk
 * Changeover in algorithm of isolating the Cherenkov angle
 *
 **************************************************************************/
#ifndef StRichWindowHistogram_h
#define StRichWindowHistogram_h

#include <iostream.h>
#include <math.h>
#include <stdio.h>
#include <vector>
#include <utility>
#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using units;
using std::vector;
using std::pair;
#endif

class StRichWindowBin {
public:
    StRichWindowBin(double theta=0, double wt=0, double sigma=0)
	:mAngle(theta),mWeight(wt),mSigma(sigma) {}
    ~StRichWindowBin() {}

    double mAngle;
    double mWeight;
    double mSigma;
};
ostream& operator<<(ostream& os, const StRichWindowBin& win);

class weight {
public:
    bool operator()(const StRichWindowBin& a, const StRichWindowBin& b) const
    {
	return(a.mWeight>b.mWeight);
    }
};


class StRichWindowHistogram {
public:
    StRichWindowHistogram(double binSize=0, double phiCut=0);
    ~StRichWindowHistogram();

    //StRichWindowHistogram(const StRichWindowHistogram&){/* nopt */}    
    //operator =(StRichWindowHistogram(const StRichWindowHistogram&) {/* nopt */}

    void init();
    void clearData();

    bool addEntry(StRichWindowBin);

    double binSize()  const;
    double phiCut()   const;

    StRichWindowBin bin(int) const;
    StRichWindowBin* maxBin() const;
    int   numberOfEntries(int threshold=0)  const;

    void process();
    void status();
    
private:
#ifndef __CINT__
    vector<StRichWindowBin> mWindowHistogram;//!
#endif
    double mBinSize;
    double mPhiCut;

    StRichWindowBin* mMaxBin;
};
inline double StRichWindowHistogram::binSize() const { return mBinSize; }
inline double StRichWindowHistogram::phiCut()  const { return mPhiCut; }
inline StRichWindowBin* StRichWindowHistogram::maxBin() const { return mMaxBin;}
inline StRichWindowBin StRichWindowHistogram::bin(int index) const { return mWindowHistogram[index];}
#endif
