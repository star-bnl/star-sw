/***************************************************************************
 *
 * $Id: StRichWindowHistogram.h,v 1.3 2003/09/02 17:58:55 perev Exp $
 *
 * Author:  bl Nov 2, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              for use in Cerenkov Ray Tracing Algorithm
 ***************************************************************************
 *
 * $Log: StRichWindowHistogram.h,v $
 * Revision 1.3  2003/09/02 17:58:55  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2002/01/12 00:10:23  lasiuk
 * debin addition; quartz cerenkov angle, tuple modification, shift
 * to 183 nm for ray tracing, no temperature effect yet
 *
 * Revision 1.1  2001/12/19 20:18:38  lasiuk
 * Changeover in algorithm of isolating the Cherenkov angle
 *
 **************************************************************************/
#ifndef StRichWindowHistogram_h
#define StRichWindowHistogram_h

#include <Stiostream.h>
#include <math.h>
#include <stdio.h>
#include <vector>
#include <utility>
#include "SystemOfUnits.h"

using namespace units;
using std::vector;
using std::pair;

class StRichWindowBin {
public:
    StRichWindowBin(double theta=0, double wt=0, double sigma=0, double symmetry=0, short number=0)
	:mAngle(theta),mWeight(wt),mSigma(sigma),mSymmetry(symmetry),mNumber(number) {}
    ~StRichWindowBin() {}

    double mAngle;
    double mWeight;
    double mSigma;
    double mSymmetry;
    short  mNumber;
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
    void clear();

    bool addEntry(StRichWindowBin);

    void setWindowSize(double);
    void setPhiCut(double);
    
    double binSize()  const;
    double phiCut()   const;

    StRichWindowBin bin(int) const;
    StRichWindowBin* maxBin() const;
    int   numberOfEntries(int threshold=0)  const;

    void process();
    void status();
    size_t size() const;
    
private:
#ifndef __CINT__
    vector<StRichWindowBin> mWindowHistogram;//!
#endif
    double mBinSize;
    double mPhiCut;

    StRichWindowBin* mMaxBin;
};
inline void StRichWindowHistogram::setWindowSize(double s) {mBinSize = s; }
inline void StRichWindowHistogram::setPhiCut(double c) {mPhiCut = c; }
inline double StRichWindowHistogram::binSize() const { return mBinSize; }
inline double StRichWindowHistogram::phiCut()  const { return mPhiCut; }
inline StRichWindowBin* StRichWindowHistogram::maxBin() const { return mMaxBin;}
inline StRichWindowBin StRichWindowHistogram::bin(int index) const { return mWindowHistogram[index];}
inline size_t  StRichWindowHistogram::size() const { return mWindowHistogram.size();}
#endif
