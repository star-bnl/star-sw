/***************************************************************************
 *
 * $Id: StRichCerenkovHistogram.h,v 1.2 2001/12/19 20:18:38 lasiuk Exp $
 *
 * Author:  bl Mar 2, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              for use in Cerenkov Ray Tracing Algorithm
 * old version in ~/test
 ***************************************************************************
 *
 * $Log: StRichCerenkovHistogram.h,v $
 * Revision 1.2  2001/12/19 20:18:38  lasiuk
 * Changeover in algorithm of isolating the Cherenkov angle
 *
 * Revision 1.1  2001/08/21 17:58:33  lasiuk
 * for 2000 analysis
 *
 **************************************************************************/
#ifndef StRichCerenkovHistogram_h
#define StRichCerenkovHistogram_h

#include <iostream.h>
#include <math.h>
#include <stdio.h>
#include <vector>
#include <utility>

#include "StRichCerenkovPhoton.h"
#include "StRichWindowHistogram.h"
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::pair;
#endif

class StRichCerenkovHistogram {

public:
    StRichCerenkovHistogram();
    ~StRichCerenkovHistogram();

    //StRichCerenkovHistogram(const StRichCerenkovHistogram&){/* nopt */}    
    //operator =(StRichCerenkovHistogram(const StRichCerenkovHistoram&) {/* nopt */}
    
    bool addEntry(StRichCerenkovPhoton);
    void clearData();

    int  numberOfEntries()      const;

    //
    // functions that manuipulate the effective counts
    // in the histogram bins
    //
    double phiCut()             const;
    double windowSize()         const;
    
    void   setPhiCut(double);
    void   setWindowSize(double);


    void   doPhiCut(bool);
    
    bool   setCerenkovQuantities(pair<double,double>);
    bool   checkHypothesis();

    //
    // Calculator
    //
    void   calculate();
    void   evaluate();

    //
    // these are the interface to be called
    //
    void   status();
    double cerenkovAngle(unsigned short*);
    double numberOfPhotons()      const;
    double cerenkovSigma()        const;
    double bestAngle();

protected:
    void calculateSlidingWindow();
    double weight(double) const;

    
    double mean()         const;
    double secondMoment() const;
    double sigma()        const;
    double rms()          const;
    double mostProbable() const;


    
protected:
    void   init();
    
private:
#ifndef __CINT__
    //
    // Raw Data (INPUT)
    vector<StRichCerenkovPhoton> mHistogram;//!
    //
    // Expected Values
    vector<pair<double, double> > mCerenkovQuantities;//!
    
    vector<StRichWindowHistogram> mResults;//!
#endif
    double      mSmallestTheta;
    double      mLargestTheta;

    double      mPhiCut;
    double      mWindowSize;
    
    bool        mDoPhiCut;
    bool        mCalculationDone;

    double      mCerenkovAngle;
    double      mCerenkovSigma;
    double      mNumberOfPhotons;
};

inline int StRichCerenkovHistogram::numberOfEntries() const { return mHistogram.size(); }
inline void   StRichCerenkovHistogram::setPhiCut(double phiCut) { mPhiCut = phiCut; }
inline double StRichCerenkovHistogram::phiCut() const { return mPhiCut;}
inline void   StRichCerenkovHistogram::setWindowSize(double windowSize) { mWindowSize = windowSize; }
inline double StRichCerenkovHistogram::windowSize() const { return mWindowSize;}
inline void   StRichCerenkovHistogram::doPhiCut(bool doit) { mDoPhiCut = doit; }
inline double StRichCerenkovHistogram::weight(double value) const {cout << "histo weight" << (1./tan(value)) << endl;return 1./tan(value); }
inline double StRichCerenkovHistogram::numberOfPhotons() const { return mNumberOfPhotons; }

#endif
