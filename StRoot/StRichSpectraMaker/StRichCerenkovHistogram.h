/***************************************************************************
 *
 * $Id: StRichCerenkovHistogram.h,v 1.1 2001/08/21 17:58:33 lasiuk Exp $
 *
 * Author:  bl Mar 2, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              for use in Cerenkov Ray Tracing Algorithm
 ***************************************************************************
 *
 * $Log: StRichCerenkovHistogram.h,v $
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

#include "StRichCerenkovPhoton.h"

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
enum StRichHistoCuts {eNoCuts=0,
		      eTHOnly=1,
		      ePhiOnly=2,
		      eTHPhi=3,
		      eNumberOfHistoCuts=4};

enum StRichAngle {eMeanAngle=0,
		  eSigmaAngle=1,
		  eMostProbableAngle=2,
		  eTruncatedAngle=3,
		  eBestAngle=4,
		  ePedestal=5,
		  eNumberOfEntries=6};

class StRichCerenkovHistogram {

public:
    StRichCerenkovHistogram(int bins=50);
    ~StRichCerenkovHistogram();

    //StRichCerenkovHistogram(const StRichCerenkovHistogram&){/* nopt */}    
    //operator =(StRichCerenkovHistogram(const StRichCerenkovHistoram&) {/* nopt */}
    
    bool addEntry(StRichCerenkovPhoton);
    void clearData();

    int    numberOfBins()           const;
    int    rawEntries(int thresh=0) const;
    int    binEntries(size_t)       const;
    int    numberOfEntries()        const;

    //
    // functions that manuipulate the effective counts
    // in the histogram bins
    //
    double phi()             const;
    void   setPhi(double);
    bool   thresholdCut()    const;
    void   setThreshold(int);

    void   doPhiCut(bool);
    void   doThresholdCut(bool);
    void   doCuts(bool thresh = false, bool phi = false);
    

    //
    // Calculator
    //
    void   calculate();
    double mean()         const;
    double secondMoment() const;
    double sigma()        const;
    double rms()          const;
    double mostProbable() const;

    double bestAngle();
    double truncatedAngle();

    //
    // these are the interface to be called
    //
    void   status();
    double cerenkovAngle(unsigned short*); 
    double cerenkovSigma()        const;
    double cerenkovMostProbable() const;
    
protected:
    void   init();
    double binValue(size_t) const;
    int    countPhi(int) const;
    size_t whichBin(double) const;
    
    double pedestal() const;
    double pedestalSigma() const;
    
private:
#ifndef __CINT__
    vector<vector<StRichCerenkovPhoton> >mHistogram;//!
    vector<vector<double> > mResults; //!
#endif
    short       mNumberOfBins;
    double      mBinSize;
    double      mAngleRange;

    double      mPhi;
    int         mThreshold;

    bool        mDoPhiCut;
    bool        mDoThresholdCut;
    bool        mCalculationDone;
};

inline int    StRichCerenkovHistogram::numberOfBins() const {return mNumberOfBins;}
inline void   StRichCerenkovHistogram::setPhi(double phi) { mPhi = phi; }
inline double StRichCerenkovHistogram::phi() const { return mPhi;}
inline bool   StRichCerenkovHistogram::thresholdCut() const { return mThreshold;}
inline void   StRichCerenkovHistogram::setThreshold(int thresh) { mThreshold = thresh;}
inline void   StRichCerenkovHistogram::doPhiCut(bool doit) { mDoPhiCut = doit; }
inline void   StRichCerenkovHistogram::doThresholdCut(bool doit) { mDoThresholdCut = doit; }
inline void   StRichCerenkovHistogram::doCuts(bool thresh, bool phi)
{ mDoThresholdCut = thresh; mDoPhiCut = phi; }
#endif
