#ifndef StAngleCorrFunction_HH
#define StAngleCorrFunction_HH

///////////////////////////////////////////////////////////////////////////////
//
// StAngleCorrFunction
//
// Description: 
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Craig Ogilvie, MIT
//  Matt Horsley,  YALE
// History:
//
///////////////////////////////////////////////////////////////////////////////

#include "StTrackForPool.h"
#include "TString.h"
#include "StEvent.h"
#include <TH1.h>


class StAngleCorrFunction {

public:
                         StAngleCorrFunction();
virtual              ~StAngleCorrFunction();
virtual TString GetName();
virtual void      Fill(StTrackForPool* t1, StTrackForPool* t2, TH1D* hist);
virtual double GetCorr(StTrackForPool* t1, StTrackForPool* t2);

private:
virtual double func(StTrackForPool* t1, StTrackForPool* t2);

Double_t correlation,weight;
Double_t p1,p2;
Double_t px1,py1,pz1;
Double_t px2,py2,pz2;
  
};

#endif
