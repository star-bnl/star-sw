/***************************************************************************
 *
 * $Id: StTrackFitTraits.h,v 2.1 1999/10/28 22:27:35 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackFitTraits.h,v $
 * Revision 2.1  1999/10/28 22:27:35  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/28 22:27:35  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:43:02  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StTrackFitTraits_hh
#define StTrackFitTraits_hh
#include "StObject.h"
#include "StEnumerations.h"
#include "StMatrixF.hh"

class dst_track_st;
class StParticleDefinition;

class StTrackFitTraits : public StObject {
public:
    StTrackFitTraits();
    StTrackFitTraits(UShort_t, UShort_t, Float_t[2], Float_t[15]);
    StTrackFitTraits(const dst_track_st&);
    // StTrackFitTraits(const StTrackFitTraits&);            use default
    // StTrackFitTraits& operator=(const StTrackFitTraits&); use default
    virtual ~StTrackFitTraits();

    UShort_t              numberOfFitPoints() const;
    StParticleDefinition* pidHypothesis() const;
    StMatrixF             covariantMatrix() const;
    Double_t              chi2(UInt_t = 0) const;
        
protected:
    UShort_t mPidHypothesis;       // GeantId
    UShort_t mNumberOfFitPoints;
    Float_t  mChi2[2];
    Float_t  mCovariantMatrix[15];
    
    ClassDef(StTrackFitTraits,1)
};
#endif
