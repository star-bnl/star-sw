/*!
 * \class StTrackFitTraits 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StTrackFitTraits.h,v 2.8 2002/02/22 22:56:52 jeromel Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackFitTraits.h,v $
 * Revision 2.8  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.7  2001/05/04 19:50:52  perev
 * Streamer to account old ROOT2
 *
 * Revision 2.6  2001/04/05 04:00:45  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.5  2001/03/24 03:35:00  perev
 * clone() -> clone() const
 *
 * Revision 2.4  2001/03/16 21:31:42  ullrich
 * Changed version number from 1 to 2.
 *
 * Revision 2.3  2001/03/16 20:57:45  ullrich
 * Covariant matrix now stored in TArrayF.
 *
 * Revision 2.2  1999/11/01 12:45:17  ullrich
 * Modified unpacking of point counter
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
#include "TArrayF.h"

class dst_track_st;
class StParticleDefinition;

class StTrackFitTraits : public StObject {
public:
    StTrackFitTraits();
    StTrackFitTraits(unsigned short, unsigned short, float[2], float[15]);
    StTrackFitTraits(const dst_track_st&);
    // StTrackFitTraits(const StTrackFitTraits&);            use default
    // StTrackFitTraits& operator=(const StTrackFitTraits&); use default
    virtual ~StTrackFitTraits();

    unsigned short         numberOfFitPoints() const;
    unsigned short         numberOfFitPoints(StDetectorId) const;
    StParticleDefinition*  pidHypothesis() const;
    StMatrixF              covariantMatrix() const;
    double                 chi2(unsigned int = 0) const;

    void                   clearCovariantMatrix();
    
protected:
    UShort_t mPidHypothesis;       // GeantId
    UShort_t mNumberOfFitPoints;
    Float_t  mChi2[2];
    TArrayF  mCovariantMatrix;
    
    ClassDef(StTrackFitTraits,3) //!OWNSTREAMER
};
#endif
