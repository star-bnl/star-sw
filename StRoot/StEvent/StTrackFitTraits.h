/***************************************************************************
 *
 * $Id: StTrackFitTraits.h,v 1.5 1999/04/30 13:16:29 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackFitTraits.h,v $
 * Revision 1.5  1999/04/30 13:16:29  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.5  1999/04/30 13:16:29  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:37  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.3  1999/01/30 23:03:17  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:54:07  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.1  1999/10/28 22:27:35  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
#include "tables/dst_track.h"
 * Revision 2.0  1999/10/12 18:43:02  ullrich
 *
#include "dst_track.h"
#include "StObject.h"
#include "StEnumerations.h"
#include "StMatrixF.hh"

    ~StTrackFitTraits();
    StTrackFitTraits(dst_track_st*);
    // StTrackFitTraits(const StTrackFitTraits&);   use default
    // const StTrackFitTraits & operator=(const StTrackFitTraits&);

    Short_t            numberOfFitPoints();
    Short_t            degreesOfFreedom();
    Short_t            numberOfPossiblePoints();
    StMatrixF& covariantMatrix();
    Short_t            qualityBitmap();
    Float_t            chiSquaredInXY();
    Float_t            chiSquaredInPlaneZ();

    void setCovariantMatrix(const StMatrixF&);         
    void setChiSquaredInXY(Float_t);          
    void setChiSquaredInPlaneZ(Float_t);      
    void setDegreesOfFreedom(Short_t);        
    void setNumberOfFitPoints(Short_t);       
    void setNumberOfPossiblePoints(Short_t);  
    void setQualityBitmap(Short_t);           
        
    
private:
    StMatrixF mCovariantMatrix;
    Float_t           mChiSquaredInXY;
    Float_t           mChiSquaredInPlaneZ;
    Short_t           mDegreesOfFreedom;
    Short_t           mNumberOfFitPoints;
    Short_t           mNumberOfPossiblePoints;
    Short_t           mQualityBitmap;                      
  ClassDef(StTrackFitTraits,1)  //StTrackFitTraits structure
    UShort_t mPidHypothesis;       // GeantId

inline Short_t StTrackFitTraits::numberOfFitPoints() { return mNumberOfFitPoints; }

inline Short_t StTrackFitTraits::degreesOfFreedom() { return mDegreesOfFreedom; }

inline Short_t StTrackFitTraits::numberOfPossiblePoints() { return mNumberOfPossiblePoints; }

inline StMatrixF& StTrackFitTraits::covariantMatrix() { return mCovariantMatrix; }

inline Short_t StTrackFitTraits::qualityBitmap() { return mQualityBitmap; }

inline Float_t StTrackFitTraits::chiSquaredInXY() { return mChiSquaredInXY; }

inline Float_t StTrackFitTraits::chiSquaredInPlaneZ() { return mChiSquaredInPlaneZ; }

    UShort_t mNumberOfFitPoints;
    Float_t  mChi2[2];
    Float_t  mCovariantMatrix[15];
    
    ClassDef(StTrackFitTraits,1)
};
#endif
