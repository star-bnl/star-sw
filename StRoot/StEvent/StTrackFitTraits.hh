/***************************************************************************
 *
 * $Id: StTrackFitTraits.hh,v 1.3 1999/01/30 23:03:17 wenaus Exp $
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
 * $Log: StTrackFitTraits.hh,v $
 * Revision 1.3  1999/01/30 23:03:17  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:54:07  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StTrackFitTraits_hh
#define StTrackFitTraits_hh
#include "StMatrix.hh"
#include "tables/dst_track.h"

class StTrackFitTraits {
public:
    StTrackFitTraits();
    ~StTrackFitTraits();
    StTrackFitTraits(dst_track_st*);
    // StTrackFitTraits(const StTrackFitTraits&);   use default
    // const StTrackFitTraits & operator=(const StTrackFitTraits&);

    short            numberOfFitPoints();
    short            degreesOfFreedom();
    short            numberOfPossiblePoints();
    StMatrix<float>& covariantMatrix();
    short            qualityBitmap();
    float            chiSquaredInXY();
    float            chiSquaredInPlaneZ();

    void setCovariantMatrix(const StMatrix<float>&);         
    void setChiSquaredInXY(float);          
    void setChiSquaredInPlaneZ(float);      
    void setDegreesOfFreedom(short);        
    void setNumberOfFitPoints(short);       
    void setNumberOfPossiblePoints(short);  
    void setQualityBitmap(short);           
    
    
private:
    StMatrix<float> mCovariantMatrix;
    float           mChiSquaredInXY;
    float           mChiSquaredInPlaneZ;
    short           mDegreesOfFreedom;
    short           mNumberOfFitPoints;
    short           mNumberOfPossiblePoints;
    short           mQualityBitmap;                      
};

inline short StTrackFitTraits::numberOfFitPoints() { return mNumberOfFitPoints; }

inline short StTrackFitTraits::degreesOfFreedom() { return mDegreesOfFreedom; }

inline short StTrackFitTraits::numberOfPossiblePoints() { return mNumberOfPossiblePoints; }

inline StMatrix<float>& StTrackFitTraits::covariantMatrix() { return mCovariantMatrix; }

inline short StTrackFitTraits::qualityBitmap() { return mQualityBitmap; }

inline float StTrackFitTraits::chiSquaredInXY() { return mChiSquaredInXY; }

inline float StTrackFitTraits::chiSquaredInPlaneZ() { return mChiSquaredInPlaneZ; }

#endif
