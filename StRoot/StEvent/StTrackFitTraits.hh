/***************************************************************************
 *
 * $Id: StTrackFitTraits.hh,v 1.1 1999/01/15 20:40:14 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackFitTraits.hh,v $
 * Revision 1.1  1999/01/15 20:40:14  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#ifndef StTrackFitTraits_hh
#define StTrackFitTraits_hh
#include "StMatrix.hh"

class StTrackFitTraits {
public:
    StTrackFitTraits();
    ~StTrackFitTraits();
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
