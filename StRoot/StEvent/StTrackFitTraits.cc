/***************************************************************************
 *
 * $Id: StTrackFitTraits.cc,v 1.1 1999/01/15 20:40:13 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackFitTraits.cc,v $
 * Revision 1.1  1999/01/15 20:40:13  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#include "StTrackFitTraits.hh"

StTrackFitTraits::StTrackFitTraits()
{
    mCovariantMatrix = StMatrix<float>(5,5,0);
    mChiSquaredInXY = 0;              
    mChiSquaredInPlaneZ = 0;          
    mDegreesOfFreedom = 0;            
    mNumberOfFitPoints = 0;           
    mNumberOfPossiblePoints = 0;      
    mQualityBitmap = 0;                  
}

StTrackFitTraits::~StTrackFitTraits() { /* noop */ }

void StTrackFitTraits::setCovariantMatrix(const StMatrix<float>& val) { mCovariantMatrix = val; }         

void StTrackFitTraits::setChiSquaredInXY(float val) { mChiSquaredInXY = val; }          

void StTrackFitTraits::setChiSquaredInPlaneZ(float val) { mChiSquaredInPlaneZ = val; }      

void StTrackFitTraits::setDegreesOfFreedom(short val) { mDegreesOfFreedom = val; }        

void StTrackFitTraits::setNumberOfFitPoints(short val) { mNumberOfFitPoints = val; }       

void StTrackFitTraits::setNumberOfPossiblePoints(short val) { mNumberOfPossiblePoints = val; }  

void StTrackFitTraits::setQualityBitmap(short val) { mQualityBitmap = val; }           
