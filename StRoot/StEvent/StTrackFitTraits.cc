/***************************************************************************
 *
 * $Id: StTrackFitTraits.cc,v 1.2 1999/01/15 22:54:05 wenaus Exp $
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
 * $Log: StTrackFitTraits.cc,v $
 * Revision 1.2  1999/01/15 22:54:05  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StTrackFitTraits.hh"

static const char rcsid[] = "$Id: StTrackFitTraits.cc,v 1.2 1999/01/15 22:54:05 wenaus Exp $";

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

StTrackFitTraits::StTrackFitTraits(dst_track_st* trk)
{
    mDegreesOfFreedom = trk->ndegf;
    mQualityBitmap = trk->iflag;
    mNumberOfFitPoints = trk->n_fit_point;
    mNumberOfPossiblePoints = trk->n_max_point;
    mChiSquaredInXY = trk->chisq[0];
    mChiSquaredInPlaneZ = trk->chisq[1];
    mCovariantMatrix = StMatrix<float>(5,5,0);
    int i; for (i=0; i<5; i++) {
      mCovariantMatrix[i][i] = trk->covar_diag[i];
    }
}

StTrackFitTraits::~StTrackFitTraits() { /* noop */ }

void StTrackFitTraits::setCovariantMatrix(const StMatrix<float>& val) { mCovariantMatrix = val; }         

void StTrackFitTraits::setChiSquaredInXY(float val) { mChiSquaredInXY = val; }          

void StTrackFitTraits::setChiSquaredInPlaneZ(float val) { mChiSquaredInPlaneZ = val; }      

void StTrackFitTraits::setDegreesOfFreedom(short val) { mDegreesOfFreedom = val; }        

void StTrackFitTraits::setNumberOfFitPoints(short val) { mNumberOfFitPoints = val; }       

void StTrackFitTraits::setNumberOfPossiblePoints(short val) { mNumberOfPossiblePoints = val; }  

void StTrackFitTraits::setQualityBitmap(short val) { mQualityBitmap = val; }           
