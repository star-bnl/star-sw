/***************************************************************************
 *
 * $Id: StTrackFitTraits.cxx,v 1.1 1999/01/30 03:58:08 fisyak Exp $
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
 * $Log: StTrackFitTraits.cxx,v $
 * Revision 1.1  1999/01/30 03:58:08  fisyak
 * Root Version of StEvent
 *
 * Revision 1.4  1999/04/28 22:27:37  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:54:05  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.4  2000/01/20 14:43:39  ullrich
 *
#ifdef __ROOT__
 * Revision 2.2  1999/11/01 12:45:14  ullrich
static const Char_t rcsid[] = "$Id: StTrackFitTraits.cxx,v 1.1 1999/01/30 03:58:08 fisyak Exp $";
#endif
#include "tables/dst_track.h"
ClassImp(StTrackFitTraits)
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:59  ullrich
    mCovariantMatrix = StMatrixF(5,5,0);
    mChiSquaredInXY = 0;              
    mChiSquaredInPlaneZ = 0;          
    mDegreesOfFreedom = 0;            
    mNumberOfFitPoints = 0;           
    mNumberOfPossiblePoints = 0;      
    mQualityBitmap = 0;                  
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
StTrackFitTraits::StTrackFitTraits(dst_track_st* trk)
#endif
    mDegreesOfFreedom = trk->ndegf;
    mQualityBitmap = trk->iflag;
    mNumberOfFitPoints = trk->n_fit_point;
    mNumberOfPossiblePoints = trk->n_max_point;
    mChiSquaredInXY = trk->chisq[0];
    mChiSquaredInPlaneZ = trk->chisq[1];
    mCovariantMatrix = StMatrixF(5,5,0);
    Int_t i; for (i=0; i<5; i++) {
      mCovariantMatrix[i][i] = trk->covar_diag[i];
    }

StTrackFitTraits::StTrackFitTraits()
StTrackFitTraits::~StTrackFitTraits() { /* noop */ }
{
void StTrackFitTraits::setCovariantMatrix(const StMatrixF& val) { mCovariantMatrix = val; }         
    mNumberOfFitPoints = t.n_fit_point;
void StTrackFitTraits::setChiSquaredInXY(Float_t val) { mChiSquaredInXY = val; }          
}
void StTrackFitTraits::setChiSquaredInPlaneZ(Float_t val) { mChiSquaredInPlaneZ = val; }      
	return mNumberOfFitPoints;
void StTrackFitTraits::setDegreesOfFreedom(Short_t val) { mDegreesOfFreedom = val; }        

void StTrackFitTraits::setNumberOfFitPoints(Short_t val) { mNumberOfFitPoints = val; }       

void StTrackFitTraits::setNumberOfPossiblePoints(Short_t val) { mNumberOfPossiblePoints = val; }  
    m(1,3) = m(3,1) = mCovariantMatrix[2];
void StTrackFitTraits::setQualityBitmap(Short_t val) { mQualityBitmap = val; }           
    m(1,4) = m(4,1) = mCovariantMatrix[3];
    m(1,5) = m(5,1) = mCovariantMatrix[4];
    m(2,2) = mCovariantMatrix[5];
    m(2,3) = m(3,2) = mCovariantMatrix[6];
    m(2,4) = m(4,2) = mCovariantMatrix[7];
    m(2,5) = m(5,2) = mCovariantMatrix[8];
    m(3,3) = mCovariantMatrix[9];
    m(3,4) = m(4,3) = mCovariantMatrix[10];
    m(3,5) = m(5,3) = mCovariantMatrix[11];
    m(4,4) = mCovariantMatrix[12];
    m(4,5) = m(5,4) = mCovariantMatrix[13];
    m(5,5) = mCovariantMatrix[14];
    return m;
}

