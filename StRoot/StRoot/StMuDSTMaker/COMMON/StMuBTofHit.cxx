/***************************************************************************
 *
 * $Id: StMuBTofHit.cxx,v 1.1 2009/02/20 17:05:59 tone421 Exp $
 *
 * Author: Xin Dong, Feb. 2009
 *
 ***************************************************************************
 *
 * Description: Tof Hit structure in MuDst
 *
 ***************************************************************************
 *
 * $Log: StMuBTofHit.cxx,v $
 * Revision 1.1  2009/02/20 17:05:59  tone421
 * *** empty log message ***
 *
 *
 ***************************************************************************/
#include "StTrack.h"
#include "StBTofHit.h"
#include "StMuBTofHit.h"

ClassImp(StMuBTofHit)

/// constructor
StMuBTofHit::StMuBTofHit()
{
  mTray   = 0;
  mModule = 0;
  mCell   = 0;

  mLeadingEdgeTime   = 0.0;
  mTrailingEdgeTime  = 0.0;
  mAssociatedTrackId = -1;
  mIndex2Primary = -1;
  mIndex2Global  = -1;
  mIdTruth       = 0;
  mQuality       = 0;
}

/// constructor from StBTofHit
StMuBTofHit::StMuBTofHit(const StBTofHit* tofHit)
{
  mTray   = tofHit->tray();
  mModule = tofHit->module();
  mCell   = tofHit->cell();

  mLeadingEdgeTime   = tofHit->leadingEdgeTime();
  mTrailingEdgeTime  = tofHit->trailingEdgeTime();;
  mAssociatedTrackId = (tofHit->associatedTrack()) ? tofHit->associatedTrack()->key() : -1;
  mIndex2Primary = -1;
  mIndex2Global  = -1;
  mIdTruth       = tofHit->idTruth();
  mQuality       = tofHit->qaTruth();
}

StMuBTofHit::~StMuBTofHit()
{}

void
StMuBTofHit::setIdTruth(int idtru,int qatru)
{
    if (qatru==0) qatru = (idtru>>16);
    idtru    = idtru&((1<<16)-1);
    mIdTruth = static_cast<UShort_t>(idtru);
    mQuality = static_cast<UShort_t>(qatru);
}
