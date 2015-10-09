/***************************************************************************
 *
 * $Id: StMtdHit.cxx,v 2.3 2015/10/09 17:46:14 ullrich Exp $
 *
 * Author: Frank Geurts, April 25, 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMtdHit.cxx,v $
 * Revision 2.3  2015/10/09 17:46:14  ullrich
 * Changed type of mIdTruth from ushort to int.
 *
 * Revision 2.2  2012/02/28 01:24:51  perev
 * tof() implementation added
 *
 * Revision 2.1  2011/04/25 21:24:02  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StMtdHit.h"
#include "StTrack.h"

ClassImp(StMtdHit)

StMtdHit::StMtdHit()
{
    mBackLeg          = 0;
    mModule           = 0;
    mCell             = 0;
    mLeadingEdgeTime.first  = 0.;
    mTrailingEdgeTime.second = 0.;
    mLeadingEdgeTime.first  = 0.;
    mTrailingEdgeTime.second = 0.;

    mAssociatedTrack  = 0;
    mIdTruth          = 0;
    mQuality          = 0;
}

StMtdHit::~StMtdHit() {/* noop */}

int
StMtdHit::backleg() const { return mBackLeg; }

int
StMtdHit::module() const { return mModule; }

int
StMtdHit::cell() const { return mCell; }

pair<double,double>
StMtdHit::leadingEdgeTime() const { return mLeadingEdgeTime; }

pair<double,double>
StMtdHit::trailingEdgeTime() const { return mTrailingEdgeTime; }

pair<double,double>
StMtdHit::tot() const { return pair<double,double>(mTrailingEdgeTime.first - mLeadingEdgeTime.first, mTrailingEdgeTime.second - mLeadingEdgeTime.second); }

double StMtdHit::tof() const { return 0.5*(mLeadingEdgeTime.first+mLeadingEdgeTime.second); }

StTrack*
StMtdHit::associatedTrack() { return mAssociatedTrack; }

const StTrack*
StMtdHit::associatedTrack() const { return mAssociatedTrack; }

int
StMtdHit::idTruth() const { return mIdTruth; }

int
StMtdHit::qaTruth() const { return mQuality; }

void
StMtdHit::setBackleg(unsigned char backlegId) { mBackLeg = backlegId; }

void
StMtdHit::setModule(unsigned char moduleId) { mModule = moduleId; }

void
StMtdHit::setCell(unsigned char cellId) { mCell = cellId; }

void
StMtdHit::setLeadingEdgeTime(pair<double,double> time) { mLeadingEdgeTime = time; }

void
StMtdHit::setTrailingEdgeTime(pair<double,double> time) { mTrailingEdgeTime = time; }

void
StMtdHit::setAssociatedTrack(StTrack* val) { mAssociatedTrack = val; }

void
StMtdHit::setIdTruth(int idtru,int qatru)
{
    if (qatru==0) qatru = (idtru>>16);
    idtru    = idtru&((1<<16)-1);
    mIdTruth = idtru;
    mQuality = static_cast<UShort_t>(qatru);
}

ostream&
operator<<(ostream &os, const StMtdHit& hit)
{
    os << " Backleg:" << hit.backleg() << "  Module:" << hit.module()
       << " Cell:" << hit.cell() << endl
       << " LeTime " << hit.leadingEdgeTime().first << " " << hit.leadingEdgeTime().second
       << " TeTime " << hit.trailingEdgeTime().first << " " << hit.trailingEdgeTime().second << endl
       << " Track " << (hit.associatedTrack() ? hit.associatedTrack()->key() : 0) << endl
       << " IdTruth " << hit.idTruth() << " Quality " << hit.qaTruth() << endl;
    return os;
}
