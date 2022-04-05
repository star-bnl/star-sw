/***************************************************************************
 *
 * $Id: StRpsCollection.cxx,v 2.3 2015/10/02 19:50:50 ullrich Exp $
 *
 * Author: Thomas Ullrich, Nov 2009
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRpsCollection.cxx,v $
 * Revision 2.3  2015/10/02 19:50:50  ullrich
 * Added containers for tracks and points.
 *
 * Revision 2.2  2010/02/04 18:16:09  ullrich
 * Added new member mSiliconBunch and referring access methods.
 *
 * Revision 2.1  2009/11/23 22:18:25  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StRpsCollection.h"
#include "StRpsPlane.h"

static const char rcsid[] = "$Id: StRpsCollection.cxx,v 2.3 2015/10/02 19:50:50 ullrich Exp $";

ClassImp(StRpsCollection)

StRpsCollection::StRpsCollection()
{
    //
    // Set up all the roman pot and plane IDs
    //
    for (unsigned int i=0; i<mNumberOfRomanPots; i++)  { // Roman Pots
        mRomanPots[i].setRomanPotId(static_cast<unsigned char>(i));
        for (unsigned int j=0; j<mRomanPots[i].numberOfPlanes(); j++) { // planes
            mRomanPots[i].plane(j)->setPlaneId(static_cast<unsigned char>(j));
            mRomanPots[i].plane(j)->setRomanPotId(static_cast<unsigned char>(i));
        }
    }
    mSiliconBunch = 0;
}

StRpsCollection::~StRpsCollection() { /* no op */ }

unsigned int
StRpsCollection::numberOfRomanPots() const
{
    return mNumberOfRomanPots;
}

const StRpsRomanPot*
StRpsCollection::romanPot(unsigned int i) const
{
    if (i < mNumberOfRomanPots)
        return &mRomanPots[i];
    else
        return 0;
}

StRpsRomanPot*
StRpsCollection::romanPot(unsigned int i)
{
    if (i < mNumberOfRomanPots)
        return &mRomanPots[i];
    else
        return 0;
}

StPtrVecRpsCluster
StRpsCollection::clusters() const
{
    StPtrVecRpsCluster vec;
    for (unsigned int i=0; i<mNumberOfRomanPots; i++) {
        const StRpsRomanPot *seq = &mRomanPots[i];
        for (unsigned int j=0; j<seq->numberOfPlanes(); j++) {
            const StRpsPlane *plane = seq->plane(j);
            for (unsigned int k=0; k<plane->numberOfClusters(); k++)
                vec.push_back(plane->cluster(k));
        }
    }
    return vec;
}

StPtrVecRpsTrackPoint StRpsCollection::trackPoints() const {
    StPtrVecRpsTrackPoint trackPointsVec;
    for (unsigned int i=0; i<mTrackPoints.size(); ++i) {
        trackPointsVec.push_back( mTrackPoints[i] );
    }
    return trackPointsVec;
}

StPtrVecRpsTrack StRpsCollection::tracks() const {
    StPtrVecRpsTrack tracksVec;
    for(unsigned int i=0; i<mTracks.size(); ++i){
        tracksVec.push_back( mTracks[i] );
    }
    return tracksVec;
}

unsigned char
StRpsCollection::siliconBunch() const
{
    return mSiliconBunch;
}

void
StRpsCollection::setSiliconBunch(unsigned char val)
{
    mSiliconBunch = val;
}

