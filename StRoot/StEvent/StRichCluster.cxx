/***************************************************************************
 *
 * $Id: StRichCluster.cxx,v 2.1 2000/05/22 21:44:53 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description: Implementation of Cluster definition
 *
 ***************************************************************************
 *
 * $Log: StRichCluster.cxx,v $
 * Revision 2.1  2000/05/22 21:44:53  ullrich
 * Initial Revision
 *
 * Revision 2.1  2000/05/22 21:44:53  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StRichCluster.h"

static const char rcsid[] = "$Id: StRichCluster.cxx,v 2.1 2000/05/22 21:44:53 ullrich Exp $";

StRichCluster::StRichCluster()
    : mNumberOfPads(0), mNumberOfLocalMax(0), mFirstPad(0),
      mAmplitudeSum(0), mAmplitude2Sum(0) {/* noop */}

StRichCluster::StRichCluster(Int_t nPads, Int_t nLocMax, Int_t fPad, Float_t ampSum, Float_t amp2Sum, Float_t rms2)
    :  mNumberOfPads(nPads), mNumberOfLocalMax(nLocMax), mFirstPad(fPad),
       mAmplitudeSum(ampSum), mAmplitude2Sum(amp2Sum), mRms2(rms2) {/* noop */}

StRichCluster::~StRichCluster() {/* noop */}

Int_t
StRichCluster::operator==(const StRichCluster& cluster) const 
{
   return ( mNumberOfPads == cluster.mNumberOfPads   &&
	    mFirstPad == cluster.mFirstPad &&
            mNumberOfLocalMax == cluster.mNumberOfLocalMax &&
	    mMinimumAmplitudeOfLocalMax == cluster.mMinimumAmplitudeOfLocalMax &&
            mAmplitudeSum == cluster.mAmplitudeSum  &&
	    mRms2 == cluster.mRms2 );
}
