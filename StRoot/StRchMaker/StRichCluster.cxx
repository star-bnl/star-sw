/***************************************************************************
 *
 * $Id: StRichCluster.cxx,v 1.1 2000/04/05 16:39:20 lasiuk Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Implementation of Cluster definition
 *
 ***************************************************************************
 *
 * $Log: StRichCluster.cxx,v $
 * Revision 1.1  2000/04/05 16:39:20  lasiuk
 * Initial Revision
 *
 * Revision 1.1  2000/04/05 16:39:20  lasiuk
 * Initial Revision
 *
 **************************************************************************/

#include "StRichCluster.h"

StRichCluster::StRichCluster()
    : mNumberOfPads(0), mNumberOfLocalMax(0), mFirstPad(0),
      mAmplitudeSum(0), mAmplitude2Sum(0) {/* nopt */}

StRichCluster::~StRichCluster() {/* nopt */}

bool StRichCluster::operator==(const StRichCluster& cluster) const 
{
   return ( mNumberOfPads == cluster.mNumberOfPads   &&
	    mFirstPad == cluster.mFirstPad &&
            mNumberOfLocalMax == cluster.mNumberOfLocalMax &&
	    mMinimumAmplitudeOfLocalMax == cluster.mMinimumAmplitudeOfLocalMax &&
            mAmplitudeSum == cluster.mAmplitudeSum  &&
	    mRms2 == cluster.mRms2 );
}
