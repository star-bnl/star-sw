/***************************************************************************
 *
 * $Id: StRichSimpleCluster.cxx,v 1.1 2000/05/18 11:34:21 lasiuk Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Implementation of Cluster definition
 *
 ***************************************************************************
 *
 * $Log: StRichSimpleCluster.cxx,v $
 * Revision 1.1  2000/05/18 11:34:21  lasiuk
 * iRename revision
 *
 * Revision 1.1  2000/04/05 16:39:20  lasiuk
 * Initial Revision
 *
 **************************************************************************/

#include "StRichSimpleCluster.h"

StRichSimpleCluster::StRichSimpleCluster()
    : mNumberOfPads(0), mNumberOfLocalMax(0), mFirstPad(0),
      mAmplitudeSum(0), mAmplitude2Sum(0) {/* nopt */}

StRichSimpleCluster::~StRichSimpleCluster() {/* nopt */}

bool StRichSimpleCluster::operator==(const StRichSimpleCluster& cluster) const 
{
   return ( mNumberOfPads == cluster.mNumberOfPads   &&
	    mFirstPad     == cluster.mFirstPad &&
            mNumberOfLocalMax           == cluster.mNumberOfLocalMax &&
	    mMinimumAmplitudeOfLocalMax == cluster.mMinimumAmplitudeOfLocalMax &&
            mAmplitudeSum == cluster.mAmplitudeSum  &&
	    mRms2         == cluster.mRms2 );
}
