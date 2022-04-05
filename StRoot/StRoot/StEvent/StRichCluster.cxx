/***************************************************************************
 *
 * $Id: StRichCluster.cxx,v 2.3 2001/04/05 04:00:52 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description: Implementation of Cluster definition
 *
 ***************************************************************************
 *
 * $Log: StRichCluster.cxx,v $
 * Revision 2.3  2001/04/05 04:00:52  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2000/08/08 14:42:10  ullrich
 * Added missing ClassDef and ClassImp macros.
 *
 * Revision 2.1  2000/05/22 21:44:53  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StRichCluster.h"

static const char rcsid[] = "$Id: StRichCluster.cxx,v 2.3 2001/04/05 04:00:52 ullrich Exp $";

ClassImp(StRichCluster)

StRichCluster::StRichCluster()
    : mNumberOfPads(0), mNumberOfLocalMax(0), mFirstPad(0),
      mAmplitudeSum(0), mAmplitude2Sum(0) {/* noop */}

StRichCluster::StRichCluster(int nPads, int nLocMax, int fPad, float ampSum, float amp2Sum, float rms2)
    :  mNumberOfPads(nPads), mNumberOfLocalMax(nLocMax), mFirstPad(fPad),
       mAmplitudeSum(ampSum), mAmplitude2Sum(amp2Sum), mRms2(rms2) {/* noop */}

StRichCluster::~StRichCluster() {/* noop */}

int
StRichCluster::operator==(const StRichCluster& cluster) const
{
   return ( mNumberOfPads == cluster.mNumberOfPads   &&
            mFirstPad == cluster.mFirstPad &&
            mNumberOfLocalMax == cluster.mNumberOfLocalMax &&
            mMinimumAmplitudeOfLocalMax == cluster.mMinimumAmplitudeOfLocalMax &&
            mAmplitudeSum == cluster.mAmplitudeSum  &&
            mRms2 == cluster.mRms2 );
}
