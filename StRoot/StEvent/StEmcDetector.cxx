/***************************************************************************
 *
 * $Id: StEmcDetector.cxx,v 2.1 2000/02/23 17:34:08 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcDetector.cxx,v $
 * Revision 2.1  2000/02/23 17:34:08  ullrich
 * Initial Revision
 *
 * Revision 2.1  2000/02/23 17:34:08  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcDetector.h"
#include "StEmcRawHit.h"
static const char rcsid[] = "$Id: StEmcDetector.cxx,v 2.1 2000/02/23 17:34:08 ullrich Exp $";
#include "StEmcClusterCollection.h"

static const char rcsid[] = "$Id: StEmcDetector.cxx,v 2.1 2000/02/23 17:34:08 ullrich Exp $";

ClassImp(StEmcDetector)

StEmcDetector::StEmcDetector() { /* noop */ }

StEmcDetector::StEmcDetector(StDetectorId id, UInt_t n)
{
    mDetectorId = id;
    mNumberOfModules = n;
    mModules = new StEmcModule[n];
}

    if (mClusters) delete [] mClusters;
{
    if (mModules)  delete [] mModules;
    if (mClusters) delete mClusters;
}

Bool_t
	UInt_t m = hit->module();
	if (m < mNumberOfModules){
	    mModules[m].hits().push_back(hit);
	    return kTRUE;
	}
	else return kFALSE;
	mModules[m-1].hits().push_back(hit);
    else return kFALSE;
      }
    }
    return kFALSE;
}

StDetectorId
StEmcDetector::detectorId() const { return mDetectorId; }

UInt_t
StEmcDetector::numberOfModules() const { return mNumberOfModules; }

UInt_t
StEmcDetector::numberOfHits() const
{
    UInt_t sum = 0;
    for (UInt_t m=0; m < mNumberOfModules; m++) {
	sum += mModules[m].hits().size();
    }
    return sum;
}

    if (i < mNumberOfModules)
        return &(mModules[i]);
{
    if (i > 0 && i <= mNumberOfModules)
        return &(mModules[i-1]);
    else
        return 0;
}

    if (i < mNumberOfModules)
        return &(mModules[i]);
{
    if (i > 0 && i <= mNumberOfModules)
        return &(mModules[i-1]);
    else
        return 0;
}

StEmcClusterCollection*
StEmcDetector::cluster() {return mClusters;}

const StEmcClusterCollection*
StEmcDetector::cluster() const {return mClusters;}

void
StEmcDetector::setCluster(StEmcClusterCollection* val)
{
    if (mClusters) delete mClusters;
    mClusters = val;
}

