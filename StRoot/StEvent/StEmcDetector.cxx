/***************************************************************************
 *
 * $Id: StEmcDetector.cxx,v 2.8 2003/09/12 21:54:53 jeromel Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcDetector.cxx,v $
 * Revision 2.8  2003/09/12 21:54:53  jeromel
 * Zeroing
 *
 * Revision 2.7  2001/04/05 04:00:48  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.6  2000/12/08 03:39:50  ullrich
 * Fixed warning because of signed/unsigned comparison.
 *
 * Revision 2.5  2000/10/26 00:02:21  ullrich
 * Fixed various problems causing I/O failures.
 *
 * Revision 2.4  2000/07/28 19:49:27  akio
 * Change in Detector Id for Endcap SMD
 *
 * Revision 2.3  2000/06/30 17:23:19  akio
 * minor bug fix for return kFalse
 *
 * Revision 2.2  2000/05/22 19:21:53  akio
 * Bug fix, add delta into EMcPoint, wider bits for Eta in RawHit
 *
 * Revision 2.1  2000/02/23 17:34:08  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcDetector.h"
#include "StEmcRawHit.h"
#include "StEmcModule.h"
#include "StEmcClusterCollection.h"

static const char rcsid[] = "$Id: StEmcDetector.cxx,v 2.8 2003/09/12 21:54:53 jeromel Exp $";

ClassImp(StEmcDetector)

StEmcDetector::StEmcDetector() { 
  Zero();
}

StEmcDetector::StEmcDetector(StDetectorId id, unsigned int n)
{
  Zero();
  mDetectorId = id;
  mNumberOfModules = n;
  for(int i=0; i<120;i++)
    {
      StEmcModule * module = new StEmcModule();
      this->setModule(module,i);
    }
}

StEmcDetector::~StEmcDetector()
{
    for(int i=0; i<120;i++) if(mModules[i]) delete mModules[i];
    if (mClusters) delete mClusters;
}

StEmcDetector::Zero()
{
  for(int i=0; i<120;i++) mModules[i] = 0;
  mClusters = 0;
}




bool
StEmcDetector::addHit(StEmcRawHit* hit)
{
    if (hit){
      unsigned int m = hit->module();
      if (m > 0 && m <= mNumberOfModules)
      {
              mModules[m-1]->hits().push_back(hit);
              return kTRUE;
      }
    }
    return kFALSE;
}

StDetectorId
StEmcDetector::detectorId() const { return mDetectorId; }

unsigned int
StEmcDetector::numberOfModules() const { return mNumberOfModules; }

unsigned int
StEmcDetector::numberOfHits() const
{
    unsigned int sum = 0;
    for (unsigned int m=0;m<mNumberOfModules;m++) sum+= mModules[m]->hits().size();
    return sum;
}

StEmcModule*
StEmcDetector::module(unsigned int i)
{
    if (i > 0 && i <= mNumberOfModules) return (mModules[i-1]);
    else return 0;
}

const StEmcModule*
StEmcDetector::module(unsigned int i) const
{
    if (i > 0 && i <= mNumberOfModules) return (mModules[i-1]);
    else return 0;
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

void
StEmcDetector::setModule(StEmcModule* val,int IdMod)
{
    if (val)
    {
      if (IdMod >= 0 && IdMod < static_cast<int>(mNumberOfModules))
      {
        if (mModules[IdMod]) delete mModules[IdMod];
        mModules[IdMod] = val;
      }
   }
}
