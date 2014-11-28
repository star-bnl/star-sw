/***************************************************************************
 *
 * $Id: StEmcDetector.cxx,v 2.12 2004/10/14 20:00:18 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcDetector.cxx,v $
 * Revision 2.12  2004/10/14 20:00:18  ullrich
 * Added member and methods to deal with crate status flags.
 *
 * Revision 2.11  2004/07/20 17:07:49  perev
 * Pavlinov corrs for TBrowser
 *
 * Revision 2.10  2003/10/02 22:34:05  jeromel
 * LEAK_SCOPE patch (Alex)
 *
 * Revision 2.9  2003/09/12 22:00:57  jeromel
 * Forgot the void
 *
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
#include <TBrowser.h>
#include <StAutoBrowse.h>

static const char rcsid[] = "$Id: StEmcDetector.cxx,v 2.12 2004/10/14 20:00:18 ullrich Exp $";

ClassImp(StEmcDetector)

StEmcDetector::StEmcDetector() { clear(); }

StEmcDetector::StEmcDetector(StDetectorId id, unsigned int n)
{
    clear();
    mDetectorId = id;
    mNumberOfModules = n;
    for(int i=0; i<120;i++) {
        mModules[i] = new StEmcModule();
        //StEmcModule * module = new StEmcModule();
        //this->setModule(new StEmcModule(),i);
    }
}

StEmcDetector::~StEmcDetector()
{
    for(int i=0; i<120;i++) if(mModules[i]) delete mModules[i];
    if (mClusters) delete mClusters;
}

void
StEmcDetector::clear()
{
    for(int i=0; i<120;i++) mModules[i] = 0;
    mClusters = 0;
    for(int i=0; i<mMaxNumberOfCrates; i++)
        mCrateStatusFlag[i] = crateUnknown;
}


bool
StEmcDetector::addHit(StEmcRawHit* hit)
{
    if (hit) {
        unsigned int m = hit->module();
        if (m > 0 && m <= mNumberOfModules) {
	  mModules[m-1]->hits().push_back(hit);
	  return true;
        }
    }
    return false;
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

void
StEmcDetector::printNumberOfHits() const 
{
  printf(" Detector %i : nhits %i\n", int(mDetectorId), numberOfHits()); 
  return;
}

double 
StEmcDetector::getEnergy(const int pri) const
{
    float e = 0., eM = 0.;
    for (unsigned int m=0;m<mNumberOfModules;m++) {
        if(mModules[m]->numberOfHits()==0) continue; 
        eM = mModules[m]->getEnergy();
        e += eM;
        if(pri>1) {
	  if(eM !=0 ) printf("%3i(m) : e %9.4f ", m+1, eM);
	  if(eM < 0.) printf(" !!");
	  printf("\n");
        }
    }
    
    if(pri>0) printf("det %i : energy %9.4f GeV/c \n", int(mDetectorId), e);
    
    return e;
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

StEmcCrateStatus StEmcDetector::crateStatus(int crate) const {
    if (crate > 0 && crate <= mMaxNumberOfCrates)
        return mCrateStatusFlag[crate-1];
    else 
        return crateUnknown;
}

void
StEmcDetector::setCluster(StEmcClusterCollection* val)
{
    if (mClusters) delete mClusters;
    mClusters = val;
}

void
StEmcDetector::setModule(StEmcModule* val,int IdMod)
{
    if (val) {
        if (IdMod >= 0 && IdMod < static_cast<int>(mNumberOfModules)) {
	  if (mModules[IdMod]) delete mModules[IdMod];
	  mModules[IdMod] = val;
        }
    }
}

void StEmcDetector::setCrateStatus(int crate, StEmcCrateStatus flag) {
    if (crate > 0 && crate <= mMaxNumberOfCrates)
        mCrateStatusFlag[crate-1] = flag;
}

bool  StEmcDetector::IsFolder() const
{
    if(numberOfHits()) return true;
    else               return false;
}

void
StEmcDetector::Browse(TBrowser *b)
{
    char name[10];
    for (unsigned int m=0;m<mNumberOfModules;m++) {
        if(mModules[m]->IsFolder()) {
	  // 1 var; module[01] -> mHits 
	  // StAutoBrowse::Browse(mModules[m], b); 
	  // 2 var; mHits StEmcModule
	  sprintf(name,"Module%3.3i",m+1);
	  b->Add(mModules[m],name); 
        }
    }
    StAutoBrowse::Browse(mClusters, b);
}
