/*******************************************************
 *
 * $Id: StPmdDetector.cxx,v 1.4 2004/06/29 17:31:41 perev Exp $
 *
 * Author:  Subhasis Chattopadhyay, July 2002
 *******************************************************
 *
 * Description: Base class for PMD detector
 *
 *********************************************************
 * $Log: StPmdDetector.cxx,v $
 * Revision 1.4  2004/06/29 17:31:41  perev
 * Zeroing in ctr added and tests for null pointers
 *
 * Revision 1.3  2003/10/14 10:16:50  subhasis
 * zeroed before delete
 *
 * Revision 1.2  2003/05/12 12:07:13  subhasis
 * Mapping added
 *
 *********************************************************/
#include "StPmdDetector.h"
#include "StPmdHit.h"
#include "StPmdModule.h"
#include "StPmdClusterCollection.h"


ClassImp(StPmdDetector)

StPmdDetector::StPmdDetector() 
{
  mDetectorId=0;
  mNumberOfModules=0;
    
  memset(mModules_NHit,0,12*sizeof(*mModules_NHit));
  memset(mModules     ,0,12*sizeof(*mModules     ));
  mClusters=0;    
}

StPmdDetector::StPmdDetector(Int_t id, unsigned int n)
{
    mClusters = 0;
    mDetectorId = id;
    mNumberOfModules = n;
    memset(mModules_NHit,0,12*sizeof(*mModules_NHit));
    for(int i=0; i<12;i++)
    {
      StPmdModule * module = new StPmdModule();
      this->setModule(module,i);
    }

}

StPmdDetector::~StPmdDetector()
{
    for(int i=0; i<12;i++) if(mModules[i]) delete mModules[i];
}

bool
StPmdDetector::addHit(StPmdHit* hit)
{
    if (hit){
      Int_t m = hit->module();
      if (m > 0 && m <= 12)
      {
              mModules[m-1]->Hits()->Add(hit);
              mModules_NHit[m-1]++;
              return kTRUE;
      }
    }
    return kFALSE;
}

Int_t
StPmdDetector::module_hit(Int_t i)  { 
  if (i > 0 && i <= 12) {
  return mModules_NHit[i-1]; }
  else return 0;
}

unsigned int
StPmdDetector::numberOfModules() const { return mNumberOfModules; }

unsigned int
StPmdDetector::numberOfHits() const
{
    unsigned int sum = 0;
    return sum;
}

StPmdModule*
StPmdDetector::module(unsigned int i) 
{
    if (i > 0 && i <= 12) return (mModules[i-1]);
    else return 0;
}

void
StPmdDetector::setModule(StPmdModule* val,int IdMod)
{
    if (val)
    {
      if (IdMod >= 0 && IdMod < static_cast<int>(mNumberOfModules))
      {
	if (mModules[IdMod]) mModules[IdMod]=0;
	mModules_NHit[IdMod]=0;
        if (mModules[IdMod]) delete mModules[IdMod];
        mModules[IdMod] = val;
      }
   }
}

StPmdClusterCollection*
StPmdDetector::cluster() {return mClusters;}

void
StPmdDetector::setCluster(StPmdClusterCollection* val)
{
//VP    if (mClusters) mClusters=0;
    delete mClusters;
    mClusters = val;
}

