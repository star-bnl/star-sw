/*******************************************************
 *
 * $Id: StPmdDetector.cxx,v 1.1 2002/08/27 12:20:38 subhasis Exp $
 *
 * Author:  Subhasis Chattopadhyay, July 2002
 *******************************************************
 *
 * Description: Base class for PMD detector
 *
 *********************************************************
 * $Log: StPmdDetector.cxx,v $
 * Revision 1.1  2002/08/27 12:20:38  subhasis
 * First version
 *
 *********************************************************/
#include "StPmdDetector.h"
#include "StPmdHit.h"
#include "StPmdModule.h"
#include "StPmdClusterCollection.h"


ClassImp(StPmdDetector)

StPmdDetector::StPmdDetector() { /* noop */ }

StPmdDetector::StPmdDetector(Int_t id, unsigned int n)
{
    mDetectorId = id;
    mNumberOfModules = n;
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
    if (mClusters) delete mClusters;
    mClusters = val;
}

