#include "StEmcOldFinder.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "TH2.h"
#include "TString.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcPreCluster.h"
//#define TTABLESORTER
#ifdef TTABLESORTER
#include <TTableSorter.h>
#else
#include "TArrayI.h"
#include "TMath.h"
#endif
#include "Stiostream.h"
#include "StMessMgr.h"

ClassImp(StEmcOldFinder)

//_____________________________________________________________________________
StEmcOldFinder::StEmcOldFinder():StEmcVirtualFinder()
{
    Float_t seed[] = {0.7, 0.1, 0.4, 0.4};
    Float_t add
        []  =
            {
                0.001, 0.001, 0.001, 0.001
            };
    Float_t all[]  =
        {
            0.1,0.1,0.1,0.1
        };
    Int_t   size[] =
        {
            4,1,5,5
        };
    for(Int_t i = 0; i < MAXDETBARREL; i++)
    {
        mEnergySeed[i] = seed[i];
        mEnergyAdd[i] = add
                            [i];
        mEnergyThresholdAll[i] = all[i];
        mSizeMax[i] = size[i];
        if(mEnergyThresholdAll[i]<mEnergySeed[i])
            mEnergyThresholdAll[i]=mEnergySeed[i];
    }
}
//_____________________________________________________________________________
StEmcOldFinder::~StEmcOldFinder()
{}
Bool_t StEmcOldFinder::findClusters(StEvent* event)
{
    StEmcCollection *emc = event->emcCollection();
    if(!emc)
        return kFALSE;

    for(Int_t i = 0; i<MAXDETBARREL; i++)
    {
        Int_t det = i+1;
        StDetectorId id = static_cast<StDetectorId>(det-1+kBarrelEmcTowerId);
        StEmcDetector* detector=emc->detector(id);
        if(detector)
            findClustersInDetector(detector);
    }
    return kTRUE;
}
Bool_t StEmcOldFinder::findClustersInDetector(StEmcDetector* detector)
{
    if(!detector)
        return kFALSE;
    Int_t det = (Int_t)(detector->detectorId()-kBarrelEmcTowerId)+1;
    Int_t NM = detector->numberOfModules();
    for(Int_t m = 1; m<=NM; m++)
        findClustersInModule(det,detector->module(m));
    LOG_DEBUG <<"Number of clusters found for detector "<<det<<" = "<<mColl[det-1]->GetSize()<<endm;

    return kTRUE;
}
Bool_t StEmcOldFinder::findClustersInModule(Int_t det, StEmcModule* module)
{
    if(!module)
        return kFALSE;

    StSPtrVecEmcRawHit& hits=module->hits();

    mFirst = 0;
    mLast  = hits.size();
    mNH    = mLast - mFirst;
    if(mNH<=0)
        return kFALSE;

    mEnergy.Set(mNH);   // set dimensions of the arrays for this module
    mEW.Set(mNH);
    mSW.Set(mNH);
    mEnergy.Reset();   // reset arrays
    mEW.Reset();
    mSW.Reset();
    mUsed.Set(mNH);   // array with flags of mUsed hits in the module
    mUsed.Reset();

    Int_t  ih;
    Int_t  jh = mFirst;
    for(ih=mFirst; ih<mLast; ih++) // fill arrays
    {
        jh=ih-mFirst;
        Int_t cal = hits[ih]->calibrationType();
        if(cal<128) // ONLY hits with calibrationType <128 should be mUsed for cluster finding...
        {
            mEnergy[jh] = hits[ih]->energy();
            mEW[jh]=hits[ih]->eta();
            mSW[jh]=abs(hits[ih]->sub());
        }
        else
        {
            mEnergy[jh] = 0;
            mEW[jh]=hits[ih]->eta();
            mSW[jh]=abs(hits[ih]->sub());
        }
    }

    // only 1 hit in this module
    if(mNH == 1)
    {
        if(mEnergy[0] > mEnergyThresholdAll[det-1])
        {
            StEmcPreCluster *cl = mColl[det-1]->newCluster();
            cl->addHit(hits[0]);
            cl->update();
            mNHit = 0;
        }
        return kTRUE;
    }
#ifdef TTABLESORTER
    TTableSorter index(mEnergy.GetArray(), mNH); // The mLast element is biggest
#else
    TArrayI index(mNH);
    TMath::Sort(mNH,mEnergy.GetArray(),index.GetArray(),0);
#endif
    mHitsId.Set(10);
    mHitsId.Reset();
    Int_t i, ii,l;
    Int_t loop = (det==BTOW)?2:1;
    Float_t eClW=0;

    for(i=mNH-1; i>=0; i--) //loop from the more energetic hit to the less one
    {
#ifdef TTABLESORTER
        Int_t j = index.GetIndex(i); //get index of the hit
#else
	Int_t j = index[i];
#endif
        if(mEnergy[j] < mEnergySeed[det-1])
            break; //if the hit is below threshold for cluster find -> break

        if(mUsed[j] == 0) // test if this hit is not mUsed
        {
            mHitsId[0]=j; // First hit in cluster
            mNHit=1;
            mUsed[j]=1;
            mKeyEta=0;
            mKeyPhi=0;
            eClW  = mEnergy[j]; // 19-apr
            if(mSizeMax[det-1] == 1)
                goto TESTClUSTER; // cluster = hit

            for(l=0; l<loop; l++)
            {
                for(ii=i-1; ii>=0; ii--)
                {
#ifdef TTABLESORTER
                    int jj = index.GetIndex(ii);
#else
                    int jj = index[ii];
#endif
                    if(mEnergy[jj] < mEnergyAdd[det-1])
                        break; // 19-apr
                    if(mUsed[jj] == 0)
                    {
                        if(!testOnNeighbor(det, jj))
                        {
                            mUsed[jj]=1;
                            mHitsId[mNHit]=jj;
                            mNHit++;
                            eClW += mEnergy[jj]; // 19-apr
                            if(mNHit == mSizeMax[det-1])
                                goto TESTClUSTER;
                        }
                    }
                }
            }
TESTClUSTER:
            if(mNHit>0)
            {
                if(eClW > mEnergyThresholdAll[det-1])
                {
                    StEmcPreCluster *cl = mColl[det-1]->newCluster();
                    for(Int_t H = 0; H<mNHit; H++)
                        cl->addHit(hits[mHitsId[H]+mFirst]);
                    cl->update();
                    mNHit = 0;
                }
                else
                {
                    Int_t jj;
                    for(Int_t i1=0; i1<mNHit; i1++)
                    {
                        jj = mHitsId[i1];
                        mUsed[jj] = 0;
                    }
                }
            }
        }
    }
    return kTRUE;
}
Bool_t StEmcOldFinder::testOnNeighbor(Int_t det, Int_t jn)
{
    mOverlapFlag = 0;

    // find SMD-eta clusters
    if(det==BSMDE)
    {
        if(mNHit == 1)
        {
            mEtaFirst=mEW[mHitsId[0]];
            mEtaLast=mEtaFirst;
            mEnergyFirst=mEnergy[mHitsId[0]];
            mEnergyLast=mEnergyFirst;
        }

        if (mEtaFirst-mEW[jn] == 1)
        {
            if (mEnergy[jn]<mEnergyFirst)
            {
                mEtaFirst=mEW[jn];
                mEnergyFirst=mEnergy[jn];
                return kFALSE;
            }
            else
                mOverlapFlag++;

        }
        else if (mEW[jn]-mEtaLast == 1)
        {
            if (mEnergy[jn]<mEnergyLast)
            {
                mEtaLast=mEW[jn];
                mEnergyLast=mEnergy[jn];
                return kFALSE;
            }
            else
                mOverlapFlag++;
        }
        else
            return kTRUE;
    }

    // find SMD-phi clusters
    else if(det==BSMDP)
    {
        if(mNHit == 1)
        {
            mEtaSeed=mEW[mHitsId[0]];
            mPhiFirst=mSW[mHitsId[0]];
            mPhiLast=mPhiFirst;
            mEnergyFirst=mEnergy[mHitsId[0]];
            mEnergyLast=mEnergyFirst;
        }

        if(mEtaSeed == mEW[jn])  // Same eta bin
        {
            if(mPhiFirst-mSW[jn] == 1)
            {
                if (mEnergy[jn]<mEnergyFirst)
                {
                    mPhiFirst=mSW[jn];
                    mEnergyFirst=mEnergy[jn];
                    return kFALSE;
                }
                else
                    mOverlapFlag++;
            }
            else if(mSW[jn]-mPhiLast == 1)
            {
                if (mEnergy[jn]<mEnergyLast)
                {
                    mPhiLast=mSW[jn];
                    mEnergyLast=mEnergy[jn];
                    return kFALSE;
                }
                else
                    mOverlapFlag++;
            }
            else
                return kTRUE;
        }
    }

    // find tower clusters
    else
    {
        // The vector mNb can be used for further studies/checks.
        Int_t mNb[5] = {0,0,0,0,0};
        Int_t js = mHitsId[0];

        if     (mEW[js] == mEW[jn] && abs(mSW[js]-mSW[jn]) == 1 && mNb[2] == 0)
        {
            mNb[2] = 1;
            return kFALSE;
        }
        else if(mSW[js] == mSW[jn] && abs(mEW[js]-mEW[jn]) == 1)
        {
            if      ((mEW[js]-mEW[jn]) == -1 && mNb[0] == 0)
            {
                mNb[0] = 1;
                return kFALSE;
            }
            else if ((mEW[js]-mEW[jn]) == 1 && mNb[1] == 0)
            {
                mNb[1] = 1;
                return kFALSE;
            }
            else
                return kTRUE;
        }
        else if(abs(mSW[js]-mSW[jn]) == 1 && abs(mEW[js]-mEW[jn]) == 1)
        {
            if      ((mEW[js]-mEW[jn]) == -1 && mNb[3] == 0)
            {
                mNb[3] = 1;
                return kFALSE;
            }
            else if ((mEW[js]-mEW[jn]) == 1 && mNb[4] == 0)
            {
                mNb[4] = 1;
                return kFALSE;
            }
            else
                return kTRUE;
        }
    }
    return kTRUE;
}
