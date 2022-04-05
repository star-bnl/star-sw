#include "StEmcPreCluster.h"
#include "StEventTypes.h"
ClassImp(StEmcPreCluster)

StEmcPreCluster::StEmcPreCluster(Int_t detector)
{
    mEnergy = 0.0;
    mEta = 0.0;
    mPhi = 0.0;
    mSigmaEta =0.0;
    mSigmaPhi=0.0;
    mDetector=detector;
    mMatchingId = 0;
    mGeom = StEmcGeom::instance(detector);
}
StEmcPreCluster::StEmcPreCluster(StEmcPreCluster& cluster)
{
    Int_t nh = cluster.nHits();
    for(Int_t i = 0;i<nh;i++)
        addHit(cluster.getHit(i));
    mDetector = cluster.detector();
    mMatchingId = cluster.matchingId();
    mGeom = StEmcGeom::instance(mDetector);
    update();
}
StEmcPreCluster::StEmcPreCluster(StEmcCluster* cluster)
{
    StPtrVecEmcRawHit& hits=cluster->hit();
    for(Int_t i=0;i<(Int_t)hits.size();i++)
        addHit(hits[i]);
    mDetector = (Int_t)(hits[0]->detector()-kBarrelEmcTowerId+1);
    mMatchingId = cluster->GetUniqueID();
    mGeom = StEmcGeom::instance(mDetector);
    update();
}
StEmcPreCluster::~StEmcPreCluster()
{
    mHits.Clear("nodelete"); // StEvent hits are not owned by this cluster
}
void StEmcPreCluster::addHit(StEmcRawHit* hit)
{
    if(!mHits.FindObject(hit))
        mHits.Add(hit);
}
void StEmcPreCluster::removeHit(StEmcRawHit* hit)
{
    if(mHits.FindObject(hit))
        mHits.Remove(hit);
}
void StEmcPreCluster::removeHit(Int_t hitId)
{
    if(mHits.At(hitId))
        mHits.Remove(mHits.At(hitId));
}
void StEmcPreCluster::update()
{
    Int_t nH = nHits();
    mEnergy = 0.0;
    mEta = 0.0;
    mPhi = 0.0;
    mSigmaEta =0.0;
    mSigmaPhi=0.0;
    if(nH==0)
        return;

    Int_t m,e,s;
    Float_t E,P,energy,phi0;
    phi0 = -999; //this is always initialized below if there are any hits at all
    Bool_t firstHit = true;
    for(Int_t i = 0;i<nH;i++)
    {
        StEmcRawHit* hit = getHit(i);
        if(hit)
        {
            m=(Int_t)hit->module();
            e=(Int_t)hit->eta();
            s=abs(hit->sub());
            energy=hit->energy();

            mGeom->getEta(m,e, E);
            mGeom->getPhi(m,s, P);
            // Rotate to the system of first hit
            if(firstHit)
            {
                phi0 =  P;
                P = 0.0;
		firstHit = false;
            }
            else
                P-=phi0;
            P = StEmcMath::getPhiPlusMinusPi(P);

            mEta+=E*energy;
            mPhi+=P*energy;
            mSigmaEta+=E*E*energy;
            mSigmaPhi+=P*P*energy;
            mEnergy+=energy;
        }
    }
    mEta /= mEnergy;
    mSigmaEta = mSigmaEta/mEnergy - mEta*mEta;
    if(mSigmaEta <= 0.0)
        mSigmaEta = 0.0;
    else
        mSigmaEta = sqrt(mSigmaEta);

    mPhi /= mEnergy;
    mSigmaPhi = mSigmaPhi/mEnergy - mPhi*mPhi;
    if(mSigmaPhi <= 0.0)
        mSigmaPhi = 0.0;
    else
        mSigmaPhi = sqrt(mSigmaPhi);

    // Rotate back to STAR system
    mPhi += phi0;
    mPhi  = StEmcMath::getPhiPlusMinusPi(mPhi);

    return;
}
void StEmcPreCluster::addCluster(StEmcPreCluster* cluster)
{
    if(!cluster)
        return;
    if(mDetector!=cluster->detector())
        return;
    Int_t nH = cluster->nHits();
    for(Int_t i = 0;i<nH;i++)
        addHit(cluster->getHit(i));
    update();
}
void StEmcPreCluster::addCluster(StEmcCluster* cluster)
{
    if(!cluster)
        return;
    StPtrVecEmcRawHit& hits=cluster->hit();
    if(mDetector!=(Int_t)(hits[0]->detector()-kBarrelEmcTowerId+1))
        return;
    for(Int_t i=0;i<(Int_t)hits.size();i++)
        addHit(hits[i]);
    update();
}
void StEmcPreCluster::subtractCluster(StEmcPreCluster* cluster)
{
    if(!cluster)
        return;
    if(mDetector!=cluster->detector())
        return;
    Int_t nH = cluster->nHits();
    for(Int_t i = 0;i<nH;i++)
        removeHit(cluster->getHit(i));
    update();
}
void StEmcPreCluster::subtractCluster(StEmcCluster* cluster)
{
    if(!cluster)
        return;
    StPtrVecEmcRawHit& hits=cluster->hit();
    if(mDetector!=(Int_t)(hits[0]->detector()-kBarrelEmcTowerId+1))
        return;
    for(Int_t i=0;i<(Int_t)hits.size();i++)
        removeHit(hits[i]);
    update();
}
StEmcPreCluster* StEmcPreCluster::splitInEta(Float_t eta)
{
    Int_t nH = nHits();
    if(nH<2)
        return NULL; // no way to split only with 1 hit
    StEmcPreCluster *cluster = NULL;
    TList *above = new TList();
    Float_t E;
    Int_t m,e;
    Bool_t hasBelow = kFALSE;
    for(Int_t i=0;i<nH;i++)
    {
        StEmcRawHit* hit = getHit(i);
        if(hit)
        {
            m=(Int_t)hit->module();
            e=(Int_t)hit->eta();
            mGeom->getEta(m,e, E);
            if(E<=eta)
                hasBelow=kTRUE;
            else
                above->Add(hit);
        }
    }
    if(hasBelow && above->GetSize()>0)
    {
        cluster = new StEmcPreCluster(detector());
        Int_t na = above->GetSize();
        for(Int_t i = 0;i<na;i++)
        {
            StEmcRawHit* hit = (StEmcRawHit*)above->At(i);
            removeHit(hit);
            cluster->addHit(hit);
        }
        update();
        cluster->update();
    }
    delete above;
    return cluster;
}
StEmcPreCluster* StEmcPreCluster::splitInPhi(Float_t phi)
{
    Int_t nH = nHits();
    if(nH<2)
        return NULL; // no way to split only with 1 hit
    StEmcPreCluster *cluster = NULL;
    TList *above = new TList();
    Float_t P;
    Int_t m,s;
    Bool_t hasBelow = kFALSE;
    for(Int_t i=0;i<nH;i++)
    {
        StEmcRawHit* hit = getHit(i);
        if(hit)
        {
            m=(Int_t)hit->module();
            s=abs((Int_t)hit->sub());
            mGeom->getPhi(m,s, P);
            P-= phi;
            P = StEmcMath::getPhiPlusMinusPi(P);
            if(P<=0)
                hasBelow=kTRUE;
            else
                above->Add(hit);
        }
    }
    if(hasBelow && above->GetSize()>0)
    {
        cluster = new StEmcPreCluster(detector());
        Int_t na = above->GetSize();
        for(Int_t i = 0;i<na;i++)
        {
            StEmcRawHit* hit = (StEmcRawHit*)above->At(i);
            removeHit(hit);
            cluster->addHit(hit);
        }
        update();
        cluster->update();
    }
    delete above;
    return cluster;
}
StEmcCluster* StEmcPreCluster::makeStCluster()
{
    Int_t nH = nHits();
    if(nH==0)
        return NULL;
    StEmcCluster *cluster = new StEmcCluster();
    for(Int_t i = 0;i<nH;i++)
        cluster->addHit(getHit(i));
    update();
    cluster->setEta(mEta);
    cluster->setPhi(mPhi);
    cluster->setSigmaEta(mSigmaEta);
    cluster->setSigmaPhi(mSigmaPhi);
    cluster->setEnergy(mEnergy);
    cluster->SetUniqueID(mMatchingId);
    return cluster;
}
