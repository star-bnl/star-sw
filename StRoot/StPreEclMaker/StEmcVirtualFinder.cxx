
#include "StEmcVirtualFinder.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "TString.h"
#include "StEmcUtil/others/emcDetectorName.h"

ClassImp(StEmcVirtualFinder)

//_____________________________________________________________________________
StEmcVirtualFinder::StEmcVirtualFinder():TObject()
{
    TString name,title;
    for(Int_t i = 0; i<MAXDETBARREL; i++)
    {
        mColl[i] = new StEmcPreClusterCollection(i+1);

        name  = detname[i];
        name += "_NClusters";
        title = "Number of clusters for detector ";
        title+= detname[i];
        mHist1D[0][i] = new TH1F(name.Data(),title.Data(),500,0,500);

        name  = detname[i];
        name += "_NHits";
        title = "Number of hits/cluster for detector ";
        title+= detname[i];
        mHist1D[1][i] = new TH1F(name.Data(),title.Data(),20,0,20);

        name  = detname[i];
        name += "_Energy";
        title = "Cluster energy for detector ";
        title+= detname[i];
        mHist1D[2][i] = new TH1F(name.Data(),title.Data(),500,0,50);

        name  = detname[i];
        name += "_RMSEta";
        title = "Cluster RMS/eta for detector ";
        title+= detname[i];
        mHist1D[3][i] = new TH1F(name.Data(),title.Data(),20,0,0.1);

        name  = detname[i];
        name += "_RMSPhi";
        title = "Cluster RMS/phi for detector ";
        title+= detname[i];
        mHist1D[4][i] = new TH1F(name.Data(),title.Data(),20,0,0.1);

        name  = detname[i];
        name += "_EtaPhi";
        title = "Cluster (eta,phi) for detector ";
        title+= detname[i];
        mHist2D[0][i] = new TH2F(name.Data(),title.Data(),40,-1,1,120,-3.1415,3.1415);
    }
}
//_____________________________________________________________________________
StEmcVirtualFinder::~StEmcVirtualFinder()
{
    for(Int_t i = 0; i<MAXDETBARREL; i++)
        if(mColl[i])
            delete mColl[i];
}
Bool_t StEmcVirtualFinder::findClusters(StEvent* event)
{
    return kFALSE;
}
Bool_t StEmcVirtualFinder::clear(StEvent* event)
{
    StEmcCollection *emc = event->emcCollection();
    if(!emc)
        return kFALSE;

    StSPtrVecEmcPoint& pvec = emc->barrelPoints();
    if(pvec.size()>0)
        pvec.clear();

    for(Int_t i=0; i<MAXDETBARREL; i++)
    {
        StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
        StEmcDetector* detector=emc->detector(id);
        if(detector)
            if(detector->cluster())
            {
                StSPtrVecEmcCluster& cluster=detector->cluster()->clusters();
                if(cluster.size()>0)
                    cluster.clear();
            }
    }
    return kTRUE;
}
Bool_t StEmcVirtualFinder::clear()
{
    for(Int_t i = 0;i<MAXDETBARREL; i++)
        mColl[i]->empty();
    return kTRUE;
}
Bool_t StEmcVirtualFinder::fillStEvent(StEvent* event)
{
    StEmcCollection *emc = event->emcCollection();
    if(!emc)
        return kFALSE;

    for(Int_t i = 0;i<MAXDETBARREL;i++)
        if(mColl[i])
        {
            StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
            StEmcDetector* detector = emc->detector(id);
            if(detector)
            {
                Int_t n = mColl[i]->getNClusters();
                if(n>0)
                {
                    StEmcClusterCollection* coll = detector->cluster();
                    if(!coll)
                    {
                        coll = new StEmcClusterCollection();
                        detector->setCluster(coll);
                    }
                    coll->setDetector(id);
                    coll->setClusterFinderId(1);
                    coll->setClusterFinderParamVersion(1);

                    for(Int_t j = 0;j<n;j++)
                    {
                        StEmcPreCluster *pc = mColl[i]->getCluster(j);
                        if(pc)
                        {
                            StEmcCluster *c = pc->makeStCluster();
                            if(c)
                                coll->addCluster(c);
                        }
                    }
                }
                else
                    detector->setCluster(NULL);
            }
        }
    return kTRUE;
}
Bool_t StEmcVirtualFinder::fillHistograms(StEvent* event)
{
    StEmcCollection *emc = event->emcCollection();
    if(!emc)
        return kFALSE;
    for(Int_t i = 0;i<MAXDETBARREL;i++)
    {
        StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
        StEmcDetector* detector = emc->detector(id);
        if(detector)
        {
            StEmcClusterCollection* coll = detector->cluster();
            if(coll)
            {
                StSPtrVecEmcCluster& clusters = coll->clusters();
                Int_t n = clusters.size();
                mHist1D[0][i]->Fill(n);
                for(Int_t j = 0;j<n;j++)
                {
                    StEmcCluster *c =clusters[j];
                    if(c)
                    {
                        mHist1D[1][i]->Fill(c->nHits());
                        mHist1D[2][i]->Fill(c->energy());
                        mHist1D[3][i]->Fill(c->sigmaEta());
                        mHist1D[4][i]->Fill(c->sigmaPhi());
                        mHist2D[0][i]->Fill(c->eta(),c->phi());
                    }
                }
            }
        }
    }
    return kFALSE;
}
