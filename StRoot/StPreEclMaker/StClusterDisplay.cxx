#include "StClusterDisplay.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include "StTrack.h"
#include <math.h>
#include "TFile.h"
#include "TROOT.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"

ClassImp(StClusterDisplay)

//_____________________________________________________________________________
StClusterDisplay::StClusterDisplay(const char *name):StMaker(name)
{}
//_____________________________________________________________________________
StClusterDisplay::~StClusterDisplay()
{}
//_____________________________________________________________________________
Int_t StClusterDisplay::Init()
{
    TString name,title;
    for(Int_t i = 0; i<MAXDETBARREL; i++)
    {
        mGeo[i]=StEmcGeom::getEmcGeom(detname[i].Data());
        mCanvas[i] = 0;
        mTh[i] = 0.2;

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


        Int_t nEta=mGeo[i]->NEta();
        Int_t nSub=mGeo[i]->NSub();

        TArrayF EtaB(nEta+1,mGeo[i]->EtaB());
        TArrayF PhiB(nSub+1,mGeo[i]->PhiB());

        TArrayF EtaBins(2*nEta+2);
        for(Int_t j=0;j<2*nEta+2;j++)
        {
            if (j<nEta+1)
                EtaBins[j]=-EtaB[nEta-j];
            else
                EtaBins[j]=EtaB[j-nEta-1];
        }

        TArrayF PhiBins1(60*(nSub+1));
        TArrayF PhiBins(60*(nSub+1));
        Int_t j=0;
        for(Int_t m=1;m<=60;m++)
            for(Int_t s=1;s<=nSub+1;s++)
            {
                Float_t center;
                mGeo[i]->getPhiModule(m,center);
                PhiBins1[j]=center+PhiB[s-1];
                j++;
            }

        Bool_t again=kTRUE;
        j=0;
        do
        {
            again=kFALSE;
            Float_t phitmp=6.4;
            Int_t ktmp=-1;
            for(Int_t k=0;k<60*(nSub+1);k++)
            {
                if(PhiBins1[k]<phitmp)
                {
                    phitmp=PhiBins1[k];
                    ktmp=k;
                }
            }
            if(ktmp!=-1)
            {
                PhiBins[j]=phitmp;
                again=kTRUE;
                PhiBins1[ktmp]=999;
                j++;
            }
        }
        while(again);

        name  = detname[i];
        name += "_HitDisplay";
        title = "Hits distribution for detector ";
        title+= detname[i];
        mHist2D[1][i] = new TH2F(name.Data(),title.Data(),2*nEta+2-1,EtaBins.GetArray(),60*(nSub+1)-1,PhiBins.GetArray());

        name  = detname[i];
        name += "_ClusterDisplay";
        title = "Cluster distribution for detector ";
        title+= detname[i];
        Int_t BF=10;
        mHist2D[2][i] = new TH2F(name.Data(),title.Data(),BF*2*nEta,-1,1,BF*60*nSub,-3.1415,3.1415);
    }
    return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StClusterDisplay::Make()
{
    StEvent *ev = (StEvent*)GetInputDS("StEvent");
    if(!ev)
        return kStOk;

    StEmcCollection *emc = ev->emcCollection();
    if(!emc)
        return kStOk;
    for(Int_t i = 0;i<MAXDETBARREL;i++)
    {
        mHist2D[1][i]->Reset();
        mHist2D[2][i]->Reset();
        StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
        StEmcDetector* detector = emc->detector(id);
        if(detector)
        {
            for(UInt_t j=1;j<121;j++)
            {
                StEmcModule* module = detector->module(j);
                if(module)
                {
                    StSPtrVecEmcRawHit& rawHit=module->hits();

                    for(Int_t k=0;k<(Int_t)rawHit.size();k++)
                    {
                        Float_t eta,phi;
                        Int_t m=rawHit[k]->module();
                        Int_t e=rawHit[k]->eta();
                        Int_t s=abs(rawHit[k]->sub());
                        Float_t energy = rawHit[k]->energy();

                        mGeo[i]->getEta(m,e,eta);
                        mGeo[i]->getPhi(m,s,phi);
                        if(energy>mTh[i])
                            mHist2D[1][i]->Fill(eta,phi,energy);
                    }
                }
            }
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
                        mHist2D[2][i]->Fill(c->eta(),c->phi());
                    }
                }
            }
        }
        if(mDraw)
        {
            TString CN = "Display_";
            CN+=detname[i];
            if(!gROOT->FindObject(CN.Data()))
                mCanvas[i] = 0;
            if(!mCanvas[i])
                mCanvas[i] = new TCanvas(CN.Data(),CN.Data(),400,600);
            mCanvas[i]->cd();
            mHist2D[1][i]->Draw("colz");
            mHist2D[1][i]->GetXaxis()->UnZoom();
            mHist2D[1][i]->GetYaxis()->UnZoom();

            mHist2D[2][i]->SetMarkerStyle(24);
            mHist2D[2][i]->SetMarkerColor(2);
            mHist2D[2][i]->Draw("sameP");
        }
    }

    return StMaker::Make();
}
//_____________________________________________________________________________
Int_t StClusterDisplay::Finish()
{
    SaveHist();
    return kStOk;
}
//_____________________________________________________________________________
Int_t StClusterDisplay::SaveHist()
{
    return kStOk;
}
