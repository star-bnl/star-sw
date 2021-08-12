//
// $id$
//
// $Log: StPointCollection.cxx,v $
// Revision 1.28  2007/07/25 16:53:20  kocolosk
// bugfix for the bugfix in 1.27
//
// Revision 1.27  2007/07/24 15:41:44  kocolosk
// bugFix from Oleksandr:
// http://www.star.bnl.gov/HyperNews-star/get/emc2/2444.html
//
// Revision 1.26  2007/01/22 19:13:50  kocolosk
// use STAR logger for all output
//
// Revision 1.25  2006/09/20 13:44:28  kocolosk
// fix autobuild warnings
//
// Revision 1.24  2006/03/24 19:31:15  suaide
// fixed break segmentation error.
//
// Revision 1.23  2005/05/23 12:35:14  suaide
// New Point maker code
//
// Revision 1.22  2004/08/13 13:08:01  suaide
// small fixed introduced by Marco to remove ineficiencies on the
// edges of phi bins
//
// Revision 1.20  2003/10/21 15:35:25  suaide
// fix a break segmentation introduced when a memory leak was fixed
//
// Revision 1.19  2003/10/12 02:56:51  perev
// LeakOff TClonesArray::Delete added
//
// Revision 1.18  2003/09/17 00:55:59  suaide
// Fixed bug. Chain was crashing because some data members were not initialized
//
// Revision 1.17  2003/09/02 17:58:03  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.16  2003/05/26 13:44:34  suaide
// added setPrint() method
//
// Revision 1.15  2003/01/23 04:03:21  jeromel
// Include fixed
//
// Revision 1.14  2001/12/03 22:24:28  pavlinov
// tuned for case of no tracks
//
// Revision 1.13  2001/12/01 02:44:50  pavlinov
// Cleanp for events with zero number of tracks
//
// Revision 1.12  2001/11/06 23:35:27  suaide
// fixed bug in the way we get magnetic field
//
// Revision 1.11  2001/10/24 13:55:05  suaide
// small bugs fixed
//
// Revision 1.10  2001/09/29 01:15:17  pavlinov
// Clean up for production
//
// Revision 1.9  2001/09/24 15:14:55  pavlinov
// No public constructor for StEmcGeom
//
// Revision 1.8  2001/08/18 22:13:49  subhasis
// phi-cluster attached to cat#3 points corrected
//
// Revision 1.7  2001/04/25 17:27:44  perev
// HPcorrs
//
// Revision 1.6  2001/04/24 23:06:29  subhasis
// clusters attached to Points, QA hists are made for all category separately
//
// Revision 1.3  2000/08/29 20:33:04  subhasis
//  Modified to accept input from StEvent and writing output to StEvent for Emc
//
// Revision 1.1  2000/05/15 21:18:33  subhasis
// initial version
//
// Pi0 Candidate Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhyay , Jan 2000
//
//////////////////////////////////////////////////////////////////////////
//
// StPointCollection
//

//
/////////////////////////////////////////////////
/////////////////////////
#include "StPointCollection.h"
#include "StPi0Candidate.h"
#include "emc_def.h"
#include <Stiostream.h>
#include <TBrowser.h>
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StarCallf77.h"
#include "TMath.h"

// declaring cernlib routine (mathlib, H301) assndx to be used for matching.
#define    assndx  F77_NAME(assndx,ASSNDX)
extern "C"
{
    void type_of_call assndx ( Int_t &, Float_t *, Int_t &, Int_t &,
                               Int_t &, Int_t *, Float_t &,Int_t*,Int_t &);
}

ClassImp(StPointCollection)

const TString detname[] =
    {"Bemc", "Bsmde", "Bsmdp"
    };
// Extern for sorted emc-smd
StMatchVecClus matchlist_bemc_clus[Epc::nModule][Epc::nPhiBin];
StMatchVecClus matchlist_bprs_clus[Epc::nModule][Epc::nPhiBin];
StMatchVecClus matchlist_bsmde_clus[Epc::nModule][Epc::nPhiBin];
StMatchVecClus matchlist_bsmdp_clus[Epc::nModule][Epc::nPhiBin];

FloatVector HitTrackEta;
FloatVector HitTrackPhi;
FloatVector HitTrackMom;

//_____________________________________________________________________________
StPointCollection::StPointCollection():TDataSet("Default")
{
    SetTitle("EmcPoints");
    mPrint = kTRUE;
    mBField = 0.5;
    mNPoints =0;
    mNPointsReal=0;
    mPosition = new StEmcPosition();
}
//_____________________________________________________________________________
StPointCollection::StPointCollection(const Char_t *Name):TDataSet(Name)
{
    SetTitle("EmcPoints");
    mPrint = kTRUE;
    mBField = 0.5;
    mNPoints =0;
    mNPointsReal=0;
    mPosition = new StEmcPosition();
}
//_____________________________________________________________________________
StPointCollection::~StPointCollection()
{
    mPoints.Delete();
    //mPointsReal.Delete(); // The objects saved here are owned by StEvent
    mNPoints =0;
    mNPointsReal=0;
    delete mPosition;
}

void
StPointCollection::Browse(TBrowser* b)
{
    TDataSet::Browse(b);
}
//*************** FIND EMC POINTS **********************************
Int_t StPointCollection::makeEmcPoints(StEvent* event)
{
    LOG_DEBUG <<"Finding EMC Points ..." << endm;
    if(!event)
        return 0;

    StEmcCollection* emc = event->emcCollection();
    if(!emc)
        return 0;

    StSPtrVecEmcPoint& points = emc->barrelPoints();
    if(points.size())
        points.clear();

    StEmcClusterCollection* cluster[4] = {0,0,0,0};

    for(Int_t i = 0;i<4;i++)
    {
        StDetectorId EmcId = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
        ;
        StEmcDetector* EmcDet = emc->detector(EmcId);
        if(EmcDet)
            cluster[i] = EmcDet->cluster();
    }

    // first find points that were already matched in the cluster finder

    findMatchedClusters(cluster[0],cluster[1],cluster[2],cluster[3]);

    //Sort BEMC, SMDe, SMDp, PRS clusters according to location
    LOG_DEBUG <<"Making points for non-matched clusters ..." <<endm;
    ClusterSort(cluster[0],cluster[1],cluster[2],cluster[3]);

    // MATCHING BEMC detector clusters
    for(Int_t im=0;im<Epc::nModule;im++)
    {
        for(Int_t is=0;is<Epc::nPhiBin;is++)
        {
            if(matchlist_bemc_clus[im][is].size()>0)
            {
                matchClusters(matchlist_bemc_clus[im][is],
                              matchlist_bprs_clus[im][is],
                              matchlist_bsmde_clus[im][is],
                              matchlist_bsmdp_clus[im][is]);
            }
        }
    }

    // MATCHING points to tracks
    matchToTracks(event);
    return 1;
}
//*************** Make points for the clusters already matched ********
Int_t StPointCollection::findMatchedClusters(StEmcClusterCollection* Bemccluster,
        StEmcClusterCollection *Bprscluster,
        StEmcClusterCollection *Bsmdecluster,
        StEmcClusterCollection *Bsmdpcluster)
{
    LOG_DEBUG <<"Making points for already matched clusters ..." <<endm;
    if(Bemccluster)
    {
        Int_t Ncluster0=Bemccluster->numberOfClusters();
        if(Ncluster0>0)
        {
            const StSPtrVecEmcCluster& emcclusters= Bemccluster->clusters();
            for(UInt_t i=0;i<emcclusters.size();i++)
            {
                StEmcCluster *btow=(StEmcCluster*)emcclusters[i];
                UInt_t matchId = btow->GetUniqueID();
                if(matchId!=0)
                {
                    StEmcCluster *bprs = NULL;
                    StEmcCluster *bsmde = NULL;
                    StEmcCluster *bsmdp = NULL;
                    if(Bprscluster)
                    {
                        const StSPtrVecEmcCluster& clusters= Bprscluster->clusters();
                        for(UInt_t i=0;i<clusters.size();i++)
                            if(  ((StEmcCluster*)clusters[i])->GetUniqueID() == matchId)
                            {
                                bprs = (StEmcCluster*)clusters[i];
                                break;
                            }
                    }
                    if(Bsmdecluster)
                    {
                        const StSPtrVecEmcCluster& clusters= Bsmdecluster->clusters();
                        for(UInt_t i=0;i<clusters.size();i++)
                            if(  ((StEmcCluster*)clusters[i])->GetUniqueID() == matchId)
                            {
                                bsmde = (StEmcCluster*)clusters[i];
                                break;
                            }
                    }
                    if(Bsmdpcluster)
                    {
                        const StSPtrVecEmcCluster& clusters= Bsmdpcluster->clusters();
                        for(UInt_t i=0;i<clusters.size();i++)
                            if(  ((StEmcCluster*)clusters[i])->GetUniqueID() == matchId)
                            {
                                bsmdp = (StEmcCluster*)clusters[i];
                                break;
                            }
                    }

                    makePoint(btow,bprs,bsmde,bsmdp);
                }
            }
        }
    }
    return 0;
}
StEmcPoint* StPointCollection::makePoint(StEmcCluster* btow,StEmcCluster* bprs,StEmcCluster* bsmde,StEmcCluster* bsmdp,Float_t fraction)
{
    if(!btow)
        return NULL;

    LOG_DEBUG <<"Making point"<< endm;

    Int_t Category = 0;
    if(btow)
        Category = Category | 1;
    if(bprs)
        Category = Category | 2;
    if(bsmde)
        Category = Category | 4;
    if(bsmdp)
        Category = Category | 8;

    Float_t en[4] = {0,0,0,0};
    Float_t si[4] = {0,0,0,0};

    Float_t eta    = btow->eta();
    Float_t phi    = btow->phi();

    // if fraction >0 it calculates the point energy as fraction*Energy from BTOW cluster
    // if fraction <0 the point energy is taken as -fraction

    Float_t energy = btow->energy()*fraction;
    if(fraction<0)
        energy = fabs(fraction);
    Float_t sigEta = btow->sigmaEta();
    Float_t sigPhi = btow->sigmaPhi();
    en[0]          = btow->energy();
    si[0]          = sqrt(eta*eta+phi*phi);

    if(bprs)
    {
        en[1]  = bprs->energy();
        si[1]  = sqrt(bprs->sigmaEta()*bprs->sigmaEta()+bprs->sigmaPhi()*bprs->sigmaPhi());
    }

    if(bsmde)
    {
        eta    = bsmde->eta();
        sigEta = bsmde->sigmaEta();
        en[2]  = bsmde->energy();
        si[2]  = sqrt(bsmde->sigmaEta()*bsmde->sigmaEta()+bsmde->sigmaPhi()*bsmde->sigmaPhi());
    }

    if(bsmdp)
    {
        phi    = bsmdp->phi();
        sigPhi = bsmdp->sigmaPhi();
        en[3]  = bsmdp->energy();
        si[3]  = sqrt(bsmdp->sigmaEta()*bsmdp->sigmaEta()+bsmdp->sigmaPhi()*bsmdp->sigmaPhi());
    }

    Float_t xp,yp,zp;

    // Point position
    xp=(StEpcCut::RAD_SMD_E())*cos(phi);
    yp=(StEpcCut::RAD_SMD_E())*sin(phi);
    zp=(StEpcCut::RAD_SMD_E())*sinh(eta);
    StThreeVectorF PointPosition(xp*centimeter, yp*centimeter, zp*centimeter);

    //Error in location of Point
    xp=(StEpcCut::RAD_SMD_E())*(cos(phi+sigPhi)-cos(phi-sigPhi))/2;
    yp=(StEpcCut::RAD_SMD_E())*(sin(phi+sigPhi)-sin(phi-sigPhi))/2;
    zp=(StEpcCut::RAD_SMD_E())*(sinh(eta+sigEta)-sinh(eta-sigEta))/2;
    StThreeVectorF ErrorPosition(xp*centimeter, yp*centimeter, zp*centimeter);

    StThreeVectorF size(sigEta,sigPhi,0.0);

    // Chisquare
    Float_t ChiSquare = 0.0;


    StEmcPoint *point = new StEmcPoint();
    point->setQuality(Category);
    point->setPosition(PointPosition);
    point->setPositionError(ErrorPosition);
    point->setSize(size);
    point->setChiSquare(ChiSquare);
    point->setEnergy(energy);
    point->setDeltaEta(9999);
    point->setDeltaPhi(9999);
    for(Int_t i=0;i<4;i++)
    {
        StDetectorId id=static_cast<StDetectorId>(i+kBarrelEmcTowerId);
        point->setEnergyInDetector(id,en[i]);
        point->setSizeAtDetector(id,si[i]);
        if(i==0 && btow)
            point->addCluster(id,btow);
        else
            point->addCluster(id,NULL);
        if(i==1 && bprs)
            point->addCluster(id,bprs);
        else
            point->addCluster(id,NULL);
        if(i==2 && bsmde)
            point->addCluster(id,bsmde);
        else
            point->addCluster(id,NULL);
        if(i==3 && bsmdp)
            point->addCluster(id,bsmdp);
        else
            point->addCluster(id,NULL);
    }

    mPointsReal.Add(point);
    mNPointsReal++;

    return point;

}
//--------------------------------------------------------------------------
Int_t StPointCollection::matchClusters(const StMatchVecClus mvec,
                                       const StMatchVecClus prsvec,
                                       const StMatchVecClus evec,
                                       const StMatchVecClus pvec)

{
    Int_t na=evec.size();
    Int_t ma=pvec.size();
    Float_t smin;
    Int_t mode=1;
    Int_t Category=-1; //this one *always* changes to 0,1,2,3 in the if-block below
    Float_t EmcTot = 0;
    Float_t totAvg = 0;
    Int_t idw =Epc::nMaxNoOfClinBin;
    Int_t ida =Epc::nMaxNoOfClinBin;
    Float_t ep[Epc::nMaxNoOfClinBin][Epc::nMaxNoOfClinBin];
    Int_t   iw[Epc::nMaxNoOfClinBin][Epc::nMaxNoOfClinBin];
    Int_t   k[Epc::nMaxNoOfClinBin];
    
    
    for (Int_t iF=0;iF<Epc::nMaxNoOfClinBin;iF++)
    {
        k[iF]=0;
        for (Int_t iL=0;iL<Epc::nMaxNoOfClinBin;iL++)
        {
            ep[iF][iL]=0.0;
            iw[iF][iL]=0;
        }
    }
    if(evec.size()==0 && pvec.size()==0)
    {
        Category=0;
        na=mvec.size();
        ma=mvec.size();
    }
    if(evec.size()>0 && pvec.size()==0)
    {
        Category=1;
        na=mvec.size();
        ma=evec.size();
    }
    if(evec.size()==0 && pvec.size()>0)
    {
        Category=2;
        na=mvec.size();
        ma=pvec.size();
    }
    if(evec.size()>0 && pvec.size()>0)
    {
        Category=3;
        na=evec.size();
        ma=pvec.size();
    }

    // getting total BTOW energy in the patch
    if(mvec.size()>0)
    {
        for (UInt_t ims=0;ims<mvec.size();ims++)
        {
            StEmcCluster *cl0;
            cl0=(StEmcCluster*)mvec[ims];
            Float_t emen=cl0->energy();
            EmcTot+=emen;
        }
    }

    //
    for(Int_t ie=0;ie<na;ie++)
    {
        StEmcCluster *cl1 = NULL;
        for(Int_t ip=0;ip<ma;ip++)
        {
            StEmcCluster *cl2 = NULL;
            switch (Category)
            {
            case 0:
                cl1 = (StEmcCluster*)mvec[ie];
                cl2 = (StEmcCluster*)mvec[ip];
                break;
            case 1:
                cl1 = (StEmcCluster*)mvec[ie];
                cl2 = (StEmcCluster*)evec[ip];
                break;
            case 2:
                cl1 = (StEmcCluster*)mvec[ie];
                cl2 = (StEmcCluster*)pvec[ip];
                break;
            case 3:
                cl1 = (StEmcCluster*)evec[ie];
                cl2 = (StEmcCluster*)pvec[ip];
                break;
            }

	    if(cl1 && cl2){
            	Float_t diff=TMath::Abs((cl1->energy())-(cl2->energy()));
            	Float_t summ= (cl1->energy())+(cl2->energy());
            	ep[ip][ie]=diff/summ;
	    }
        }
    }

    assndx(mode,ep[0],na,ma,ida,k,smin,iw[0],idw);

    int i1;
    switch (Category)
    {
    case 0:
        for(i1=0;i1<na;i1++)
        {
            if((k[i1]-1)>=0)
            {
                StEmcCluster *cl1;
                cl1 = (StEmcCluster*)mvec[i1];
                Float_t avg_en = cl1->energy();
                totAvg += avg_en;
            }
        }
        break;
    case 1:
        for(i1=0;i1<na;i1++)
        {
            if((k[i1]-1)>=0)
            {
                StEmcCluster *cl1;
                cl1 = (StEmcCluster*)mvec[i1];
                Float_t avg_en = cl1->energy();
                totAvg += avg_en;
            }
        }
        break;
    case 2:
        for(i1=0;i1<na;i1++)
        {
            if((k[i1]-1)>=0)
            {
                StEmcCluster *cl1;
                cl1 = (StEmcCluster*)mvec[i1];
                Float_t avg_en = cl1->energy();
                totAvg += avg_en;
            }
        }
        break;
    case 3:
        for(i1=0;i1<na;i1++)
        {
            if((k[i1]-1)>=0)
            {
                StEmcCluster *cl1;
                cl1 = (StEmcCluster*)evec[i1];
                StEmcCluster *cl2;
                cl2 = (StEmcCluster*)pvec[k[i1]-1];
                Float_t avg_en = 0.5*(cl1->energy()+cl2->energy());
                totAvg += avg_en;
            }
        }
        break;
    }

    for(i1=0;i1<na;i1++)
    {

        StEmcCluster *btow  = NULL;
        StEmcCluster *bprs  = NULL;
        StEmcCluster *bsmde = NULL;
        StEmcCluster *bsmdp = NULL;
        Float_t fraction    = 1;
        Float_t eta, phi;
	eta = phi = -999.; //these should always be initialized, but it would be nice to prove it

        switch (Category)
        {
        case 0:
            if((k[i1]-1)>=0)
            {
                btow = (StEmcCluster*)mvec[i1];
                eta = btow->eta();
                phi = btow->phi();
                fraction = 1;
            }
            break;
        case 1:
            if((k[i1]-1)>=0)
            {
                btow  = (StEmcCluster*)mvec[i1];
                bsmde = (StEmcCluster*)evec[k[i1]-1];
                eta = bsmde->eta();
                phi = btow->phi();
                fraction = EmcTot/totAvg;
            }
            break;
        case 2:
            if((k[i1]-1)>=0)
            {
                btow  = (StEmcCluster*)mvec[i1];
                bsmdp = (StEmcCluster*)pvec[k[i1]-1];
                eta=btow->eta();
                phi=bsmdp->phi();
                fraction = EmcTot/totAvg;
            }
            break;
        case 3:
            if((k[i1]-1)>=0)
            {
                bsmde = (StEmcCluster*)evec[i1];
                bsmdp = (StEmcCluster*)pvec[k[i1]-1];
                fraction = -fabs(EmcTot*0.5*(bsmde->energy()+bsmdp->energy())/totAvg);
                eta = bsmde->eta();
                phi = bsmdp->phi();
                Float_t delta = 999999;
                for (UInt_t ims=0;ims<mvec.size();ims++)
                {
                    StEmcCluster *cl0 = (StEmcCluster*)mvec[ims];
                    Float_t de = sqrt((eta-cl0->eta())*(eta-cl0->eta()) +
                                      (phi-cl0->phi())*(phi-cl0->phi()));
                    if(de<delta)
                    {
                        btow = cl0;
                        delta = de;
                    }
                }
            }
            break;
        }

        if(prsvec.size()>0)
        {
            Float_t delta = 999999;
            for (UInt_t ims=0;ims<prsvec.size();ims++)
            {
                StEmcCluster *cl0 = (StEmcCluster*)prsvec[ims];
                Float_t de = sqrt((eta-cl0->eta())*(eta-cl0->eta()) +
                                  (phi-cl0->phi())*(phi-cl0->phi()));
                if(de<delta)
                {
                    bprs = cl0;
                    delta = de;
                }
            }
        }
        makePoint(btow,bprs,bsmde,bsmdp,fraction);
    }
    return 1;
}
//-----------------------------------------------------------------------
void StPointCollection::ClusterSort(StEmcClusterCollection* Bemccluster,
                                    StEmcClusterCollection* Bprscluster,
                                    StEmcClusterCollection* Bsmdecluster,
                                    StEmcClusterCollection* Bsmdpcluster)
{
    // The TObject::GetUniqueID() method is used in the cluster
    // finder to allow matching at clustering level. StEpcMaker
    // will try to match only clusters which have UniqueID = 0
    // clusters with UniqueID!=0 were matched at cluster finder
    // StEpcMaker will, in these cases, use the UniqueID as plain
    // matching information to create the corresponding points.
    const Int_t eta_shift_fix=1;
    LOG_DEBUG <<" I am inside PointCalc***"<<endm;
    for(Int_t i1=0;i1<Epc::nModule;i1++)
    {
        for(Int_t i2=0;i2<Epc::nPhiBin;i2++)
        {
            matchlist_bemc_clus[i1][i2].clear();
            matchlist_bprs_clus[i1][i2].clear();
            matchlist_bsmde_clus[i1][i2].clear();
            matchlist_bsmdp_clus[i1][i2].clear();
        }
    }

    StEmcGeom* GeomIn  = StEmcGeom::getEmcGeom("bemc");

    //BEMC
    if(Bemccluster)
    {
        Int_t Ncluster0=Bemccluster->numberOfClusters();
        if(Ncluster0>0)
        {
            const StSPtrVecEmcCluster& emcclusters= Bemccluster->clusters();
            for(UInt_t i=0;i<emcclusters.size();i++)
            {
                StEmcCluster *cl1=(StEmcCluster*)emcclusters[i];
                LOG_DEBUG <<"BEMC cluster UniqueId = "<<cl1->GetUniqueID()<<endm;
                if(cl1->GetUniqueID()==0)
                {
                    Float_t eta_emc=cl1->eta();
                    Float_t phi_emc=cl1->phi();
                    //Get the module number
                    Int_t ebin,pbin;
                    Int_t emc_module=0;
                    Int_t & imd =emc_module;
                    Int_t testb=GeomIn->getBin(phi_emc,eta_emc,imd,ebin,pbin);
                    if(testb==0)
                        emc_module=imd;
                    if(testb==0)
                    {
                        Int_t emc_phi_bin=Int_t(TMath::Abs(eta_emc*10));
                        //keeping the cluster very close to phibin boundry to the previous bin
                        //
                        if(!eta_shift_fix && emc_phi_bin>0)
                        {
                            if((TMath::Abs(eta_emc*10)-Float_t(emc_phi_bin))<0.2)
                            {
                                emc_phi_bin--;
                            }
                        }
                        if(emc_phi_bin>9)
                        {
                            emc_phi_bin=9;
                        }
                        //copy cl1 pointer to StMatchvec
                        matchlist_bemc_clus[emc_module-1][emc_phi_bin].push_back(cl1);
                    }
                }
            }
        }
    }

    //PRS
    if(Bprscluster)
    {
        Int_t Ncluster1=Bprscluster->numberOfClusters();
        if(Ncluster1>0)
        {
            const StSPtrVecEmcCluster& emcclusters= Bprscluster->clusters();
            for(UInt_t i=0;i<emcclusters.size();i++)
            {
                StEmcCluster *cl2=(StEmcCluster*)emcclusters[i];
                if(cl2->GetUniqueID()==0)
                {
                    Float_t eta_emc=cl2->eta();
                    Float_t phi_emc=cl2->phi();
                    //Get the module number
                    Int_t ebin,pbin;
                    Int_t emc_module=0;
                    Int_t emc_phi_bin=0;
                    Int_t & imd =emc_module;
                    Int_t testb=GeomIn->getBin(phi_emc,eta_emc,imd,ebin,pbin);
                    if(testb==0)
                        emc_module=imd;
                    if(testb==0)
                    {
                        emc_phi_bin=Int_t(TMath::Abs(eta_emc*10));
                        //keeping the cluster very close to phibin boundry to the previous bin
                        if(!eta_shift_fix && emc_phi_bin>0)
                        {
                            if((TMath::Abs(eta_emc*10)-Float_t(emc_phi_bin))<0.2)
                            {
                                emc_phi_bin--;
                            }
                        }
                        if(emc_phi_bin>9)
                        {
                            emc_phi_bin=9;
                        }
                        //copy cl1 pointer to StMatchvec
                        matchlist_bprs_clus[emc_module-1][emc_phi_bin].push_back(cl2);
                    }
                }
            }
        }
    }

    //BSMD_ETA
    if(Bsmdecluster)
    {
        Int_t Ncluster2=Bsmdecluster->numberOfClusters();
        if(Ncluster2>0)
        {
            const StSPtrVecEmcCluster& emcclusters= Bsmdecluster->clusters();
            for(UInt_t i=0;i<emcclusters.size();i++)
            {
                StEmcCluster *cl3=(StEmcCluster*)emcclusters[i];
                if(cl3->GetUniqueID()==0)
                {
                    Float_t eta_emc=cl3->eta();
                    Float_t phi_emc=cl3->phi();
                    //Get the module number
                    Int_t ebin,pbin;
                    Int_t emc_module=0;
                    Int_t emc_phi_bin=0;
                    Int_t & imd =emc_module;
                    Int_t testb=GeomIn->getBin(phi_emc,eta_emc,imd,ebin,pbin);
                    if(testb==0)
                        emc_module=imd;
                    if(testb==0)
                    {
                        emc_phi_bin=Int_t(TMath::Abs(eta_emc*10));
                        //keeping the cluster very close to phibin boundry to the previous bin
                        if(!eta_shift_fix && emc_phi_bin>0)
                        {
                            if((TMath::Abs(eta_emc*10)-Float_t(emc_phi_bin))<.2)
                            {
                                emc_phi_bin--;
                            }
                        }
                        if(emc_phi_bin>9)
                        {
                            emc_phi_bin=9;
                        }
                        //copy cl1 pointer to StMatchvec
                        matchlist_bsmde_clus[emc_module-1][emc_phi_bin].push_back(cl3);
                    }
                }
            }
        }
    }

    // BSMDP
    if(Bsmdpcluster)
    {
        Int_t Ncluster3=Bsmdpcluster->numberOfClusters();
        if(Ncluster3>0)
        {
            const StSPtrVecEmcCluster& emcclusters= Bsmdpcluster->clusters();
            for(UInt_t i=0;i<emcclusters.size();i++)
            {
                StEmcCluster *cl4=(StEmcCluster*)emcclusters[i];
                if(cl4->GetUniqueID()==0)
                {
                    Float_t eta_emc=cl4->eta();
                    Float_t phi_emc=cl4->phi();
                    //Get the module number
                    Int_t ebin,pbin;
                    Int_t emc_module=0;
                    Int_t & imd =emc_module;
                    Int_t testb=GeomIn->getBin(phi_emc,eta_emc,imd,ebin,pbin);
                    if(testb==0)
                        emc_module=imd;
                    if(testb==0)
                    {
                        Int_t emc_phi_bin=Int_t(TMath::Abs(eta_emc*10));
                        //keeping the cluster very close to phibin boundry to the previous bin
                        if(!eta_shift_fix && emc_phi_bin>0)
                        {
                            if((TMath::Abs(eta_emc*10)-Float_t(emc_phi_bin))<=0.01)
                            {
                                emc_phi_bin--;
                            }
                        }
                        if(emc_phi_bin>9)
                        {
                            emc_phi_bin=9;
                        }
                        //copy cl1 pointer to StMatchvec
                        matchlist_bsmdp_clus[emc_module-1][emc_phi_bin].push_back(cl4);
                    }
                }
            }
        }
    }
}
//--------------------------------------------------------------------------
Int_t StPointCollection::matchToTracks(StEvent* event)
{
    if(!event)
        return 0;

    Float_t field = 0.5;
    StEventSummary *evtSummary = event->summary();
    if (evtSummary)
        field = evtSummary->magneticField()/10;

    Int_t nR = NPointsReal();
    StEmcGeom* geom = StEmcGeom::instance("bemc");
    if(nR>0)
    {
        LOG_DEBUG << "Matching to tracks... NP = " << nR << endm;
        StSPtrVecTrackNode& tracks=event->trackNodes();
        Int_t nTracks =  tracks.size();
        StThreeVectorD momentum,position;
        for(Int_t t=0;t<nTracks;t++)
        {
            StTrack *track = tracks[t]->track(0);
            if(track)
            {
                if(track->geometry())
                {
                    Bool_t tok = mPosition->trackOnEmc(&position,&momentum,
                                                       track,(Double_t)field,
                                                       (Double_t)geom->Radius());
                    if(tok)
                    {
                        Float_t eta = position.pseudoRapidity();
                        Float_t phi = position.phi();
                        if(fabs(eta)<1)
                        {
                            TIter next(PointsReal());
                            StEmcPoint *cl;

                            for(Int_t i=0; i<nR; i++)
                            {
                                cl = (StEmcPoint*)next();
                                if(cl)
                                {
                                    StThreeVectorF pos = cl->position();
                                    Float_t etaP = pos.pseudoRapidity();
                                    Float_t phiP = pos.phi();
                                    Float_t D = sqrt(cl->deltaEta()*cl->deltaEta()+cl->deltaPhi()*cl->deltaPhi());
                                    Float_t d = sqrt((eta-etaP)*(eta-etaP)+(phi-phiP)*(phi-phiP));
                                    if(d<D)
                                    {
                                        cl->setDeltaEta(eta-etaP);
                                        cl->setDeltaPhi(phi-phiP);
                                    }

                                    StThreeVectorF err = cl->positionError();
                                    Float_t etaE = err.pseudoRapidity();
                                    Float_t phiE = err.phi();

                                    Float_t dPhi = fabs(phi-phiP);
                                    if (dPhi>TMath::Pi())
                                        dPhi=2*TMath::Pi()-dPhi;

                                    if(fabs(eta-etaP)<fabs(etaE) && dPhi<fabs(phiE))
                                    {
                                        Int_t Category = cl->quality();
                                        Category = Category | 16;
                                        cl->setQuality(Category);
                                        cl->addTrack(track);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    return 0;
}
