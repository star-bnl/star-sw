/****************************************************
 *
 * $Id: StPmdDiscriminator.cxx,v 1.1 2002/08/27 12:10:32 subhasis Exp $
 *
 * Author: Subhasis Chattopadhyay
 *
 ******************************************************
 *
 * Description: Class for discrimination through energy
 * cut is defined.
 *
 ******************************************************
 *
 * $Log: StPmdDiscriminator.cxx,v $
 * Revision 1.1  2002/08/27 12:10:32  subhasis
 * First version
 *
 *
 ******************************************************/

#include<iostream.h>
#include<assert.h>
#include<math.h>
#include"TROOT.h"
#include<TRandom.h>
#include<TBrowser.h>
#include<TPad.h>
#include<StMessMgr.h>
#include<TFile.h>

#include "StBFChain.h"
#include "StPmdUtil/StPmdGeom.h"
#include "StPmdUtil/StPmdDetector.h"
#include "StPmdDiscriminatorMaker.h"
#include "StPmdDiscriminator.h"
#include "StPmdUtil/StPmdClusterCollection.h"
#include "StPmdUtil/StPmdCluster.h"

ClassImp(StPmdDiscriminator)

 
StPmdDiscriminator::StPmdDiscriminator(Float_t de,StPmdDetector* pmd_det, StPmdDetector* cpv_det)
{
	m_PmdDet=pmd_det;
	m_CpvDet=cpv_det;
	mEdepThreshold=de;
        m_photonlike=0;
}


StPmdDiscriminator::~StPmdDiscriminator()
{
  //destructor
}

void StPmdDiscriminator::getClusterPID() //! returns the ClusterId  
{

        StPmdClusterCollection* clusters = (StPmdClusterCollection*)m_PmdDet->cluster();

        Int_t nclust = clusters->Nclusters();
         TIter next(clusters->Clusters());
	            StPmdCluster *spmcl1;
        for(Int_t i=0; i<nclust ; i++)
        {
	       Int_t ClusterPID=0;
         spmcl1 = (StPmdCluster*)next();
         Float_t edep=spmcl1->CluEdep();
         if(edep>=mEdepcut)ClusterPID=1;
         if(edep<mEdepcut)ClusterPID=8;
//	 cout<<"The energy deposition is "<<edep<<endl;
	 cout<<"The PID is "<<ClusterPID<<endl;
	 spmcl1->setCluEdepPID(ClusterPID);
       }
}

void StPmdDiscriminator::Discriminate() // Discrimnation through Energy cut
{
StPmdClusterCollection* clustersd = (StPmdClusterCollection*)m_PmdDet->cluster();

         Int_t nclustd = clustersd->Nclusters();
         TIter next(clustersd->Clusters());
	            StPmdCluster *spmcl1d;
         for(Int_t i=0; i<nclustd ; i++)
         {
         spmcl1d = (StPmdCluster*)next();
         Float_t edepd=spmcl1d->CluEdep();
         if(edepd>=mEdepcut)m_photonlike++;
//	 spmcl1d->setCluPID(ClusterPID);
         }
}

void StPmdDiscriminator::Print()
{
   if(m_photonlike>0){
cout<<"*************** DISCRIMINATE **************"<<endl;
cout<<"photonlike "<<m_photonlike<<"for Cut "<<mEdepcut<<endl;
}
}


/*
void StPmdDiscriminator::Matching()
{
StPmdClusterCollection* clusters = (StPmdClusterCollection*)m_PmdDet->cluster();
StPmdClusterCollection* clusters1 = (StPmdClusterCollection*)m_CpvDet->cluster();
//StPmdClusterCollection* clusters = (StPmdClusterCollection*)pmd_det->cluster();
 cout<<"This is to test5"<<endl;
 //StPmdClusterCollection* clusterscpv = (StPmdClusterCollection*)cpv_det->cluster();
 cout<<"This is to test6"<<endl;
        Int_t nclustpmd = clusters->Nclusters();
 cout<<"This is to test7"<<endl;
         Int_t nclust1 = clusters1->Nclusters();
 cout<<"This is to test8"<<endl;
	Float_t etapmd[1000],phipmd[1000],edeppmd[1000];
	Float_t etacpv[1000],phicpv[1000],edepcpv[1000];
	cout<<"total no of clusters ***"<<nclustpmd<<"    "<<endl;
         TIter next(clusters->Clusters());
 cout<<"This is to test8"<<endl;
            StPmdCluster *spmcl1;
        for(Int_t i=0; i<nclustpmd ; i++)
               {
                   spmcl1 = (StPmdCluster*)next();
            etapmd[i]=spmcl1->CluEta();
            phipmd[i]=spmcl1->CluPhi();
            edeppmd[i]=spmcl1->CluEdep();
	       }
 cout<<"This is to test9"<<endl;
 	TIter next(clusters1->Clusters());
            StPmdCluster *spmcl1cpv;
        for(Int_t icpv=0; icpv<nclust1 ; icpv++)
               {
                   spmcl1cpv = (StPmdCluster*)next();
            etacpv[icpv]=spmcl1cpv->CluEta();
            phicpv[icpv]=spmcl1cpv->CluPhi();
            edepcpv[icpv]=spmcl1cpv->CluEdep();
	       }
 
 cout<<"This is to test10"<<endl;
}
*/



//void StPmdDiscriminator::Browse(TBrowser *b)
//{
//  TDataSet::Browse(b);
//}












































































































































































































