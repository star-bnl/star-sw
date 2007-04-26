/****************************************************
 *
 * $Id: StPmdDiscriminator.cxx,v 1.7 2007/04/26 04:12:10 perev Exp $
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
 * Revision 1.7  2007/04/26 04:12:10  perev
 * Remove StBFChain dependency
 *
 * Revision 1.6  2004/07/16 14:29:49  subhasis
 * more checks on edep Discriminate
 *
 * Revision 1.5  2003/10/23 04:24:53  perev
 * Stiostream again
 *
 * Revision 1.4  2003/10/14 07:26:17  subhasis
 * fabs changed to ::fabs
 *
 * Revision 1.2  2003/05/29 13:12:51  subhasis
 * several changes to include NN
 *
 *
 ******************************************************/

#include "Stiostream.h"
#include<assert.h>
#include<math.h>
#include"TROOT.h"
#include<TRandom.h>
#include<TBrowser.h>
#include<TPad.h>
#include<StMessMgr.h>
#include<TFile.h>

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
	 spmcl1->setCluEdepPID(ClusterPID);
       }
}

void StPmdDiscriminator::Discriminate() // Discrimnation through Energy cut
{
StPmdClusterCollection* clustersd = (StPmdClusterCollection*)m_PmdDet->cluster();
if(clustersd){

         Int_t nclustd = clustersd->Nclusters();
	 if(nclustd>0){
         TIter next(clustersd->Clusters());
	            StPmdCluster *spmcl1d;
		    int nedepPID=0;
         for(Int_t i=0; i<nclustd ; i++)
         {
         spmcl1d = (StPmdCluster*)next();
         Float_t edepd=spmcl1d->CluEdep();
	 int ClusterPID=0;
         if(edepd>=mEdepcut){
		 ClusterPID=1;
		 nedepPID++;
	 }
         if(edepd<mEdepcut)ClusterPID=8;
	 spmcl1d->setCluEdepPID(ClusterPID);
         }
	 }
}

}

void StPmdDiscriminator::Print()
{
   if(m_photonlike>0){
cout<<"*************** DISCRIMINATE **************"<<endl;
cout<<"photonlike "<<m_photonlike<<"for Cut "<<mEdepcut<<endl;
}
}












































































































































































































