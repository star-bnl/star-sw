/*****************************************************************
 * $Id: StMuPmdUtil.cxx,v 1.1 2004/04/02 03:36:21 jeromel Exp $
 *
 * Class : StMuPmdUtil
 * Author: Supriya Das
 * ****************************************************************
 *
 * Description: This is the utility class for PMD to convert
 *              StEvent to StMuDst and vice versa
 * ****************************************************************
 * $Log: StMuPmdUtil.cxx,v $
 * Revision 1.1  2004/04/02 03:36:21  jeromel
 * New files for PMD
 *
 * ****************************************************************/

#include "StMuPmdUtil.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StMuPmdCollection.h"

#include "StPhmdDetector.h"
#include "StPhmdClusterCollection.h"
#include "StPhmdCluster.h"

ClassImp(StMuPmdUtil)
  
  StMuPmdUtil::StMuPmdUtil()
{
}

StMuPmdUtil::~StMuPmdUtil()
{
}

StMuPmdCollection* StMuPmdUtil::getMuPmd(StPhmdCollection *phmdColl)
{
  if(!phmdColl) return NULL;
  StMuPmdCollection* muPmd = new StMuPmdCollection();
  fillMuPmd(phmdColl,muPmd);
  return muPmd;
}

StPhmdCollection* StMuPmdUtil::getPmd(StMuPmdCollection* muPmd)
{
  if(!muPmd) return NULL;
  
  StPhmdCollection *phmdColl=new StPhmdCollection();
  fillPmd(muPmd,phmdColl);
  return phmdColl;
}

void StMuPmdUtil::fillMuPmd(StPhmdCollection *phmdColl, StMuPmdCollection *muPmd)
{
  if(!muPmd)return;
  if(!phmdColl)return;
  
  for(Int_t d=0; d<2; d++){
    Int_t PmdDet=d+1;
    
    StDetectorId pdet = static_cast<StDetectorId>(kPhmdId-d);
    StPhmdDetector* detector = (StPhmdDetector*)phmdColl->detector(StDetectorId(pdet));
    
    if(detector)
      {
	StPhmdClusterCollection* cluscol = detector->cluster();
	if(cluscol)
	  {
	    Int_t Ncluster0 = cluscol->numberOfclusters();
	    cout<<"Ncluster0, "<<Ncluster0<<" "<<PmdDet<<endl;
	    if(Ncluster0 > 0)
	      {
		const StSPtrVecPhmdCluster& pmdclusters = cluscol->clusters();	  
		//cout<<"cluster size "<<pmdclusters.size()<<endl; 
		for(UInt_t i=0; i<pmdclusters.size(); i++)
		  {
		    muPmd->addCluster(PmdDet); // adding to muPmd
//cout<<"clusterId "<<muPmd->getNClusters(PmdDet)<<endl;
		    StPhmdCluster *pmdcl = (StPhmdCluster*)pmdclusters[i];
		    Int_t sm = pmdcl->module();
		    Int_t ncell = pmdcl->numberOfCells();
		    Float_t eta = pmdcl->eta();
		    Float_t phi = pmdcl->phi();
		    Float_t sigma = pmdcl->sigma();
		    Float_t energy = pmdcl->energy();
		    Int_t energyPID = pmdcl->energyPid(); //PID based on energy cut
		    Int_t PID = pmdcl->pid(); //PID based on other methods like Neural Network 
		    Int_t mcPID = pmdcl->mcPid(); //PID got from GEANT, 0 for Data
		    
		    StMuPmdCluster *mupmdcl=muPmd->getCluster(muPmd->getNClusters(PmdDet)-1,PmdDet);
		//    cout<<"size "<<i<<" "<<muPmd->getNClusters(PmdDet)<<" "<<mupmdcl<<endl;

		    mupmdcl->SetSuperModule(Int_t(sm));
		    mupmdcl->SetNcell(Int_t(ncell));
		    mupmdcl->SetEta(Float_t(eta));
		    mupmdcl->SetPhi(Float_t(phi));
		    mupmdcl->SetSigma(Float_t(sigma));
		    mupmdcl->SetEnergy(Float_t(energy));
		    mupmdcl->SetEnergyPID(Int_t(energyPID));
		    mupmdcl->SetPID(Int_t(PID));
		    mupmdcl->SetMcPID(Int_t(mcPID));
		    
		  } //Loop over clusters
	      } //Check for non-zero cluster.size()
	  } //Check for existence of ClusterCollection
      } //Check for Detector
  } //Loop over detectors
  
  return;
}

void StMuPmdUtil::fillPmd(StMuPmdCollection* muPmd, StPhmdCollection* phmdColl)
{
  if(!muPmd) return;
  if(!phmdColl) return;
  
  for(Int_t d=0; d<2; d++)
    {
      StDetectorId pdet = static_cast<StDetectorId>(kPhmdId-d);
      StPhmdDetector* detector = (StPhmdDetector*)phmdColl->detector(StDetectorId(pdet));
    //  phmdColl->setDetector(detector);
      
      Int_t PmdDet=d+1;
      Int_t nClusters = muPmd->getNClusters(PmdDet);
     cout<<"PmdDet, nclusters "<<PmdDet<<" "<<nClusters<<endl; 
      if(nClusters>0)
	{
          StPhmdClusterCollection* phmdClusColl = new StPhmdClusterCollection();
     //cout<<"cluscoll "<<phmdClusColl<<endl; 
	  //phmdClusColl->setDetector(pdet);
	  detector->setCluster(phmdClusColl);
     //cout<<"cluscoll Set "<<phmdClusColl<<endl; 
	  
	  for(Int_t n=0; n<nClusters; n++)
	    {
	      StMuPmdCluster *muPmdCl = muPmd->getCluster(n,PmdDet);
	      if(muPmdCl)
		{
		  Int_t sm = muPmdCl->GetSuperModule();
		  Int_t nCell = muPmdCl->GetNcell();   
		  Float_t eta = muPmdCl->GetEta();
		  Float_t phi = muPmdCl->GetPhi();
		  Float_t energy = muPmdCl->GetEnergy();
		  Int_t energyPID = muPmdCl->GetEnergyPID();
		  Int_t PID = muPmdCl->GetPID();
		  Int_t McPID = muPmdCl->GetMcPID();
		  
		  StPhmdCluster* phmdCl = new StPhmdCluster();
		  
		  phmdCl->setModule(sm);
		  phmdCl->setNumberOfCells(nCell);
		  phmdCl->setEta(eta);
		  phmdCl->setPhi(phi);
		  phmdCl->setEnergy(energy);
		  phmdCl->setEnergyPid(energyPID);
		  phmdCl->setPid(PID);
		  phmdCl->setMcPid(McPID);	
		  
	          phmdClusColl->addCluster(phmdCl);
		}
	    }
	}
    }
 return;
} 




