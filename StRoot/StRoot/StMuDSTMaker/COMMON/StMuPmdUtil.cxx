/*****************************************************************
 * $Id: StMuPmdUtil.cxx,v 1.4 2004/10/19 01:43:37 mvl Exp $
 *
 * Class : StMuPmdUtil
 * Author: Supriya Das
 * ****************************************************************
 *
 * Description: This is the utility class for PMD to convert
 *              StEvent to StMuDst and vice versa
 * ****************************************************************
 * $Log: StMuPmdUtil.cxx,v $
 * Revision 1.4  2004/10/19 01:43:37  mvl
 * Added code to copy hits
 *
 * Revision 1.3  2004/04/26 00:12:33  perev
 * include StMuPmdCluster.h added
 *
 * Revision 1.2  2004/04/14 17:15:57  subhasis
 * Xin's TOF reinclusion
 *
 * Revision 1.1  2004/04/02 03:36:21  jeromel
 * New files for PMD
 *
 * ****************************************************************/

#include "StMuPmdUtil.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StMuPmdCluster.h"
#include "StMuPmdHit.h"
#include "StMuPmdCollection.h"

#include "StPhmdDetector.h"
#include "StPhmdClusterCollection.h"
#include "StPhmdCluster.h"
#include "StPhmdModule.h"
#include "StPhmdHit.h"

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
    
    Int_t totalHit=0;
    StDetectorId pdet = static_cast<StDetectorId>(kPhmdId-d);
    StPhmdDetector* detector = (StPhmdDetector*)phmdColl->detector(StDetectorId(pdet));
    
    if(detector) {
      // Added to accommodate PmdHits : Supriya 11/10/04
      for(UInt_t j=0;j<12;j++) {
	StPhmdModule* module = detector->module(j);
	//	module=NULL;
	if(module) {
	  //container for Hit
	  StSPtrVecPhmdHit& rawHit=module->hits();
	  if(rawHit.size()>0) {
	    //loop over Hits
	    totalHit+=rawHit.size();

	    for(Int_t k=0;k<(Int_t)rawHit.size();k++) {

	      muPmd->addHit(PmdDet); //adding to muPmd
	
	      StPhmdHit *pmdhit = (StPhmdHit*)rawHit[k];
	      Int_t sm=pmdhit->superModule();
	      Int_t det=pmdhit->subDetector();
	      Int_t row=pmdhit->row();
	      Int_t col=pmdhit->column();
	      Float_t energy=pmdhit->energy();
	      Int_t adc=pmdhit->adc();
	      //		    Int_t vol=det*1000000+sm*100000+row*100+col;
	      //cout<<"sm,det,row,col,adc,energy "<<sm<<" "<<det<<" "<<row<<" "<<col<<" "<<adc<<" "<<energy<<endl;	
	      StMuPmdHit *mupmdhit = muPmd->getHit(muPmd->getNHits(PmdDet)-1,PmdDet);

	      mupmdhit->setEnergy(energy);
	      mupmdhit->setADC(adc);
	      mupmdhit->setSuperModule((Short_t)sm);
	      mupmdhit->setSubDetector((Short_t)det);
	      mupmdhit->setRow((Short_t)row);
	      mupmdhit->setColumn((Short_t)col);
	    } // Loop over Hits
	  } // checking rawHit.saize()>0
	} // checking module existance
      }  //Loop over 12 Modules

	// Now fill clusters
	StPhmdClusterCollection* cluscol = detector->cluster();
//cout<<"NHITS, nclust******** "<<totalHit<<" "<<cluscol->numberOfclusters()<<endl;
//	cluscol=NULL;
	if(cluscol)
	  {
	    Int_t Ncluster0 = cluscol->numberOfclusters();
	    //cout<<"Ncluster0, "<<Ncluster0<<" "<<PmdDet<<endl;
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
		    
		    mupmdcl->setSuperModule(Int_t(sm));
		    mupmdcl->setNcell(Int_t(ncell));
		    mupmdcl->setEta(Float_t(eta));
		    mupmdcl->setPhi(Float_t(phi));
		    mupmdcl->setSigma(Float_t(sigma));
		    mupmdcl->setEnergy(Float_t(energy));
		    mupmdcl->setEnergyPID(Int_t(energyPID));
		    mupmdcl->setPID(Int_t(PID));
		    mupmdcl->setMcPID(Int_t(mcPID));
		    
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
  
  for(Int_t d=0; d<2; d++) {
      StDetectorId pdet = static_cast<StDetectorId>(kPhmdId-d);
      StPhmdDetector* detector = (StPhmdDetector*)phmdColl->detector(StDetectorId(pdet));
    //  phmdColl->setDetector(detector);
      
      Int_t PmdDet=d+1;

      // Getting Clusters
      Int_t nClusters = muPmd->getNClusters(PmdDet);
    // cout<<"PmdDet, nclusters "<<PmdDet<<" "<<nClusters<<endl; 
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
		  Int_t sm = muPmdCl->superModule();
		  Int_t nCell = muPmdCl->ncell();   
		  Float_t eta = muPmdCl->eta();
		  Float_t phi = muPmdCl->phi();
		  Float_t energy = muPmdCl->energy();
		  Int_t energyPID = muPmdCl->energyPID();
		  Int_t PID = muPmdCl->PID();
		  Int_t McPID = muPmdCl->mcPID();
		  
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
  
      //Getting Hits
 
      Int_t nHits = muPmd->getNHits(PmdDet);

      if (nHits>0) {
	for(Int_t nh=0; nh<nHits; nh++) {

	  StMuPmdHit	*muPmdHit = muPmd->getHit(nh,PmdDet);

	  if(muPmdHit) {
	    Float_t energy= muPmdHit->energy();  
	    Int_t ADC= muPmdHit->adc();  
	    Int_t sm= muPmdHit->superModule();  
	    Int_t subDet= muPmdHit->subDetector();  
	    Int_t row= muPmdHit->row();  
	    Int_t col= muPmdHit->column();  
	    StPhmdHit* phmdHit = new StPhmdHit();
		  
	    phmdHit->setSuperModule(sm);
	    phmdHit->setEnergy(energy);
	    phmdHit->setAdc(ADC);	
	    phmdHit->setSubDetector(subDet);
	    phmdHit->setRow(row);
	    phmdHit->setColumn(col);
	    detector->addHit(phmdHit);
	  }
	}
      }
  }
  return;
} 




