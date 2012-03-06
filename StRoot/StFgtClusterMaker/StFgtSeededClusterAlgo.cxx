//
//  $Id: StFgtSeededClusterAlgo.cxx,v 1.4 2012/03/06 18:54:28 avossen Exp $
//  $Log: StFgtSeededClusterAlgo.cxx,v $
//  Revision 1.4  2012/03/06 18:54:28  avossen
//  added weighted mean and error to seeded clustering
//
//  Revision 1.3  2012/03/01 16:38:13  avossen
//  implemented tweaks to clustering
//
//  Revision 1.2  2012/02/29 20:29:08  avossen
//  changes to seed and cluster algo
//
//  Revision 1.1  2012/02/28 19:34:29  avossen
//   added new cluster maker
//

// \class StFgtSeededClusterAlgo
// \author Anselm Vossen (avossen@indiana.edu)
//
#include "StFgtSeededClusterAlgo.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"
//for floor
#include <math.h>

StFgtSeededClusterAlgo::StFgtSeededClusterAlgo():mIsInitialized(0),up(true),down(false)
{
  //nothing else to do....
};

Int_t StFgtSeededClusterAlgo::Init()
{
  mIsInitialized=true;
  return kStOk;
};

///fill in cluster info like charge from the strips 
void StFgtSeededClusterAlgo::FillClusterInfo(StFgtHit* cluster)
{
  Double_t ordinate, lowerSpan, upperSpan;
  Short_t disc, quadrant;
  Char_t layer;
  Double_t accuCharge=0;
  Double_t accuChargeSq=0;
  Double_t accuChargeError=0;
  Int_t numStrips=0;
  Double_t meanOrdinate=0;
  Double_t meanSqOrdinate=0;
  Double_t meanGeoId=0;

  stripWeightMap_t &strips = cluster->getStripWeightMap();



  for(stripWeightMap_t::iterator it=strips.begin();it!=strips.end();it++)
    {
      Double_t charge=it->first->getCharge();
      accuCharge+=charge;
      accuChargeSq+=(charge*charge);

      accuChargeError+=it->first->getChargeUncert();
      numStrips++;
      StFgtGeom::getPhysicalCoordinate(it->first->getGeoId(),disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      meanOrdinate+=ordinate*charge;
      meanSqOrdinate+=ordinate*ordinate*charge*charge;
      //      cout <<"charge: " << charge << " ordinate: " << ordinate << " meanSqOrd: " << meanSqOrdinate << endl;
      meanGeoId+=((it->first->getGeoId())*(charge));
    }

  

  stripWeightMap_t::reverse_iterator itBack=strips.rbegin();
  //  if(strips.size()>1)
  //    {
      if(itBack->first->getClusterSeed()==kFgtClusterPart)
	itBack->first->setClusterSeed(kFgtClusterEndUp);
      //      if(strips.begin()->first->getClusterSeed()>=kFgtSeedType1 && strips.begin()->first->getClusterSeed()<=kFgtSeedType3)
      if(strips.begin()->first->getClusterSeed()==kFgtClusterPart)
	(strips.begin())->first->setClusterSeed(kFgtClusterEndDown);
      //    }

  cluster->setCharge(accuCharge);
  numStrips > 1 ? cluster->setChargeUncert(sqrt(accuChargeError/((Double_t)numStrips-1))) : cluster->setChargeUncert(sqrt(accuChargeError/((Double_t)numStrips)));
  meanOrdinate /= (Double_t)accuCharge;
  meanGeoId /= accuCharge;
  meanSqOrdinate /= (Double_t)accuChargeSq;
  //  cout <<" accuCharge: " << accuCharge << " meanOrd now: " << meanOrdinate << " meanSqOrdinate: " << meanSqOrdinate <<endl;
  meanSqOrdinate -= meanOrdinate*meanOrdinate;
  //  cout <<" manOrdinate: " << meanOrdinate <<" error: " << meanSqOrdinate <<endl;

  if( meanSqOrdinate > 0 )
    meanSqOrdinate = sqrt(meanSqOrdinate);
  // meanSqOrdinate is now the st. dev. of the ordinate
  // avoid unreasonable small uncertainty, due to small cluster sizes

  Double_t pitch = ( layer == 'R' ? StFgtGeom::radStrip_pitch() : StFgtGeom::phiStrip_pitch() );
  //  cout <<" pitch is : " <<pitch <<endl;
  //  if( meanSqOrdinate < 2*pitch )
  if( meanSqOrdinate < 0.001 )
    meanSqOrdinate = pitch;
  if(layer=='R')
    {
      cluster->setPositionR(meanOrdinate );
      cluster->setErrorR(meanSqOrdinate);
    }
  else
    {
      cluster->setPositionPhi(meanOrdinate );
      cluster->setErrorPhi(meanSqOrdinate);
    }

  cluster->setCentralStripGeoId(floor(meanGeoId+0.5));

}


Bool_t StFgtSeededClusterAlgo::isSameCluster(StSPtrVecFgtStripIterator itSeed,StSPtrVecFgtStripIterator nextStrip)
{
  Float_t chargeUncert = (*itSeed)->getChargeUncert() > (*nextStrip)->getChargeUncert() ? (*itSeed)->getChargeUncert() : (*nextStrip)->getChargeUncert();

  //  if((*itSeed)->getCharge()  > (*nextStrip)->getCharge() - 2*chargeUncert && (*nextStrip)->getCharge() > 2*(*nextStrip)->getChargeUncert())
  if((*nextStrip)->getCharge() > 2*(*nextStrip)->getChargeUncert())
    return true;
  else
    return false;
}


//	  addStrips2Cluster(newCluster, it, strips.getStripsVec().begin();strip.getStripVec().end(), up);
Int_t StFgtSeededClusterAlgo::addStrips2Cluster(StFgtHit* clus, StSPtrVecFgtStripIterator itSeed, StSPtrVecFgtStripIterator itVecBegin, StSPtrVecFgtStripIterator itVecEnd,Bool_t direction, Int_t sidedSize, Char_t seedLayer)
{
  bool isPhi, isR;
  Short_t disc, quadrant,prvDisc,prvQuad;
  Char_t layer,prvLayer,noLayer='z';
  Double_t ordinate, lowerSpan, upperSpan, prvOrdinate;
  //  cout <<"addint strip to cluster from geo:  " << (*itSeed)->getGeoId();

  Int_t inc=1;
  if(direction==down)
    {
      //      cout <<"dir is down " <<endl;
    inc=(-1);
    }
  isPhi=(layer=='P');
  isR=(!isPhi);
  Bool_t stepTwo=false;
  StSPtrVecFgtStripIterator nextStrip=itSeed+inc;
  Int_t deadStripsSkipped=0;

   while(nextStrip>=itVecBegin && nextStrip <itVecEnd &&(*nextStrip)->getClusterSeed()==kFgtDeadStrip)    {
     //          cout <<"looking now next strip, which is dead: " << (*nextStrip)->getGeoId();
      nextStrip+=inc;
      deadStripsSkipped++;

      //the next printout might lead to a crash...
      // cout <<"now looking at "<< (*nextStrip)->getGeoId()<<endl;
    }

  if(nextStrip >=itVecBegin && nextStrip <itVecEnd)
    {
      //      cout <<"still looking at "<< (*nextStrip)->getGeoId()<<endl;
      StFgtGeom::getPhysicalCoordinate((*nextStrip)->getGeoId(),disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      bool adjacentStrip=((abs((*nextStrip)->getGeoId()-(*itSeed)->getGeoId())<(2+deadStripsSkipped))|| (( abs((*nextStrip)->getGeoId()-(*itSeed)->getGeoId()==(2+deadStripsSkipped)) && stepTwo && isPhi && ((*nextStrip)->getGeoId()%2==0))) &&   seedLayer==layer);
      //if the new strip is adjacent and it seems to belong to the same cluster, add it
      if(adjacentStrip && isSameCluster(itSeed,nextStrip))
	{
	  (*nextStrip)->setClusterSeed(kFgtClusterPart);
	  stripWeightMap_t &stripWeightMap = clus->getStripWeightMap();
	  stripWeightMap[ *nextStrip ] = 1;
	  ///if the last add was successful and the cluster is not too big, go to next one...
	  if(sidedSize+1<=kFgtMaxClusterSize/2)
	    return addStrips2Cluster(clus,nextStrip,itVecBegin,itVecEnd,direction,sidedSize+1,seedLayer);//to return a kFgtClusterTooBig which is discovered downthe line
	  else
	    return kFgtClusterTooBig;
	}
    }
  return true;
}


Int_t StFgtSeededClusterAlgo::doClustering( StFgtStripCollection& strips, StFgtHitCollection& clusters )
{
  //  cout.precision(10);
  //we make use of the fact, that the hits are already sorted by geoId

  strips.sortByGeoId();

  Float_t defaultError = 0.001;
  Short_t disc, quadrant,prvDisc,prvQuad;
  Char_t layer,prvLayer,noLayer='z';
  Double_t ordinate, lowerSpan, upperSpan, prvOrdinate;
  Int_t prvGeoId;
  Double_t accuCharge=0; 
  Double_t accuChargeError=0;
  Double_t meanOrdinate=0;
  Double_t meanSqOrdinate=0;
  Int_t numStrips=0;
  prvLayer=noLayer;
  bool isPhi, isR;
  StFgtHit* newCluster=0;
  //to compute energy weighted strip id
  Double_t meanGeoId=0;
  //for R < R/2 cm the difference in geo id of the phi strips is 2 and only even numbers are used...
  bool stepTwo=false;
  //const 

  Int_t lastGeoIdInCluster=-1;

  for( StSPtrVecFgtStripIterator it=strips.getStripVec().begin();it!=strips.getStripVec().end();++it)
    {
      //the last cluster includes this seed strip
      //      cout <<"last geo id is: " << lastGeoIdInCluster <<endl;
      ///this code only ensures that we don't swallow other seeds, however it does not prevent that a close seed eats into the cluster we are building
      ///but since we check for rising strips, the next seed is a true seed of its own, 
      //so maybe the strip just has to be shared (which it is) because it is eaten by both

      if((*it)->getGeoId()<lastGeoIdInCluster && lastGeoIdInCluster>0)
	continue;
      //found seed for a cluster
      if((*it)->getClusterSeed() >=kFgtSeedType1 && ((*it)->getClusterSeed() <= kFgtSeedType3))
	{


	  ////
	  ///check for ringing around cluster
	  //	  if(ringing( it, strips.getStripVec().begin(),strips.getStripVec().end(), down,0,layer))
	  //	    continue;  //was it
	  StSPtrVecFgtStripIterator firstStrip=it-(int)(floor(kFgtMaxClusterSize/2)+kFgtNumAdditionalStrips);
	  StSPtrVecFgtStripIterator lastStrip=it+(int)(floor(kFgtMaxClusterSize/2)+kFgtNumAdditionalStrips);
	  if(firstStrip<strips.getStripVec().begin())
	    firstStrip=strips.getStripVec().begin();
	  Int_t stripsW_Charge=0;
	  Int_t stripsWO_Charge=0;


	  //compare with energy in cluster
	  //	  cout << " looking around " << (*it)->getGeoId() << ": " << (*firstStrip)->getGeoId() <<" to something... " <<endl;
	  for(StSPtrVecFgtStripIterator it2=firstStrip;(it2!=strips.getStripVec().end())&&(it2<=lastStrip);it2++)
	    {
	      if((*it2)->getClusterSeed()!=kFgtDeadStrip)
		{
		  if((*it2)->getCharge()>2*(*it2)->getChargeUncert())
		    {
		      //		      cout <<"   strip: " << (*it2)->getGeoId() << " has high charge " <<endl;
		      stripsW_Charge++;
		    }
		  else
		    {
		      stripsWO_Charge++;
		      //      cout <<"   strip: " << (*it2)->getGeoId() << " does not " <<endl;
		    }
		}
	    }
	  //	  cout<<" high charge strips: " << stripsW_Charge <<" low: " << stripsWO_Charge<<endl;
	  if(stripsW_Charge>stripsWO_Charge)
	    {
	      (*it)->setClusterSeed(kFgtClusterSeedInSeaOfNoise);
	      continue;
	    }

	  /////


	  StFgtGeom::getPhysicalCoordinate((*it)->getGeoId(),disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
	  isPhi=(layer=='P');
	  isR=(!isPhi);
	  newCluster=new StFgtHit(clusters.getHitVec().size(),meanGeoId,accuCharge, disc, quadrant, layer, ordinate, defaultError,ordinate, defaultError,0.0,0.0);
	  stripWeightMap_t &stripWeightMap = newCluster->getStripWeightMap();
	  stripWeightMap[ *it ] = 1;
	  //add strips to cluster going down
	  if(addStrips2Cluster(newCluster, it, strips.getStripVec().begin(),strips.getStripVec().end(), down,0,layer)==kFgtClusterTooBig)
	    {
	      //	      cout <<"cluster too big!, begin at: " << newCluster->getStripWeightMap().begin()->first->getGeoId()<<endl;
	      //reset strips
	     for(stripWeightMap_t::iterator it=newCluster->getStripWeightMap().begin();it!=newCluster->getStripWeightMap().end();it++)
	       {
		 it->first->setClusterSeed(kFgtSeedTypeNo);
	       }
	      delete newCluster;
	      continue;
	    }
	  //add strips to cluster going up
	  if(addStrips2Cluster(newCluster, it, strips.getStripVec().begin(),strips.getStripVec().end(), up,0,layer)==kFgtClusterTooBig)
	    {
	     for(stripWeightMap_t::iterator it=newCluster->getStripWeightMap().begin();it!=newCluster->getStripWeightMap().end();it++)
	       {
		 it->first->setClusterSeed(kFgtSeedTypeNo);
	       }
	      delete newCluster;
	      continue;
	    }


	  FillClusterInfo(newCluster);
	  clusters.getHitVec().push_back(newCluster);
	  //now of course we have to check where the cluster ends so that we don't start another cluster if there is another seed
	  lastGeoIdInCluster=newCluster->getStripWeightMap().rbegin()->first->getGeoId();
	}
    }
  return kStOk;
}

//Bool_t StFgtSeededClusterAlgo::up;
//Bool_t StFgtSeededClusterAlgo::down;

ClassImp(StFgtSeededClusterAlgo);
