//
//  $Id: StFgtSeededClusterAlgo.cxx,v 1.18 2012/06/14 04:43:03 avossen Exp $
//  $Log: StFgtSeededClusterAlgo.cxx,v $
//  Revision 1.18  2012/06/14 04:43:03  avossen
//  fixed geoId bug
//
//  Revision 1.17  2012/06/12 19:28:48  avossen
//  *** empty log message ***
//
//  Revision 1.16  2012/06/10 01:05:19  avossen
//  *** empty log message ***
//
//  Revision 1.15  2012/06/09 18:03:50  avossen
//  fixed bug
//
//  Revision 1.14  2012/06/08 20:22:21  avossen
//  updated cluster algo to find even p clusters
//
//  Revision 1.13  2012/06/08 03:52:41  avossen
//  added one sigma strips to clusters
//
//  Revision 1.12  2012/03/17 23:30:16  avossen
//  fudged the cluster maker
//
//  Revision 1.11  2012/03/16 19:50:18  balewski
//  *** empty log message ***
//
//  Revision 1.10  2012/03/16 19:49:46  avossen
//  *** empty log message ***
//
//  Revision 1.9  2012/03/16 19:43:19  avossen
//  added option to allow to jump strips
//
//  Revision 1.8  2012/03/16 19:41:15  avossen
//  added option to allow to jump strips
//
//  Revision 1.7  2012/03/08 17:43:40  avossen
//  added default cluster algo, made StFgtIClusterAlgo destructor =0
//
//  Revision 1.6  2012/03/07 18:07:45  sgliske
//  StFgtStrip::getClusterSeed() -> StFgtStrip::getClusterSeedType
//  StFgtStrip::setClusterSeed() -> StFgtStrip::setClusterSeedType
//
//  Revision 1.5  2012/03/07 03:57:23  avossen
//  various updates
//
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

StFgtSeededClusterAlgo::StFgtSeededClusterAlgo():up(true),down(false),stepTwo(false)
{
  //nothing else to do....
};

Int_t StFgtSeededClusterAlgo::Init()
{
  return kStOk;
};

///fill in cluster info like charge from the strips 
void StFgtSeededClusterAlgo::FillClusterInfo(StFgtHit* cluster)
{
  Double_t ordinate, lowerSpan, upperSpan;
  Short_t disc, quadrant;
  Char_t layer;
  Double_t accuCharge=0;
  Double_t accuChargeWeight=0;
  Double_t accuChargeEven=0;
  Double_t accuChargeUneven=0;
  Double_t accuChargeSq=0;
  Double_t accuChargeSqWeight=0;
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
      accuChargeWeight+=charge;
      if(it->first->getGeoId()%2)
	accuChargeUneven+=charge;
      else
	accuChargeEven+=charge;

      accuChargeSq+=(charge*charge);
      accuChargeError+=it->first->getChargeUncert();
      numStrips++;
      StFgtGeom::getPhysicalCoordinate(it->first->getGeoId(),disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      meanOrdinate+=ordinate*charge;
      meanSqOrdinate+=ordinate*ordinate*charge*charge;
      //      cout <<"charge: " << charge << " ordinate: " << ordinate << " meanSqOrd: " << meanSqOrdinate << endl;
      meanGeoId+=((it->first->getGeoId())*(charge));

    }

  if(((accuChargeEven-accuChargeUneven)/accuCharge)>0.75&& layer=='P')
    {
      //            cout <<"r probably < 20"<<endl;
      //r is probably in r<20
      accuCharge-=accuChargeUneven;
      stripWeightMap_t::iterator it=strips.begin();
      while(it!=strips.end())
	{
	  if(it->first->getGeoId()%2)
	    {
	      strips.erase(it);
	      if(!strips.empty())
		 it=strips.begin();
	      else
		break;
	    }
	  else
	    it++;
	}
      //      cout <<"done erasing"<<endl;
    }

  stripWeightMap_t::reverse_iterator itBack=strips.rbegin();
  //  if(strips.size()>1)
  //    {
      if(itBack->first->getClusterSeedType()==kFgtClusterPart)
	itBack->first->setClusterSeedType(kFgtClusterEndUp);
      //      if(strips.begin()->first->getClusterSeedType()>=kFgtSeedType1 && strips.begin()->first->getClusterSeedType()<=kFgtSeedType3)
      if(strips.begin()->first->getClusterSeedType()==kFgtClusterPart)
	(strips.begin())->first->setClusterSeedType(kFgtClusterEndDown);
      //    }

  cluster->setCharge(accuCharge);
  numStrips > 1 ? cluster->setChargeUncert(sqrt(accuChargeError/((Double_t)numStrips-1))) : cluster->setChargeUncert(sqrt(accuChargeError/((Double_t)numStrips)));
  meanOrdinate /= (Double_t)accuChargeWeight;
  meanGeoId /= accuChargeWeight;
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

///function to check if the strip belongs to the cluster. If it returns false we stop adding strips to the cluster
Bool_t StFgtSeededClusterAlgo::isSameCluster(StSPtrVecFgtStripIterator itSeed,StSPtrVecFgtStripIterator nextStrip)
{
  //  Float_t chargeUncert = (*itSeed)->getChargeUncert() > (*nextStrip)->getChargeUncert() ? (*itSeed)->getChargeUncert() : (*nextStrip)->getChargeUncert();
  //  if((*itSeed)->getCharge()  > (*nextStrip)->getCharge() - 2*chargeUncert && (*nextStrip)->getCharge() > 2*(*nextStrip)->getChargeUncert())
  if((*nextStrip)->getCharge() > 1*(*nextStrip)->getChargeUncert())
    return true;
  else
    return false;
}

///function to add strips to clusters, used recursively
Int_t StFgtSeededClusterAlgo::addStrips2Cluster(StFgtHit* clus, StSPtrVecFgtStripIterator itSeed, StSPtrVecFgtStripIterator itVecBegin, StSPtrVecFgtStripIterator itVecEnd,Bool_t direction, Int_t sidedSize, Char_t seedLayer)
{
  bool isPhi, isR;
  Short_t disc, quadrant;
  //,noLayer='z';
    Char_t layer;
  Double_t ordinate, lowerSpan, upperSpan;
  //  cout <<"addint strip to cluster from geo:  " << (*itSeed)->getGeoId();

  Int_t inc=1;
  if(direction==down)
    {
      //      cout <<"dir is down " <<endl;
    inc=(-1);
    }
  //  cout <<"layer: " << seedLayer<<endl;
  isPhi=(seedLayer=='P');
  isR=(!isPhi);

  StSPtrVecFgtStripIterator nextStrip=itSeed+inc;
  Int_t deadStripsSkipped=0;

   while(nextStrip>=itVecBegin && nextStrip <itVecEnd &&(*nextStrip)->getClusterSeedType()==kFgtDeadStrip)    {
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
      //      if(isPhi)
	//	cout <<"stepTwo: " << stepTwo << " nextStripID: " << (*nextStrip)->getGeoId() << " seedLayer: " << seedLayer <<" layer: " << layer <<endl;
      bool adjacentStrip=((abs((*nextStrip)->getGeoId()-(*itSeed)->getGeoId()))<(2+deadStripsSkipped));
      //      cout <<"looking at "<< (*nextStrip)->getGeoId()<< " adjacent: " << adjacentStrip <<endl;
      bool adjacentStrip2=false;

      if(adjacentStrip && !isSameCluster(itSeed,nextStrip))
	{
	  //	  cout <<" not no adjacent Strip" <<endl;
	  if((nextStrip+inc) >=itVecBegin && (nextStrip+inc <itVecEnd))
	    {
	      //	      cout <<"geoid of next next: " << (*(nextStrip+inc))->getGeoId() <<" of seed: " << (*itSeed)->getGeoId() <<" deadStrips skipped: " << deadStripsSkipped<<endl;
	      //  cout <<"diff in geo: " <<(abs((*nextStrip+inc)->getGeoId()-(*itSeed)->getGeoId()==(2+deadStripsSkipped)))<< endl;
	      adjacentStrip2=((abs((*(nextStrip+inc))->getGeoId()-(*itSeed)->getGeoId()==(2+deadStripsSkipped))) && stepTwo && isPhi && ((*(nextStrip+inc))->getGeoId()%2==0) &&   seedLayer==layer);
	    }
	}
      if(adjacentStrip2)
	{
	  //	  cout <<"jump Strip" <<endl;
	  nextStrip=nextStrip+inc;//jump strip
	}


      //if the new strip is adjacent and it seems to belong to the same cluster, add it
      if((adjacentStrip2|| adjacentStrip) && isSameCluster(itSeed,nextStrip))
	{
	  (*nextStrip)->setClusterSeedType(kFgtClusterPart);
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

/**
main interface to the clustering. 
 */
Int_t StFgtSeededClusterAlgo::doClustering( StFgtStripCollection& strips, StFgtHitCollection& clusters )
{
  //  cout.precision(10);
  //we make use of the fact, that the hits are already sorted by geoId
  strips.sortByGeoId();
  Float_t defaultError = 0.001;
  Short_t disc, quadrant;
//,noLayer='z';
  Char_t layer;
  Double_t ordinate, lowerSpan, upperSpan;//, prvOrdinate;
  Double_t accuCharge=0; 
  bool isPhi, isR;
  StFgtHit* newCluster=0;
  //to compute energy weighted strip id
  Double_t meanGeoId=0;
  //for R < R/2 cm the difference in geo id of the phi strips is 2 and only even numbers are used...
  //const 

  /**
run over all strips, find seeds, use those to start clusters
   */
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
      if((*it)->getClusterSeedType() >=kFgtSeedType1 && ((*it)->getClusterSeedType() <= kFgtSeedType3))
	{
	  ////
	  ///check for ringing around cluster
	  //	  if(ringing( it, strips.getStripVec().begin(),strips.getStripVec().end(), down,0,layer))
	  //	    continue;  //was it
	  ///this might not be such a good idea since it is not taking into account geo ids. If we run zs, this will not work
	  int searchRange=(int)(floor(kFgtMaxClusterSize/2)+kFgtNumAdditionalStrips);
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
	      if(fabs((*it)->getGeoId()-(*it2)->getGeoId())<searchRange)
		{
		  if((*it2)->getClusterSeedType()!=kFgtDeadStrip)
		    {
		      if((*it2)->getCharge()>2*(*it2)->getChargeUncert())
			{
			  //		      cout <<"   strip: " << (*it2)->getGeoId() << " has high charge " <<endl;
			  stripsW_Charge++;
			}
		    }
		}
	    }
	  //	  cout<<" high charge strips: " << stripsW_Charge <<" low: " << stripsWO_Charge<<endl;
	  //if more than half the strips have charge
	  if(stripsW_Charge>searchRange && stripsW_Charge > kFgtMaxClusterSize)
	    {
	      (*it)->setClusterSeedType(kFgtClusterSeedInSeaOfNoise);
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
	  //the function is recursive, stops if the condition that the next strip 
	  //belongs to the cluster is not met anymore, 
	  //big clusters return clusterToBig, then we erase the seed because ist was probably noise
	  if(addStrips2Cluster(newCluster, it, strips.getStripVec().begin(),strips.getStripVec().end(), down,0,layer)==kFgtClusterTooBig)
	    {
	      //	      cout <<"cluster too big!, begin at: " << newCluster->getStripWeightMap().begin()->first->getGeoId()<<endl;
	      //reset strips
	     for(stripWeightMap_t::iterator it=newCluster->getStripWeightMap().begin();it!=newCluster->getStripWeightMap().end();it++)
	       {
		 it->first->setClusterSeedType(kFgtSeedTypeNo);
	       }
	      delete newCluster;
	      continue;
	    }
	  //add strips to cluster going up
	  if(addStrips2Cluster(newCluster, it, strips.getStripVec().begin(),strips.getStripVec().end(), up,0,layer)==kFgtClusterTooBig)
	    {
	     for(stripWeightMap_t::iterator it=newCluster->getStripWeightMap().begin();it!=newCluster->getStripWeightMap().end();it++)
	       {
		 it->first->setClusterSeedType(kFgtSeedTypeNo);
	       }
	      delete newCluster;
	      continue;
	    }
	  //
	  //compute errors etc
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

StFgtSeededClusterAlgo::~StFgtSeededClusterAlgo()
{
}

ClassImp(StFgtSeededClusterAlgo);
