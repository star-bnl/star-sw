//
//  $Id
//  $Log
//
// \class StFgtSimpleClusterAlgo
// \author Anselm Vossen (avossen@indiana.edu)
//

#include "StFgtSimpleClusterAlgo.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

StFgtSimpleClusterAlgo::StFgtSimpleClusterAlgo():mIsInitialized(0)
{
  //nothing else to do....
};

Int_t StFgtSimpleClusterAlgo::Init()
{
  mIsInitialized=true;
};

Int_t StFgtSimpleClusterAlgo::doClustering(StFgtRawHitArray& hits, StFgtClusterArray& clusters)
{
  const Char_t noLayer='N';
  //we make use of the fact, that the hits are already sorted by geoId
  StFgtRawHitConstPtrVec mSortPtr;
  hits.getSortedPtrVec(mSortPtr);
  Short_t disc, quadrant,prvDisc,prvQuad;
  Char_t layer,prvLayer;
  Double_t ordinate, lowerSpan, upperSpan, prvOrdinate, prvLowerSpan, prvUpperSpan;
  Int_t prvGeoId;
  Double_t accuCharge=0; 
  Double_t meanOrdinate=0;
  Int_t numStrips=0;
  bool lookForNewCluster=true;
  prvLayer=noLayer;
  bool isPhi, isR;

  StFgtCluster* newCluster=0;
  if(mSortPtr.size()!=0)
    newCluster=new StFgtCluster();



  for(StFgtRawHitConstPtrVec::iterator it=mSortPtr.begin();it!=mSortPtr.end();it++)
    {
      StFgtGeom::getPhysicalCoordinate((*it)->getGeoId(),disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      isPhi=(layer=='P');
      isR=(!isPhi);

      bool adjacentStrips=(((abs(prvOrdinate-ordinate)<StFgtGeom::kFgtPhiAnglePitch) &&isPhi)|| ((abs(prvOrdinate-ordinate)<StFgtGeom::kFgtRadPitch) && isR));
      //if the strip is adjacent to the last one or if we are looking at the first strip in the list
      if((layer==prvLayer && adjacentStrips)||prvLayer==noLayer) 
	{
	  //should really be charge...
	  accuCharge+=(*it)->getCharge();  
	  meanOrdinate+=ordinate;
	  numStrips++;
	  newCluster->pushBack((*it)->getGeoId());
	}
      else
	{
	  //make sure that we are not in the beginning
	  //set charge, push back cluster, start new one
	  if(prvLayer!=noLayer)
	    {
	      //set layer etc of cluster
	      newCluster->setLayer(prvLayer);
	      newCluster->setKey(prvGeoId);
	      newCluster->setCharge(accuCharge);
	      newCluster->setPosition(meanOrdinate/numStrips);
	      clusters.pushBack(*newCluster);
	      //
	      delete newCluster;
	      accuCharge=(*it)->getCharge();
	      meanOrdinate=ordinate;
	      numStrips=1;
	      newCluster=new StFgtCluster((*it)->getGeoId(),layer,ordinate,accuCharge);

	      //add the current stuff
	      newCluster->pushBack((*it)->getGeoId());
	      prvLayer=layer;
	      prvGeoId=(*it)->getGeoId();
	      prvDisc=disc;
	      prvQuad=quadrant;
	      prvOrdinate=ordinate;
	      prvLowerSpan=lowerSpan;
	      prvUpperSpan=upperSpan;
	    }
	}
    }


  if(newCluster)
    {
      //new cluster was started but not included yet..
      newCluster->setLayer(prvLayer);
      newCluster->setKey(prvGeoId);
      newCluster->setPosition(meanOrdinate/numStrips);
      newCluster->setCharge(accuCharge);
      clusters.pushBack(*newCluster);
      delete newCluster;
    }

  return kStOk;
};


//sort hits according to geoId. This function is probably not needed anymore since the hits are sorted by geoId to start with
bool StFgtSimpleClusterAlgo::sortHits(StFgtRawHit first, StFgtRawHit second)
{
  return (first.getGeoId()< second.getGeoId());

}

ClassImp(StFgtSimpleClusterAlgo);
