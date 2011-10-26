//
//  $Id: StFgtSimpleClusterAlgo.cxx,v 1.10 2011/10/26 20:56:50 avossen Exp $
//  $Log: StFgtSimpleClusterAlgo.cxx,v $
//  Revision 1.10  2011/10/26 20:56:50  avossen
//  use geoIds to determine if two strips are adjacent
//
//  Revision 1.9  2011/10/14 18:45:27  avossen
//  fixed some bugs in simple cluster algo
//
//  Revision 1.8  2011/10/13 20:35:22  balewski
//  cleanup, added missing return value
//
//  Revision 1.7  2011/10/10 20:35:08  avossen
//  fixed strip-cluster association in MaxCluster algo, made other files cvs compliant
//
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
  return 0; // ?,jan
};

Int_t StFgtSimpleClusterAlgo::doClustering(StFgtRawHitArray& hits, StFgtClusterArray& clusters)
{
  const Char_t noLayer='N';
  //we make use of the fact, that the hits are already sorted by geoId
  StFgtRawHitConstPtrVec mSortPtr;
  hits.getSortedPtrVec(mSortPtr);
  Short_t disc, quadrant,prvDisc,prvQuad;
  Char_t layer,prvLayer;
  Double_t ordinate, lowerSpan, upperSpan, prvOrdinate;
  Int_t prvGeoId;
  Double_t accuCharge=0; 
  Double_t meanOrdinate=0;
  Int_t numStrips=0;
  bool lookForNewCluster=true;
  prvLayer=noLayer;
  bool isPhi, isR;

  StFgtCluster* newCluster=0;




  for(StFgtRawHitConstPtrVec::iterator it=mSortPtr.begin();it!=mSortPtr.end();it++)
    {
      StFgtGeom::getPhysicalCoordinate((*it)->getGeoId(),disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      isPhi=(layer=='P');
      isR=(!isPhi);

      if(prvLayer==noLayer)//first hit
	{
	  accuCharge=(*it)->getAdc();  
	  newCluster=new StFgtCluster((*it)->getGeoId(),layer,ordinate,accuCharge);
	  newCluster->pushBack((*it)->getGeoId());
	  meanOrdinate=ordinate;
	  prvLayer=layer;
	  prvGeoId=(*it)->getGeoId();
	  numStrips=1;
	  prvOrdinate=ordinate;
	  //go to next hit
	  continue;
	}

      //      bool adjacentStrips=(((abs(prvOrdinate-ordinate)<StFgtGeom::kFgtPhiAnglePitch) &&isPhi)|| ((abs(prvOrdinate-ordinate)<StFgtGeom::kFgtRadPitch) && isR));
      bool adjacentStrips=(abs(prvGeoId-(*it)->getGeoId())<2);

      //if the strip is adjacent to the last one? Then we add it to the cluster
      if((layer==prvLayer && adjacentStrips)&& prvLayer!=noLayer) 
	{
	  //should really be charge...
	  //accuCharge+=(*it)->getCharge();  
	  accuCharge+=(*it)->getAdc();
	  meanOrdinate+=ordinate;
	  numStrips++;
	  newCluster->pushBack((*it)->getGeoId());
	  prvLayer=layer;
	  prvGeoId=(*it)->getGeoId();
	  prvOrdinate=ordinate;
	  continue;
	}
      else
	{//we are looking at a new cluster because we are not at the beginning and the new strip is not adjacent to the old one
	  //set charge, push back cluster, start new one
	  //set layer etc of cluster
	  newCluster->setLayer(prvLayer);
	  newCluster->setKey(prvGeoId);
	  newCluster->setCharge(accuCharge);
	  newCluster->setPosition(meanOrdinate/numStrips);
	  if(numStrips<=10)
	    clusters.pushBack(*newCluster);
	  //	      cout <<"cluster has size: " << numStrips <<endl;
	  //
	  delete newCluster;
	  //	      	      accuCharge=(*it)->getCharge();
	  accuCharge=(*it)->getAdc();
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
	}
    }

  //if there has been any 1+ clusters, we have to add the last cluster to the list
  if(newCluster)
    {
      //new cluster was started but not included yet..
      newCluster->setLayer(prvLayer);
      newCluster->setKey(prvGeoId);
      newCluster->setPosition(meanOrdinate/numStrips);
      newCluster->setCharge(accuCharge);
      if(numStrips<=10)
	clusters.pushBack(*newCluster);
      //      cout <<"cluster has size: " << numStrips <<endl;
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
