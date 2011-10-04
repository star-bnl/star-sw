//
//  $Id
//  $Log
//
// \class StFgtMaxClusterAlgo
// \author Anselm Vossen (avossen@indiana.edu)
//

#include "StFgtMaxClusterAlgo.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include <iostream>

StFgtMaxClusterAlgo::StFgtMaxClusterAlgo():mIsInitialized(0)
{
  //nothing else to do....
};

Int_t StFgtMaxClusterAlgo::Init()
{
  mIsInitialized=true;
};

Int_t StFgtMaxClusterAlgo::doClustering(StFgtRawHitArray& hits, StFgtClusterArray& clusters)
{
  cout <<"doing max clustering, we look at " << hits.getEntries() << " hits " << endl;
  //we make use of the fact, that the hits are already sorted by geoId
  StFgtRawHitConstPtrVec mSortPtr;
  hits.getSortedPtrVec(mSortPtr);
  Short_t disc, quadrant;
  Char_t layer;
  Double_t ordinate, lowerSpan, upperSpan;
  Double_t maxRCharge=0;
  Double_t maxPhiCharge=0;
  Double_t phiOrdinate,rOrdinate;
  Int_t phiGeoId, rGeoId;
  bool isPhi, isR;


  for(StFgtRawHitConstPtrVec::iterator it=mSortPtr.begin();it!=mSortPtr.end();it++)
    {
      StFgtGeom::getPhysicalCoordinate((*it)->getGeoId(),disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      isPhi=(layer=='P');
      isR=(!isPhi);


      if(isR && ((*it)->getCharge() > maxRCharge))
	{
	  maxRCharge=(*it)->getCharge();
	  rOrdinate=ordinate;
	  rGeoId=(*it)->getGeoId();
	}
      if(isPhi && ((*it)->getCharge() > maxPhiCharge))
	{
	  maxPhiCharge=(*it)->getCharge();
	  phiOrdinate=ordinate;
	  phiGeoId=(*it)->getGeoId();
	}
    }

  if(maxRCharge>0)
    {
      StFgtCluster newCluster;
      //new cluster was started but not included yet..
      newCluster.setLayer('R');
      newCluster.setKey(rGeoId);
      newCluster.setPosition(rOrdinate);
      newCluster.setCharge(maxRCharge);
      clusters.pushBack(newCluster);
    } 
  if(maxPhiCharge>0)
    {
      StFgtCluster newCluster;
      //new cluster was started but not included yet..
      newCluster.setLayer('P');
      newCluster.setKey(phiGeoId);
      newCluster.setPosition(phiOrdinate);
      newCluster.setCharge(maxPhiCharge);
      clusters.pushBack(newCluster);
    } 

  return kStOk;
};




ClassImp(StFgtMaxClusterAlgo);
