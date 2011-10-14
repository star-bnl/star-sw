//
//  $Id: StFgtMaxClusterAlgo.cxx,v 1.6 2011/10/14 18:45:27 avossen Exp $
//  $Log: StFgtMaxClusterAlgo.cxx,v $
//  Revision 1.6  2011/10/14 18:45:27  avossen
//  fixed some bugs in simple cluster algo
//
//  Revision 1.5  2011/10/10 20:35:08  avossen
//  fixed strip-cluster association in MaxCluster algo, made other files cvs compliant
//
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
  return kStOk;
};

Int_t StFgtMaxClusterAlgo::doClustering(StFgtRawHitArray& hits, StFgtClusterArray& clusters)
{

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
  //  cout <<"we have " << mSortPtr.size() << " points " <<endl;
  for(StFgtRawHitConstPtrVec::iterator it=mSortPtr.begin();it!=mSortPtr.end();it++)
    {
      StFgtGeom::getPhysicalCoordinate((*it)->getGeoId(),disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      isPhi=(layer=='P');
      isR=(!isPhi);
      //      cout <<"charge is: " << (*it)->getAdc() <<  " maxRcharge: " <<maxRCharge <<" maxPhicharge: " << maxPhiCharge << " isPhi: "<< isPhi <<endl;
      if(isR && ((*it)->getAdc() > maxRCharge))
	{
	  maxRCharge=(*it)->getAdc();
	  rOrdinate=ordinate;
	  rGeoId=(*it)->getGeoId();
	}
      if(isPhi && ((*it)->getAdc() > maxPhiCharge))
	{
	  maxPhiCharge=(*it)->getAdc();
	  phiOrdinate=ordinate;
	  phiGeoId=(*it)->getGeoId();
	}
    }

  if(maxRCharge>0)
    {
      //      cout <<"have maxR" <<endl;
      StFgtCluster newCluster(rGeoId,'R',rOrdinate,maxRCharge);
      newCluster.pushBack(rGeoId);
      //new cluster was started but not included yet..
      //      cout <<"1" <<e ndl;
      //      newCluster.setLayer('R');
      //      newCluster.setKey(rGeoId);
      //      newCluster.setPosition(rOrdinate);
      //      newCluster.setCharge(maxRCharge);
      //      cout <<" new r cluster " << endl;
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
      newCluster.pushBack(phiGeoId);
      clusters.pushBack(newCluster);
      //      cout <<" new phi cluster " << endl;
    } 

  return kStOk;
};


ClassImp(StFgtMaxClusterAlgo);

