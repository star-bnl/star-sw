//
//  $Id: StFgtMaxClusterAlgo.cxx,v 1.8 2011/11/02 18:44:45 sgliske Exp $
//  $Log: StFgtMaxClusterAlgo.cxx,v $
//  Revision 1.8  2011/11/02 18:44:45  sgliske
//  updated for changed StFgtHit constructor:
//  changed saving central strip ptr to geoId in StFgtHit
//
//  Revision 1.7  2011/11/01 18:46:30  sgliske
//  Updated to correspond with StEvent containers, take 2.
//
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

#include "Stypes.h"
#include "StFgtMaxClusterAlgo.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include <iostream>

#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"

StFgtMaxClusterAlgo::StFgtMaxClusterAlgo():mIsInitialized(0)
{
  //nothing else to do....
};

Int_t StFgtMaxClusterAlgo::Init()
{
  mIsInitialized=true;
  return kStOk;
};


Int_t StFgtMaxClusterAlgo::doClustering( StFgtStripCollection& strips, StFgtHitCollection& clusters )
{

  //we make use of the fact, that the hits are already sorted by geoId
  strips.sortByGeoId();

  Float_t defaultError = 0.001;

  Short_t disc, quadrant;
  Char_t layer;
  Double_t ordinate, lowerSpan, upperSpan;
  Double_t maxRCharge=0;
  Double_t maxPhiCharge=0;
  Double_t phiOrdinate,rOrdinate;
  Int_t phiGeoId, rGeoId;
  bool isPhi, isR;
  //  cout <<"we have " << mSortPtr.size() << " points " <<endl;
  StFgtStrip *stripPtr = 0;

  for( StSPtrVecFgtStripIterator it=strips.getStripVec().begin();it!=strips.getStripVec().end();++it)
    {
      StFgtGeom::getPhysicalCoordinate((*it)->getGeoId(),disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      isPhi=(layer=='P');
      isR=(!isPhi);
      //      cout <<"charge is: " << (*it)->getCharge() <<  " maxRcharge: " <<maxRCharge <<" maxPhicharge: " << maxPhiCharge << " isPhi: "<< isPhi <<endl;
      if(isR && ((*it)->getCharge() > maxRCharge))
	{
	  maxRCharge=(*it)->getCharge();
	  rOrdinate=ordinate;
	  rGeoId=(*it)->getGeoId();
          stripPtr = *it;
	}
      if(isPhi && ((*it)->getCharge() > maxPhiCharge))
	{
	  maxPhiCharge=(*it)->getCharge();
	  phiOrdinate=ordinate;
	  phiGeoId=(*it)->getGeoId();
          stripPtr = *it;
	}
    }

  StFgtHit *hit = 0;
  if(maxRCharge>0)
    {
      //      cout <<"have maxR" <<endl;
       hit = new StFgtHit( disc, quadrant, 'R', rOrdinate, defaultError, maxRCharge, rGeoId, rGeoId );
    } 
  if(maxPhiCharge>0)
    {
       hit = new StFgtHit( disc, quadrant, 'P', rOrdinate, defaultError, maxRCharge, phiGeoId, phiGeoId );
      //      cout <<" new phi cluster " << endl;
    } 

  if( hit ){
     clusters.getHitVec().push_back( hit );
     stripWeightMap_t &stripWeightMap = hit->getStripWeightMap();
     stripWeightMap[ stripPtr ] = 1;
  };

  return kStOk;
};


ClassImp(StFgtMaxClusterAlgo);

