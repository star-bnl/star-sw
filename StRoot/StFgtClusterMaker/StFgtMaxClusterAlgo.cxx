//
//  $Id: StFgtMaxClusterAlgo.cxx,v 1.16 2013/02/20 01:32:27 avossen Exp $
//  $Log: StFgtMaxClusterAlgo.cxx,v $
//  Revision 1.16  2013/02/20 01:32:27  avossen
//  added n strips before and after cluster
//
//  Revision 1.15  2013/02/19 18:29:05  avossen
//  signature of max cluster algo now updated to conform with interface
//
//  Revision 1.14  2012/03/08 17:43:40  avossen
//  added default cluster algo, made StFgtIClusterAlgo destructor =0
//
//  Revision 1.13  2012/03/07 03:57:23  avossen
//  various updates
//
//  Revision 1.12  2012/02/28 19:32:25  avossen
//  many changes to enable new clustering algo: New strip fields, identification of seed strips, passing neighboring strips, new order in strip collections
//
//  Revision 1.11  2011/11/17 19:23:54  ckriley
//  fixed small bug
//
//  Revision 1.10  2011/11/03 20:04:17  avossen
//  updated clustering makers and algos to reflect new containers
//
//  Revision 1.9  2011/11/03 14:59:49  sgliske
//  Error estimate set to twice the pitch
//
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
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtDbMaker/StFgtDb.h"

StFgtMaxClusterAlgo::StFgtMaxClusterAlgo()
{
  //nothing else to do....
};

Int_t StFgtMaxClusterAlgo::Init()
{
  return kStOk;
};


///algo just loops over the strips and selects the one with the highest charge in each layer
Int_t StFgtMaxClusterAlgo::doClustering(const StFgtCollection& fgtCollection, StFgtStripCollection& strips, StFgtHitCollection& clusters )
{

  //we make use of the fact, that the hits are already sorted by geoId
  strips.sortByGeoId();

  Short_t disc, quadrant;
  Char_t layer;
  Double_t ordinate, lowerSpan, upperSpan;
  Double_t maxRCharge=0;
  Double_t maxPhiCharge=0;
  Double_t phiOrdinate,rOrdinate;
  Int_t phiGeoId, rGeoId;
  bool isPhi, isR;
  //  cout <<"we have " << mSortPtr.size() << " points " <<endl;
  StFgtStrip *rStripPtr = 0;
  StFgtStrip *phiStripPtr = 0;

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
          rStripPtr = *it;
	}
      if(isPhi && ((*it)->getCharge() > maxPhiCharge))
	{
	  maxPhiCharge=(*it)->getCharge();
	  phiOrdinate=ordinate;
	  phiGeoId=(*it)->getGeoId();
          phiStripPtr = *it;
	}
    }


  StFgtHit *rHit = 0;
  StFgtHit *phiHit = 0;
  if(maxRCharge>0)
    {
      //      cout <<"have maxR" <<endl;
      rHit = new StFgtHit( clusters.getHitVec().size(),rGeoId,maxRCharge, disc, quadrant, 'R', rOrdinate, 2*StFgtGeom::radStrip_pitch(), 0.0, 0.0, 0.0, 0.0);
    } 
  if( rHit ){
     clusters.getHitVec().push_back( rHit );
     stripWeightMap_t &stripWeightMap = rHit->getStripWeightMap();
     stripWeightMap[ rStripPtr ] = 1;

  };

  if(maxPhiCharge>0)
    {
      phiHit = new StFgtHit( clusters.getHitVec().size(), phiGeoId,maxPhiCharge, disc, quadrant, 'P', 0.0, 0.0,phiOrdinate, 2*StFgtGeom::phiStrip_pitch(),0.0,0.0);
      //      cout <<" new phi cluster " << endl;
    } 
  if(phiHit ){
     clusters.getHitVec().push_back( phiHit );
     stripWeightMap_t &stripWeightMap = phiHit->getStripWeightMap();
     stripWeightMap[ phiStripPtr ] = 1;
  };


  return kStOk;
};

StFgtMaxClusterAlgo::~StFgtMaxClusterAlgo()
{
}

ClassImp(StFgtMaxClusterAlgo);

