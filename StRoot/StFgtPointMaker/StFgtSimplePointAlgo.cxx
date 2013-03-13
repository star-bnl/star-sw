
#include "StFgtSimplePointAlgo.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StEvent/StFgtHit.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtPoint.h"
#include "StRoot/StEvent/StFgtPointCollection.h"
#include "StRoot/St_base/StMessMgr.h"



StFgtSimplePointAlgo::StFgtSimplePointAlgo():StFgtIPointAlgo("simple point algo","simple point algo"),mIsInitialized(0),m_maxChargeAsymmetry(0.1)
{
  //nothing else to do....
};

StFgtSimplePointAlgo::~StFgtSimplePointAlgo()
{

  //nothing to do...
};

Int_t StFgtSimplePointAlgo::Init()
{
  mIsInitialized=true;
  return kStOk;
};



//Int_t StFgtSimplePointAlgo::makePoints( const StFgtHitCollection& clusters,  StFgtPointCollection& points)
Int_t StFgtSimplePointAlgo::makePoints( StFgtCollection& c)
{
  Int_t iErr=kStOk;

  StFgtPointCollection* pPointCollection=c.getPointCollection();
  if( !pPointCollection ){
    LOG_ERROR << "$Id: StFgtSimplePointAlgo.cxx,v 1.1 2013/03/13 20:36:29 jeromel Exp $ Error getting pointer to StFgtPointCollection from StFgtCollection" << endm;
    return kStErr;
  }
  StFgtPointCollection& points=*pPointCollection;
  for( UInt_t discIdx=0; discIdx<c.getNumDiscs() && !iErr; ++discIdx ){
    const StFgtHitCollection* pClusters=c.getHitCollection(discIdx);
    if(!pClusters)
      continue;
    const StFgtHitCollection& clusters=*(pClusters);
    Int_t numClusters=clusters.getNumHits();
    //no point in making points w/ one cluster
    if(numClusters<2)
      continue;
    //prevent looping for too long
    if(numClusters > 40)
      {
	LOG_WARN <<"$Id: StFgtSimplePointAlgo.cxx,v 1.1 2013/03/13 20:36:29 jeromel Exp $ :  number of cluster too large in disk " << discIdx<<"! " <<endl;
	continue;
      }

    //  const Char_t noLayer='N';
    //we make use of the fact, that the hits are already sorted by geoId
    //  Short_t disc, quadrant,prvDisc,prvQuad;
    //  Char_t layer,prvLayer;
    //  Double_t ordinate, lowerSpan, upperSpan, prvOrdinate, prvLowerSpan, prvUpperSpan;
    //  Int_t prvGeoId;
    //  Double_t accuCharge=0; 
    //  Double_t meanOrdinate=0;
    //  Int_t numStrips=0;
    //  bool lookForNewPoint=true;
    //  prvLayer=noLayer;
    Int_t clusterCounter=-1;

    const StSPtrVecFgtHit& hitVec = clusters.getHitVec();
    StSPtrVecFgtHitConstIterator iter1, iter2;

    StSPtrVecFgtPoint& pointVec = points.getPointVec();

    for( iter1 = hitVec.begin(); iter1 != hitVec.end(); ++iter1 )
      {
	Char_t iter1_layer = (*iter1)->getLayer();

	for( iter2 = hitVec.begin(); iter2 != iter1; ++iter2 )
	  if( (*iter2)->getLayer() != iter1_layer)
	    {
	      Float_t chargeAsymmetry=fabs( ((*iter1)->charge()-(*iter2)->charge())/((*iter1)->charge()+(*iter2)->charge()));
	      //	    cout <<"charge asymmetry: " <<chargeAsymmetry <<" max: " << m_maxChargeAsymmetry <<endl;
	      if(chargeAsymmetry>m_maxChargeAsymmetry)
		{
		  continue;
		}
	      //have other cuts based on timing?
	      size_t rank=1;
	      //set this to some meaningful value...
	      //	    cout <<"combining charge1: " << (*iter1)->charge() <<" and charge2 "<< (*iter2)->charge() <<endl;
	      pointVec.push_back( new StFgtPoint ( *iter1, *iter2, ++clusterCounter,rank ) );
	      if( pointVec.back()->getKey() != clusterCounter ){ // error during construction
		delete pointVec.back();
		pointVec.pop_back();
	      };
	    };
      }

    //  cout <<" we have " << points.getEntries() <<" points " << endl;
  }
  return iErr;
};

ClassImp(StFgtSimplePointAlgo);
//
//
//  $Id: StFgtSimplePointAlgo.cxx,v 1.1 2013/03/13 20:36:29 jeromel Exp $
//  $Log: StFgtSimplePointAlgo.cxx,v $
//  Revision 1.1  2013/03/13 20:36:29  jeromel
//  Initial revision, Anselm Vossen
//
//  Revision 1.8  2012/12/11 00:11:07  avossen
//  update of StFgtPoint
//
//  Revision 1.7  2012/12/10 23:56:05  avossen
//  added charge asymmetry condition
//
//  Revision 1.6  2012/03/06 22:28:01  sgliske
//  asserts removed in StFgtPoint constructor.
//  Now must check key after constructing
//
//  Revision 1.5  2011/11/01 18:48:34  sgliske
//  Updated to correspond with StEvent containers, take 2.
//
//  Revision 1.4  2011/10/28 14:55:26  sgliske
//  fixed CVS tags
//
//
//
// \class StFgtSimplePointAlgo
// \author Anselm Vossen (avossen@indiana.edu)
//
