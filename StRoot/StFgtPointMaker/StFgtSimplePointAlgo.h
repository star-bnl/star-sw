//
//
//
//  $Id: StFgtSimplePointAlgo.h,v 1.1 2013/03/13 20:36:29 jeromel Exp $
//  $Log: StFgtSimplePointAlgo.h,v $
//  Revision 1.1  2013/03/13 20:36:29  jeromel
//  Initial revision, Anselm Vossen
//
//  Revision 1.5  2012/12/10 23:56:05  avossen
//  added charge asymmetry condition
//
//  Revision 1.4  2011/11/01 18:48:34  sgliske
//  Updated to correspond with StEvent containers, take 2.
//
//  Revision 1.3  2011/10/28 14:55:26  sgliske
//  fixed CVS tags
//
//
/*!
 \author Anselm Vossen (avossen@indiana.edu) 
  \class StFgtSimpleClusterAlgo
 Simple Point making algorithm taking the clusters from the StFgtEvent and adding points.
It considers all combinations of r-phi clusters in a quadrant up to a maximum of 40.
It combines all clusters to a point with a charge asymmetry smaller than what the user has set.
Default allowed charge asymmetry is 0.1

*/

//
//
//


#ifndef STAR_StFgtSimplePointAlgo_HH
#define STAR_StFgtSimplePointAlgo_HH

#include "StFgtIPointAlgo.h"

class StFgtSimplePointAlgo :public StFgtIPointAlgo
{

 public:
  StFgtSimplePointAlgo();
  virtual ~StFgtSimplePointAlgo();

  //  virtual Int_t makePoints( const StFgtHitCollection&, StFgtPointCollection& );
  virtual Int_t makePoints( StFgtCollection&);
  virtual Int_t Init();

  void setMaxChargeAsym(Float_t chargeAsym);

 protected:

 private:

  Bool_t mIsInitialized;
  Float_t m_maxChargeAsymmetry;
  ClassDef(StFgtSimplePointAlgo,1);
};
inline void StFgtSimplePointAlgo::setMaxChargeAsym(Float_t chargeAsym){m_maxChargeAsymmetry=chargeAsym;}

#endif
