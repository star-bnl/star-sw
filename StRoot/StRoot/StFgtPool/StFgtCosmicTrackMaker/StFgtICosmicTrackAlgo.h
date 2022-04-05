/***************************************************************************
 *
 * $Id: StFgtICosmicTrackAlgo.h,v 1.1 2012/01/31 23:25:36 avossen Exp $ 
 * Author: ckriley, Oct. 19 2011 
 *
 ***************************************************************************
 *
 * Description: abstract base class for CosmicTrackAlgo implementation
 *
 ***************************************************************************
 *
 * $Log: StFgtICosmicTrackAlgo.h,v $
 * Revision 1.1  2012/01/31 23:25:36  avossen
 * moved StFgtCosmicTrackMaker to StFgtPool
 *
 * Revision 1.8  2011/11/16 22:15:07  ckriley
 * now looks at all points on quadrants and gets best track
 *
 * Revision 1.7  2011/11/01 18:50:13  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.6  2011/10/28 18:30:09  ckriley
 * make things up-to-date
 *
 * Revision 1.4  2011/10/20 17:13:44  ckriley
 * major update -> headers, tracks stored in StFgtEvent instead of StFgtDisc, changes to trackmaker and algorithms
 *
 *
 **************************************************************************/

#ifndef STAR_StFgtICosmicTrackAlgo_HH
#define STAR_StFgtICosmicTrackAlgo_HH

#include "StFgtCosmicTrack.h"
class StFgtPointCollection;

class StFgtICosmicTrackAlgo {
 public:
  //subclasses must implement this function that takes point from StEvent and fills the CosmicTrack collection
  virtual Int_t makeCosmicTracks(StFgtPointCollection&, StFgtCosmicTrackVec&, Int_t)=0;
  virtual Int_t Init()=0;
  
};

#endif
