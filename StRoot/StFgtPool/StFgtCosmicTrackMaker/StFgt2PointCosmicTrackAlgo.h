/***************************************************************************
 *
 * $Id: StFgt2PointCosmicTrackAlgo.h,v 1.1 2012/01/31 23:25:35 avossen Exp $ 
 * Author: C. K. Riley (ckriley@bnl.gov), Oct. 11 2011 
 *
 ***************************************************************************
 *
 * Description:  2PointAlgo CosmicTrack making algorithm takes 2 points 
 * from outer quads, fits actual hit to expected hit on middle quad
 *
 ***************************************************************************
 *
 * $Log: StFgt2PointCosmicTrackAlgo.h,v $
 * Revision 1.1  2012/01/31 23:25:35  avossen
 * moved StFgtCosmicTrackMaker to StFgtPool
 *
 * Revision 1.5  2011/11/16 22:15:07  ckriley
 * now looks at all points on quadrants and gets best track
 *
 * Revision 1.4  2011/11/01 18:50:13  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.3  2011/10/20 17:13:44  ckriley
 * major update -> headers, tracks stored in StFgtEvent instead of StFgtDisc, changes to trackmaker and algorithms
 *
 *
 **************************************************************************/

#ifndef STAR_StFgt2PointCosmicTrackAlgo_HH
#define STAR_StFgt2PointCosmicTrackAlgo_HH

#include "StFgtICosmicTrackAlgo.h"

#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"

class StFgt2PointCosmicTrackAlgo :public StFgtICosmicTrackAlgo {

 public:
  // constructor
  StFgt2PointCosmicTrackAlgo();

  // takes in data from maker, calculates line parameters etc.
  virtual Int_t makeCosmicTracks(StFgtPointCollection&,  StFgtCosmicTrackVec&, Int_t);

  // initialize
  virtual Int_t Init();

 private:
  // check if algorithm is initialized
  Bool_t mIsInitialized;

  ClassDef(StFgt2PointCosmicTrackAlgo,1);

};

#endif
