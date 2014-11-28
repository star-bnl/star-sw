/***************************************************************************
 *
 * $Id: StFgtSimpleCosmicTrackAlgo.h,v 1.1 2012/01/31 23:25:36 avossen Exp $ 
 * Author: C. K. Riley (ckriley@bnl.gov), Oct. 11 2011 
 *
 ***************************************************************************
 *
 * Description:  Simple CosmicTrack making algorithm takes 3 points 
 * from all 3 quads in test stand and does a chi^2 fit
 *
 ***************************************************************************
 *
 * $Log: StFgtSimpleCosmicTrackAlgo.h,v $
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

#ifndef STAR_StFgtSimpleCosmicTrackAlgo_HH
#define STAR_StFgtSimpleCosmicTrackAlgo_HH

#include "StFgtICosmicTrackAlgo.h"

#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"

class StFgtSimpleCosmicTrackAlgo :public StFgtICosmicTrackAlgo {

 public:
  // constructor
  StFgtSimpleCosmicTrackAlgo();

  // takes in data from maker, calculates line parameters etc.
  virtual Int_t makeCosmicTracks(StFgtPointCollection&, StFgtCosmicTrackVec&, Int_t);

  // initialize
  virtual Int_t Init();

 private:
  // check if algorithm is initialized
  Bool_t mIsInitialized;

  ClassDef(StFgtSimpleCosmicTrackAlgo,1);
};

#endif
