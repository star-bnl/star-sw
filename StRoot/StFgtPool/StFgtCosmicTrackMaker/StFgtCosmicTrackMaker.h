/***************************************************************************
 *
 * $Id: StFgtCosmicTrackMaker.h,v 1.1 2012/01/31 23:25:35 avossen Exp $
 * Author: C. K. Riley (ckriley@bnl.gov), Oct. 10 2011 
 *
 ***************************************************************************
 *
 * Description: CosmicTrackMaker for the cosmic stand
 *
 ***************************************************************************
 *
 * $Log: StFgtCosmicTrackMaker.h,v $
 * Revision 1.1  2012/01/31 23:25:35  avossen
 * moved StFgtCosmicTrackMaker to StFgtPool
 *
 * Revision 1.11  2011/11/25 20:24:59  ckriley
 * now will look at all possible point combinations for tracks and pick the best one
 *
 * Revision 1.10  2011/11/16 22:15:07  ckriley
 * now looks at all points on quadrants and gets best track
 *
 * Revision 1.9  2011/11/09 20:56:58  ckriley
 * working version for new containers
 *
 * Revision 1.8  2011/11/01 18:50:13  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.7  2011/10/28 18:30:09  ckriley
 * make things up-to-date
 *
 * Revision 1.6  2011/10/28 14:54:11  sgliske
 * Changed to get StFgtEvent from StEvent rather than another maker.
 * Added some sanity checks to ensure the algo has been set.
 * Also pClusterAlgo changed to mClusterAlgoPtr to conform with STAR guidelines.
 *
 * Revision 1.4  2011/10/20 17:13:44  ckriley
 * major update -> headers, tracks stored in StFgtEvent instead of StFgtDisc, changes to trackmaker and algorithms
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_COSMIC_TRACK_MAKER_
#define _ST_FGT_COSMIC_TRACK_MAKER_

#include "StMaker.h"
class StFgtICosmicTrackAlgo;
#include "StFgtCosmicTrack.h"

class StFgtCosmicTrackMaker : public StMaker {
  public:
    // constructors
    StFgtCosmicTrackMaker( const Char_t* name = "FGT_Cosmic_Track_Maker" );

    // deconstructor
    ~StFgtCosmicTrackMaker();

    Int_t Init();
    Int_t Make();
    Int_t Finish();
    void Clear( Option_t *opt = "" );

    Int_t setCosmicTrackAlgo(StFgtICosmicTrackAlgo*);
    //void setRand( Float_t rndm=1 );

          StFgtCosmicTrackVec& getCosmicTrackVec();
    const StFgtCosmicTrackVec& getCosmicTrackVec() const;

  protected:
    // pointer to algo
    StFgtICosmicTrackAlgo* mCosmicTrackAlgoPtr;

    // tracks are stored here
    StFgtCosmicTrackVec mTrackVec;

  private:   
    Int_t eventCounter;

    ClassDef(StFgtCosmicTrackMaker,1);

}; 

// inline functions
inline       StFgtCosmicTrackVec& StFgtCosmicTrackMaker::getCosmicTrackVec(){
   return mTrackVec;
};

inline const StFgtCosmicTrackVec& StFgtCosmicTrackMaker::getCosmicTrackVec() const{
   return mTrackVec;
};


#endif

