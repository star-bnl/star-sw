// $Id: StHitFilterMaker.h,v 1.1 2003/02/07 02:16:15 jeromel Exp $

#ifndef STAR_StHitFilterMaker
#define STAR_StHitFilterMaker

/*!
 *                                                                     
 * \class  StHitFilterMaker
 * \author Dunlop
 * \date   2003/02/05
 * \brief  Filters out TPC hits from StEvent on tracks that don't pass cuts.
 *         This is based on the StEventScavenger-type zombie methods,
 *         and so should be run after StEvent creation
 *         Currently the only things it zombies are TPC hits
 *
 *
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif



#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
class StTrackNode;
class StTrack;

class StEvent;

class StHitFilterMaker : public StMaker {
 private:
  // Currently Available Cuts.  Negative == don't use.
  Double_t mPtLowerCut;
  Double_t mPtUpperCut;
  Double_t mAbsEtaCut;
    
    
 protected:
  // Protected method if any

 public: 
  StHitFilterMaker(const char *name="StHitFilterMaker",Double_t ptLowerCut=1.5,Double_t ptUpperCut=-1, Double_t absEtaCut=1.);
  virtual       ~StHitFilterMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  bool accept(StTrack *);
    
  bool removeTpcHitsNotOnTracks(StEvent *, vector<StTrackNode*>&); //!
  void setPtLowerCut(Double_t pt) {mPtLowerCut = pt;}
  void setPtUpperCut(Double_t pt) {mPtUpperCut = pt;}
  void setAbsEtaCut(Double_t eta) {mAbsEtaCut = eta;}
  Double_t ptLowerCut() { return mPtLowerCut;}
  Double_t ptUpperCut() { return mPtUpperCut;}
  Double_t absEtaCut() { return mAbsEtaCut;}

    
  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StHitFilterMaker.h,v 1.1 2003/02/07 02:16:15 jeromel Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
    
  //StAF chain virtual base class for Makers
  ClassDef(StHitFilterMaker, 1)   
};

#endif


// $Log: StHitFilterMaker.h,v $
// Revision 1.1  2003/02/07 02:16:15  jeromel
// First version of a generlized HitFilter/removal maker. Expeditious review
// done.
//
//
