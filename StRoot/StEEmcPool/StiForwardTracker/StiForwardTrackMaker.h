// $Id: StiForwardTrackMaker.h,v 1.3 2005/09/12 21:08:21 balewski Exp $

#ifndef STAR_StiForwadrTrackMaker
#define STAR_StiForwadrTrackMaker

/*!
 *                                                                     
 * \class  StiForwadrTrackMaker
 * \author Adam & Jan

 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
class StiToolkit;
class StiKalmanTrack;
class StPrimaryVertex; //tmp

class StiForwardTrackMaker : public StMaker {
 private:
  // Private method declaration if any
  // event realted info
  int eveID;
  
  // misc counter
  int mTotEve;

  //.... params,cuts
  double mMaxTrkDcaRxy;   //DCA to nominal beam line for each track
  double mMaxZdca;     //zDCA cutoff for prim tracks in cm
 
  //..... util
  StiToolkit     *mToolkit;
  enum {mxHA=8};
  TH1F *hA[mxHA];
  class VertexV{public: float z,ez;};
  vector<VertexV> vertL;

  void initHisto();

  bool examinTrackDca(const StiKalmanTrack *track,float &zDca, float &ezDca, float &rxyDca);
 public: 
  StiForwardTrackMaker(const char *name="forwTrack");
  virtual       ~StiForwardTrackMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  Int_t  MakeInSti(); // tmp, split of make on 2 parts
  Int_t  MakeAfterSti(); // tmp,
  virtual Int_t Finish();
  virtual void Clear(const char* opt);
  TObjArray * HList;
  void saveHisto(TString fname);
  void addVertex(float z, float ez); //tmp, hack Sti

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StiForwardTrackMaker.h,v 1.3 2005/09/12 21:08:21 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(StiForwardTrackMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StiForwardTrackMaker.h,v $
// Revision 1.3  2005/09/12 21:08:21  balewski
// split Make to InSti and AfterSti
//
// Revision 1.2  2005/09/09 15:55:00  balewski
// prototype with hardcoded hacks
//
// Revision 1.1  2005/09/08 21:42:03  balewski
// star
//
