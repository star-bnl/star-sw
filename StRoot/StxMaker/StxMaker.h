// $Id: StxMaker.h,v 1.2 2013/09/16 19:54:04 fisyak Exp $
#ifndef STAR_StHTLCAMaker
#define STAR_StHTLCAMaker

/*!
 *                                                                     
 * \class  StxMaker
 * \author Yuri Fisyak
 * \date   2013/08/12
 * \brief  
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TVector3.h"
#include "TMatrixDSym.h"
#include "GenFit/Track.h"
#include "StThreeVectorD.hh"
class AliHLTTPCCAGBTrack;
class StEvent;
class StTrack;
class StTrackDetectorInfo;
class StxMaker : public StMaker {
 public: 
 StxMaker(const char *name="Stx") : StMaker(name) {}
  virtual       ~StxMaker() {}
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t FitTrack(const AliHLTTPCCAGBTrack &tr);
  static Double_t ConvertCA2XYZ(const AliHLTTPCCAGBTrack &tr, TVector3 &pos, TVector3 &mom, TMatrixDSym &covM);
  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
  Bool_t Accept(genfit::Track *kTrack);
  void FillGlobalTrack(genfit::Track *kTrack);
  void FillEventPrimaries();
  void FillDetectorInfo(StTrack *gTrack,StTrackDetectorInfo* detInfo, genfit::Track * track, bool refCountIncr) ;
  void FillGeometry(StTrack* gTrack, genfit::Track * track, bool outer);
  void FillFitTraits(StTrack* gTrack, genfit::Track * track);
  void FillFlags(StTrack* gTrack);
  void FillTrack(StTrack* gTrack, genfit::Track * track,StTrackDetectorInfo* detInfo );
  Double_t impactParameter(genfit::Track * track, StThreeVectorD &vertexPosition);
  Double_t impactParameter(StTrack* track, StThreeVectorD &vertex);
  void FillDca(StTrack* stTrack, genfit::Track * track);
  //  void FillStHitErr(StHit *hh,const StxKalmanTrackNode *node);

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StxMaker.h,v 1.2 2013/09/16 19:54:04 fisyak Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
 private:
  StEvent *mEvent;
  ClassDef(StxMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StxMaker.h,v $
