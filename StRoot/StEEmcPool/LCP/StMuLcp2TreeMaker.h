// $Id: StMuLcp2TreeMaker.h,v 1.6 2014/08/06 11:42:58 jeromel Exp $

#ifndef STAR_StMuLcp2TreeMaker
#define STAR_StMuLcp2TreeMaker

/*!
 *                                                                     
 * \class  StMuLcp2TreeMaker
 * \author Balewski
 * \date   
 * \brief  
 *
 * This commented block at the top ...
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StMuDstMaker;
class TFile;
class TH1F;
class TH2F;
//class TClonesArray;
class TTree;
class StMuTrack;
//class ExampleUsage;
class StMuEvent;
class CtbMatching;

class StMuLcp2TreeMaker : public StMaker {
 private:
  StMuDstMaker* mMuDstMaker;
  TObjArray* primTrA;
  //ExampleUsage *rejector;
  TFile *hfile;
  TH1F *h[16]; 
  TH1F *hc[8]; 
  TTree *tree ;  
  int runID; 
  int off48;
  TString treeName;

  // tree content
  Float_t eve_vz; // vertex in Z
  Int_t eve_bx48,eve_bx120;// bXing raw,corrected
  Int_t eve_id; // event ID
  Int_t eve_sb; // spin bits
  Int_t eve_nPrim; // # of good primary tracks
  Int_t eve_nGlob; // # of good global tracks
  Int_t eve_cosm; //Mike's Cosmics Rejector & other tests value
  Float_t lcp_eta,lcp_phi,lcp_pt; // momentum
  Int_t lcp_q,lcp_nFit;// charge, # of hits on track
 
  void clearLCP();
  const StMuTrack*  findLCP( float minPt,int minNFitP, float maxDCAxy, float maxEta,  float minFitPfrac) ;
  void examinCut(const StMuTrack* lcp0);
  // cuts on prim tracks
  int   C_minNFitPoint;
  float C_minFitPfrac; // fraction of fitPoints/possiblePoints
  float C_maxDCAxy; // (cm)
  float C_maxEta; 
  float C_minPt; // (GeV/c) 
  float C_maxZvertex; // (cm)

  CtbMatching *ctb;
  
 public: 
  StMuLcp2TreeMaker(const char *self="jasLcp2Tree", const char* muDstMakerName="muDstMaker");
  virtual       ~StMuLcp2TreeMaker();
  virtual Int_t Init();
  Int_t InitRunFromMake  (int runumber); 
  virtual Int_t  Make();
  void SetOutDir(const char * path) {treeName=path;}
  void SetOff48(int off) {off48=off;}
  void SetMinNFitPoint(int n){C_minNFitPoint=n;}
  void SetMaxDCAxy(float x){C_maxDCAxy=x;}
  void SetMaxEta(float x){C_maxEta=x;}
  void SetMinPt(float x){C_minPt=x;}
  void SetMaxZvert(float x){C_maxZvertex=x;}
  void SetMinFitPfrac(float x){C_minFitPfrac=x;}


  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StMuLcp2TreeMaker.h,v 1.6 2014/08/06 11:42:58 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StMuLcp2TreeMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StMuLcp2TreeMaker.h,v $
// Revision 1.6  2014/08/06 11:42:58  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.5  2009/12/02 16:35:58  fine
// Fix StMuTrack interface
//
// Revision 1.4  2005/08/23 21:09:23  balewski
// fix to follow muDst evolution
//
// Revision 1.3  2003/11/12 18:43:41  balewski
// final for LCP paper
//
// Revision 1.2  2003/10/20 17:04:39  balewski
// LCP analysis code
//
// Revision 1.1  2003/09/16 19:18:36  balewski
// extraction of LCP from muDst
//
