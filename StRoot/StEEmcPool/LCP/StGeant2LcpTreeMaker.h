// $Id: StGeant2LcpTreeMaker.h,v 1.2 2014/08/06 11:42:58 jeromel Exp $

#ifndef STAR_StGeant2LcpTreeMaker
#define STAR_StGeant2LcpTreeMaker

/*!
 *                                                                     
 * \class  StGeant2LcpTreeMaker
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
class particle_st;
class  St_particle  ;

class StGeant2LcpTreeMaker : public StMaker {
 private:
  StMuDstMaker* mMuDstMaker;
  TFile *hfile;
  TH1F *h[16]; 
  TH1F *hc[8]; 
  TTree *tree ;  
  int runID; 
  TString treeName;

  // tree content
  Int_t eve_id; // event ID
  Int_t eve_sub; // geant subprocess ID
  Int_t eve_nPKPi; // # of p,pbar,K+/-, pi+/- after 
  Float_t lcp_eta,lcp_phi,lcp_pt; // momentum
  Int_t lcp_idhep; // geant particle ID
  Float_t lcp_e; // energy (GeV)

  void clearLCP();

  // cuts on prim tracks
  float C_maxEta; 
  float C_minPt; // (GeV/c) 

  void printTrack( particle_st* part);
  particle_st* findGeantLcp( St_particle    *tab);

 public: 
  StGeant2LcpTreeMaker(const char *self="jasLcp2Tree", const char* muDstMakerName="muDstMaker");
  virtual       ~StGeant2LcpTreeMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  void SetOutDir(const char * path) {treeName=path;}
  void SetMaxEta(float x){C_maxEta=x;}
  void SetMinPt(float x){C_minPt=x;}
  void InitRunFromMake(int run_id);

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StGeant2LcpTreeMaker.h,v 1.2 2014/08/06 11:42:58 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StGeant2LcpTreeMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StGeant2LcpTreeMaker.h,v $
// Revision 1.2  2014/08/06 11:42:58  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.1  2004/01/06 17:25:26  balewski
// get LCP from Geant info
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
