// Hey Emacs this is -*-c++-*-
// $Id: EEmcTTMMaker.h,v 1.1 2004/01/06 17:45:11 zolnie Exp $

#ifndef STAR_EETowTrackMatchMaker
#define STAR_EETowTrackMatchMaker

/*!
 *                                                                     
 * \class  EETowTrackMatchMaker
 * \author Piotr A. Zolnierczuk
 * \date   2003/12/08
 * \brief  EEMC tower calibration using TPC tracks 
 *
 * This a MuDST based class to get tower calibration from matching TPC tracks
 * See README file for detailed info/instructions
 * 
 */                                                                      



#ifndef StMaker_H
#include "StMaker.h"
#endif

#include <cstring>
#include <vector>
#include <map>
#include <string>
#include "TString.h"
#include "TVector3.h"

#if !defined(ST_NO_NAMESPACES)
using std::map;
#endif

class TH1F;
class TTree;
class TFile;
class TString;

class StMuTrack;
class EEmcGeomSimple;

class StMuDstMaker;
class StEEmcDbMaker;

// constantology .... 
const int       kNTupleTTM_MaxTracks  =  128; // max # matched tracks/event
const int       kNTupleTTM_MaxTrigger =   32; // max # of triggers
const unsigned  kNTupleTTM_MaxZ       =    8; // max # of z-positions where we want a match

class EETowTrackMatchMaker : public StMaker {
public: 

  //default values for the cuts
  static const Int_t    kDefMinTrackHits   ;
  static const Double_t kDefMinTrackLength ;
  static const Double_t kDefMinTrackPt     ;


  // structure to hold the results
  struct NTupleTTM_t {
    Int_t    numtracks;

    Int_t    sector  [kNTupleTTM_MaxTracks]; // sector
    Int_t    subsec  [kNTupleTTM_MaxTracks]; // subsector
    Int_t    etabin  [kNTupleTTM_MaxTracks]; // tower# a.k.a eta bin
    Float_t  adc     [kNTupleTTM_MaxTracks]; // adc - pedestal value
    Float_t  edep    [kNTupleTTM_MaxTracks]; // energy deposited : (adc-pedestal)/gain
    //
    Int_t    nhits   [kNTupleTTM_MaxTracks];
    Float_t  pt      [kNTupleTTM_MaxTracks];
    Float_t  ptot    [kNTupleTTM_MaxTracks];
    Float_t  length  [kNTupleTTM_MaxTracks];
    Float_t  dedx    [kNTupleTTM_MaxTracks];
    //
    Int_t    numz;
    Float_t  zpos[kNTupleTTM_MaxZ];
    Float_t  deta[kNTupleTTM_MaxZ][kNTupleTTM_MaxTracks];
    Float_t  dphi[kNTupleTTM_MaxZ][kNTupleTTM_MaxTracks];
    
    // for Trigger Info 
    Int_t    numtrig;
    Int_t    trigid[kNTupleTTM_MaxTrigger];
    Int_t    daqbits;

    inline void Clear() { memset(this,0x00,sizeof(*this)); }
  };
  
  


  EETowTrackMatchMaker(const char          *self    = "EETowTrackMatchMaker",
		       class StMuDstMaker  *mumaker =  NULL,
		       class StEEmcDbMaker *dbmaker =  NULL);
  virtual       ~EETowTrackMatchMaker();

  // MAKER STUFF 
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   Clear(Option_t *option = "");
  virtual Int_t  Finish();


  // debug level manipulations
  Int_t   SetDebugLevel(const int d) { mDebugLevel=d; return d; } // use kInfo,kWarning,etc. 
  Int_t   GetDebugLevel(           ) { return mDebugLevel;      } //


  // z positions to match
  void     ResetZPositionsArray()                           { mZ.clear(); }
  void     AddZPosition(const TString s, const double zpos) { mZ[s]=zpos; }


  // cuts
  Int_t    GetMinTrackHits  () const  { return mMinTrackHits  ;}
  Double_t GetMinTrackLength() const  { return mMinTrackLength;}
  Double_t GetMinTrackPt    () const  { return mMinTrackPt    ;}
  void     SetMinTrackHits  (Int_t    v) { mMinTrackHits=v  ;}
  void     SetMinTrackLength(Double_t v) { mMinTrackLength=v;}
  void     SetMinTrackPt    (Double_t v) { mMinTrackPt=v    ;}
  
  Double_t GetPhiFactor() const { return mPhiFac; }
  Double_t GetEtaFactor() const { return mEtaFac; }
  void     SetPhiFactor(Double_t v=1.0) { mPhiFac=v;  }
  void     SetEtaFactor(Double_t v=1.0) { mEtaFac=v;  }

  //
  void     PrintCutSummary();

  // output file name 
  void     SetFileName( const char *string) { mFileName=TString(string); }

  // 
  ULong_t  GetNMatched() { return mNMatch; };



 protected:
  Int_t    mDebugLevel           ;  // debug level:  use kInfo,kWarning,etc. from TError.h

  // cuts
  Int_t    mMinTrackHits;        ;  // min hits/track required
  Double_t mMinTrackLength       ;  // min track length required
  Double_t mMinTrackPt           ;  // min track transv. momentum required
  
  Double_t mPhiFac;
  Double_t mEtaFac;

  // default criterion of track acceptance
  Bool_t  AcceptTrack( const StMuTrack *track);
  // default criterion whether a track matches a tower
  Bool_t  MatchTrack ( const double dphi,   // track hit to tower centre distance 
		       const double deta,   // 
		       const double phihw,  // tower half-widt in phi
		       const double etahw); // tower half-widt in eta

  // given track and position z return TVector3 
  static  Bool_t  ExtrapolateToZ    ( const StMuTrack *track , const double  z, TVector3 &r); 

  // control histograms for tracks
  TH1F *hTrackNHits;
  TH1F *hTrackLen;
  TH1F *hTrackPt ;
  TH1F *hTrackPtot;

  TH1F *hTrackDCA[3];
  TH1F *hVertex[3]  ;

 private:
  StMuDstMaker   *mMuDstMaker; // toplevel muDST maker
  StEEmcDbMaker  *mEEmcDb;     // EEMC database maker
  EEmcGeomSimple *mGeom;       // tower geometry

  TString         mFileName;   // output file name
  TFile          *mFile;       // output file
  TTree          *mTree;       // output tree
  ULong_t         mNMatch;     // number of matched tracks

  NTupleTTM_t    *mMatch;      // data in "compatible" format 

  map<TString,double> mZ;      // a map that hold z positions 

 public:
  //  StMaker jumbo mumbo
  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: EEmcTTMMaker.h,v 1.1 2004/01/06 17:45:11 zolnie Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(EETowTrackMatchMaker, 1)   // 
};

#endif


// $Log: EEmcTTMMaker.h,v $
// Revision 1.1  2004/01/06 17:45:11  zolnie
// close to release
//
