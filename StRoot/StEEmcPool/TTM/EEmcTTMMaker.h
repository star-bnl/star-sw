// Hey Emacs this is -*-c++-*-
#ifndef STAR_EEmcTTMMaker
#define STAR_EEmcTTMMaker
// $Id: EEmcTTMMaker.h,v 1.9 2004/04/14 16:20:25 zolnie Exp $

/*!
 *                                                                     
 * \class  EEmcTTMMaker
 * \author Piotr A. Zolnierczuk
 * \date   2003/12/08
 *
 * \brief  EEMC tower to track matching
 *
 * This a MuDST based class to get tower calibration from matching TPC tracks
 * See README file for detailed info/instructions
 * 
 */                                                                      



#ifndef StMaker_H
#include "StMaker.h"
#endif

#include <ostream>
#include <map>

#include "TString.h"
#include "TVector3.h"

#if !defined(ST_NO_NAMESPACES)
using std::map;
using std::ostream;
#endif

class TH1F;
class TTree;
class TFile;
class TString;
class TList;
class TMap;

class StChain;
class StMuTrack;
class EEmcGeomSimple;

class StMuDstMaker;
class StEEmcDbMaker;

const int       kNTupleTTM_MaxTracks  =  128;
const int       kNTupleTTM_MaxTrigger =   32;
const unsigned  kNTupleTTM_MaxZ       =    8;


class EEmcTower : public TObject  {
public:
  EEmcTower()  { sec=sub=eta=0; edep=0.0; };
  ~EEmcTower() {}
  EEmcTower(int s, int ss, int e, float ene) { 
    sec =(unsigned char)s;
    sub =(unsigned char)ss;
    eta =(unsigned char)e;
    edep=ene;
  };
  ostream& Out ( ostream &out ) const ;  
  unsigned char sec;
  unsigned char sub;
  unsigned char eta;
  float         edep;

  ClassDef(EEmcTower, 1)   // 
};

class EEmcTTMMaker : public StMaker {
public: 

  /// default values for the cuts
  static const Int_t    kDefMinTrackHits   ;  
  /// default values for the cuts
  static const Double_t kDefMinTrackLength ;  
  /// default values for the cuts
  static const Double_t kDefMinTrackPt     ;  


  /// structure to hold the results from EEmcTTMMaker
  struct NTupleTTM_t {
    Int_t    numtracks;                      /**<- number of tracks */
    Int_t    sector  [kNTupleTTM_MaxTracks]; /**<- sector */
    Int_t    subsec  [kNTupleTTM_MaxTracks]; /**<- subsector */
    Int_t    etabin  [kNTupleTTM_MaxTracks]; /**<- tower# a.k.a eta bin */
    Float_t  adc     [kNTupleTTM_MaxTracks]; /**<- pedestal subtracted adc = adc - pedestal */
    Float_t  edep    [kNTupleTTM_MaxTracks]; /**<- energy deposited : (adc-pedestal)/gain */
    Int_t    ntrack  [kNTupleTTM_MaxTracks]; /**<- number of tracks in an event that hit a tower */
    //
    Int_t    nhits   [kNTupleTTM_MaxTracks]; /**<- number of hits/track */
    Float_t  pt      [kNTupleTTM_MaxTracks]; /**<- track transverse momentum */
    Float_t  ptot    [kNTupleTTM_MaxTracks]; /**<- track total momentum  */
    Float_t  length  [kNTupleTTM_MaxTracks]; /**<- track length */
    Float_t  dedx    [kNTupleTTM_MaxTracks]; /**<- track energy loss (dE/dx) */
    // 
    Int_t    numz;                           /**<- number of z positions to match */
    Float_t  zpos[kNTupleTTM_MaxZ];          /**<- z positions to match */
    Float_t  deta[kNTupleTTM_MaxZ][kNTupleTTM_MaxTracks]; /**<- distance in eta between track hit and the tower center */
    Float_t  dphi[kNTupleTTM_MaxZ][kNTupleTTM_MaxTracks]; /**<- distance in eta between track hit and the tower center */
    
    // for Trigger Info 
    Int_t    numtrig;                        /**<- number of trigger present */
    Int_t    trigid[kNTupleTTM_MaxTrigger];  /**<- a list of trigger id's    */
    Int_t    daqbits;                        /**<- DAQ trigger bits          */
    Int_t    ctbsum;                         /**<- CTB sum                   */

    /// zeroes the structure
    inline void Clear() { memset(this,0x00,sizeof(*this)); }
  };
  
  
  //! the TTM constructor
  /// \param self     this maker name (const char*)
  /// \param mumaker a pointer to a StMuDstMaker 
  /// \param dbmaker a pointer to a StEEmcDbMaker 
  EEmcTTMMaker(const char          *self    = "EEmcTTMMaker", 
		       class StMuDstMaker  *mumaker =  NULL,
		       class StEEmcDbMaker *dbmaker =  NULL);

  virtual       ~EEmcTTMMaker();

  // MAKER STUFF 
  virtual Int_t  Init();   
  virtual Int_t  Make();   
  virtual void   Clear(Option_t *option = ""); 
  virtual Int_t  Finish(); 
  
  /// clear z positions array
  void     ResetZPositionsArray()                           { mZ.clear(); }
  /// adds a z position to z positions array
  void     AddZPosition(const TString s, const double zpos) { mZ[zpos]=s; }

  /// returns min hits/track cut value
  Int_t    GetMinTrackHits  () const  { return mMinTrackHits  ;}
  /// returns min track length cut value
  Double_t GetMinTrackLength() const  { return mMinTrackLength;}
  /// returns min pt cut value
  Double_t GetMinTrackPt    () const  { return mMinTrackPt    ;}
  /// sets min hits/track cut value
  void     SetMinTrackHits  (Int_t    v) { mMinTrackHits=v  ;}
  /// sets min track length cut value
  void     SetMinTrackLength(Double_t v) { mMinTrackLength=v;}
  /// sets min pt cut value
  void     SetMinTrackPt    (Double_t v) { mMinTrackPt=v    ;}
  
  /// gets phi factor
  Double_t GetPhiFactor() const { return mPhiFac; }
  /// gets eta factor
  Double_t GetEtaFactor() const { return mEtaFac; }
  /// sets phi factor
  void     SetPhiFactor(Double_t v=1.0) { mPhiFac=v;  }
  /// sets eta factor
  void     SetEtaFactor(Double_t v=1.0) { mEtaFac=v;  }
  /// set output file name 
  void     SetFileName( const char *string) { mFileName=TString(string); }
  /// returns number of matched tracks
  ULong_t  GetNMatched() const { return mNMatched; };

  /// returns a list of good StMuTracks
  TList *GetTracks() { return mTrackList; };
  /// returns a list of good EEmcTower
  TList *GetTowers() { return mTowerList; };
  /// returns a map  of matches
  TMap  *GetMatch()  { return mMatchMap ; }; 

  ostream&   Summary    ( ostream &out ) const ;

  /// a static method to be called from root4star 
  /// also an example how to use TTM
  static void Run(
	    StChain* chain, 
	    char* inpDir    ,
	    char* inpFile   ,
	    char* outFile   ,
	    Int_t nFiles    ,
	    Int_t nEvents   ,
	    Int_t timeStamp );


 protected:

  Int_t    mMinTrackHits         ;  /**<- min hits per track required   */
  Double_t mMinTrackLength       ;  /**<- min track length required */
  Double_t mMinTrackPt           ;  /**<- min track transv. momentum required */
  
  Double_t mPhiFac;                 /**<- phi factor */
  Double_t mEtaFac;                 /**<- phi factor */

  /// resets the collected statistics 
  void     ResetStats() { mNMatched=mNEvents=0L; };  
  Bool_t   AcceptTrack( const StMuTrack *track);
  Bool_t   MatchTrack ( const double dphi,   const double deta,  const double phihw,  const double etahw); 

  static  Bool_t  ExtrapolateToZ    ( const StMuTrack *track , const double  z, TVector3 &r); 

  // control histograms for tracks
  TH1F *hTrackNHits; /**<- phi factor */
  TH1F *hTrackLen;   /**<- phi factor */
  TH1F *hTrackPt ;   /**<- phi factor */
  TH1F *hTrackPtot;  /**<- phi factor */

  TH1F *hTrackDCA[3];/**<- phi factor */
  TH1F *hVertex[3]  ;/**<- phi factor */

 private:
  StMuDstMaker   *mMuDstMaker; // toplevel muDST maker
  StEEmcDbMaker  *mEEmcDb;     // EEMC database maker
  EEmcGeomSimple *mGeom;       // tower geometry

  TString         mFileName;   // output file name
  TFile          *mFile;       // output file
  TTree          *mTree;       // output tree

  NTupleTTM_t    *mMatch;      // output data in "ntuple" format 

  map<double,TString> mZ;      // a map that hold z positions 

  // stats
  ULong_t         mNMatched;   // number of matched tracks
  ULong_t         mNEvents;    // total number of events 

  //
  TList          *mTrackList;
  TList          *mTowerList;
  TMap           *mMatchMap;

 public:
  //  StMaker jumbo mumbo
  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]=
      "Tag $Name:  $ $Id: EEmcTTMMaker.h,v 1.9 2004/04/14 16:20:25 zolnie Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  ClassDef(EEmcTTMMaker, 1)   // 
};

/// for nice printing
ostream&  Out(ostream &out , const StMuTrack &t);
ostream&  Out(ostream &out , const EEmcTower &t);
ostream&  operator<<(ostream &out, const EEmcTTMMaker &ttm); 
ostream&  operator<<(ostream &out, const EEmcTower    &t  );
ostream&  operator<<(ostream &out, const StMuTrack    &t  );  
#endif


// $Log: EEmcTTMMaker.h,v $
// Revision 1.9  2004/04/14 16:20:25  zolnie
// added static method Run for faster analysis under root4star
//
// Revision 1.8  2004/04/13 17:26:09  zolnie
// more adaptation needed
//
// Revision 1.7  2004/01/26 21:51:54  zolnie
// shorter names
//
// Revision 1.6  2004/01/26 21:08:32  zolnie
// working track/tower display (before big farewell cleanup)
//
// Revision 1.5  2004/01/19 22:07:51  zolnie
// toward track/tower display
//
// Revision 1.4  2004/01/14 22:59:02  zolnie
// use doxygen for documentation
//
// Revision 1.3  2004/01/06 22:42:56  zolnie
// provide summary/statistics info
//
// Revision 1.2  2004/01/06 21:33:51  zolnie
// release
//
// Revision 1.1  2004/01/06 17:45:11  zolnie
// close to release
//
