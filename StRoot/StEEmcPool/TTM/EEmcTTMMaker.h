// Hey Emacs this is -*-c++-*-
#ifndef STAR_EEmcTTMMaker
#define STAR_EEmcTTMMaker
// $Id: EEmcTTMMaker.h,v 1.11 2004/05/04 18:28:56 zolnie Exp $

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

#if !defined(ST_NO_NAMESPACES)
using std::map;
using std::ostream;
#endif

class TH1F;
class TTree;
class TFile;
class TString;
class TList;

class StChain;
class StMuTrack;
class EEmcGeomSimple;

class StMuDstMaker;
class StEEmcDbMaker;

class EEmcTower;

//const int       kNTupleTTM_MaxTracks  =  128;
//const int       kNTupleTTM_MaxTrigger =   32;
//const unsigned  kNTupleTTM_MaxZ       =    8;




class EEmcTTMMaker : public StMaker {
public: 
  /// default values for the cuts
  static const Int_t    kDefMaxCTBsum      ;
  /// default values for the cuts
  static const Int_t    kDefMinTrackHits   ;  
  /// default values for the cuts
  static const Double_t kDefMinTrackLength ;  
  /// default values for the cuts
  static const Double_t kDefMinTrackPt     ;  
  /// default values for the cuts
  static const Double_t kDefMinTrackEta    ;  
  /// default values for the cuts
  static const Double_t kDefMaxTrackEta    ;  
  /// default values for the cuts
  static const Double_t kDefDeltaPhiCut    ;
  /// default values for the cuts
  static const Double_t kDefDeltaEtaCut    ;
  
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


  /// returns max CTB sum allowed
  Int_t    GetMaxCTBSum     () const  { return mMaxCTBsum   ; }
  /// sets max track eta cut value
  void     SetMaxCTBSum     (Int_t    v) { mMaxCTBsum=v     ; }


  /// returns min hits/track cut value
  Int_t    GetMinTrackHits  () const  { return mMinTrackHits; }
  /// sets min hits/track cut value
  void     SetMinTrackHits  (Int_t    v) { mMinTrackHits=v  ; }


  /// returns min track length cut value
  Double_t GetMinTrackLength() const  { return mMinTrackLength ; }
  /// sets min track length cut value
  void     SetMinTrackLength(Double_t v) { mMinTrackLength=v   ; }

  /// returns min pt cut value
  Double_t GetMinTrackPt    () const  { return mMinTrackPt    ; }
  /// sets min pt cut value
  void     SetMinTrackPt    (Double_t v) { mMinTrackPt=v      ; }

  /// returns min track eta cut value
  Double_t GetMinTrackEta   () const  { return mMinTrackEta   ; }
  /// sets min track eta cut value
  void     SetMinTrackEta   (Double_t v) { mMinTrackEta=v     ; }

  /// returns max track eta cut value
  Double_t GetMaxTrackEta   () const  { return mMaxTrackEta   ; }
  /// sets max track eta cut value
  void     SetMaxTrackEta   (Double_t v) { mMaxTrackEta=v     ; }

  
  /// gets delta phi cut
  Double_t GetDeltaPhiCut() const { return mPhiFac; }
  /// sets delta phi cut
  void     SetDeltaPhiCut(Double_t v=1.0) { mPhiFac=v;  }

  /// gets delta eta cut
  Double_t GetDeltaEtaCut() const { return mEtaFac; }
  /// sets delta eta cut
  void     SetDeltaEtaCut(Double_t v=1.0) { mEtaFac=v;  }

  /// set output file name 
  void     SetFileName( const char *string) { mFileName=TString(string); }

  /// returns number of matched tracks
  ULong_t  GetNMatched() const { return mNMatched; };

  /// returns a list of good StMuTracks
  TList *GetTracks() { return mTrackList; };
  /// returns a list of good EEmcTower
  TList *GetTowers() { return mTowerList; };
  /// returns a map  of matches
  TList *GetMatchList()  { return mMatchList ; }; 

  /// prints a summary of run
  ostream&   Summary    ( ostream &out ) const ;

 protected:
  Int_t    mMaxCTBsum            ;  /**<- max CTB sum allowed                 */
  Int_t    mMinTrackHits         ;  /**<- min hits per track required         */
  Double_t mMinTrackLength       ;  /**<- min track length required           */
  Double_t mMinTrackPt           ;  /**<- min track transv. momentum required */
  Double_t mMinTrackEta          ;  /**<- min track pseudorapidity required   */
  Double_t mMaxTrackEta          ;  /**<- min track pseudorapidity required   */
  
  Double_t mPhiFac;                 /**<- phi factor */
  Double_t mEtaFac;                 /**<- eta factor */

  /// resets the collected statistics 
  void     ResetStats() { mNMatched=mNEvents=0L; };  
  Bool_t   AcceptTrack( const StMuTrack *track);
  Bool_t   MatchTrack ( const double dphi,   const double deta,  const double phihw,  const double etahw); 



  // control histograms for tracks
  TH1F *hTrackNHits; /**<- number of hits/track */
  TH1F *hTrackLen;   /**<- track  length        */
  TH1F *hTrackPt ;   /**<- track  p_T           */
  TH1F *hTrackPtot;  /**<- track  p_tot         */

  TH1F *hTrackDCA[3];/**<- tracks DCA           */
  TH1F *hVertex[3]  ;/**<- vertex               */

 private:
  StMuDstMaker   *mMuDstMaker; // toplevel muDST maker
  StEEmcDbMaker  *mEEmcDb;     // EEMC database maker
  EEmcGeomSimple *mGeom;       // tower geometry

  TString         mFileName;   // output file name
  TFile          *mFile;       // output file
  TTree          *mTree;       // output tree

  map<double,TString> mZ;      // a map that hold z positions 

  // stats
  ULong_t         mNMatched;   // number of matched tracks
  ULong_t         mNEvents;    // total number of events 

  //
  TList          *mTrackList;
  TList          *mTowerList;
  TList          *mMatchList;

 public:
  //  StMaker jumbo mumbo
  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]=
      "Tag $Name:  $ $Id: EEmcTTMMaker.h,v 1.11 2004/05/04 18:28:56 zolnie Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  ClassDef(EEmcTTMMaker, 1)   // 
};

/// for nice printing
ostream&  operator<<(ostream &out, const EEmcTTMMaker &ttm); 
#endif

// $Log: EEmcTTMMaker.h,v $
// Revision 1.11  2004/05/04 18:28:56  zolnie
// version after split
//
// Revision 1.10  2004/04/14 16:40:34  zolnie
// *** empty log message ***
//
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
