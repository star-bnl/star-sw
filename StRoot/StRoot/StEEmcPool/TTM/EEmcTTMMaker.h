// Hey Emacs this is -*-c++-*-
// $Id: EEmcTTMMaker.h,v 1.20 2014/08/06 11:43:02 jeromel Exp $
#ifndef STAR_EEmcTTMMaker
#define STAR_EEmcTTMMaker

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

class StEventInfo; 
class StEventSummary; 
class StMuTriggerIdCollection; 

class StMuDstMaker;
class StEEmcDb;

class EEmcTower;

/// class EEmcTTMMaker
class EEmcTTMMaker : public StMaker {
public: 
  /// default value for the maximum CTB sum allowed
  static const Int_t    kDefMaxCTBsum      ;
  /// default value for the minimum TPC hits/track allowed
  static const Int_t    kDefMinTrackHits   ;  
  /// default value for the minimum track length allowed
  static const Double_t kDefMinTrackLength ;  
  /// default value for the minimum track pT allowed
  static const Double_t kDefMinTrackPt     ;  
  /// default value for the minimum track pseudorapidity at origin allowed
  static const Double_t kDefMinTrackEta    ;  
  /// default value for the maximum track pseudorapidity at origin allowed
  static const Double_t kDefMaxTrackEta    ;  
  /// default value for the maximum distance in phi from the tower center
  static const Double_t kDefDeltaPhiCut    ;
  /// default value for the maximum distance in eta from the tower center
  static const Double_t kDefDeltaEtaCut    ;
  
  /// the TTM constructor
  /// \param self     this maker name (const char*)
  /// \param mumaker a pointer to a StMuDstMaker 
  EEmcTTMMaker(const char          *self    = "ttmmk", 
	       class StMuDstMaker  *mumaker =  NULL);

  /// the destructor
  virtual       ~EEmcTTMMaker();

  /// initialize maker 
  virtual Int_t  Init();   
  /// process one event
  virtual Int_t  Make();   
  /// clears maker 
  /// \param option not used at the moment
  virtual void   Clear(Option_t *option = ""); 
  /// cleans up at the end
  virtual Int_t  Finish(); 
  
  /// clears z positions array
  void     ResetZPositionsArray()                              { mZ.clear(); }
  /// adds a z position to z positions array
  /// \param name  position name
  /// \param zpos  position depth
  void     AddZPosition(const TString name, const double zpos) { mZ[zpos]=name; }


  /// gets maximum CTB sum allowed
  Int_t    GetMaxCTBSum     ()     const  { return mMaxCTBsum   ; }
  /// sets maximum CTB sum allowed
  void     SetMaxCTBSum     (Int_t    v) { mMaxCTBsum=v     ; }


  /// gets minimum number of hits/track required
  Int_t    GetMinTrackHits  () const  { return mMinTrackHits; }
  /// sets minimum number of hits/track required
  void     SetMinTrackHits  (Int_t    v) { mMinTrackHits=v  ; }


  /// gets minimum track length required
  Double_t GetMinTrackLength() const  { return mMinTrackLength ; }
  /// sets minimum track length required
  void     SetMinTrackLength(Double_t v) { mMinTrackLength=v   ; }

  /// gets minimum track pT required
  Double_t GetMinTrackPt    () const  { return mMinTrackPt    ; }
  /// sets minimum track pT required
  void     SetMinTrackPt    (Double_t v) { mMinTrackPt=v      ; }

  /// gets minimum pseudorapidity at the origin required
  Double_t GetMinTrackEta   () const  { return mMinTrackEta   ; }
  /// sets minimum pseudorapidity at the origin required
  void     SetMinTrackEta   (Double_t v) { mMinTrackEta=v     ; }

  /// gets minimum pseudorapidity at the origin required
  Double_t GetMaxTrackEta   () const  { return mMaxTrackEta   ; }
  /// sets minimum pseudorapidity at the origin required
  void     SetMaxTrackEta   (Double_t v) { mMaxTrackEta=v     ; }

  
  /// gets delta phi cut see \ref matchparams
  Double_t GetDeltaPhiCut() const { return mPhiFac; }
  /// sets delta phi cut see \ref matchparams
  void     SetDeltaPhiCut(Double_t v=1.0) { mPhiFac=v;  }

  /// gets delta eta cut see \ref matchparams
  Double_t GetDeltaEtaCut() const { return mEtaFac; }
  /// sets delta eta cut see \ref matchparams
  void     SetDeltaEtaCut(Double_t v=1.0) { mEtaFac=v;  }

  /// set output file name
  void     SetFileName( const char *string) { mFileName=TString(string); mTreeOut=true; }
  /// decide whether write out matches to a tree
  void     WriteTree( const bool f ) { mTreeOut = f ; }

  /// returns number of matched tracks
  ULong_t  GetNMatched() const { return mNMatched; };

  /// returns a list of accepted StMuTracks objects
  TList *GetTracks() { return mTrackList; };
  /// returns a list of accepted EEmcTower objects
  TList *GetTowers() { return mTowerList; };

  /// returns a list of matches (EEmcTTMatch objects)
  TList *GetMatchList()  { return mMatchList ; }; 


  /// prints matching cuts and statistics summary
  ostream&   Summary    ( ostream &out ) const ;

protected:
  /// resets the collected statistics 
  void     ResetStats() { mNMatched=mNEvents=0L; };  
  /// whether accept the track or not
  Bool_t   AcceptTrack( const StMuTrack *track);
  /// whether track matches or not
  Bool_t   MatchTrack ( const double dphi,   const double deta,  const double phihw,  const double etahw); 


private:
  Int_t    mMaxCTBsum            ;  /**<- max CTB sum allowed                 */
  Int_t    mMinTrackHits         ;  /**<- min hits per track required         */
  Double_t mMinTrackLength       ;  /**<- min track length required           */
  Double_t mMinTrackPt           ;  /**<- min track transv. momentum required */
  Double_t mMinTrackEta          ;  /**<- min track pseudorapidity required   */
  Double_t mMaxTrackEta          ;  /**<- min track pseudorapidity required   */
  
  Double_t mPhiFac;                 /**<- phi factor */
  Double_t mEtaFac;                 /**<- eta factor */


  // control histograms for tracks
  ///TList *histList;
  TH1F  *hTrackNHits; /**<- number of hits/track */
  TH1F  *hTrackLen;   /**<- track  length        */
  TH1F  *hTrackPt ;   /**<- track  p_T           */
  TH1F  *hTrackPtot;  /**<- track  p_tot         */

  TH1F  *hTrackDCA[3];/**<- tracks DCA           */
  TH1F  *hVertex[3]  ;/**<- vertex               */

  StMuDstMaker   *mMuDstMaker; // toplevel muDST maker
  StEEmcDb  *mEEmcDb;     // EEMC database maker
  //
  EEmcGeomSimple& mGeom;       // tower geometry

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
  //
  StEventInfo             *mEvInfo;
  StEventSummary          *mEvSumm;
  StMuTriggerIdCollection *mEvTrig;

  bool            mTreeOut;

 public:
  //  StMaker jumbo mumbo
  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]=
      "Tag $Name:  $ $Id: EEmcTTMMaker.h,v 1.20 2014/08/06 11:43:02 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  
  ClassDef(EEmcTTMMaker, 1)   // 
};

/// for nice printing
ostream&  operator<<(ostream &out, const EEmcTTMMaker &ttm); 
#endif

// $Log: EEmcTTMMaker.h,v $
// Revision 1.20  2014/08/06 11:43:02  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.19  2009/02/04 20:33:24  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.18  2004/06/03 21:02:29  zolnie
// fixed subtle bug: when e.g. dphi = +180.(tower center) - -180.0(track)
//  the match would be rejected - in practice it never happen
//
// Revision 1.17  2004/05/10 23:02:53  zolnie
// EEmcTTMMaker produces now  nanoDST
//
// Revision 1.16  2004/05/07 22:02:56  zolnie
// fixed a nasty memory leak in EEmcTTMMaker
//
// Revision 1.15  2004/05/06 16:02:49  zolnie
// more docs
//
// Revision 1.14  2004/05/05 23:00:57  zolnie
// more docs
//
// Revision 1.13  2004/05/05 22:04:16  zolnie
// forgor about EEmcTower
//
// Revision 1.12  2004/05/05 21:37:37  zolnie
// ver 2.0 released
//
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
