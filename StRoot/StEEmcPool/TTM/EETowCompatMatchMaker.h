// $Id: EETowCompatMatchMaker.h,v 1.1.1.1 2003/12/18 18:00:54 zolnie Exp $

#ifndef STAR_EETowCompatMatchMaker
#define STAR_EETowCompatMatchMaker

/*!
 *                                                                     
 * \class  EETowCompatMatchMaker
 * \author Piotr A. Zolnierczuk
 * \date   2003/12/08
 * \brief  EEMC tower calibration using TPC tracks
 *
 * This a MuDST based class to get tower calibration from matching TPC tracks
 *
 */                                                                      



#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "TString.h"


class TH1F;
class TTree;
class TFile;
class TString;

class StMuTrack;
class EEmcGeomSimple;

class StMuDstMaker;
class StEEmcDbMaker;

//
const int kCompatTTM_MaxTracks  = 1024;
const int kCompatTTM_MaxTrigger =   32;

struct CompatTTM {

  int   numtracks;
  int   sector[kCompatTTM_MaxTracks];
  int   subsec[kCompatTTM_MaxTracks];
  int   etabin[kCompatTTM_MaxTracks];
  float adcval[kCompatTTM_MaxTracks];
  //
  int   nhits [kCompatTTM_MaxTracks];
  float pt    [kCompatTTM_MaxTracks];
  float ptot  [kCompatTTM_MaxTracks];
  float length[kCompatTTM_MaxTracks];
  float dedx  [kCompatTTM_MaxTracks];
  //
  float xvert [kCompatTTM_MaxTracks];
  float yvert [kCompatTTM_MaxTracks];
  float zvert [kCompatTTM_MaxTracks];
  //
  float xsmd  [kCompatTTM_MaxTracks];
  float ysmd  [kCompatTTM_MaxTracks];
  float etasmd[kCompatTTM_MaxTracks];
  float phismd[kCompatTTM_MaxTracks];
  //
  float detasmd[kCompatTTM_MaxTracks];
  float dphismd[kCompatTTM_MaxTracks];
  //
  float detapres[kCompatTTM_MaxTracks];
  float dphipres[kCompatTTM_MaxTracks];
  //
  float detapost[kCompatTTM_MaxTracks];
  float dphipost[kCompatTTM_MaxTracks];
  
  // for Trigger Info 
  int   numtrig;
  int   trigid[kCompatTTM_MaxTrigger];
  int   daqbits;
};





class EETowCompatMatchMaker : public StMaker {
 public: 
  EETowCompatMatchMaker(const char          *self      ="MuEmcTowerCalib", 
		       StMuDstMaker  *mumaker = NULL,
		       StEEmcDbMaker *dbmaker = NULL);
  virtual       ~EETowCompatMatchMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   Clear(Option_t *option = "");
  virtual Int_t  Finish();


  Int_t   SetDebugLevel(const int d) { mDebugLevel=d; return d; } // use kInfo,kWarning,etc. 
  Int_t   GetDebugLevel(           ) { return mDebugLevel;      } //

  void    SetFileName( const char *string) { mFileName=TString(string); }
   

 protected:
  Int_t            mDebugLevel           ;  // debug level:  use kInfo,kWarning,etc. from TError.h
  Bool_t           accept(StMuTrack*    ) ; 

  // control histograms for tracks
  TH1F *hTrackNHits;
  TH1F *hTrackLen;
  TH1F *hTrackPt ;
  TH1F *hTrackPtot;

  TH1F *hTrackDCAX;
  TH1F *hTrackDCAY;
  TH1F *hTrackDCAZ;

  TH1F *hVertexX ;
  TH1F *hVertexY ;
  TH1F *hVertexZ ;


 private:
  StMuDstMaker   *mMuDstMaker; // toplevel muDST maker
  StEEmcDbMaker  *mEEmcDb;     // EEMC database maker
  EEmcGeomSimple *mGeom;       // tower geometry

  TString         mFileName;  // output file name
  TFile          *mFile;      // output file
  TTree          *mTree;      // output tree

  struct CompatTTM *mMatch;   // data in "compatible" format 

 public:
  //  StMaker jumbo mumbo
  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: EETowCompatMatchMaker.h,v 1.1.1.1 2003/12/18 18:00:54 zolnie Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }


  ClassDef(EETowCompatMatchMaker, 1)   // 
};

#endif


// $Log: EETowCompatMatchMaker.h,v $
// Revision 1.1.1.1  2003/12/18 18:00:54  zolnie
// Imported sources
//
// Revision 1.1.1.1  2003/12/15 22:48:47  zolnie
// Imported sources
//
