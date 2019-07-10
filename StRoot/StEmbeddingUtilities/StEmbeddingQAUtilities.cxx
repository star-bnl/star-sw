/****************************************************************************************************
 * $Id: StEmbeddingQAUtilities.cxx,v 1.18 2019/07/10 05:46:43 zhux Exp $
 * $Log: StEmbeddingQAUtilities.cxx,v $
 * Revision 1.18  2019/07/10 05:46:43  zhux
 * added option for btof pid for primary real tracks
 *
 * Revision 1.17  2012/06/11 14:35:55  fisyak
 * std namespace
 *
 * Revision 1.16  2012/03/05 10:32:43  cpowell
 * Functions added to cut on refMult
 *
 * Revision 1.15  2011/08/25 17:37:26  cpowell
 * Remove use of function getGeantId(const UInt_t geantid)
 *
 * Revision 1.14  2011/08/05 00:26:50  cpowell
 * Fix by Xianglei related to the update in StMiniMcMaker for calculating dcaGl of StTinyRcTrack
 *
 * Revision 1.13  2011/04/26 20:27:22  hmasui
 * Add isGamma function
 *
 * Revision 1.12  2011/04/05 23:12:36  hmasui
 * Added getGeantId() function
 *
 * Revision 1.11  2011/04/01 05:02:50  hmasui
 * Implement track selections (moved from StEmbeddingQATrack)
 *
 * Revision 1.10  2011/02/11 23:20:56  hmasui
 * Bug fix for e+ and e-
 *
 * Revision 1.9  2010/08/13 21:54:51  hmasui
 * Separate charge for pi/K/p
 *
 * Revision 1.8  2010/07/12 21:27:32  hmasui
 * Added StParticleTable & StParticleDefinition utilities
 *
 * Revision 1.7  2010/06/10 14:50:28  hmasui
 * Use TString::KIgnoreCase
 *
 * Revision 1.6  2010/02/16 02:10:46  hmasui
 * Add TStyle date attributes
 *
 * Revision 1.5  2009/12/22 21:37:54  hmasui
 * Add comments for functions and members
 *
 ****************************************************************************************************/

#include <assert.h>

#include "TError.h"
#include "TH1.h"
#include "TStyle.h"

#include "StEmbeddingQAUtilities.h"
#include "StMessMgr.h"
#include "StParticleDefinition.hh"
#include "StParticleTable.hh"
using namespace std;
  StEmbeddingQAUtilities* StEmbeddingQAUtilities::mInstance = 0 ;

//____________________________________________________________________________________________________
StEmbeddingQAUtilities::StEmbeddingQAUtilities()
{
  /// Constructor

  /// Initialize category in minimc nodes
  ///   category id (integer)   Category      category name (TString)   category title (TString)
  ///           0                 MC                  MC                    MC tracks
  ///           1                 MATCHED             MATCHED               Matched pairs
  ///           2                 GHOST               GHOST                 Ghost pairs
  ///           3                 CONTAM              CONTAM                Contaminated pairs
  ///           4                 MATGLOB             MATGLOB               Matched global pairs
  ///           5                 PRIMARY             PRIMARY               Primary tracks (real)
  ///           6                 GLOBAL              GLOBAL                Global tracks (real)
  //
  mCategory[0] = MC ;       mCategoryName[0] = "MC";        mCategoryTitle[0] = "MC tracks";
  mCategory[1] = MATCHED ;  mCategoryName[1] = "MATCHED";   mCategoryTitle[1] = "Matched pairs" ;
  mCategory[2] = GHOST ;    mCategoryName[2] = "GHOST";     mCategoryTitle[2] = "Ghost pairs" ;
  mCategory[3] = CONTAM ;   mCategoryName[3] = "CONTAM";    mCategoryTitle[3] = "Contaminated pairs" ;
  mCategory[4] = MATGLOB ;  mCategoryName[4] = "MATGLOB";   mCategoryTitle[4] = "Matched global pairs" ;
                            mCategoryName[5] = "PRIMARY";   mCategoryTitle[5] = "Primary tracks (real)";
                            mCategoryName[6] = "GLOBAL";    mCategoryTitle[6] = "Global tracks (real)";

  mCategoryId.insert( pair<const TString, const UInt_t>(mCategoryName[0], 0) );
  mCategoryId.insert( pair<const TString, const UInt_t>(mCategoryName[1], 1) );
  mCategoryId.insert( pair<const TString, const UInt_t>(mCategoryName[2], 2) );
  mCategoryId.insert( pair<const TString, const UInt_t>(mCategoryName[3], 3) );
  mCategoryId.insert( pair<const TString, const UInt_t>(mCategoryName[4], 4) );
  mCategoryId.insert( pair<const TString, const UInt_t>(mCategoryName[5], 5) );
  mCategoryId.insert( pair<const TString, const UInt_t>(mCategoryName[6], 6) );

  // Default cut
  mPtMinCut       = 0.1 ;   /// Minimum pt cut, pt > 0.1 GeV/c
  mPtMaxCut       = 10.0 ;  /// Minimum pt cut, pt < 10 GeV/c
  mEtaCut         = 1.5 ;   /// Eta cut, |eta| < 1.5
  mNHitCut        = 10 ;    /// Minimum Nfit cut, Nfit >= 10
  mNHitToNPossCut = 0.51 ;  /// Minimum Nfit cut, NHitFit/NHitPoss > 0.51
  mDcaCut         = 3.0 ;   /// Global dca cut, |dca_{gl}| < 3 cm
  mNSigmaCut      = 2.0 ;   /// Nsigma cut, |Nsigma| < 2
  mBTofPid        = kFALSE ;/// Nsigma cut, |Nsigma| < 2, on TPC dE/dx Pid by default 
  mRapidityCut    = 10.0 ;  /// Rapidity cut, |y| < 10
  mZVertexCut     = 30.0 ;  /// z-vertex cut, |vz| < 30 cm
  mRefMultMinCut  = 0 ;  		/// refMult cut, refMult >= 0
  mRefMultMaxCut  = 1000 ;	/// refMult cut, refMult < 1000
  mTriggerIdCut.clear() ;   /// No trigger cut by default
}

//____________________________________________________________________________________________________
StEmbeddingQAUtilities::~StEmbeddingQAUtilities()
{
  /// Destructor
}

//____________________________________________________________________________________________________
StEmbeddingQAUtilities* StEmbeddingQAUtilities::instance()
{
  /// Interface to access the StEmbeddingQAUtilities

  /// Create new StEmbeddingQAUtilities if it has not been defined before
  if ( !mInstance ) mInstance = new StEmbeddingQAUtilities();

  /// Return instance if it has already been defined
  return mInstance ;
}


//__________________________________________________________________________________________
Category StEmbeddingQAUtilities::getCategory(const UInt_t id) const
{
  /// Get category from category id
  if ( id >= StEmbeddingQAConst::mNEmbedding ){
    Error("StEmbeddingQAUtilities::getCategory", "Unknown category id, id=%3d. Return MC", id);
    return mCategory[0] ;
  }

  return mCategory[id] ;
}

//__________________________________________________________________________________________
TString StEmbeddingQAUtilities::getCategoryName(const UInt_t id) const
{
  /// Get category name from category id
  if( id >= StEmbeddingQAConst::mNCategory ){
    Error("StEmbeddingQAUtilities::getCategoryName", "Unknown category id, id=%3d. Return MC name", id);
    return mCategoryName[0];
  }

  return mCategoryName[id] ;
}

//__________________________________________________________________________________________
TString StEmbeddingQAUtilities::getCategoryTitle(const UInt_t id) const
{
  /// Get category title from category id
  if( id >= StEmbeddingQAConst::mNCategory ){
    Error("StEmbeddingQAUtilities::getCategoryTitle", "Unknown category id, id=%3d. Return MC title", id);
    return mCategoryTitle[0] ;
  }

  return mCategoryTitle[id] ;
}

//__________________________________________________________________________________________
Int_t StEmbeddingQAUtilities::getCategoryId(const TString name) const
{
  /// Find category id from the category name

  map<const TString, const UInt_t>::const_iterator iter = mCategoryId.find(name);

  if( iter != mCategoryId.end() ) return iter->second ;
  else{
    Error("StEmbeddingQAUtilities::getCategoryId", "Unknown category name, name=%s. Return MC id", name.Data());
    return 0; // return MC track
  }
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::CompareString(const TString s0, const TString s1, const Bool_t isExact) const
{
  /// Utility function to compare two TString
  ///   - Comparison is case insensitive by default
  ///   - You can do the exact match by setting isExact = true

  return (isExact) ? s0.CompareTo(s1)==0 : s0.CompareTo(s1, TString::kIgnoreCase)==0 ; 
}
  
//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isMc(const TString name) const
{
  /// Check the input string whether it is MC tracks or not

  return CompareString(name, mCategoryName[0]);
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isMatched(const TString name) const
{
  /// Check the input string whether it is Mathced pairs or not

  return CompareString(name, mCategoryName[1]);
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isGhost(const TString name) const
{
  /// Check the input string whether it is Ghost pairs or not

  return CompareString(name, mCategoryName[2]);
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isContaminated(const TString name) const
{
  /// Check the input string whether it is Contaminated pairs or not

  return CompareString(name, mCategoryName[3]);
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isMatchedGlobal(const TString name) const
{
  /// Check the input string whether it is Matched global pairs or not

  return CompareString(name, mCategoryName[4]);
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isPrimary(const TString name) const
{
  /// Check the input string whether it is Primary tracks (real) or not

  return CompareString(name, mCategoryName[5]) ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isGlobal(const TString name) const
{
  /// Check the input string whether it is Global tracks (real) or not

  return CompareString(name, mCategoryName[6]) ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isEmbedding(const TString name) const
{
  /// Check the input string whether it is Embeeding pairs or not
  /// Embedding pairs = (Matched || Ghost || Contaminated || Matched global)

  return isMatched(name) || isGhost(name) || isContaminated(name) || isMatchedGlobal(name) ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isReal(const TString name) const
{
  /// Check the input string whether it is Real tracks or not
  /// Real tracks = (Primary || Global)

  return isPrimary(name) || isGlobal(name) ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isPositron(const Int_t geantid) const { return geantid==2 ; }

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isElectron(const Int_t geantid) const { return geantid==3 ; }

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isPiPlus(const Int_t geantid) const { return geantid==8 ; }

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isPiMinus(const Int_t geantid) const { return geantid==9 ; }

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isKPlus(const Int_t geantid) const { return geantid==11 ; }

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isKMinus(const Int_t geantid) const { return geantid==12 ; }

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isProton(const Int_t geantid) const { return geantid==14 ; }

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isPBar(const Int_t geantid) const { return geantid==15 ; }

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isElectrons(const Int_t geantid) const
{
  /// Check the input string geant id is electron/positron
  /// NOTE: electron/positron id's are currently hard-coded. I'm not sure I can avoid this at this point (H. Masui)

  return (isElectron(geantid) || isPositron(geantid));
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isPions(const Int_t geantid) const
{
  /// Check the input string geant id is pions
  /// NOTE: pion id's are currently hard-coded. I'm not sure I can avoid this at this point (H. Masui)

  return (isPiPlus(geantid) || isPiMinus(geantid));
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isKaons(const Int_t geantid) const
{
  /// Check the input string geant id is kaons
  /// NOTE: kaon id's are currently hard-coded. I'm not sure I can avoid this at this point (H. Masui)

  return (isKPlus(geantid) || isKMinus(geantid));
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isProtons(const Int_t geantid) const
{
  /// Check the input string geant id is protons
  /// NOTE: proton id's are currently hard-coded. I'm not sure I can avoid this at this point (H. Masui)

  return (isProton(geantid) || isPBar(geantid)) ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isEPiKP(const Int_t geantid) const
{
  /// Check the input string geant id is e/pi/K/p

  return isElectrons(geantid) || isPions(geantid) || isKaons(geantid) || isProtons(geantid) ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isGamma(const Int_t geantid) const
{
  /// Check geantid is gamma or not

  return (geantid == 1) ;
}

//____________________________________________________________________________________________________
void StEmbeddingQAUtilities::setStyle() const
{
  // Set styles for ROOT

  /// Set font id (=42)
  const Int_t font = 42;

  //_______________________________________________________________
  /// Do not display statistics
  gStyle->SetOptStat(0);

  /// SetPalette(1)
  gStyle->SetPalette(1);

  //_______________________________________________________________
  ///  Set Canvas style
  gStyle->SetPadColor(10);
  gStyle->SetCanvasColor(10);
  gStyle->SetFrameLineWidth(2);
  gStyle->SetPadTickX(1);
  gStyle->SetPadTickY(1);
  gStyle->SetPadRightMargin(0.15);
  gStyle->SetPadLeftMargin(0.21);
  gStyle->SetPadTopMargin(0.10);
  gStyle->SetPadBottomMargin(0.20);

  //_______________________________________________________________
  ///  Set fill color (=10) for Statistics
  gStyle->SetStatColor(10);

  //_______________________________________________________________
  ///  Set Text size and font
  gStyle->SetTextSize(0.07);
  gStyle->SetTextFont(font);

  //_______________________________________________________________
  ///  Set Histogram style
  ///  Set Ndivision=505
  gStyle->SetNdivisions(505,"XYZ");

  /// Set label size, offset and font
  gStyle->SetLabelSize(0.07, "XYZ");
  gStyle->SetLabelOffset(0.011, "XYZ");
  gStyle->SetLabelFont(font, "XYZ");

  /// Set title size, offset and font for x/y/z axes
  gStyle->SetTitleSize(0.085, "XYZ");
  gStyle->SetTitleOffset(1.05, "X");
  gStyle->SetTitleOffset(1.18, "Y");
  gStyle->SetTitleFont(font, "XYZ");

  /// Set pad title font
  gStyle->SetTitleFont(42, "t"); // Set pad title font if the option is not "X or Y or Z"

  /// Set title style for histograms
  gStyle->SetTitleH(0.07);
  gStyle->SetTitleW(0.6);
  gStyle->SetTitleBorderSize(0);
  gStyle->SetTitleFillColor(10);
  gStyle->SetTitleX(0.1);

  //_______________________________________________________________
  /// Set Legend border size (=0)
  gStyle->SetLegendBorderSize(0);

  /// Set date attributes
  gStyle->SetOptDate(3);
  gStyle->GetAttDate()->SetTextFont(52);
  gStyle->GetAttDate()->SetTextColor(10);
  gStyle->GetAttDate()->SetTextSize(0.03);
}

//____________________________________________________________________________________________________
void StEmbeddingQAUtilities::setStyle(TH1* h) const
{
  /// Set style for input histogram

  LOG_DEBUG << "setStyle() for " << h->GetName() << endm;

  /// Set font (=42) for x/y/z axes
  const Int_t font = 42 ;
  h->GetXaxis()->SetTitleFont(font);
  h->GetYaxis()->SetTitleFont(font);
  h->GetZaxis()->SetTitleFont(font);

  /// Set title size and offset for x/y/z axes
  h->SetTitleSize(0.085, "X"); h->SetTitleSize(0.085, "Y"); h->SetTitleSize(0.085, "Z");
  h->SetTitleOffset(1.05, "X");
  h->SetTitleOffset(1.18, "Y");

  /// Set label size, offset, font and Ndivisions for x/y/z axes
  h->SetLabelSize(0.07, "X"); h->SetLabelSize(0.07, "Y"); h->SetLabelSize(0.07, "Z");
  h->SetLabelOffset(0.011, "X"); h->SetLabelOffset(0.011, "Y"); h->SetLabelOffset(0.011, "Z");
  h->SetLabelFont(font, "X"); h->SetLabelFont(font, "Y"); h->SetLabelFont(font, "Z");
  h->SetNdivisions(505,"X"); h->SetNdivisions(505,"Y"); h->SetNdivisions(505,"Z");

  /// Set line width (=2)
  h->SetLineWidth(2);
}

//____________________________________________________________________________________________________
StParticleDefinition* StEmbeddingQAUtilities::getParticleDefinition(const UInt_t geantid) const
{
  /// Take into account the modulus of geant id by 10k
  if(!isGeantIdOk(geantid)) {
    LOG_ERROR << Form("StEmbeddingQAUtilities::getParticleDefinition  Cannot find geantid=%d in StParticleTable", geantid) << endm;
    LOG_ERROR << "StEmbeddingQAUtilities::getParticleDefinition  You should check out the latest StRoot/StarClassLibrary from CVS" << endm;
    assert(0);
  }

  return StParticleTable::instance()->findParticleByGeantId(geantid);
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isGeantIdOk(const UInt_t geantid) const
{
  /// Check geant id in StParticleTable
  ///  Take into account the modulus of geantid by 10k

  return StParticleTable::instance()->containsGeantId(geantid);
}

//__________________________________________________________________________________________
Int_t StEmbeddingQAUtilities::getGeantId(const UInt_t geantid) const
{
  /// Return geantid itself
  /// except for geantid > 50000
  return ( geantid > 50000 ) ? geantid%50000 : geantid ;
}

//__________________________________________________________________________________________
Float_t StEmbeddingQAUtilities::getPtMinCut() const { return mPtMinCut ; }

//__________________________________________________________________________________________
Float_t StEmbeddingQAUtilities::getPtMaxCut() const { return mPtMaxCut ; }

//__________________________________________________________________________________________
Float_t StEmbeddingQAUtilities::getEtaCut() const { return mEtaCut ; }

//__________________________________________________________________________________________
Short_t StEmbeddingQAUtilities::getNHitCut() const { return mNHitCut ; }

//__________________________________________________________________________________________
Float_t StEmbeddingQAUtilities::getNHitToNPossCut() const { return mNHitToNPossCut ; }

//__________________________________________________________________________________________
Float_t StEmbeddingQAUtilities::getDcaCut() const { return mDcaCut ; }

//__________________________________________________________________________________________
Double_t StEmbeddingQAUtilities::getNSigmaCut() const { return mNSigmaCut ; }

//__________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::getBTofPid() const { return mBTofPid ; }

//__________________________________________________________________________________________
Float_t StEmbeddingQAUtilities::getRapidityCut() const { return mRapidityCut ; }

//__________________________________________________________________________________________
Int_t StEmbeddingQAUtilities::getRefMultMinCut() const { return mRefMultMinCut ; }

//__________________________________________________________________________________________
Int_t StEmbeddingQAUtilities::getRefMultMaxCut() const { return mRefMultMaxCut ; }

//__________________________________________________________________________________________
Float_t StEmbeddingQAUtilities::getZVertexCut() const { return mZVertexCut ; }

//__________________________________________________________________________________________
vector<UInt_t> StEmbeddingQAUtilities::getTriggerIdCut() const { return mTriggerIdCut ; }

//__________________________________________________________________________________________
Float_t StEmbeddingQAUtilities::setPtMinCut(const Float_t val)
{
  mPtMinCut = val ;
  LOG_INFO << "StEmbeddingQAUtilities::setPtMinCut  Minimum pt cut off = " << mPtMinCut
    << endm;
  return getPtMinCut() ;
}

//__________________________________________________________________________________________
Float_t StEmbeddingQAUtilities::setPtMaxCut(const Float_t val)
{
  mPtMaxCut = val ;
  LOG_INFO << "StEmbeddingQAUtilities::setPtMaxCut  Maximum pt cut off = " << mPtMaxCut
    << endm;
  return getPtMaxCut() ;
}

//__________________________________________________________________________________________
Float_t StEmbeddingQAUtilities::setEtaCut(const Float_t val)
{
  mEtaCut = val ;
  LOG_INFO << "StEmbeddingQAUtilities::setEtaCut  eta cut = " << mEtaCut
    << endm;
  return getEtaCut() ;
}

//__________________________________________________________________________________________
Short_t StEmbeddingQAUtilities::setNHitCut(const Short_t val)
{
  mNHitCut = val ;
  LOG_INFO << "StEmbeddingQAUtilities::setNHitCut  nHitsFit cut = " << mNHitCut
    << endm;
  return getNHitCut() ;
}

//__________________________________________________________________________________________
Float_t StEmbeddingQAUtilities::setNHitToNPossCut(const Float_t val)
{
  mNHitToNPossCut = val ;
  LOG_INFO << "StEmbeddingQAUtilities::setNHitToNPossCut  Ratio of nHitsFit to nHitsPoss cut = " << mNHitToNPossCut
    << endm;
  return getNHitToNPossCut() ;
}

//__________________________________________________________________________________________
Float_t StEmbeddingQAUtilities::setDcaCut(const Float_t val)
{
  mDcaCut = val ;
  LOG_INFO << "StEmbeddingQAUtilities::setDcaCut  Dca cut = " << mDcaCut
    << endm;
  return getDcaCut() ;
}

//__________________________________________________________________________________________
Double_t  StEmbeddingQAUtilities::setNSigmaCut(const Double_t val)
{
  mNSigmaCut = val ;
  LOG_INFO << "StEmbeddingQAUtilities::setNSigmaCut  NSigma cut = " << mNSigmaCut
    << endm;
  return getNSigmaCut() ;
}

//__________________________________________________________________________________________
Bool_t  StEmbeddingQAUtilities::setBTofPid(const Bool_t val)
{
  mBTofPid = val ;
  LOG_INFO << "StEmbeddingQAUtilities::setBTofPid  NSigma cut using BTof Pid = " << (mBTofPid?"true":"false")
    << endm;
  return getBTofPid() ;
}

//__________________________________________________________________________________________
Float_t StEmbeddingQAUtilities::setRapidityCut(const Float_t val)
{
  mRapidityCut = val ;
  LOG_INFO << "StEmbeddingQAUtilities::setRapidityCut  Rapidity cut = " << mRapidityCut
    << endm;
  return getRapidityCut() ;
}

//__________________________________________________________________________________________
Int_t StEmbeddingQAUtilities::setRefMultMinCut(const Int_t val)
{
  mRefMultMinCut = val ;
  LOG_INFO << "StEmbeddingQAUtilities::setRefMultCut  refMult cut >= " << mRefMultMinCut 
    << endm;
  return getRefMultMinCut() ;
}

//__________________________________________________________________________________________
Int_t StEmbeddingQAUtilities::setRefMultMaxCut(const Int_t val)
{
  mRefMultMaxCut = val ;
  LOG_INFO << "StEmbeddingQAUtilities::setRefMultCut  refMult cut < " << mRefMultMaxCut  
    << endm;
  return getRefMultMaxCut() ;
}

//__________________________________________________________________________________________
Float_t StEmbeddingQAUtilities::setZVertexCut(const Float_t val)
{
  mZVertexCut = val ;
  LOG_INFO << "StEmbeddingQAUtilities::setZVertexCut  z-vertex cut = " << mZVertexCut
    << endm;
  return getZVertexCut() ;
}

//__________________________________________________________________________________________
void StEmbeddingQAUtilities::addTriggerIdCut(const UInt_t val)
{
  mTriggerIdCut.push_back(val);
  LOG_INFO << "StEmbeddingQAUtilities::addTriggerIdCut  add new trigger id = " << val
    << endm;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isPtOk(const Float_t pt) const
{
  return (pt > mPtMinCut && pt < mPtMaxCut ) ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isEtaOk(const Float_t eta) const
{
  return TMath::Abs(eta) < mEtaCut ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isNHitsFitOk(const Float_t nHitsFit) const
{
  return nHitsFit > mNHitCut ;
}
//__________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isNHitToNPossOk(const Float_t ratio) const
{
  return ratio > mNHitToNPossCut ;
}
//__________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isDcaOk(const Float_t dca) const
{
  return ( dca > -mDcaCut && dca < mDcaCut ) ;
}
//__________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isNSigmaOk(const Float_t nsigma) const
{
  return TMath::Abs(nsigma) < mNSigmaCut ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isRapidityOk(const Float_t y) const
{
  return TMath::Abs(y) < mRapidityCut ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isRefMultOk(const Int_t refMult) const
{
  return ((refMult >= mRefMultMinCut) && (refMult < mRefMultMaxCut)) ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isZVertexOk(const Float_t vz) const
{
  return TMath::Abs(vz) < mZVertexCut ;
}

//__________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isTriggerOk(const UInt_t trigger) const
{
  // No trigger is required
  if ( mTriggerIdCut.empty() ) return kTRUE ;

  for(UInt_t i=0; i<mTriggerIdCut.size(); i++) {
    if ( trigger == mTriggerIdCut[i] ) return kTRUE ;
  }

  // doesn't match any trigger id's
  return kFALSE ;
}

//__________________________________________________________________________________________
void StEmbeddingQAUtilities::PrintCuts() const
{
  LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;
  LOG_INFO << "  StEmbeddingQAUtilities::PrintCuts()" << endm;
  LOG_INFO << "  Track-wise selections ==================================================" << endm;
  LOG_INFO << Form("    pt cut             : %1.1f - %1.1f GeV/c", getPtMinCut(), getPtMaxCut()) << endm;
  LOG_INFO << Form("    eta cut            : |eta| < %1.1f", getEtaCut()) << endm;
  LOG_INFO << Form("    nHitsFit cut       : nHitsfit > %2d", getNHitCut()) << endm;
  LOG_INFO << Form("    nHits/nHitsMax cut : ratio > %1.2f", getNHitToNPossCut()) << endm;
  LOG_INFO << Form("    dca cut            : dca < %1.1f cm", getDcaCut()) << endm;
  LOG_INFO << Form("    nsigma cut         : nsigma < %1.1f", getNSigmaCut()) << endm;
  LOG_INFO << Form("      using BTofPid    : %s", getBTofPid()?"true":"false") << endm;
  LOG_INFO << Form("    rapidity cut       : |y| < %1.1f", getRapidityCut()) << endm;
  LOG_INFO << "  Event-wise selections ==================================================" << endm;
  LOG_INFO << Form("    z-vertex cut       : |vz| < %1.1f cm", getZVertexCut()) << endm;
  LOG_INFO << Form("    refMult cut        : %i <= refMult < %i", getRefMultMinCut(), getRefMultMaxCut()) << endm;
  if ( !mTriggerIdCut.empty() ) {
    for(UInt_t i=0; i<mTriggerIdCut.size(); i++) {
      LOG_INFO << Form("    trigger id cut     : id = %10d", mTriggerIdCut[i]) << endm;
    }
  }
  LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;
}


