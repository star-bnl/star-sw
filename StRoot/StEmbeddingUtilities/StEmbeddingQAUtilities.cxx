
#include "TError.h"
#include "TH1.h"
#include "TStyle.h"
#include "StEmbeddingQAUtilities.h"
#include "StMessMgr.h"

using namespace std ;

  StEmbeddingQAUtilities* StEmbeddingQAUtilities::mInstance = 0 ;

//____________________________________________________________________________________________________
StEmbeddingQAUtilities::StEmbeddingQAUtilities()
{
  // Initialize category in minimc nodes
  //   category id (integer)   Category      category name (TString)   category title (TString)
  //           0                 MC                  MC                    MC tracks
  //           1                 MATCHED             MATCHED               Matched pairs
  //           2                 GHOST               GHOST                 Ghost pairs
  //           3                 CONTAM              CONTAM                Contaminated pairs
  //           4                 MATGLOB             MATGLOB               Matched global pairs
  //           5                 PRIMARY             PRIMARY               Primary tracks (real)
  //           6                 GLOBAL              GLOBAL                Global tracks (real)
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
}

//____________________________________________________________________________________________________
StEmbeddingQAUtilities::~StEmbeddingQAUtilities()
{

}

//____________________________________________________________________________________________________
StEmbeddingQAUtilities* StEmbeddingQAUtilities::instance()
{
  if ( !mInstance ) mInstance = new StEmbeddingQAUtilities();

  return mInstance ;
}


//__________________________________________________________________________________________
Category StEmbeddingQAUtilities::getCategory(const UInt_t id) const
{
  // Get category from category id
  if ( id >= StEmbeddingQAConst::mNEmbedding ){
    Error("StEmbeddingQAUtilities::getCategory", "Unknown category id, id=%3d. Return MC", id);
    return mCategory[0] ;
  }

  return mCategory[id] ;
}

//__________________________________________________________________________________________
TString StEmbeddingQAUtilities::getCategoryName(const UInt_t id) const
{
  // Get category name from category id
  if( id >= StEmbeddingQAConst::mNCategory ){
    Error("StEmbeddingQAUtilities::getCategoryName", "Unknown category id, id=%3d. Return MC name", id);
    return mCategoryName[0];
  }

  return mCategoryName[id] ;
}

//__________________________________________________________________________________________
TString StEmbeddingQAUtilities::getCategoryTitle(const UInt_t id) const
{
  // Get category title from category id
  if( id >= StEmbeddingQAConst::mNCategory ){
    Error("StEmbeddingQAUtilities::getCategoryTitle", "Unknown category id, id=%3d. Return MC title", id);
    return mCategoryTitle[0] ;
  }

  return mCategoryTitle[id] ;
}

//__________________________________________________________________________________________
Int_t StEmbeddingQAUtilities::getCategoryId(const TString name) const
{
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
  // Utility function to compare two TString
  //   - Comparison is case insensitive by default
  //   - You can do the exact match by setting isExact = true
  TString s0lower(s0);
  TString s1lower(s1);

  if( !isExact ){
    s0lower.ToLower();
    s1lower.ToLower();
  }

  return s0lower.Contains(s1lower);
}
  
//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isMc(const TString name) const
{
  return CompareString(name, mCategoryName[0]);
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isMatched(const TString name) const
{
  return CompareString(name, mCategoryName[1]);
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isGhost(const TString name) const
{
  return CompareString(name, mCategoryName[2]);
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isContaminated(const TString name) const
{
  return CompareString(name, mCategoryName[3]);
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isMatchedGlobal(const TString name) const
{
  return CompareString(name, mCategoryName[4]);
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isPrimary(const TString name) const
{
  return CompareString(name, mCategoryName[5]) ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isGlobal(const TString name) const
{
  return CompareString(name, mCategoryName[6]) ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isEmbedding(const TString name) const
{
  return isMatched(name) || isGhost(name) || isContaminated(name) || isMatchedGlobal(name) ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isReal(const TString name) const
{
  return isPrimary(name) || isGlobal(name) ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isElectrons(const Int_t geantid) const
{
  return (geantid==2 || geantid==3) ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isPions(const Int_t geantid) const
{
  return (geantid==8 || geantid==9) ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isKaons(const Int_t geantid) const
{
  return (geantid==11 || geantid==12) ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isProtons(const Int_t geantid) const
{
  return (geantid==14 || geantid==15) ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAUtilities::isEPiKP(const Int_t geantid) const
{
  return isElectrons(geantid) || isPions(geantid) || isKaons(geantid) || isProtons(geantid) ;
}

//____________________________________________________________________________________________________
void StEmbeddingQAUtilities::setStyle() const
{
  const Int_t font = 42;

  //_______________________________________________________________
  gStyle->SetOptStat(0);
  gStyle->SetPalette(1);

  //_______________________________________________________________
  //  Canvas style
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
  //  Statistics
  gStyle->SetStatColor(10);

  //_______________________________________________________________
  //  Text
  gStyle->SetTextSize(0.07);
  gStyle->SetTextFont(font);

  //_______________________________________________________________
  //  Histogram style
  gStyle->SetNdivisions(505,"XYZ");

  // Label
  gStyle->SetLabelSize(0.07, "XYZ");
  gStyle->SetLabelOffset(0.011, "XYZ");
  gStyle->SetLabelFont(font, "XYZ");

  // Title
  gStyle->SetTitleSize(0.085, "XYZ");
  gStyle->SetTitleOffset(1.05, "X");
  gStyle->SetTitleOffset(1.18, "Y");
  gStyle->SetTitleFont(font, "XYZ");

  // Pad title
  gStyle->SetTitleFont(42, "t"); // Set pad title font if the option is not "X or Y or Z"
  gStyle->SetTitleH(0.07);
  gStyle->SetTitleW(0.6);
  gStyle->SetTitleBorderSize(0);
  gStyle->SetTitleFillColor(10);
  gStyle->SetTitleX(0.1);

  //_______________________________________________________________
  // Legend
  gStyle->SetLegendBorderSize(0);
}

//____________________________________________________________________________________________________
void StEmbeddingQAUtilities::setStyle(TH1* h) const
{
  LOG_DEBUG << "setStyle() for " << h->GetName() << endm;

  const Int_t font = 42 ;
  h->GetXaxis()->SetTitleFont(font);
  h->GetYaxis()->SetTitleFont(font);
  h->GetZaxis()->SetTitleFont(font);

  h->SetTitleSize(0.085, "X"); h->SetTitleSize(0.085, "Y"); h->SetTitleSize(0.085, "Z");
  h->SetTitleOffset(1.05, "X");
  h->SetTitleOffset(1.18, "Y");

  h->SetLabelSize(0.07, "X"); h->SetLabelSize(0.07, "Y"); h->SetLabelSize(0.07, "Z");
  h->SetLabelOffset(0.011, "X"); h->SetLabelOffset(0.011, "Y"); h->SetLabelOffset(0.011, "Z");
  h->SetLabelFont(font, "X"); h->SetLabelFont(font, "Y"); h->SetLabelFont(font, "Z");

  h->SetNdivisions(505,"X"); h->SetNdivisions(505,"Y"); h->SetNdivisions(505,"Z");

  h->SetLineWidth(2);
}

