#ifndef StMcAnalysisMaker_HH
#define StMcAnalysisMaker_HH

#ifndef StMaker_H
#include "StMaker.h"
#endif

//  - if not using the methods of the class, then we can just put class TCanvas;
//   -  however, if we are using the methods of TCanvas, then #include "TCanvas.h"

class TH1F;
class TH2F;
class TFile;
class TNtuple;
class TCanvas;


class StMcAnalysisMaker : public StMaker {

 public:

    StMaker* currentChain;
    StMcAnalysisMaker(const char* name = "StMcAnalysisMaker",
		       const char* title = "event/StMcAnalysisMaker");
    virtual ~StMcAnalysisMaker();
    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    // SetZones --> divide canvas into 2 x 2 zones
    virtual void   SetZones(Int_t columns=2, Int_t rows=2);

    TH1F*     mMomResolution;    //! Diff. between p of rec. & Monte Carlo, in %
    TH2F*     mHitResolution;    //! Diff. between x and z coordinates of the hits.
    TH2F*     mPxlHitResolution;    //! Diff. between x and z coordinates of the hits.
    TH2F*     mIstHitResolution;    //! Diff. between x and z coordinates of the hits.
    TH2F*     mSsdHitResolution;    //! Diff. between x and z coordinates of the hits.
    TH2F*     coordRec;          //! X and Y coord of rec. Track.
    TH2F*     coordMcPartner;    //! X and Y coord of  MC  Track.
    TFile*    mNtupleFile;       //! File to contain the mTrackNtuple, otherwise it is deleted!
    TNtuple*  mTrackNtuple;      //! Miscellaneous info of the track pairs
    TNtuple*  mTpcHitNtuple;     //! Miscellaneous info of the TPC hit pairs
    TNtuple*  mPxlHitNtuple;     //! Miscellaneous info of the PXL hit pairs
    TNtuple*  mIstHitNtuple;     //! Miscellaneous info of the IST hit pairs
    TNtuple*  mSsdHitNtuple;     //! Miscellaneous info of the SSD hit pairs
    // Data-members to make up the output Canvases
    TCanvas*       mAssociationCanvas;    //!   
    Int_t          mPadColumns;     // Number of the columns (TPad's) on the single Canvas
    Int_t          mPadRows;        // Number of the rows (TPad's) on the single Canvas

private:

    Bool_t drawinit;
    
    //! Histograms booking constants
    static const Int_t mNumDeltaX;
    static const Int_t mNumDeltaZ;
    static const Float_t mMinDeltaX;
    static const Float_t mMaxDeltaX;
    static const Float_t mMinDeltaZ;
    static const Float_t mMaxDeltaZ;

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StMcAnalysisMaker.h,v 1.11 2015/03/12 23:07:06 perev Exp $ built " __DATE__ " " __TIME__; return cvs;}	
    
    // the following is a ROOT macro  that is needed in all ROOT accessible code
    ClassDef(StMcAnalysisMaker,0)

};

inline void StMcAnalysisMaker::SetZones(Int_t columns, Int_t rows){ mPadColumns =columns; mPadRows = rows;}

#endif
