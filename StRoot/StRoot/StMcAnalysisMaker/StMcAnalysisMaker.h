/**********************************************
 *
 * $Id: StMcAnalysisMaker.h,v 1.12 2015/03/13 18:44:50 perev Exp $
 * $Log: StMcAnalysisMaker.h,v $
 * Revision 1.12  2015/03/13 18:44:50  perev
 * Roll back
 *
 * Revision 1.10  2014/08/06 11:43:26  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.9  2005/11/22 21:51:53  fisyak
 * Add NTuple for svt and ssd hit
 *
 * Revision 1.8  2004/01/13 21:06:04  fisyak
 * Add TpcHitNtuple usind IdTruth info
 *
 * Revision 1.7  2003/09/10 19:47:22  perev
 * ansi corrs
 *
 * Revision 1.6  2000/04/20 16:59:47  calderon
 * Pick up the makers with the new names
 * Change the name from "McAnalysis" to "StMcAnalysisMaker"
 * No longer use the helix, use the primary track momentum when available
 * (also avoids dependency on StEventSummary)
 * Denominator for mom. resolution histograms is now simulated momentum.
 *
 * Revision 1.5  1999/09/10 19:11:15  calderon
 * Write the Ntuple in StMcAnalysisMaker into a file.
 * This way it can be accessed after the macro finishes,
 * otherwise it gets deleted.
 *
 * Revision 1.4  1999/09/09 23:59:38  calderon
 * Made the following changes:
 *
 * -book histograms and ntuple in Init()
 * -do not delete the histograms, they are supposed to be
 *  deleted automatically by the chain
 * -don't create the canvas here, now done in the macro
 *
 * Revision 1.3  1999/07/29 15:08:33  calderon
 * Include Mom. Resolution example (Histograms & Ntuple)
 *
 * Revision 1.2  1999/07/28 20:27:30  calderon
 * Version with SL99f libraries
 *
 *
 * Examples to use the structures from
 * StMcEvent and StAssociationMaker
 *
 **********************************************/

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
    TH2F*     mSvtHitResolution;    //! Diff. between x and z coordinates of the hits.
    TH2F*     mSsdHitResolution;    //! Diff. between x and z coordinates of the hits.
    TH2F*     coordRec;          //! X and Y coord of rec. Track.
    TH2F*     coordMcPartner;    //! X and Y coord of  MC  Track.
    TFile*    mNtupleFile;       //! File to contain the mTrackNtuple, otherwise it is deleted!
    TNtuple*  mTrackNtuple;      //! Miscellaneous info of the track pairs
    TNtuple*  mTpcHitNtuple;     //! Miscellaneous info of the TPC hit pairs
    TNtuple*  mSvtHitNtuple;     //! Miscellaneous info of the TPC hit pairs
    TNtuple*  mSsdHitNtuple;     //! Miscellaneous info of the TPC hit pairs
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
    {static const char cvs[]="Tag $Name:  $ $Id: StMcAnalysisMaker.h,v 1.12 2015/03/13 18:44:50 perev Exp $ built " __DATE__ " " __TIME__; return cvs;}	
    
    // the following is a ROOT macro  that is needed in all ROOT accessible code
    ClassDef(StMcAnalysisMaker,0)

};

inline void StMcAnalysisMaker::SetZones(Int_t columns, Int_t rows){ mPadColumns =columns; mPadRows = rows;}

#endif
