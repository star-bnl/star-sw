//
// $Id EfficiencyAnalysis.h$ 
// MCBS
//
// $Log: EfficiencyAnalysis.h,v $
// Revision 1.1  2002/11/27 00:08:55  calderon
// New version of evaluator using the minimctrees
//

#ifndef EfficiencyAnalysis_HH
#define EfficiencyAnalysis_HH
#include "TObject.h"
#include <string>
class TH2D;
class TH1D;
class TProfile;
class StMiniMcEvent;
class StMiniMcPair;

class EfficiencyAnalysis {
public:
    EfficiencyAnalysis();
    EfficiencyAnalysis(const EfficiencyAnalysis&);
    virtual ~EfficiencyAnalysis();
    void createHistograms();
    int fillHistograms(StMiniMcEvent*);
    int writeHistograms();
    int resetHistograms();
    bool acceptPair(StMiniMcPair*);
    bool acceptGeantId(int);
    string suffix() {return mSuffix;} //!

    void setFitPtsLimit(double);
    void setDcaLimit(double);
    void setGeantId(int);
    void setFileName(char*);
    void setSuffix(string); //!
private:
    StMiniMcEvent* minimcevent; //!
    
    unsigned int mNEvents; //!
    double mFitPtsLimit; //!
    double mDcaLimit; //!
    int   mGeantId;  //!
    char* mFileName; //! 
    string mSuffix;    //! to identify centrality bin
    TH2D* rawMcPiKP; //!
    TH2D* accMcPiKP; //!
    TH2D* matMcPiKP; //!
    TH2D* merMcPiKP; //!
    TH2D* matchedSpectrumRec; //!
    TH2D* electronBackground; //!
    TH2D* decayBackground; //!
    TH2D* splitSpectrum; //!
    TH2D* ghostSpectrum; //!
    TH2D* rawSpectrum; //!

    // these histograms are obtained from the previous 9
    TH2D* matchedPlusMergedMc; //!
    TH2D* rawSpectrumNoDecayBg; //!
    TH2D* rawSpectrumNoElectronBg; //!
    TH2D* rawSpectrumNoGhosts; //!

    TProfile* primaryTrackEffMult;    //!
    TH1D*     fitPointsUsed;          //!
    TH1D*     fitPointsPossible;      //!
    TH1D*     fitPointsUsedPossRatio; //!
    TH1D*     dcaGlobal;              //!

    ClassDef(EfficiencyAnalysis,1)
};
#endif
