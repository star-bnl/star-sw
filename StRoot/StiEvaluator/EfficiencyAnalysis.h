//
// $Id EfficiencyAnalysis.h$ 
// MCBS
//
// $Log: EfficiencyAnalysis.h,v $
// Revision 1.3  2003/09/02 17:59:47  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.2  2003/06/19 01:37:03  calderon
// Adding all the histograms from Zbigniew.
//
// Revision 1.1  2002/11/27 00:08:55  calderon
// New version of evaluator using the minimctrees
//

#ifndef EfficiencyAnalysis_HH
#define EfficiencyAnalysis_HH
#include "TObject.h"
#include <string>
using namespace std;
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
    bool acceptEtaCut(double);
    bool acceptPair(StMiniMcPair*);
    bool acceptGeantId(int);
    string suffix() {return mSuffix;}

    void setFitPtsLimit(double);
    void setDcaLimit(double);
    void setGeantId(int);
    void setEtaRange(double,double);
    void setPullType(bool);
    void setFileName(char*);
    void setSuffix(string);
private:
    StMiniMcEvent* minimcevent; //!
    
    unsigned int mNEvents; //!
    double mFitPtsLimit; //!
    double mDcaLimit;    //!
    int   mGeantId;      //!
    double mEtaMinimum;  //!
    double mEtaMaximum;  //!
    bool mPullType;      //!
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
    TH1D*     dcaPrimary;             //!    

    TProfile* primaryPtBiasVsP;       //!
    TProfile* primaryPxBiasVsPx;      //!
    TProfile* primaryPxBiasVsP;	      //!
    TProfile* primaryPyBiasVsP;	      //!
    TProfile* primaryPyBiasVsPy;      //!
    TProfile* primaryPzBiasVsP;	      //!
    TProfile* primaryPzBiasVsPz;      //!
    TProfile* primaryPBiasVsP;	      //!
    TProfile* primaryEtaBiasVsP;      //!
    TProfile* primaryEtaBiasVsEta;    //!
    TProfile* primaryPhiBiasVsP;      //!
    TProfile* primaryPhiBiasVsPhi;    //!

    TH2D* PtDistVsPt;                 //!
    TH2D* PxDistVsPx;		      //!
    TH2D* PxDistVsPt;		      //!
    TH2D* PyDistVsPt;		      //!
    TH2D* PyDistVsPy;		      //!
    TH2D* PzDistVsPt;		      //!
    TH2D* PzDistVsPz;                 //!
    TH2D* PDistVsPt;		      //!
    TH2D* PDistVsP;		      //!
    TH2D* EtaDistVsPt;		      //!
    TH2D* EtaDistVsEta;		      //!
    TH2D* PhiDistVsPt;		      //!
    TH2D* PhiDistVsPhi;               //!

    TH2D* PtDistByPtVsPt;             //!
    TH2D* PxDistByPxVsPx;	      //!
    TH2D* PxDistByPxVsPt;	      //!
    TH2D* PyDistByPyVsPy;	      //!
    TH2D* PyDistByPyVsPt;	      //!
    TH2D* PzDistByPzVsPz;	      //!
    TH2D* PzDistByPzVsPt;             //!
    TH2D* PDistByPVsP;		      //!
    TH2D* PDistByPVsPt;		      //!
    TH2D* EtaDistByEtaVsEta;	      //!
    TH2D* EtaDistByEtaVsPt;	      //!
    TH2D* PhiDistByPhiVsPhi;	      //!
    TH2D* PhiDistByPhiVsPt;           //!

    TH1D*     primaryPtDistribution;  //!
    TH1D*     primaryPxDistribution;  //!
    TH1D*     primaryPyDistribution;  //!
    TH1D*     primaryPzDistribution;  //!
    TH1D*     primaryPDistribution;   //!
    TH1D*     primaryEtaDistribution; //!
    TH1D*     primaryPhiDistribution; //!

    TH1D*    accMcPtDistribution;     //!
    TH1D*    accMcPxDistribution;     //!
    TH1D*    accMcPyDistribution;     //!
    TH1D*    accMcPzDistribution;     //!
    TH1D*    accMcPDistribution;      //!
    TH1D*    accMcEtaDistribution;    //!
    TH1D*    accMcPhiDistribution;    //!

    TH1D*    matchedMcPtDistribution; //!
    TH1D*    matchedMcPxDistribution; //!
    TH1D*    matchedMcPyDistribution; //!
    TH1D*    matchedMcPzDistribution; //!
    TH1D*    matchedMcPDistribution;  //!
    TH1D*    matchedMcEtaDistribution;//!
    TH1D*    matchedMcPhiDistribution;//!

    TH2D*     accMcHitsVsEtaDistribution;     //!
    TH2D*     matchedMcHitsVsEtaDistribution; //!

    TH1D*     rchi2Gl;                //!
    TH1D*     rchi2Pr;		      //!

    TH2D*     rchi2GlvsnPts;	      //!
    TH2D*     rchi2GlvsP;	      //!
    TH2D*     rchi2GlvsPt;	      //!
    TH2D*     rchi2GlvsEta;           //!

    TH2D*     rchi2PrvsnPts;          //!
    TH2D*     rchi2PrvsP;	      //!
    TH2D*     rchi2PrvsPt;	      //!
    TH2D*     rchi2PrvsEta;	      //!

    TH1D*     nPtsGl;		      //!
    TH1D*     nPtsPr;                 //!

    TH2D*     nFitPtsvsnPossiblePtsPr;//!

    TH2D*     nPtsGlvsPt;             //!
    TH2D*     nPtsPrvsPt;	      //!
    TH2D*     nPtsGlvsEta;	      //!
    TH2D*     nPtsPrvsEta;	      //!

    TH1D*     tanPr_pull;	      //!
    TH1D*     tanGl_pull;	      //!
    TH1D*     curvPr_pull;	      //!
    TH1D*     curvGl_pull;	      //!

    ClassDef(EfficiencyAnalysis,1)
};
#endif
