//
// $Id EfficiencyAnalysis.cxx $
//
// $Log: EfficiencyAnalysis.cxx,v $
// Revision 1.9  2004/05/03 23:41:08  perev
// Possible non init WarnOff
//
// Revision 1.8  2003/11/20 22:58:10  calderon
// Move inline functions to header file.
// Initialize minimcevent pointer to zero in constructor of StiEvaluator.  Otherwise
// we can't set the branch address properly and the code breaks.
//
// Revision 1.7  2003/09/02 17:59:47  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.6  2003/07/22 17:20:24  pruneau
// adding performance evaluation tools
//
// Revision 1.5  2003/07/09 01:08:07  calderon
// Changes to access StMiniMcEvent through methods, rather than data members
//
// Revision 1.4  2003/06/19 01:37:03  calderon
// Adding all the histograms from Zbigniew.
//
// Revision 1.3  2003/05/31 00:27:19  calderon
// Bug fix.  Was not using the values set in the macro for the fit points and
// dca cuts.
//
// Revision 1.2  2003/03/05 22:02:29  calderon
// protect against tracks which have zero possible points...
// caused a floating point exception
//
// Revision 1.1  2002/11/27 00:08:55  calderon
// New version of evaluator using the minimctrees
//
//
#include "EfficiencyAnalysis.h"

#include "Stiostream.h"

#include "StiEvalUtil.h"

#include "TH2.h"
#include "TProfile.h"
#include "TFile.h"

#include "StMiniMcEvent/StMiniMcEvent.h"
#include "StMiniMcEvent/StMiniMcPair.h"

ClassImp(EfficiencyAnalysis)
    
EfficiencyAnalysis::EfficiencyAnalysis() :
    // These values should be given in the macro, so set
    // the default values to something unusable, following the golden
    // rule: setting default values to something sensible is akin to sabotage.
    mNEvents(0),
    mFitPtsLimit(0),
    mDcaLimit(0),
    mGeantId(9999),
    mEtaMinimum(-10),
    mEtaMaximum(10),
    mPullType(0),
    mFileName(0),
    mSuffix(""),
    rawMcPiKP(0),               
    accMcPiKP(0),               
    matMcPiKP(0),               
    merMcPiKP(0),               
    matchedSpectrumRec(0),      
    electronBackground(0),      
    decayBackground(0),         
    splitSpectrum(0),           
    ghostSpectrum(0),           
    rawSpectrum(0),             
    matchedPlusMergedMc(0),     
    rawSpectrumNoDecayBg(0),    
    rawSpectrumNoElectronBg(0), 
    rawSpectrumNoGhosts(0),
    primaryTrackEffMult(0),
    fitPointsUsed(0),
    fitPointsPossible(0),
    fitPointsUsedPossRatio(0),
    dcaGlobal(0),
    dcaPrimary(0),
    primaryPtBiasVsP(0),
    primaryPxBiasVsPx(0),
    primaryPxBiasVsP(0),
    primaryPyBiasVsP(0),
    primaryPyBiasVsPy(0),
    primaryPzBiasVsP(0),
    primaryPzBiasVsPz(0),
    primaryPBiasVsP(0),
    primaryEtaBiasVsP(0),
    primaryEtaBiasVsEta(0),
    primaryPhiBiasVsP(0),
    primaryPhiBiasVsPhi(0),
    PtDistVsPt(0),
    PxDistVsPx(0),
    PxDistVsPt(0),
    PyDistVsPt(0),
    PyDistVsPy(0),
    PzDistVsPt(0),
    PzDistVsPz(0),
    PDistVsPt(0),
    PDistVsP(0),
    EtaDistVsPt(0),
    EtaDistVsEta(0),
    PhiDistVsPt(0),
    PhiDistVsPhi(0),

    PtDistByPtVsPt(0),
    PxDistByPxVsPx(0),
    PxDistByPxVsPt(0),
    PyDistByPyVsPy(0),
    PyDistByPyVsPt(0),
    PzDistByPzVsPz(0),
    PzDistByPzVsPt(0),
    PDistByPVsP(0),
    PDistByPVsPt(0),
    EtaDistByEtaVsEta(0),
    EtaDistByEtaVsPt(0),
    PhiDistByPhiVsPhi(0),
    PhiDistByPhiVsPt(0),

    primaryPtDistribution(0),
    primaryPxDistribution(0),
    primaryPyDistribution(0),
    primaryPzDistribution(0),
    primaryPDistribution(0),
    primaryEtaDistribution(0),
    primaryPhiDistribution(0),
    accMcPtDistribution(0),
    accMcPxDistribution(0),
    accMcPyDistribution(0),
    accMcPzDistribution(0),
    accMcPDistribution(0),
    accMcEtaDistribution(0),
    accMcPhiDistribution(0),
    matchedMcPtDistribution(0),
    matchedMcPxDistribution(0),
    matchedMcPyDistribution(0),
    matchedMcPzDistribution(0),
    matchedMcPDistribution(0),
    matchedMcEtaDistribution(0),
    matchedMcPhiDistribution(0),
    accMcHitsVsEtaDistribution(0),
    matchedMcHitsVsEtaDistribution(0),
    rchi2Gl(0),
    rchi2Pr(0),
    rchi2GlvsnPts(0),
    rchi2GlvsP(0),
    rchi2GlvsPt(0),
    rchi2GlvsEta(0),
    rchi2PrvsnPts(0),
    rchi2PrvsP(0),
    rchi2PrvsPt(0),
    rchi2PrvsEta(0),
    nPtsGl(0),
    nPtsPr(0),
    nFitPtsvsnPossiblePtsPr(0),
    nPtsGlvsPt(0),
    nPtsPrvsPt(0),
    nPtsGlvsEta(0),
    nPtsPrvsEta(0),
    tanPr_pull(0),
    tanGl_pull(0),
    curvPr_pull(0),
    curvGl_pull(0)
{
}
EfficiencyAnalysis::EfficiencyAnalysis(const EfficiencyAnalysis& e) :
    mNEvents(e.mNEvents),
    mFitPtsLimit(e.mFitPtsLimit),
    mDcaLimit(e.mDcaLimit),
    mGeantId(e.mGeantId),
    mEtaMinimum(e.mEtaMinimum),
    mEtaMaximum(e.mEtaMaximum),
    mPullType(e.mPullType),
    mFileName(e.mFileName),
    mSuffix(e.mSuffix),
    rawMcPiKP(0),               
    accMcPiKP(0),               
    matMcPiKP(0),               
    merMcPiKP(0),               
    matchedSpectrumRec(0),      
    electronBackground(0),      
    decayBackground(0),         
    splitSpectrum(0),           
    ghostSpectrum(0),           
    rawSpectrum(0),             
    matchedPlusMergedMc(0),     
    rawSpectrumNoDecayBg(0),    
    rawSpectrumNoElectronBg(0), 
    rawSpectrumNoGhosts(0),
    primaryTrackEffMult(0),
    fitPointsUsed(0),
    fitPointsPossible(0),
    fitPointsUsedPossRatio(0),
    dcaGlobal(0),
    dcaPrimary(0),
    primaryPtBiasVsP(0),
    primaryPxBiasVsPx(0),
    primaryPxBiasVsP(0),
    primaryPyBiasVsP(0),
    primaryPyBiasVsPy(0),
    primaryPzBiasVsP(0),
    primaryPzBiasVsPz(0),
    primaryPBiasVsP(0),
    primaryEtaBiasVsP(0),
    primaryEtaBiasVsEta(0),
    primaryPhiBiasVsP(0),
    primaryPhiBiasVsPhi(0),

    PtDistVsPt(0),
    PxDistVsPx(0),
    PxDistVsPt(0),
    PyDistVsPt(0),
    PyDistVsPy(0),
    PzDistVsPt(0),
    PzDistVsPz(0),
    PDistVsPt(0),
    PDistVsP(0),
    EtaDistVsPt(0),
    EtaDistVsEta(0),
    PhiDistVsPt(0),
    PhiDistVsPhi(0),

    PtDistByPtVsPt(0),
    PxDistByPxVsPx(0),
    PxDistByPxVsPt(0),
    PyDistByPyVsPy(0),
    PyDistByPyVsPt(0),
    PzDistByPzVsPz(0),
    PzDistByPzVsPt(0),
    PDistByPVsP(0),
    PDistByPVsPt(0),
    EtaDistByEtaVsEta(0),
    EtaDistByEtaVsPt(0),
    PhiDistByPhiVsPhi(0),
    PhiDistByPhiVsPt(0),

    primaryPtDistribution(0),
    primaryPxDistribution(0),
    primaryPyDistribution(0),
    primaryPzDistribution(0),
    primaryPDistribution(0),
    primaryEtaDistribution(0),
    primaryPhiDistribution(0),
    accMcPtDistribution(0),
    accMcPxDistribution(0),
    accMcPyDistribution(0),
    accMcPzDistribution(0),
    accMcPDistribution(0),
    accMcEtaDistribution(0),
    accMcPhiDistribution(0),
    matchedMcPtDistribution(0),
    matchedMcPxDistribution(0),
    matchedMcPyDistribution(0),
    matchedMcPzDistribution(0),
    matchedMcPDistribution(0),
    matchedMcEtaDistribution(0),
    matchedMcPhiDistribution(0),
    accMcHitsVsEtaDistribution(0),
    matchedMcHitsVsEtaDistribution(0),
    rchi2Gl(0),
    rchi2Pr(0),
    rchi2GlvsnPts(0),
    rchi2GlvsP(0),
    rchi2GlvsPt(0),
    rchi2GlvsEta(0),
    rchi2PrvsnPts(0),
    rchi2PrvsP(0),
    rchi2PrvsPt(0),
    rchi2PrvsEta(0),
    nPtsGl(0),
    nPtsPr(0),
    nFitPtsvsnPossiblePtsPr(0),
    nPtsGlvsPt(0),
    nPtsPrvsPt(0),
    nPtsGlvsEta(0),
    nPtsPrvsEta(0),
    tanPr_pull(0),
    tanGl_pull(0),
    curvPr_pull(0),
    curvGl_pull(0)
{
}

EfficiencyAnalysis::~EfficiencyAnalysis() {

    if (rawMcPiKP)               delete rawMcPiKP;
    if (accMcPiKP)               delete accMcPiKP;
    if (matMcPiKP)               delete matMcPiKP;
    if (merMcPiKP)               delete merMcPiKP;
    if (matchedSpectrumRec)      delete matchedSpectrumRec;
    if (electronBackground)      delete electronBackground;
    if (decayBackground)         delete decayBackground;
    if (splitSpectrum)           delete splitSpectrum;
    if (ghostSpectrum)           delete ghostSpectrum;
    if (matchedPlusMergedMc)     delete matchedPlusMergedMc;
    if (rawSpectrumNoDecayBg)    delete rawSpectrumNoDecayBg;
    if (rawSpectrumNoElectronBg) delete rawSpectrumNoElectronBg;
    if (rawSpectrumNoGhosts)     delete rawSpectrumNoGhosts;
    if (rawSpectrum)             delete rawSpectrum;             
    if (primaryTrackEffMult)     delete primaryTrackEffMult;             
    if (fitPointsUsed)           delete fitPointsUsed;             
    if (fitPointsPossible)       delete fitPointsPossible;             
    if (fitPointsUsedPossRatio)  delete fitPointsUsedPossRatio;             
    if (dcaGlobal)               delete dcaGlobal;             
    if (dcaPrimary)              delete dcaPrimary;             
    if (primaryPtBiasVsP)        delete primaryPtBiasVsP;             
    if (primaryPxBiasVsP)        delete primaryPxBiasVsP;             
    if (primaryPxBiasVsPx)        delete primaryPxBiasVsPx;             
    if (primaryPyBiasVsP)        delete primaryPyBiasVsP;             
    if (primaryPyBiasVsPy)        delete primaryPyBiasVsPy;             
    if (primaryPzBiasVsP)        delete primaryPzBiasVsP;             
    if (primaryPzBiasVsPz)        delete primaryPzBiasVsPz;             
    if (primaryPBiasVsP)          delete primaryPBiasVsP;
    if (primaryEtaBiasVsP)       delete primaryEtaBiasVsP;             
    if (primaryEtaBiasVsEta)      delete primaryEtaBiasVsEta;             
    if (primaryPhiBiasVsP)       delete primaryPhiBiasVsP;             
    if (primaryPhiBiasVsPhi)      delete primaryPhiBiasVsPhi;             

    if (PtDistVsPt)        delete PtDistVsPt;             
    if (PxDistVsPx)        delete PxDistVsPx;             
    if (PxDistVsPt)        delete PxDistVsPt;             
    if (PyDistVsPt)        delete PyDistVsPt;             
    if (PyDistVsPy)        delete PyDistVsPy;             
    if (PzDistVsPt)        delete PzDistVsPt;             
    if (PzDistVsPz)        delete PzDistVsPz;             
    if (PDistVsPt)         delete PDistVsPt;             
    if (PDistVsP)          delete PDistVsP;
    if (EtaDistVsPt)       delete EtaDistVsPt;             
    if (EtaDistVsEta)      delete EtaDistVsEta;             
    if (PhiDistVsPt)       delete PhiDistVsPt;             
    if (PhiDistVsPhi)      delete PhiDistVsPhi;             

    if (PtDistByPtVsPt)    delete PtDistByPtVsPt;
    if (PxDistByPxVsPx)    delete PxDistByPxVsPx;
    if (PxDistByPxVsPt)    delete PxDistByPxVsPt;
    if (PyDistByPyVsPy)    delete PyDistByPyVsPy;
    if (PyDistByPyVsPt)    delete PyDistByPyVsPt;
    if (PzDistByPzVsPz)    delete PzDistByPzVsPz;
    if (PzDistByPzVsPt)    delete PzDistByPzVsPt;
    if (PDistByPVsP)       delete PDistByPVsP;
    if (PDistByPVsPt)      delete PDistByPVsPt;
    if (EtaDistByEtaVsEta) delete EtaDistByEtaVsEta;
    if (EtaDistByEtaVsPt)  delete EtaDistByEtaVsPt;
    if (PhiDistByPhiVsPhi) delete PhiDistByPhiVsPhi;
    if (PhiDistByPhiVsPt)  delete PhiDistByPhiVsPt;

    
    if (primaryPtDistribution)   delete primaryPtDistribution;
    if (primaryPxDistribution)   delete primaryPxDistribution;
    if (primaryPyDistribution)   delete primaryPyDistribution;
    if (primaryPzDistribution)   delete primaryPzDistribution;
    if (primaryPDistribution)    delete primaryPDistribution;
    if (primaryEtaDistribution)  delete primaryEtaDistribution;
    if (primaryPhiDistribution)  delete primaryPhiDistribution;

    if (accMcPtDistribution)   delete accMcPtDistribution;
    if (accMcPxDistribution)   delete accMcPxDistribution;
    if (accMcPyDistribution)   delete accMcPyDistribution;
    if (accMcPzDistribution)   delete accMcPzDistribution;
    if (accMcPDistribution)    delete accMcPDistribution;
    if (accMcEtaDistribution)  delete accMcEtaDistribution;
    if (accMcPhiDistribution)  delete accMcPhiDistribution;

    if (matchedMcPtDistribution)   delete matchedMcPtDistribution;
    if (matchedMcPxDistribution)   delete matchedMcPxDistribution;
    if (matchedMcPyDistribution)   delete matchedMcPyDistribution;
    if (matchedMcPzDistribution)   delete matchedMcPzDistribution;
    if (matchedMcPDistribution)    delete matchedMcPDistribution;
    if (matchedMcEtaDistribution)  delete matchedMcEtaDistribution;
    if (matchedMcPhiDistribution)  delete matchedMcPhiDistribution;

    if (accMcHitsVsEtaDistribution)          delete     accMcHitsVsEtaDistribution;
    if (matchedMcHitsVsEtaDistribution)      delete     matchedMcHitsVsEtaDistribution;

    if (rchi2Gl)    delete rchi2Gl;
    if (rchi2Pr)    delete rchi2Pr;

    if (rchi2GlvsnPts) delete rchi2GlvsnPts;
    if (rchi2GlvsP)    delete rchi2GlvsP;
    if (rchi2GlvsPt)   delete rchi2GlvsPt;
    if (rchi2GlvsEta)  delete rchi2GlvsEta;
    if (rchi2PrvsnPts) delete rchi2PrvsnPts;
    if (rchi2PrvsP)    delete rchi2PrvsP;
    if (rchi2PrvsPt)   delete rchi2PrvsPt;
    if (rchi2PrvsEta)  delete rchi2PrvsEta;

    if (nPtsGl)        delete nPtsGl;
    if (nPtsPr)        delete nPtsPr;

    if (nFitPtsvsnPossiblePtsPr) delete nFitPtsvsnPossiblePtsPr;

    if (nPtsGlvsPt)    delete nPtsGlvsPt;
    if (nPtsPrvsPt)    delete nPtsPrvsPt;
    if (nPtsGlvsEta)   delete nPtsGlvsEta;
    if (nPtsPrvsEta)   delete nPtsPrvsEta;
    
    if (tanPr_pull)       delete tanPr_pull;
    if (tanGl_pull)       delete tanGl_pull;
    if (curvPr_pull)       delete curvPr_pull;
    if (curvGl_pull)       delete curvGl_pull;

    rawMcPiKP = 0;               
    accMcPiKP = 0;               
    matMcPiKP = 0;               
    merMcPiKP = 0;               
    matchedSpectrumRec = 0;      
    electronBackground = 0;      
    decayBackground = 0;         
    splitSpectrum = 0;           
    ghostSpectrum = 0;           
    matchedPlusMergedMc = 0;     
    rawSpectrumNoDecayBg = 0;    
    rawSpectrumNoElectronBg = 0; 
    rawSpectrumNoGhosts = 0;     
    rawSpectrum = 0;             
    primaryTrackEffMult = 0;             
    fitPointsUsed = 0;             
    fitPointsPossible = 0;             
    fitPointsUsedPossRatio = 0;
    dcaGlobal = 0;
    dcaPrimary = 0;
    primaryPtBiasVsP = 0;
    primaryPxBiasVsP = 0;
    primaryPxBiasVsPx = 0;
    primaryPyBiasVsP = 0;
    primaryPyBiasVsPy = 0;
    primaryPzBiasVsP = 0;
    primaryPzBiasVsPz = 0;
    primaryPBiasVsP = 0;
    primaryEtaBiasVsP = 0;
    primaryEtaBiasVsEta = 0;
    primaryPhiBiasVsP = 0;
    primaryPhiBiasVsPhi = 0;

    PtDistVsPt = 0;
    PxDistVsPt = 0;
    PyDistVsPt = 0;
    PxDistVsPx = 0;
    PyDistVsPy = 0;
    PzDistVsPt = 0;
    PzDistVsPz = 0;
    PDistVsPt = 0;
    PDistVsP = 0;
    EtaDistVsPt = 0;
    EtaDistVsEta = 0;
    PhiDistVsPt = 0;
    PhiDistVsPhi = 0;

    PtDistByPtVsPt = 0;
    PxDistByPxVsPx = 0;
    PxDistByPxVsPt = 0;
    PyDistByPyVsPy = 0;
    PyDistByPyVsPt = 0;
    PzDistByPzVsPz = 0;
    PzDistByPzVsPt = 0;
    PDistByPVsP = 0;
    PDistByPVsPt = 0;
    EtaDistByEtaVsEta = 0;
    EtaDistByEtaVsPt = 0;
    PhiDistByPhiVsPhi = 0;
    PhiDistByPhiVsPt = 0;

    primaryPtDistribution = 0;
    primaryPxDistribution = 0;
    primaryPyDistribution = 0;
    primaryPzDistribution = 0;
    primaryPDistribution = 0;
    primaryEtaDistribution = 0;
    primaryPhiDistribution = 0;

    accMcPtDistribution = 0;
    accMcPxDistribution = 0;
    accMcPyDistribution = 0;
    accMcPzDistribution = 0;
    accMcPDistribution = 0;
    accMcEtaDistribution = 0;
    accMcPhiDistribution = 0;

    matchedMcPtDistribution = 0;
    matchedMcPxDistribution = 0;
    matchedMcPyDistribution = 0;
    matchedMcPzDistribution = 0;
    matchedMcPDistribution = 0;
    matchedMcEtaDistribution = 0;
    matchedMcPhiDistribution = 0;

    accMcHitsVsEtaDistribution = 0;
    matchedMcHitsVsEtaDistribution = 0;

    rchi2Gl = 0;
    rchi2Pr = 0;
    
    rchi2GlvsnPts = 0;
    rchi2GlvsP    = 0;
    rchi2GlvsPt   = 0;
    rchi2GlvsEta  = 0;
    rchi2PrvsnPts = 0;
    rchi2PrvsP    = 0;
    rchi2PrvsPt   = 0;
    rchi2PrvsEta  = 0;
    
    nPtsGl        = 0;
    nPtsPr        = 0;
    nFitPtsvsnPossiblePtsPr = 0;
   
    nPtsGlvsPt    = 0;
    nPtsPrvsPt    = 0;
    nPtsGlvsEta   = 0;
    nPtsPrvsEta   = 0;
    tanPr_pull    = 0;
    tanGl_pull    = 0;
    curvPr_pull   = 0;
    curvGl_pull   = 0;
}

bool EfficiencyAnalysis::acceptEtaCut(double eta)
{
    if ( (eta >= mEtaMinimum) && (eta <= mEtaMaximum) ) return true;
    return false;
}

bool EfficiencyAnalysis::acceptPair(StMiniMcPair* pair) {

    if (!(pair->nHitMc()>=10)) return false;
    if (!acceptEtaCut(pair->etaMc())) return false;
    if (!(pair->fitPts()>=mFitPtsLimit)) return false; // thesis >= 24, BigBad >=25
    if (!(pair->dcaGl()<mDcaLimit)) return false; // thesis primary dca (only for P00hm), Big Bad global dca
    // Now check that the charge matches the specified ID
    if (mGeantId==9999) return true; // default value, accept all tracks regardless of the charge match.
    // pi-, k-, pbar = 9, 12, 15
    // pi+, k+, prot = 8, 11, 14
    if ((mGeantId==8 || mGeantId==11 || mGeantId==14) && pair->charge()<0) return false;
    if ((mGeantId==9 || mGeantId==12 || mGeantId==15) && pair->charge()>0) return false;

    return true;
}

bool EfficiencyAnalysis::acceptGeantId(int geantId) {
    if (mGeantId == 9999) // mGeantId is not set, use background for all Nch (pi, k, p)
	return	(geantId == 8  || geantId ==  9 ||
		 geantId == 11 || geantId == 12 ||
		 geantId == 14 || geantId == 15); 
    else return (mGeantId==geantId); 

}
void EfficiencyAnalysis::createHistograms() {
    cout << "EfficiencyAnalysis::createHistograms " << mSuffix << " " <<  mGeantId << endl;

    string namerawMcPiKP          = "rawMcPiKP";
    string nameaccMcPiKP          = "accMcPiKP";
    string namematMcPiKP          = "matMcPiKP";
    string namemerMcPiKP          = "merMcPiKP";
    string namematchedSpectrumRec = "matchedSpectrumRec";
    string nameelectronBackground = "electronBackground";
    string namedecayBackground    = "decayBackground";
    string namesplitSpectrum      = "splitSpectrum";
    string nameghostSpectrum      = "ghostSpectrum";
    string namematchedPlusMergedMc     = "matchedPlusMergedMc";
    string namerawSpectrumNoDecayBg    = "rawSpectrumNoDecayBg";
    string namerawSpectrumNoElectronBg = "rawSpectrumNoElectronBg";
    string namerawSpectrumNoGhosts     = "rawSpectrumNoGhosts";
    string namerawSpectrum             = "rawSpectrum";
    string nameprimaryTrackEffMult     = "primaryTrackEffMult";
    string namefitPointsUsed           = "fitPointsUsed";
    string namefitPointsPossible       = "fitPointsPossible";
    string namefitPointsUsedPossRatio  = "fitPointsUsedPossRatio";
    string namedcaGlobal               = "dcaGlobal";
    string namedcaPrimary               = "dcaPrimary";

    string nameprimaryPtBiasVsP        = "primaryPtBiasVsP";
    string nameprimaryPxBiasVsP        = "primaryPxBiasVsP";
    string nameprimaryPxBiasVsPx        = "primaryPxBiasVsPx";
    string nameprimaryPyBiasVsP        = "primaryPyBiasVsP";
    string nameprimaryPyBiasVsPy        = "primaryPyBiasVsPy";
    string nameprimaryPzBiasVsP        = "primaryPzBiasVsP";
    string nameprimaryPzBiasVsPz        = "primaryPzBiasVsPz";
    string nameprimaryPBiasVsP          = "primaryPBiasVsP";
    string nameprimaryEtaBiasVsP       = "primaryEtaBiasVsP";
    string nameprimaryEtaBiasVsEta      = "primaryEtaBiasVsEta";
    string nameprimaryPhiBiasVsP       = "primaryPhiBiasVsP";
    string nameprimaryPhiBiasVsPhi      = "primaryPhiBiasVsPhi";

    string namePtDistVsPt        = "PtDistVsPt";
    string namePxDistVsPx        = "PxDistVsPx";
    string namePxDistVsPt        = "PxDistVsPt";
    string namePyDistVsPt        = "PyDistVsPt";
    string namePyDistVsPy        = "PyDistVsPy";
    string namePzDistVsPt        = "PzDistVsPt";
    string namePzDistVsPz        = "PzDistVsPz";
    string namePDistVsPt         = "PDistVsPt";
    string namePDistVsP          = "PDistVsP";
    string nameEtaDistVsPt       = "EtaDistVsPt";
    string nameEtaDistVsEta      = "EtaDistVsEta";
    string namePhiDistVsPt       = "PhiDistVsPt";
    string namePhiDistVsPhi      = "PhiDistVsPhi";

    string namePtDistByPtVsPt    = "PtDistByPtVsPt";
    string namePxDistByPxVsPx    = "PxDistByPxVsPx";
    string namePxDistByPxVsPt    = "PxDistByPxVsPt";
    string namePyDistByPyVsPy    = "PyDistByPyVsPy";
    string namePyDistByPyVsPt    = "PyDistByPyVsPt";
    string namePzDistByPzVsPz    = "PzDistByPzVsPz";
    string namePzDistByPzVsPt    = "PzDistByPzVsPt";
    string namePDistByPVsP       = "PDistByPVsP";
    string namePDistByPVsPt      = "PDistByPVsPt";
    string nameEtaDistByEtaVsEta = "EtaDistByEtaVsEta";
    string nameEtaDistByEtaVsPt  = "EtaDistByEtaVsPt";
    string namePhiDistByPhiVsPhi = "PhiDistByPhiVsPhi" ;
    string namePhiDistByPhiVsPt  = "PhiDistByPhiVsPt";

    
    string nameprimaryPtDistribution   = "primaryPtDistribution";
    string nameprimaryPxDistribution   = "primaryPxDistribution";
    string nameprimaryPyDistribution   = "primaryPyDistribution";
    string nameprimaryPzDistribution   = "primaryPzDistribution";
    string nameprimaryPDistribution    = "primaryPDistribution";

    string nameprimaryEtaDistribution  = "primaryEtaDistribution";
    string nameprimaryPhiDistribution  = "primaryPhiDistribution";

    string nameaccMcPtDistribution   = "accMcPtDistribution";
    string nameaccMcPxDistribution   = "accMcPxDistribution";
    string nameaccMcPyDistribution   = "accMcPyDistribution";
    string nameaccMcPzDistribution   = "accMcPzDistribution";
    string nameaccMcPDistribution    = "accMcPDistribution";

    string nameaccMcEtaDistribution  = "accMcEtaDistribution";
    string nameaccMcPhiDistribution  = "accMcPhiDistribution";

    string namematchedMcPtDistribution   = "matchedMcPtDistribution";
    string namematchedMcPxDistribution   = "matchedMcPxDistribution";
    string namematchedMcPyDistribution   = "matchedMcPyDistribution";
    string namematchedMcPzDistribution   = "matchedMcPzDistribution";
    string namematchedMcPDistribution    = "matchedMcPDistribution";

    string namematchedMcEtaDistribution  = "matchedMcEtaDistribution";
    string namematchedMcPhiDistribution  = "matchedMcPhiDistribution";

    string nameaccMcHitsVsEtaDistribution = "accMcHitsVsEtaDistribution";
    string namematchedMcHitsVsEtaDistribution = "matchedMcHitsVsEtaDistribution";

    string namebiasP     = "biasP";
    string namebiasPx    = "biasPx";
    string namebiasPy    = "biasPy";
    string namebiasPz    = "biasPz";
    string namebiasPtPr    = "biasPtPr";
    string namebiasPtGl    = "biasPtGl";
    string namebiasCurvPr  = "biasCurvPr";
    string namebiasCurvGl  = "biasCurvGl";
    string namebiasTanLPr  = "biasTanLPr";
    string namebiasTanLGl  = "biasTanLGl";
    string namebiasEta   = "biasEta";
    string namebiasPhi   = "biasPhi";

    string namerchi2Gl   = "rchi2Gl";
    string namerchi2Pr   = "rchi2Pr";
    
    string namerchi2GlvsnPts = "rchi2GlvsnPts";
    string namerchi2GlvsP    = "rchi2GlvsP";
    string namerchi2GlvsPt   = "rchi2GlvsPt";
    string namerchi2GlvsEta  = "rchi2GlvsEta";
    string namerchi2PrvsnPts = "rchi2PrvsnPts";
    string namerchi2PrvsP    = "rchi2PrvsP";
    string namerchi2PrvsPt   = "rchi2PrvsPt";
    string namerchi2PrvsEta  = "rchi2PrvsEta";

    string namenPtsGl        = "nPtsGl";
    string namenPtsPr        = "nPtsPr";
    string namenFitPtsvsnPossiblePtsPr = "nFitPtsvsnPossiblePtsPr";
    
    string namenPtsGlvsPt    = "nPtsGlvsPt";
    string namenPtsPrvsPt    = "nPtsPrvsPt";
    string namenPtsGlvsEta   = "nPtsGlvsEta";
    string namenPtsPrvsEta   = "nPtsPrvsEta";

    string nametanPr_pull       = "tanPr_pull";
    string nametanGl_pull       = "tanGl_pull";
    string namecurvPr_pull       = "curvPr_pull";
    string namecurvGl_pull       = "curvGl_pull";

    namerawMcPiKP          += mSuffix;
    nameaccMcPiKP          += mSuffix;
    namematMcPiKP          += mSuffix;
    namemerMcPiKP          += mSuffix;
    namematchedSpectrumRec += mSuffix;
    nameelectronBackground += mSuffix;
    namedecayBackground    += mSuffix;
    namesplitSpectrum      += mSuffix;
    nameghostSpectrum      += mSuffix;
    namematchedPlusMergedMc     += mSuffix;
    namerawSpectrumNoDecayBg    += mSuffix;
    namerawSpectrumNoElectronBg += mSuffix;
    namerawSpectrumNoGhosts     += mSuffix;
    namerawSpectrum             += mSuffix;
    nameprimaryTrackEffMult     += mSuffix;
    namefitPointsUsed           += mSuffix;
    namefitPointsPossible       += mSuffix;
    namefitPointsUsedPossRatio  += mSuffix;
    namedcaGlobal               += mSuffix;
    namedcaPrimary              += mSuffix;
    nameprimaryPtBiasVsP        += mSuffix;    
    nameprimaryPxBiasVsP        += mSuffix;    
    nameprimaryPxBiasVsPx        += mSuffix;    
    nameprimaryPyBiasVsP        += mSuffix;    
    nameprimaryPyBiasVsPy        += mSuffix;    
    nameprimaryPzBiasVsP        += mSuffix;    
    nameprimaryPzBiasVsPz        += mSuffix;    
    nameprimaryPBiasVsP          += mSuffix;    
    nameprimaryEtaBiasVsP       += mSuffix;    
    nameprimaryEtaBiasVsEta      += mSuffix;    
    nameprimaryPhiBiasVsP       += mSuffix;    
    nameprimaryPhiBiasVsPhi      += mSuffix;    

    namePtDistVsPt        += mSuffix;    
    namePxDistVsPx        += mSuffix;    
    namePxDistVsPt        += mSuffix;    
    namePyDistVsPt        += mSuffix;    
    namePyDistVsPy        += mSuffix;    
    namePzDistVsPt        += mSuffix;    
    namePzDistVsPz        += mSuffix;    
    namePDistVsPt         += mSuffix;    
    namePDistVsP          += mSuffix;    
    nameEtaDistVsPt       += mSuffix;    
    nameEtaDistVsEta      += mSuffix;    
    namePhiDistVsPt       += mSuffix;    
    namePhiDistVsPhi      += mSuffix;    

    namePtDistByPtVsPt    += mSuffix ;
    namePxDistByPxVsPx    += mSuffix ;
    namePxDistByPxVsPt    += mSuffix ;
    namePyDistByPyVsPy    += mSuffix ;
    namePyDistByPyVsPt    += mSuffix ;
    namePzDistByPzVsPz    += mSuffix ;
    namePzDistByPzVsPt    += mSuffix ;
    namePDistByPVsP       += mSuffix ;
    namePDistByPVsPt      += mSuffix ;
    nameEtaDistByEtaVsEta += mSuffix ;
    nameEtaDistByEtaVsPt  += mSuffix ;
    namePhiDistByPhiVsPhi += mSuffix ;
    namePhiDistByPhiVsPt  += mSuffix ;


    nameprimaryPtDistribution   += mSuffix;
    nameprimaryPxDistribution   += mSuffix;
    nameprimaryPyDistribution   += mSuffix;
    nameprimaryPzDistribution   += mSuffix;
    nameprimaryPDistribution    += mSuffix;
    nameprimaryEtaDistribution  += mSuffix;
    nameprimaryPhiDistribution  += mSuffix;

    nameaccMcPtDistribution   += mSuffix;
    nameaccMcPxDistribution   += mSuffix;
    nameaccMcPyDistribution   += mSuffix;
    nameaccMcPzDistribution   += mSuffix;
    nameaccMcPDistribution    += mSuffix;
    nameaccMcEtaDistribution  += mSuffix;
    nameaccMcPhiDistribution  += mSuffix;

    namematchedMcPtDistribution   += mSuffix;
    namematchedMcPxDistribution   += mSuffix;
    namematchedMcPyDistribution   += mSuffix;
    namematchedMcPzDistribution   += mSuffix;
    namematchedMcPDistribution    += mSuffix;
    namematchedMcEtaDistribution  += mSuffix;
    namematchedMcPhiDistribution  += mSuffix;

    nameaccMcHitsVsEtaDistribution += mSuffix;
    namematchedMcHitsVsEtaDistribution += mSuffix;

    namebiasP   += mSuffix;
    namebiasPx  += mSuffix;
    namebiasPy  += mSuffix;
    namebiasPz  += mSuffix;
    namebiasPtPr   += mSuffix;
    namebiasPtGl   += mSuffix;
    namebiasCurvPr += mSuffix;
    namebiasCurvGl += mSuffix;
    namebiasTanLPr += mSuffix;
    namebiasTanLGl += mSuffix;
    namebiasEta += mSuffix;
    namebiasPhi += mSuffix;

    namerchi2Gl += mSuffix;
    namerchi2Pr += mSuffix;

    namerchi2GlvsnPts += mSuffix;
    namerchi2GlvsP    += mSuffix;
    namerchi2GlvsPt   += mSuffix;
    namerchi2GlvsEta  += mSuffix;
    namerchi2PrvsnPts += mSuffix;
    namerchi2PrvsP    += mSuffix;
    namerchi2PrvsPt   += mSuffix;
    namerchi2PrvsEta  += mSuffix;

    namenPtsGl      += mSuffix;
    namenPtsPr      += mSuffix;
    namenFitPtsvsnPossiblePtsPr += mSuffix;
    namenPtsGlvsPt  += mSuffix;
    namenPtsPrvsPt  += mSuffix;
    namenPtsGlvsEta += mSuffix;
    namenPtsPrvsEta += mSuffix;
    nametanPr_pull    += mSuffix;
    nametanGl_pull    += mSuffix;
    namecurvPr_pull   += mSuffix;
    namecurvGl_pull   += mSuffix;
    
    int nbinsx  = 20;
    double lowx = -1;
    double hix  = 1;
    int nbinsy  = 40;
    double lowy = 0;
    double hiy  = 2;
    
    rawMcPiKP          = new TH2D(namerawMcPiKP.c_str(),"raw      Mc Pi,K,P",nbinsx,lowx,hix,nbinsy,lowy,hiy);
    accMcPiKP          = new TH2D(nameaccMcPiKP.c_str(),"accepted Mc Pi,K,P",nbinsx,lowx,hix,nbinsy,lowy,hiy);
    matMcPiKP          = new TH2D(namematMcPiKP.c_str(),"matched  Mc Pi,K,P",nbinsx,lowx,hix,nbinsy,lowy,hiy);
    merMcPiKP          = new TH2D(namemerMcPiKP.c_str(),"merged  Mc Pi,K,P",nbinsx,lowx,hix,nbinsy,lowy,hiy);
    matchedSpectrumRec = new TH2D(namematchedSpectrumRec.c_str(),"matched  Rec Pi,K,P",nbinsx,lowx,hix,nbinsy,lowy,hiy);
    electronBackground = new TH2D(nameelectronBackground.c_str(),"electron Background",nbinsx,lowx,hix,nbinsy,lowy,hiy);
    decayBackground    = new TH2D(namedecayBackground.c_str(),"decay Background",nbinsx,lowx,hix,nbinsy,lowy,hiy);
    splitSpectrum      = new TH2D(namesplitSpectrum.c_str(),"split Spectrum",nbinsx,lowx,hix,nbinsy,lowy,hiy);
    ghostSpectrum      = new TH2D(nameghostSpectrum.c_str(),"ghost Spectrum",nbinsx,lowx,hix,nbinsy,lowy,hiy);

    // these histograms are obtained from the previous 9
    matchedPlusMergedMc     = new TH2D(namematchedPlusMergedMc.c_str(),"matched +merged  Mc Pi,K,P",nbinsx,lowx,hix,nbinsy,lowy,hiy);
    rawSpectrumNoDecayBg    = new TH2D(namerawSpectrumNoDecayBg.c_str(),"Raw Spectrum W/O Ghosts, Decay BG",nbinsx,lowx,hix,nbinsy,lowy,hiy);
    rawSpectrumNoElectronBg = new TH2D(namerawSpectrumNoElectronBg.c_str(),"Raw Spectrum W/O Electrons, Ghosts, Decay BG", nbinsx,lowx,hix,nbinsy,lowy,hiy);
    rawSpectrumNoGhosts     = new TH2D(namerawSpectrumNoGhosts.c_str(),"Raw Spectrum W/O Ghosts", nbinsx,lowx,hix,nbinsy,lowy,hiy);
    rawSpectrum             = new TH2D(namerawSpectrum.c_str(),"Raw Spectrum", nbinsx,lowx,hix,nbinsy,lowy,hiy);

    primaryTrackEffMult     = new TProfile(nameprimaryTrackEffMult.c_str(),"Efficiency vs Mult",100,0,7000);
    fitPointsUsed           = new TH1D(namefitPointsUsed.c_str(),"fit Points",50, 0, 50);
    fitPointsPossible       = new TH1D(namefitPointsPossible.c_str(),"Possible Points",50, 0, 50);
    fitPointsUsedPossRatio  = new TH1D(namefitPointsUsedPossRatio.c_str(),"Fit Points/Possible Points", 75,0,1.5);
    dcaGlobal               = new TH1D(namedcaGlobal.c_str(),"global dca", 100, 0, 3.1);
    dcaPrimary               = new TH1D(namedcaPrimary.c_str(),"global dca", 100, 0, 3.1);

    primaryPtDistribution = new TH1D(nameprimaryPtDistribution.c_str(),"primaryPtDistribution",100,0,3.1);
    primaryPxDistribution = new TH1D(nameprimaryPxDistribution.c_str(),"primaryPxDistribution",40,-3.1,3.1);
    primaryPyDistribution = new TH1D(nameprimaryPyDistribution.c_str(),"primaryPyDistribution",40,-3.1,3.1);
    primaryPzDistribution = new TH1D(nameprimaryPzDistribution.c_str(),"primaryPzDistribution",40,-3.1,3.1);
    primaryPDistribution  = new TH1D(nameprimaryPDistribution.c_str(), "primaryPDistribution", 40,-3.1,3.1);
    primaryEtaDistribution= new TH1D(nameprimaryEtaDistribution.c_str(),"primaryEtaDistribution",80,-2.0,2.0);
    primaryPhiDistribution= new TH1D(nameprimaryPhiDistribution.c_str(),"primaryPhiDistribution",80,-3.2,3.2);

    accMcPtDistribution = new TH1D(nameaccMcPtDistribution.c_str(),"accMcPtDistribution",100,0,5.0);
    accMcPxDistribution = new TH1D(nameaccMcPxDistribution.c_str(),"accMcPxDistribution",40,-3.1,3.1);
    accMcPyDistribution = new TH1D(nameaccMcPyDistribution.c_str(),"accMcPyDistribution",40,-3.1,3.1);
    accMcPzDistribution = new TH1D(nameaccMcPzDistribution.c_str(),"accMcPzDistribution",40,-3.1,3.1);
    accMcPDistribution  = new TH1D(nameaccMcPDistribution.c_str(), "accMcPDistribution", 40,-3.1,3.1);
    accMcEtaDistribution= new TH1D(nameaccMcEtaDistribution.c_str(),"accMcEtaDistribution",80,-2.0,2.0);
    accMcPhiDistribution= new TH1D(nameaccMcPhiDistribution.c_str(),"accMcPhiDistribution",80,-3.2,3.2);

    matchedMcPtDistribution = new TH1D(namematchedMcPtDistribution.c_str(),"matchedMcPtDistribution",100,0,5.0);
    matchedMcPxDistribution = new TH1D(namematchedMcPxDistribution.c_str(),"matchedMcPxDistribution",40,-3.1,3.1);
    matchedMcPyDistribution = new TH1D(namematchedMcPyDistribution.c_str(),"matchedMcPyDistribution",40,-3.1,3.1);
    matchedMcPzDistribution = new TH1D(namematchedMcPzDistribution.c_str(),"matchedMcPzDistribution",40,-3.1,3.1);
    matchedMcPDistribution  = new TH1D(namematchedMcPDistribution.c_str(), "matchedMcPDistribution", 40,-3.1,3.1);
    matchedMcEtaDistribution= new TH1D(namematchedMcEtaDistribution.c_str(),"matchedMcEtaDistribution",80,-2.0,2.0);
    matchedMcPhiDistribution= new TH1D(namematchedMcPhiDistribution.c_str(),"matchedMcPhiDistribution",80,-3.2,3.2);

    primaryPtBiasVsP = new TProfile(nameprimaryPtBiasVsP.c_str(),"primaryPtBiasVsP",40,0,3.1);
    primaryPxBiasVsP = new TProfile(nameprimaryPxBiasVsP.c_str(),"primaryPxBiasVsP",40,0,3.1);
    primaryPxBiasVsPx = new TProfile(nameprimaryPxBiasVsPx.c_str(),"primaryPxBiasVsPx",40,0,3.1);
    primaryPyBiasVsP = new TProfile(nameprimaryPyBiasVsP.c_str(),"primaryPyBiasVsP",40,0,3.1);
    primaryPyBiasVsPy = new TProfile(nameprimaryPyBiasVsPy.c_str(),"primaryPyBiasVsPy",40,0,3.1);
    primaryPzBiasVsP = new TProfile(nameprimaryPzBiasVsP.c_str(),"primaryPzBiasVsP",40,0,3.1);
    primaryPzBiasVsPz = new TProfile(nameprimaryPzBiasVsPz.c_str(),"primaryPzBiasVsPz",40,0,3.1);
    primaryPBiasVsP  = new TProfile(nameprimaryPBiasVsP.c_str() ,"primaryPBiasVsP" ,40,0,3.1);
    primaryEtaBiasVsP = new TProfile(nameprimaryEtaBiasVsP.c_str(),"primaryEtaBiasVsP",40,0,3.1);
    primaryEtaBiasVsEta = new TProfile(nameprimaryEtaBiasVsEta.c_str(),"primaryEtaBiasVsEta",40,0,3.1);
    primaryPhiBiasVsP = new TProfile(nameprimaryPhiBiasVsP.c_str(),"primaryPhiBiasVsP",40,0,3.1);
    primaryPhiBiasVsPhi = new TProfile(nameprimaryPhiBiasVsPhi.c_str(),"primaryPhiBiasVsPhi",40,0,3.1);

    PtDistVsPt = new TH2D(namePtDistVsPt.c_str(),"PtDistVsPt",40,0,3.1,100,-0.05,0.05);
    PxDistVsPx = new TH2D(namePxDistVsPx.c_str(),"PxDistVsPx",40,-3.1,3.1,100,-0.05,0.05);
    PxDistVsPt = new TH2D(namePxDistVsPt.c_str(),"PxDistVsPt",40,0,3.1,100,-0.05,0.05);
    PyDistVsPt = new TH2D(namePyDistVsPt.c_str(),"PyDistVsPt",40,0,3.1,100,-0.05,0.05);
    PyDistVsPy = new TH2D(namePyDistVsPy.c_str(),"PyDistVsPy",40,-3.1,3.1,100,-0.05,0.05);
    PzDistVsPt = new TH2D(namePzDistVsPt.c_str(),"PzDistVsPt",40,0,3.1,100,-0.05,0.05);
    PzDistVsPz = new TH2D(namePzDistVsPz.c_str(),"PzDistVsPz",40,-3.1,3.1,100,-0.05,0.05);
    PDistVsPt  = new TH2D(namePDistVsPt.c_str() ,"PDistVsPt" ,40,0,3.1,100,-0.05,0.05);
    PDistVsP  = new TH2D(namePDistVsP.c_str() ,"PDistVsP" ,40,-3.1,3.1,100,-0.05,0.05);
    EtaDistVsPt = new TH2D(nameEtaDistVsPt.c_str(),"EtaDistVsPt",40,0,3.1,100,-0.05,0.05);
    EtaDistVsEta = new TH2D(nameEtaDistVsEta.c_str(),"EtaDistVsEta",40,-1.2,1.2,100,-0.05,0.05);
    PhiDistVsPt = new TH2D(namePhiDistVsPt.c_str(),"PhiDistVsPt",40,0,3.1,100,-0.05,0.05);
    PhiDistVsPhi = new TH2D(namePhiDistVsPhi.c_str(),"PhiDistVsPhi",40,-3.2,3.2,100,-0.05,0.05);

    PtDistByPtVsPt = new TH2D(namePtDistByPtVsPt.c_str(),"PtDistByPtVsPt",40,0,3.1,100,-0.05,0.05);
    PxDistByPxVsPt = new TH2D(namePxDistByPxVsPt.c_str(),"PxDistByPxVsPt",40,0,3.1,100,-0.05,0.05);
    PxDistByPxVsPx = new TH2D(namePxDistByPxVsPx.c_str(),"PxDistByPxVsPx",40,-3.1,3.1,100,-0.05,0.05);
    PyDistByPyVsPt = new TH2D(namePyDistByPyVsPt.c_str(),"PyDistByPyVsPt",40,0,3.1,100,-0.05,0.05);
    PyDistByPyVsPy = new TH2D(namePyDistByPyVsPy.c_str(),"PyDistByPyVsPy",40,-3.1,3.1,100,-0.05,0.05);
    PzDistByPzVsPt = new TH2D(namePzDistByPzVsPt.c_str(),"PzDistByPzVsPt",40,0,3.1,100,-0.05,0.05);
    PzDistByPzVsPz = new TH2D(namePzDistByPzVsPz.c_str(),"PzDistByPzVsPz",40,-3.1,3.1,100,-0.05,0.05);
    PDistByPVsPt  = new TH2D(namePDistByPVsPt.c_str() ,"PDistByPVsPt" ,40,0,3.1,100,-0.05,0.05);
    PDistByPVsP  = new TH2D(namePDistByPVsP.c_str() ,"PDistByPVsP" ,40,-3.1,3.1,100,-0.05,0.05);
    EtaDistByEtaVsPt = new TH2D(nameEtaDistByEtaVsPt.c_str(),"EtaDistByEtaVsPt",40,0,3.1,100,-0.05,0.05);
    EtaDistByEtaVsEta = new TH2D(nameEtaDistByEtaVsEta.c_str(),"EtaDistByEtaVsEta",40,-1.2,1.2,100,-0.05,0.05);
    PhiDistByPhiVsPt = new TH2D(namePhiDistByPhiVsPt.c_str(),"PhiDistByPhiVsPt",40,0,3.1,100,-0.05,0.05);
    PhiDistByPhiVsPhi = new TH2D(namePhiDistByPhiVsPhi.c_str(),"PhiDistByPhiVsPhi",40,-3.2,3.2,100,-0.05,0.05);


    accMcHitsVsEtaDistribution = new TH2D(nameaccMcHitsVsEtaDistribution.c_str(),"accMcHitsVsEtaDistribution",80,-2.,2.0,50,0,50.);
    matchedMcHitsVsEtaDistribution = new TH2D(namematchedMcHitsVsEtaDistribution.c_str(),"matchedMcHitsVsEtaDistribution",80,-2.,2.0,50,0,50.);

    rchi2Gl = new TH1D(namerchi2Gl.c_str(),"rchi2Gl",200,-10.0,10.0);
    rchi2Pr = new TH1D(namerchi2Pr.c_str(),"rchi2Pr",200,-10.0,10.0);
    
    rchi2GlvsnPts = new TH2D(namerchi2GlvsnPts.c_str(),"rchi2GlvsnPts",100,0,100,100,0.,5.);
    rchi2GlvsP	  = new TH2D(namerchi2GlvsP.c_str(),"rchi2GlvsP",100,-3.,3.,100,0.,5.);
    rchi2GlvsPt   = new TH2D(namerchi2GlvsPt.c_str(),"rchi2GlvsPt",50,0.,3.,100,0.,5.);
    rchi2GlvsEta  = new TH2D(namerchi2GlvsEta.c_str(),"rchi2GlvsEta",80,-2.,2.,100,0.,5.);
    rchi2PrvsnPts = new TH2D(namerchi2PrvsnPts.c_str(),"rchi2PrvsnPts",100,0,100,100,0.,5.);
    rchi2PrvsP    = new TH2D(namerchi2PrvsP.c_str(),"rchi2PrvsP",100,-3.,3.,100,0.,5.);
    rchi2PrvsPt   = new TH2D(namerchi2PrvsPt.c_str(),"rchi2PrvsPt",50,0.,3.,100,0.,5.);
    rchi2PrvsEta  = new TH2D(namerchi2PrvsEta.c_str(),"rchi2PrvsEta",80,-2.,2.,100,0.,5.);

    nPtsGl        = new TH1D(namenPtsGl.c_str(),"nPtsGl",50,0,50);
    nPtsPr        = new TH1D(namenPtsPr.c_str(),"nPtsPr",50,0,50);
    nFitPtsvsnPossiblePtsPr = new TH2D(namenFitPtsvsnPossiblePtsPr.c_str(),"nFitPtsvsnPossiblePtsPr",50,0.,50.,50,0.,50.); 
    nPtsGlvsPt    = new TH2D(namenPtsGlvsPt.c_str(),"nPtsGlvsPt",50,0.,3.,50,0,50);
    nPtsPrvsPt    = new TH2D(namenPtsPrvsPt.c_str(),"nPtsPrvsPt",50,0.,3.,50,0,50);
    nPtsGlvsEta   = new TH2D(namenPtsGlvsEta.c_str(),"nPtsGlvsEta",80, -2.,2.,50,0,50);
    nPtsPrvsEta   = new TH2D(namenPtsPrvsEta.c_str(),"nPtsPrvsEta",80, -2.,2.,50,0,50);

    tanPr_pull      = new TH1D(nametanPr_pull.c_str(),"tanPr_pull",300,-5.,5.);
    tanGl_pull      = new TH1D(nametanGl_pull.c_str(),"tanGl_pull",300,-5.,5.);
    curvPr_pull     = new TH1D(namecurvPr_pull.c_str(),"curvPr_pull",300,-5.,5.);
    curvGl_pull     = new TH1D(namecurvGl_pull.c_str(),"curvGl_pull",300,-5.,5.);

    return;
}

int EfficiencyAnalysis::resetHistograms() {
    cout << "EfficiencyAnalysis::resetHistograms() " << mSuffix << endl;
    rawMcPiKP->Reset();
    accMcPiKP->Reset();
    matMcPiKP->Reset();
    merMcPiKP->Reset();
    matchedSpectrumRec->Reset();
    electronBackground->Reset();
    decayBackground->Reset();
    splitSpectrum->Reset();
    ghostSpectrum->Reset();
    
    matchedPlusMergedMc->Reset();
    rawSpectrumNoDecayBg->Reset();
    rawSpectrumNoElectronBg->Reset();
    rawSpectrumNoGhosts->Reset();
    rawSpectrum->Reset();
    primaryTrackEffMult->Reset();   
    fitPointsUsed->Reset();         
    fitPointsPossible->Reset();     
    fitPointsUsedPossRatio->Reset();
    dcaGlobal->Reset();
    dcaPrimary->Reset();

    primaryPtBiasVsP->Reset();
    primaryPxBiasVsP->Reset();
    primaryPxBiasVsPx->Reset();
    primaryPyBiasVsP->Reset();
    primaryPyBiasVsPy->Reset();
    primaryPzBiasVsP->Reset();
    primaryPzBiasVsPz->Reset();
    primaryPBiasVsP->Reset();
    primaryEtaBiasVsP->Reset();
    primaryEtaBiasVsEta->Reset();
    primaryPhiBiasVsP->Reset();
    primaryPhiBiasVsPhi->Reset();

    PtDistVsPt->Reset();
    PxDistVsPt->Reset();
    PyDistVsPt->Reset();
    PxDistVsPx->Reset();
    PyDistVsPy->Reset();
    PzDistVsPt->Reset();
    PzDistVsPz->Reset();
    PDistVsPt->Reset();
    PDistVsP->Reset();
    EtaDistVsPt->Reset();
    EtaDistVsEta->Reset();
    PhiDistVsPt->Reset();
    PhiDistVsPhi->Reset();

    PtDistByPtVsPt->Reset();
    PxDistByPxVsPx->Reset();
    PxDistByPxVsPt->Reset();
    PyDistByPyVsPy->Reset();
    PyDistByPyVsPt->Reset();
    PzDistByPzVsPz->Reset();
    PzDistByPzVsPt->Reset();
    PDistByPVsP->Reset();
    PDistByPVsPt->Reset();
    EtaDistByEtaVsEta->Reset();
    EtaDistByEtaVsPt->Reset();
    PhiDistByPhiVsPhi->Reset();
    PhiDistByPhiVsPt->Reset();


    primaryPtDistribution->Reset();
    primaryPxDistribution->Reset();
    primaryPyDistribution->Reset();
    primaryPzDistribution->Reset();
    primaryPDistribution->Reset();
    primaryEtaDistribution->Reset();
    primaryPhiDistribution->Reset();

    accMcPtDistribution->Reset();
    accMcPxDistribution->Reset();
    accMcPyDistribution->Reset();
    accMcPzDistribution->Reset();
    accMcPDistribution->Reset();
    accMcEtaDistribution->Reset();
    accMcPhiDistribution->Reset();

    matchedMcPtDistribution->Reset();
    matchedMcPxDistribution->Reset();
    matchedMcPyDistribution->Reset();
    matchedMcPzDistribution->Reset();
    matchedMcPDistribution->Reset();
    matchedMcEtaDistribution->Reset();
    matchedMcPhiDistribution->Reset();

    accMcHitsVsEtaDistribution->Reset(); 
    matchedMcHitsVsEtaDistribution->Reset();

    rchi2Gl->Reset();
    rchi2Pr->Reset();

    rchi2GlvsnPts->Reset();
    rchi2GlvsP->Reset();
    rchi2GlvsPt->Reset();
    rchi2GlvsEta->Reset();
    rchi2PrvsnPts->Reset();
    rchi2PrvsP->Reset();
    rchi2PrvsPt->Reset();
    rchi2PrvsEta->Reset();

    nPtsGl->Reset();
    nPtsPr->Reset();
    nFitPtsvsnPossiblePtsPr->Reset();
   
    nPtsGlvsPt->Reset();
    nPtsPrvsPt->Reset();
    nPtsGlvsEta->Reset();
    nPtsPrvsEta->Reset();

    tanPr_pull->Reset();
    tanGl_pull->Reset();
    curvPr_pull->Reset();
    curvGl_pull->Reset();
    return 0;
}


int EfficiencyAnalysis::fillHistograms(StMiniMcEvent* minimcevent) {

    cout << "EfficiencyAnalysis::fillHistograms " <<  mSuffix << endl;
    ++mNEvents;
    int nTracks = minimcevent->nMcTrack();
    cout << "No. MC Tracks " << nTracks <<endl;

    // raw & accepted MC tracks
    int nPiKP = 0;
    for (int j=0; j<nTracks; ++j) {
	// mMcTracks loop
	StTinyMcTrack* tinymctrack = (StTinyMcTrack*) minimcevent->tracks(MC)->At(j);
	if (!tinymctrack) continue;
	int geantId = tinymctrack->geantId(); 
	if (!acceptGeantId(geantId)) continue; // only look at pi-, k-, pbar = 9, 12, 15 and pi+, k+, prot = 8, 11, 14
	if (!acceptEtaCut(tinymctrack->etaMc())) continue;
	++nPiKP;
	float pt  = tinymctrack->ptMc();
	float px     = tinymctrack->pxMc();
	float py     = tinymctrack->pyMc();
	float pz     = tinymctrack->pzMc();
	float p      = tinymctrack->pMc();
	float eta    = tinymctrack->etaMc();
	float phi    = tinymctrack->phiMc();
	int hits     = tinymctrack->nHitMc();

	rawMcPiKP->Fill(eta,pt);
	if (tinymctrack->nHitMc()>=10) {
	    accMcPiKP->Fill(eta,pt);
	    accMcPtDistribution->Fill(pt);
	    accMcPxDistribution->Fill(px);
	    accMcPyDistribution->Fill(py);
	    accMcPzDistribution->Fill(pz);
	    accMcPDistribution ->Fill(p);
	    accMcEtaDistribution->Fill(eta);
	    accMcPhiDistribution->Fill(phi);

	    accMcHitsVsEtaDistribution->Fill(eta,hits);
	}
    }
    cout << "geant Id, # mc tracks  " << mGeantId << ", " << nPiKP << endl;
//    mNMcTrack;     
//    mNMatchedPair;
//    mNMergedPair;
//    mNSplitPair;
//    mNGhostPair;
//    mNContamPair;

    // matched and matched rec
    int nMatchedPiKP=0;
    int nMatchedPairs = minimcevent->nMatchedPair();
    cout << "# MatchedPairs  " << nMatchedPairs << " array says " << minimcevent->tracks(MATCHED)->GetEntries() << endl;
    for (int k=0; k<nMatchedPairs; ++k) {
	// mNMatchedPair loop
	StMiniMcPair* minimcpair = (StMiniMcPair*) minimcevent->tracks(MATCHED)->At(k);
	if (!minimcpair) continue;
	
	// for the matches, cut on the geant id (not needed for splits and ghosts)
	int geantId = minimcpair->geantId();
	if (!acceptGeantId(geantId)) continue;
	if (!acceptPair(minimcpair)) continue; // cut on Mc Hits, and rec charge, fit points and dca
	++nMatchedPiKP;
	float pt    = minimcpair->ptMc();
	float px    = minimcpair->pxMc();
	float py    = minimcpair->pyMc();
	float pz    = minimcpair->pzMc();
	float p     = minimcpair->pMc();
	float eta   = minimcpair->etaMc();
	float phi   = minimcpair->phiMc();
	float ptrec = minimcpair->ptPr();
	//float ptrecGl = minimcpair->ptGl(); // unused?
	float pxrec = minimcpair->pxPr();
	float pyrec = minimcpair->pyPr();
	float pzrec = minimcpair->pzPr();
	float prec  = minimcpair->pPr();
	float etarec =  minimcpair->etaPr();
	float phirec =  minimcpair->phiPr();
	int hits     = minimcpair->nHitMc();
	
	float chi2Glrec = 0; //minimcpair->chi2Gl();
	float chi2Prrec = minimcpair->chi2Pr();
	int   nofdofree = hits - 5;

	float chi2GlrecPerNoDoF = chi2Glrec/nofdofree;
	float chi2PrrecPerNoDoF = chi2Prrec/nofdofree;

	float nFitPtsGl = 0; //minimcpair->fitPtsGl();	
	float nFitPtsPr = minimcpair->fitPts(); // minimcpair->fitPtsPr();
	
	float tanPr     = minimcpair->tanLPr();
	float tanGl     = minimcpair->tanLGl();
	float curvPr    = minimcpair->curvPr();
	float curvGl    = minimcpair->curvGl();

	float tanGl_err=-999, curvGl_err=-999, ptPr_err = -999;
	float tanPr_err=-999, curvPr_err=-999, ptGl_err = -999;
	
	float B = 0.49795819;
	float c = 0.299792458;
	
        // only for pull plots: if 0 then ITTF ; if 1 then TPT
	if (mPullType==0) 
	{
	    tanGl_err = ::sqrt(minimcpair->errGl(4));
	    tanPr_err = ::sqrt(minimcpair->errPr(4));
			
	    curvGl_err= ::sqrt(minimcpair->errGl(3));
	    curvPr_err= ::sqrt(minimcpair->errPr(3));
	}
	if (mPullType==1)
	{
	    tanGl_err = ::sqrt(minimcpair->errGl(2));
            tanPr_err = ::sqrt(minimcpair->errPr(2));

            curvGl_err= B*c*::sqrt(minimcpair->errGl(4))/100.;
            curvPr_err= B*c*::sqrt(minimcpair->errPr(4))/100.;
	}

	ptPr_err  = curvPr_err * pt/curvPr;
	ptGl_err  = curvGl_err * pt/curvGl;

	matMcPiKP->Fill(eta,pt);
	matchedSpectrumRec->Fill(etarec,ptrec);
	fitPointsUsed->Fill(minimcpair->fitPts());
	fitPointsPossible->Fill(minimcpair->nPossiblePts());
	if (minimcpair->nPossiblePts())
	    fitPointsUsedPossRatio->Fill(minimcpair->fitPts()/minimcpair->nPossiblePts());
	dcaGlobal->Fill(minimcpair->dcaGl());
	dcaPrimary->Fill(minimcpair->dcaPr());
	primaryPtDistribution->Fill(ptrec);
	primaryPxDistribution->Fill(pxrec);
	primaryPyDistribution->Fill(pyrec);
	primaryPzDistribution->Fill(pzrec);
	primaryPDistribution ->Fill(prec);
	primaryEtaDistribution->Fill(etarec);
	primaryPhiDistribution->Fill(phirec);

	primaryPtBiasVsP->Fill(p,(ptrec-pt));
	primaryPxBiasVsP->Fill(p,(pxrec-px));
	primaryPxBiasVsPx->Fill(px,(pxrec-px));
	primaryPyBiasVsP->Fill(p,(pyrec-py));
	primaryPyBiasVsPy->Fill(py,(pyrec-py));
	primaryPzBiasVsP->Fill(p,(pzrec-pz));
	primaryPzBiasVsPz->Fill(pz,(pzrec-pz));
	primaryPBiasVsP-> Fill(p,(prec-p));
	primaryEtaBiasVsP->Fill(p,(etarec-eta));
	primaryEtaBiasVsEta->Fill(eta,(etarec-eta));
	primaryPhiBiasVsP->Fill(p,(phirec-phi));
	primaryPhiBiasVsPhi->Fill(phi,(phirec-phi));

	//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
	PtDistVsPt->Fill(pt,(ptrec-pt));
	PxDistVsPx->Fill(fabs(px),(pxrec-px));
	PxDistVsPt->Fill(pt,(pxrec-px));
	PyDistVsPt->Fill(pt,(pyrec-py));
	PyDistVsPy->Fill(fabs(py),(pyrec-py));
	PzDistVsPt->Fill(pt,(pzrec-pz));
	PzDistVsPz->Fill(fabs(pz),(pzrec-pz));
	PDistVsPt-> Fill(pt,(prec-p));
	PDistVsP-> Fill(fabs(p),(prec-p));
	EtaDistVsPt->Fill(pt,(etarec-eta));
	EtaDistVsEta->Fill(fabs(eta),(etarec-eta));
	PhiDistVsPt->Fill(pt,(phirec-phi));
	PhiDistVsPhi->Fill(fabs(phi),(phirec-phi));

	if (pt!=0)   PtDistByPtVsPt->Fill(pt,(ptrec-pt)/pt);
	if (px!=0)   PxDistByPxVsPx->Fill(fabs(px),(pxrec-px)/px);
	if (pt!=0)   PxDistByPxVsPt->Fill(pt,(pxrec-px)/px);
	if (py!=0)   PyDistByPyVsPy->Fill(fabs(py),(pyrec-py)/py);
	if (pt!=0)   PyDistByPyVsPt->Fill(pt,(pyrec-py)/py);
	if (pz!=0)   PzDistByPzVsPz->Fill(fabs(pz),(pzrec-pz)/pz);
	if (pt!=0)   PzDistByPzVsPt->Fill(pt,(pzrec-pz)/pz);
	if (p!=0)    PDistByPVsP ->Fill(fabs(p),(prec-p)/p);
	if (pt!=0)   PDistByPVsPt->Fill(pt,(prec-p)/p);
	if (eta!=0)  EtaDistByEtaVsEta->Fill(fabs(eta),(etarec-eta)/eta);
	if (pt!=0)   EtaDistByEtaVsPt->Fill(pt,(etarec-eta)/eta);
	if (phi!=0)  PhiDistByPhiVsPhi->Fill(fabs(phi),(phirec-phi)/phi);
	if (pt!=0)   PhiDistByPhiVsPt->Fill(pt,(phirec-phi)/phi);
	
	matchedMcPtDistribution->Fill(pt);
	matchedMcPxDistribution->Fill(px);
	matchedMcPyDistribution->Fill(py);
	matchedMcPzDistribution->Fill(pz);
	matchedMcPDistribution ->Fill(p);
	matchedMcEtaDistribution->Fill(eta);
	matchedMcPhiDistribution->Fill(phi);

	matchedMcHitsVsEtaDistribution->Fill(eta,hits);

	rchi2Gl->Fill(chi2GlrecPerNoDoF);
	rchi2Pr->Fill(chi2PrrecPerNoDoF);

	rchi2GlvsnPts->Fill(nFitPtsGl,chi2GlrecPerNoDoF);
	rchi2GlvsP->Fill(p,chi2GlrecPerNoDoF);
	rchi2GlvsPt->Fill(pt,chi2GlrecPerNoDoF);
	rchi2GlvsEta->Fill(eta,chi2GlrecPerNoDoF);
	rchi2PrvsnPts->Fill(nFitPtsPr,chi2PrrecPerNoDoF);
	rchi2PrvsP->Fill(p,chi2PrrecPerNoDoF);
	rchi2PrvsPt->Fill(pt,chi2PrrecPerNoDoF);
	rchi2PrvsEta->Fill(eta,chi2PrrecPerNoDoF);

	nPtsGl->Fill(nFitPtsGl);
	nPtsPr->Fill(nFitPtsPr); 
         nFitPtsvsnPossiblePtsPr->Fill(minimcpair->nPossiblePts(),nFitPtsPr);
	nPtsGlvsPt->Fill(pt,nFitPtsGl);
	nPtsPrvsPt->Fill(pt,nFitPtsPr);
	nPtsGlvsEta->Fill(eta,nFitPtsGl);
	nPtsPrvsEta->Fill(eta,nFitPtsPr);
	if (pt != 0)
	{
	    if (tanPr_err !=0) 
		tanPr_pull->Fill((tanPr-pz/pt)/tanPr_err);
	    if (curvPr_err !=0) 
		curvPr_pull->Fill((curvPr - B*c/100/pt)/curvPr_err);  
	    if (tanGl_err !=0) 
		tanGl_pull->Fill((tanGl-pz/pt)/tanGl_err);
	    if (curvGl_err !=0) 
		curvGl_pull->Fill((curvGl - B*c/100/pt)/curvGl_err);   
	}
    }
    cout << "# Matched Pi,K,P  " << nMatchedPiKP << endl;

    // multiplicity dependence of integrated efficiency
    // fill the profile plot of number of matched tracks/ accepted tracks
    // for the given cuts (including id) as a function of the MC multiplicity
    // the counters nMatchedPiKP and nPiKP are incremented in the two loops above
    // when we find a track that matches the respective cuts.
    if (nPiKP) 
	primaryTrackEffMult->Fill((float) nTracks,(float)nMatchedPiKP/(float)nPiKP);

    // merged
    int nMergedPair = minimcevent->nMergedPair();
    cout << "# MergedPair  " << nMergedPair << " array says " << minimcevent->tracks(MERGED)->GetEntries() << endl;
    for (int j=0; j<nMergedPair; ++j) {
	// mNMergedPair loop
	StMiniMcPair* minimcpair = (StMiniMcPair*) minimcevent->tracks(MERGED)->At(j);
	if (!minimcpair) continue;
	
	// for the merged, cut on the geant id and MC Tpc hits
	int geantId = minimcpair->geantId();
	if (!acceptGeantId(geantId)) continue;
	if (!(minimcpair->nHitMc()>=10)) continue;
	
	float pt = minimcpair->ptMc();
	float eta =  minimcpair->etaMc();
	merMcPiKP->Fill(eta,pt);
    }
    
    // background, electron and decay/secondary
    int nContamPairs = minimcevent->nContamPair();
    int nContamPiKP=0;
    cout << "minimcevent->mContamPairs " << minimcevent->tracks(CONTAM) << endl;
    if (minimcevent->tracks(CONTAM)) { 
	cout << "# ContamPairs  " << nContamPairs << " array says " << minimcevent->tracks(CONTAM)->GetEntries() << endl;
	for (int j=0; j<nContamPairs; ++j) {
	    // mNContamPair loop
	    StMiniMcPair* minimcpair = (StMiniMcPair*) minimcevent->tracks(CONTAM)->At(j);
	    if (!minimcpair) continue;
	    
	    // for the contamination, make sure the rec satisfies the cuts and
	    // then select electrons according to geant id
	    if (!acceptPair(minimcpair)) continue;
	    
	    float ptrec = minimcpair->ptPr();
	    float etarec =  minimcpair->etaPr();
	    
	    int geantId = minimcpair->geantId();
	    if (geantId == 2 || geantId == 3) {
		// e+/e- contamination (charge was selected by the acceptPair cut)
		electronBackground->Fill(etarec,ptrec);    
	    }
	    else if (acceptGeantId(geantId)) { // mGeantId is set properly, use background for a specific particle
		decayBackground->Fill(etarec,ptrec);
		++nContamPiKP;
	    }
	    
	}
	cout << "# Contam Pi,K,P " << nContamPiKP << endl;
    }
    else {
	cout << "The array mNContamPairs is null " << endl;
    }
    // split
    int nSplitPairs = minimcevent->nSplitPair();
    // 	    cout << "# SplitPairs  " << nSplitPairs << " array says " << minimcevent->tracks(SPLIT)->GetEntries() << endl;
    for (int j=0; j<nSplitPairs; ++j) { // mNSplitPair loop
	StMiniMcPair* minimcpair = (StMiniMcPair*) minimcevent->tracks(SPLIT)->At(j);
	if (!minimcpair) continue;
	
	if (!acceptPair(minimcpair)) continue;
	
	float ptrec = minimcpair->ptPr();
	float etarec =  minimcpair->etaPr();
	
	splitSpectrum->Fill(etarec,ptrec);    
	
    }
    
    
    // ghosts
    int nGhostPairs = minimcevent->nGhostPair();
    // 	    cout << "# GhostPairs  " << nGhostPairs << endl;
    for (int j=0; j<nGhostPairs; ++j) { // mNGhostPair loop
	StMiniMcPair* minimcpair = (StMiniMcPair*) minimcevent->tracks(GHOST)->At(j);
	if (!minimcpair) continue;
	
	if (!acceptPair(minimcpair)) continue;
	
	float ptrec = minimcpair->ptPr();
	float etarec =  minimcpair->etaPr();
	ghostSpectrum->Fill(etarec,ptrec);    
	
    }
    
    //
    // Make the other histograms
    //
    matchedPlusMergedMc->Add(matMcPiKP,merMcPiKP);
    rawSpectrumNoElectronBg->Add(splitSpectrum,matchedSpectrumRec);
    rawSpectrumNoDecayBg->Add(electronBackground,rawSpectrumNoElectronBg);
    rawSpectrumNoGhosts->Add(decayBackground,rawSpectrumNoDecayBg);
    rawSpectrum->Add(ghostSpectrum,rawSpectrumNoGhosts);
    return 0;
}


int EfficiencyAnalysis::writeHistograms() {
    cout << "N events " << mNEvents << " for " << mSuffix << endl;
    cout << mFileName << endl;
    
    // must open file in update mode, as all the efficiency analysis
    // classes will write into the same file.
    TFile* efficiencyfile = TFile::Open(mFileName,"UPDATE");
    
    rawMcPiKP->Write();
    accMcPiKP->Write();
    matMcPiKP->Write();
    merMcPiKP->Write();
    matchedSpectrumRec->Write();
    rawSpectrum->Write();

    matchedPlusMergedMc->Write();
    rawSpectrumNoDecayBg->Write();
    rawSpectrumNoElectronBg->Write();
    rawSpectrumNoGhosts->Write();

    primaryTrackEffMult->Write();    
    fitPointsUsed->Write();          
    fitPointsPossible->Write();      
    fitPointsUsedPossRatio->Write(); 
    dcaGlobal->Write();              
    dcaPrimary->Write();              

    primaryPtBiasVsP->Write();
    primaryPxBiasVsP->Write();
    primaryPxBiasVsPx->Write();
    primaryPyBiasVsP->Write();
    primaryPyBiasVsPy->Write();
    primaryPzBiasVsP->Write();
    primaryPzBiasVsPz->Write();
    primaryPBiasVsP->Write();
    primaryEtaBiasVsP->Write();
    primaryEtaBiasVsEta->Write();
    primaryPhiBiasVsP->Write();
    primaryPhiBiasVsPhi->Write();

    PtDistVsPt->Write();
    PxDistVsPx->Write();
    PxDistVsPt->Write();
    PyDistVsPt->Write();
    PyDistVsPy->Write();
    PzDistVsPt->Write();
    PzDistVsPz->Write();
    PDistVsPt->Write();
    PDistVsP->Write();
    EtaDistVsPt->Write();
    EtaDistVsEta->Write();
    PhiDistVsPt->Write();
    PhiDistVsPhi->Write();

    PtDistByPtVsPt->Write();
    PxDistByPxVsPx->Write();
    PxDistByPxVsPt->Write();
    PyDistByPyVsPy->Write();
    PyDistByPyVsPt->Write();
    PzDistByPzVsPz->Write();
    PzDistByPzVsPt->Write();
    PDistByPVsP->Write();
    PDistByPVsPt->Write();
    EtaDistByEtaVsEta->Write();
    EtaDistByEtaVsPt->Write();
    PhiDistByPhiVsPhi->Write();
    PhiDistByPhiVsPt->Write();

    primaryPtDistribution->Write();
    primaryPxDistribution->Write();
    primaryPyDistribution->Write();
    primaryPzDistribution->Write();
    primaryPDistribution->Write();
    primaryEtaDistribution->Write();
    primaryPhiDistribution->Write();

    accMcPtDistribution->Write();
    accMcPxDistribution->Write();
    accMcPyDistribution->Write();
    accMcPzDistribution->Write();
    accMcPDistribution->Write();
    accMcEtaDistribution->Write();
    accMcPhiDistribution->Write();

    matchedMcPtDistribution->Write();
    matchedMcPxDistribution->Write();
    matchedMcPyDistribution->Write();
    matchedMcPzDistribution->Write();
    matchedMcPDistribution->Write();
    matchedMcEtaDistribution->Write();
    matchedMcPhiDistribution->Write();

    accMcHitsVsEtaDistribution->Write();
    matchedMcHitsVsEtaDistribution->Write();


    rchi2Gl->Write();
    rchi2Pr->Write();
    rchi2GlvsnPts->Write();
    rchi2GlvsP->Write();
    rchi2GlvsPt->Write();
    rchi2GlvsEta->Write();
    rchi2PrvsnPts->Write();
    rchi2PrvsP->Write();
    rchi2PrvsPt->Write();
    rchi2PrvsEta->Write();
    
    nPtsGl->Write();
    nPtsPr->Write();
    nFitPtsvsnPossiblePtsPr->Write();
    nPtsGlvsPt->Write();
    nPtsPrvsPt->Write();
    nPtsGlvsEta->Write();
    nPtsPrvsEta->Write();

    tanPr_pull->Write();
    tanGl_pull->Write();
    curvPr_pull->Write();
    curvGl_pull->Write();

    efficiencyfile->Close();   
    return 0;
}


