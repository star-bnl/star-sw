//
// $Id EfficiencyAnalysis.cxx $
//
// $Log: EfficiencyAnalysis.cxx,v $
// Revision 1.2  2003/03/05 22:02:29  calderon
// protect against tracks which have zero possible points...
// caused a floating point exception
//
// Revision 1.1  2002/11/27 00:08:55  calderon
// New version of evaluator using the minimctrees
//
//
#include "EfficiencyAnalysis.h"

#include <iostream>

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
    dcaGlobal(0)
{
}
EfficiencyAnalysis::EfficiencyAnalysis(const EfficiencyAnalysis& e) :
    mNEvents(e.mNEvents),
    mFitPtsLimit(e.mFitPtsLimit),
    mDcaLimit(e.mDcaLimit),
    mGeantId(e.mGeantId),
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
    dcaGlobal(0)
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
}

bool EfficiencyAnalysis::acceptPair(StMiniMcPair* pair) {
    // for identified pi- use more stringent cuts (for + just flip charge sign)
    // pi-, k-, pbar = 9, 12, 15
    // pi+, k+, prot = 8, 11, 14
    if ((mGeantId==8 || mGeantId==11 || mGeantId==14) && pair->charge()<0) return false;
    if ((mGeantId==9 || mGeantId==12 || mGeantId==15) && pair->charge()>0) return false;
    if (!(pair->nHitMc()>=10)) return false;
    if (!(pair->fitPts()>=24)) return false; // thesis >= 24, BigBad >=25
    if (!(pair->dcaGl()<1.)) return false; // thesis primary dca (only for P00hm), Big Bad global dca
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
    fitPointsUsedPossRatio  = new TH1D(namefitPointsUsedPossRatio.c_str(),"Fit Points/Possible Points", 50,0,1);
    dcaGlobal               = new TH1D(namedcaGlobal.c_str(),"global dca", 100, 0, 3.1);

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
    return 0;
}


int EfficiencyAnalysis::fillHistograms(StMiniMcEvent* minimcevent) {
    // makehistograms() method
    cout << "EfficiencyAnalysis::fillHistograms " <<  mSuffix << endl;
    ++mNEvents;
    int nTracks = minimcevent->mNMcTrack;

    // raw & accepted MC tracks
    int nPiKP = 0;
    for (int j=0; j<nTracks; ++j) {
	// mMcTracks loop
	StTinyMcTrack* tinymctrack = (StTinyMcTrack*) minimcevent->mMcTracks->At(j);
	if (!tinymctrack) continue;
	int geantId = tinymctrack->geantId(); 
// 	if (geantId != 0) cout << "j id " << j << " " << geantId << endl;
	if (!acceptGeantId(geantId)) continue; // only look at pi-, k-, pbar = 9, 12, 15 and pi+, k+, prot = 8, 11, 14
	++nPiKP;
	float pt  = tinymctrack->ptMc();
	float xval = tinymctrack->etaMc();
	rawMcPiKP->Fill(xval,pt);
	if (tinymctrack->nHitMc()>=10) {
	    accMcPiKP->Fill(xval,pt);
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
    int nMatchedPairs = minimcevent->mNMatchedPair;
    cout << "# MatchedPairs  " << nMatchedPairs << " array says " << minimcevent->mMatchedPairs->GetEntries() << endl;
    for (int k=0; k<nMatchedPairs; ++k) {
	// mNMatchedPair loop
	StMiniMcPair* minimcpair = (StMiniMcPair*) minimcevent->mMatchedPairs->At(k);
	if (!minimcpair) continue;
	
	// for the matches, cut on the geant id (not needed for splits and ghosts)
	int geantId = minimcpair->geantId();
	if (!acceptGeantId(geantId)) continue;
	if (!acceptPair(minimcpair)) continue; // cut on Mc Hits, and rec charge, fit points and dca
	++nMatchedPiKP;
	float pt   = minimcpair->ptMc();
	float xval = minimcpair->etaMc();
	matMcPiKP->Fill(xval,pt);
	float ptrec = minimcpair->ptPr();
	float etarec =  minimcpair->etaPr();
	matchedSpectrumRec->Fill(etarec,ptrec);
	fitPointsUsed->Fill(minimcpair->fitPts());
	fitPointsPossible->Fill(minimcpair->nPossiblePts());
	if (minimcpair->nPossiblePts())
	    fitPointsUsedPossRatio->Fill(minimcpair->fitPts()/minimcpair->nPossiblePts());
	dcaGlobal->Fill(minimcpair->dcaGl());
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
    int nMergedPair = minimcevent->mNMergedPair;
    cout << "# MergedPair  " << nMergedPair << " array says " << minimcevent->mMergedPairs->GetEntries() << endl;
    for (int j=0; j<nMergedPair; ++j) {
	// mNMergedPair loop
	StMiniMcPair* minimcpair = (StMiniMcPair*) minimcevent->mMergedPairs->At(j);
	if (!minimcpair) continue;
	
	// for the merged, cut on the geant id and MC Tpc hits
	int geantId = minimcpair->geantId();
	if (!acceptGeantId(geantId)) continue;
	if (!(minimcpair->nHitMc()>=10)) continue;
	
	float pt = minimcpair->ptMc();
	float xval =  minimcpair->etaMc();
	merMcPiKP->Fill(xval,pt);
    }
    
    // background, electron and decay/secondary
    int nContamPairs = minimcevent->mNContamPair;
    int nContamPiKP=0;
    cout << "minimcevent->mContamPairs " << minimcevent->mContamPairs << endl;
    if (minimcevent->mContamPairs) { 
	cout << "# ContamPairs  " << nContamPairs << " array says " << minimcevent->mContamPairs->GetEntries() << endl;
	for (int j=0; j<nContamPairs; ++j) {
	    // mNContamPair loop
	    StMiniMcPair* minimcpair = (StMiniMcPair*) minimcevent->mContamPairs->At(j);
	    if (!minimcpair) continue;
	    
	    // for the contamination, make sure the rec satisfies the cuts and
	    // then select electrons according to geant id
	    if (!acceptPair(minimcpair)) continue;
	    
	    float ptrec = minimcpair->ptPr();
	    float xvalrec =  minimcpair->etaPr();
	    
	    int geantId = minimcpair->geantId();
	    if (geantId == 2 || geantId == 3) {
		// e+/e- contamination (charge was selected by the acceptPair cut)
		electronBackground->Fill(xvalrec,ptrec);    
	    }
	    else if (acceptGeantId(geantId)) { // mGeantId is set properly, use background for a specific particle
		decayBackground->Fill(xvalrec,ptrec);
		++nContamPiKP;
	    }
	    
	}
	cout << "# Contam Pi,K,P " << nContamPiKP << endl;
    }
    else {
	cout << "The array mNContamPairs is null " << endl;
    }
    // split
    int nSplitPairs = minimcevent->mNSplitPair;
    // 	    cout << "# SplitPairs  " << nSplitPairs << " array says " << minimcevent->mSplitPairs->GetEntries() << endl;
    for (int j=0; j<nSplitPairs; ++j) { // mNSplitPair loop
	StMiniMcPair* minimcpair = (StMiniMcPair*) minimcevent->mSplitPairs->At(j);
	if (!minimcpair) continue;
	
	if (!acceptPair(minimcpair)) continue;
	
	float ptrec = minimcpair->ptPr();
	float xvalrec =  minimcpair->etaPr();
	
	splitSpectrum->Fill(xvalrec,ptrec);    
	
    }
    
    
    // ghosts
    int nGhostPairs = minimcevent->mNGhostPair;
    // 	    cout << "# GhostPairs  " << nGhostPairs << endl;
    for (int j=0; j<nGhostPairs; ++j) { // mNGhostPair loop
	StMiniMcPair* minimcpair = (StMiniMcPair*) minimcevent->mGhostPairs->At(j);
	if (!minimcpair) continue;
	
	if (!acceptPair(minimcpair)) continue;
	
	float ptrec = minimcpair->ptPr();
	float xvalrec =  minimcpair->etaPr();
	ghostSpectrum->Fill(xvalrec,ptrec);    
	
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

    efficiencyfile->Close();   
    return 0;
}

inline void EfficiencyAnalysis::setFitPtsLimit(double val) {mFitPtsLimit = val;}

inline void EfficiencyAnalysis::setDcaLimit(double val) {mDcaLimit = val;}

inline void EfficiencyAnalysis::setGeantId(int val) { mGeantId = val;}

inline void EfficiencyAnalysis::setFileName(char* val) { mFileName = val;}

inline void EfficiencyAnalysis::setSuffix(string val) { mSuffix = val;}


