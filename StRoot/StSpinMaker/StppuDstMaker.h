/***************************************************************************
 *
 * $Id: StppuDstMaker.h,v 1.4 2002/12/04 20:28:10 thenry Exp $
 * $Log: StppuDstMaker.h,v $
 * Revision 1.4  2002/12/04 20:28:10  thenry
 * StppuDstMaker was modified to allow multiple jet analysis modules to be
 * run simultaneosly with various parameters while the Maker loads the events
 * and analyses them.  Four different jet analyzers exist:
 *
 * Konstanin's Analyzers:
 *     Kt type: StppKonstKtJetAnalyzer
 *     Cone type: StppKonstConeJetAnalyzer
 *
 * Mike's Analyzers:
 *     Kt type: StppMikeKtJetAnalyzer
 *     Cone type: StppMikeConeJetAnalyzer
 *
 * These modules all require the StJetFinder modules.
 *
 * Revision 1.3  2002/06/24 13:22:59  akio
 * numerous bug fix & updates
 *
 * Revision 1.2  2002/02/11 20:30:48  akio
 * Many updates, including very first version of jet finder.
 *
 * Revision 1.1  2002/01/16 20:22:54  akio
 * First version
 *
 * 
 * Author: Akio Ogawa June 2001
 ***************************************************************************
 *
 * Description:  TTree uDst for spin-pp
 *
 ***************************************************************************/
#ifndef StppuDst_h
#define StppuDst_h
#include "StMaker.h"
#include "StppJetAnalyzer.h"

//#define _GEANT_
#define _BBC_data_
//#define _FPD_data_
//#define _EMC_CLUSTERS_
//#define _EMC_POINTS_
//#define _EMC_
#define MAXANALYZERS 12

class TFile;
class TTree;
class StppEvent;
class StppGeant;
class StBbcTriggerDetector;
class StFpdCollection;
class StEmcClusterCollection;
class StEmcPoint;
class StMuDst;
class StEmcMicroEvent;

class StppuDstMaker : public StMaker {
public:
    StppuDstMaker(const Char_t *name="StppuDst");
    Int_t Init(const Char_t* filename="spinDst.root");
    Int_t Make();
    Int_t Finish(); 
    void setMuDst(StMuDst* p) {mudst=p;};
    StppEvent* event() {return ppEvent;} //added by MLM
    TTree* tree() {return ppuDst;}  //added by MLM

    void SetSaveEventWithNoJets(bool saveIt);
    void addAnalyzer(StppJetAnalyzer* a, const char *);
#ifdef _EMC_
    inline StEmcMicroEvent **GetEmcEventAd(void) {
      return &emcEvent; };
    inline StEmcMicroEvent *GetEmcEvent(void) {
      return emcEvent; };
    inline void SetStoreEMC(bool store) { storeEMC = store; };
#endif

protected:
    StppJetAnalyzer **jetBranches;
    char** names;
    Int_t numJetBranches;
    bool saveEventWithNoJets;    

private:
    StMuDst*        mudst;        //!
    size_t          mGoodCounter; //!
    size_t          mBadCounter;  //!
    TFile           *m_outfile;   //!
    TTree           *ppuDst;      //!
    StppEvent       *ppEvent;     //!
#ifdef _GEANT_
    StppGeant       *ppGeant;     //!
#endif
#ifdef _BBC_data_
    StBbcTriggerDetector *bbc;    //!
#endif
#ifdef _FPD_data_
    StFpdCollection *fpd;         //!
#endif
#ifdef _EMC_
    StEmcMicroEvent *emcEvent;    //!
    bool storeEMC; //!
#endif
#ifdef _EMC_CLUSTERS_
    StEmcClusterCollection* emcClusters[3];//!
#endif
#ifdef _EMC_POINTS_
//    TClonesArray     *emcPoints;  //!
    StppEmcPoints    *ppEmcPoints;  //!
//    TClonesArray     *ppemcPoints;  //!
#endif
    Int_t            infoLevel;
  
    ClassDef(StppuDstMaker,0)
	};
#endif
