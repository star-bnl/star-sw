/***************************************************************************
 *
 * $Id: StppuDstMaker.h,v 1.9 2004/01/24 02:02:55 akio Exp $
 * $Log: StppuDstMaker.h,v $
 * Revision 1.9  2004/01/24 02:02:55  akio
 * Adding call for init and finish for 2004
 *
 * Revision 1.8  2003/10/16 19:48:37  akio
 * updates for 2003
 *
 * Revision 1.7  2003/09/07 03:49:05  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 1.6  2003/05/14 18:00:25  akio
 * New addition for 2003 data ntuple prodction
 * Also fix a problem with MuTrack creating from StEvent tracks.
 *
 * Revision 1.5  2003/02/04 21:57:10  akio
 * Improvments on pi0 reconstruction code and ntuple
 *
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

//#defile _SPINDSTOUT_
//#define _2002ntuple_
//#define _2003ntuple_
#define _2004ntuple_
//#define _GEANT_
#define _BBC_data_
#define _FPD_data_
//#define _EMC_CLUSTERS_
//#define _EMC_POINTS_
//#define _EMC_
//VP #define MAXANALYZERS 12 //enum instead

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
  enum {MAXANALYZERS =12};
  public:
  StppuDstMaker(const Char_t *name="StppuDst");
  ~StppuDstMaker();   
  void Clear(Option_t *option=""); 
  Int_t Init();
  Int_t Init(const Char_t* filename);
  Int_t Make();
  Int_t Finish(); 
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StppuDstMaker.h,v 1.9 2004/01/24 02:02:55 akio Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
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
