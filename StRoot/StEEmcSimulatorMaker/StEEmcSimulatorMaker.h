#ifndef STAR_StEEmcSimulatorMaker
#define STAR_StEEmcSimulatorMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEEmcSimulatorMaker                                                 //
// Modified from StEmcSimulatorMaker                                    //  
//                                                                      //
//////////////////////////////////////////////////////////////////////////

/* class StEEmcSimulator
Author Wei-Ming Zhang     3/10/2005

This maker does simulation for EEmc with "RAW" geant.root data rather than
GEANT data in MuDst format as in StEEmcSlowMaker.

   1, StMcEvent, StMcTrack, and StMcEventMaker have to be updated for EEmc. 
   2, StEmcSimlatorMaker has room for both BEmc and EEmc. But we chose
      to have an indepedent StEEmcSimulatormaker for EEmc.
   3, The simulation for PRE/POST showers and SMD is exactly the same as in 
      StEmcSlowMaker. While the one for TOWER is just adding pedestal for now.
   4, A StEmcCollection and a StMuEmcCollection with EEmc simulated data 
      are filled for future use by other makers. 
   5, We have to convert StEvent to MuDst to use the reliable EEmc database 
      which is compatible with MuDst. StMuEmcUtil does not carry energy for 
      TOWER when converting EEMC data from MuDst to StEvent or from StEvent 
      to MuDst. All TOWER energy data are lost after conversion. Energy is 
      recalculated in StEEmcMixerMaker.
   6, The basic MC-physics quantity to start with is energy rather than ADC 
      as in StEEmcSlowMaker. In GEANT, only energy is simulated, but not ADC. 
      And, StMcCalorimeterHit and StMcEmcModuleHitCollection only carry 
      energy. ADC has to be calculated before smearing and other simulations. 
      The calculation is finished in method makeAllRawHitsForEEmc() 
       
 Flow chart 
------------
   1, Method makeEEmc() reads data read from geant.root file and stores them 
      in   StMcEmcHitCollection*  mEmcMcHits[NDETECTORS] for 4 EEmc 
      sub-detectors.
   2, Method makeAllRawHitsForEEmc() fills  
      St_emc_hits*  mEmcRawHits[NDETECTORS] with data converted from 
      mEmcMcHits[NDETECTORS] 
   3, Method fillStEvent() fills StEmcCollection*  mEmcCollection
   4, The mEmcCollection is converted to mMuEmcCollection.  A Simulation 
      is done in MuDst level with parameters checked out from 
      StEEmcDbMaker *eeDb as StEEmcSlowMaker.
   5, Data member  mEmcCollection is updated after the simulation. 
*/

#ifndef StMaker_H
#include "StMaker.h"
#include "SlowSimUtil.h"
#endif
#include <TH2.h>
#include <TH1.h>
#include <TCanvas.h>
#include "tables/St_emc_hits_Table.h"

/* 
 To separate StEEmcSimulatorMaker from StEmcSimulatorMaker, EEMC-specific 
 variables are defined instead of using awkward ones in StEmcUtil.  
 Note: 1, Here,      NDETECTORS = 4        rather than MAXDET = 8 in StEmcUtil. 
       2,            Etow = 0                          EEMC = 5
       3,            eemcDetname[0] = "etow"           detname[4] = "eemc"   
                     eemcDetname[1] = "eprs"           detname[5] = "eprs"   
                     eemcDetname[2] = "esmdu"          detname[6] = "esmde"   
                                [3]   "esmdv"          detname[7] = "esmdp" 
*/

#define NDETECTORS 4
#define Etow  0
#define Eprs  1
#define Esmdu 2
#define Esmdv 3

// enum copied from StEEmcUtil/EEmcMC/EEmcMCData 
enum EEmcVolId {
  // for Tower
  kEEmcTowerHalfId =  100000,
  kEEmcTowerPhiId  =    1000,
  kEEmcTowerEtaId  =      10,
  kEEmcTowerDepId  =       1,

  // for SMDs
  kEEmcSmdHalfId   = 1000000,
  kEEmcSmdPhiId    =   10000,
  kEEmcSmdPlaneId  =    1000,
  kEEmcSmdStripId  =        1
};

enum MCDepth {       
  kUnknownDepth    = 0,
  kPreShower1Depth = 1,
  kPreShower2Depth = 2,
  kTower1Depth     = 3,
  kTower2Depth     = 4,
  kPostShowerDepth = 5
};

enum MCDetectorId {
  kEEmcMCUnknownId    = 0,
  kEEmcMCTowerId      = 1,
  kEEmcMCPreShower1Id = 2,
  kEEmcMCPreShower2Id = 3,
  kEEmcMCSmdUStripId  = 4,
  kEEmcMCSmdVStripId  = 5,
  kEEmcMCPostShowerId = 6
};


class StEmcCollection;
class StMcEmcHitCollection;

class StMuEmcCollection;
class StMuEmcUtil;

class StEEmcDbMaker;
class St_g2t_emc_hit;

class StEEmcSimulatorMaker : public StMaker, public SlowSimUtil 
{
  private:

    UInt_t                 mEEmc;            // Switch 0 => off; >0 => on
    UInt_t                 mHistControl;     // Do histogramms (1) or no (0)

    Bool_t                 mPrint;
    Bool_t                 mSimEtow;
    Bool_t                 mSimEprs;
    Bool_t                 mSimEsmd;

    Bool_t                 mAddPed;   // Offset by pedestal (default false)
    Bool_t                 mSmearPed; // Smear the pedestals (default false)
    Bool_t                 mDropBad;  // Drop bad channels (default false)
    Bool_t                 mOverwrite;// Overwrite muDst values(default true)
    Float_t                mKSigma;   // # of sigma over pedestal defined in DB

    TCanvas*               mC1;              

    TDataSet*              geaIn;

    StEEmcDbMaker          *eeDb;

    St_g2t_emc_hit*        g2t_eem_hit; 
    St_g2t_emc_hit*        g2t_esm_hit;

    StMcEmcHitCollection*  mEmcMcHits[NDETECTORS];  // For convinience 
    St_emc_hits*           mEmcRawHits[NDETECTORS]; // For convinience 

    StEmcCollection*       mEmcCollection;          // As in StEvent
    StMuEmcCollection*     mMuEmcCollection;        // As in StMuEvent

    StMuEmcUtil*           mMuEmcUtil;              // 

    TH2F*                  m_nhit;                 //! 
    TH2F*                  m_etot;                 //!
    TH2F*                  m_hits[NDETECTORS];     //!
    TH2F*                  m_energy[NDETECTORS];   //!

    TH1F*                  m_adc[NDETECTORS];      //!
    TH1F*                  mEnergySum[NDETECTORS]; //!
    TH1F*                  mhSector[NDETECTORS];   //! 
    TH1F*                  mhSub[NDETECTORS];      //!
        
    void                   addEtowHit(Int_t,Int_t,Int_t,Float_t);
    void                   addEprsHit(Int_t,Int_t,Int_t,Float_t);
    void                   addEsmdHit(Int_t,Int_t,Int_t,Int_t,Float_t);

  public: 
                           StEEmcSimulatorMaker(const char *name="EEmcSimulator"); 
    virtual                ~StEEmcSimulatorMaker();
    virtual Int_t          Init();
    virtual Int_t          Make();

    Int_t                  makeEEmc();
    Int_t                  makeEtowAndEprsMcHits();
    Int_t                  makeEsmdMcHits();
    Int_t                  makeAllRawHitsForEEmc();
    Int_t                  fillStEvent();

    void                   simEtow();
    void                   simEprs();
    void                   simEsmd();

    void                   bookHistograms(const Int_t);
    void                   makeHistograms(const Int_t); 

    StMcEmcHitCollection*  getEmcMcHits(Int_t det) {return mEmcMcHits[det];}
    StMcEmcHitCollection*  getEtowMcHits() {return getEmcMcHits(Etow);}
    StMcEmcHitCollection*  getEprsMcHits() {return getEmcMcHits(Eprs);}
    StMcEmcHitCollection*  getEsmduMcHits() {return getEmcMcHits(Esmdu);}
    StMcEmcHitCollection*  getEsmdvMcHits() {return getEmcMcHits(Esmdv);}

    StEmcCollection*       getEmcCollection() {return mEmcCollection;}
    StMuEmcCollection*     getMuEmcCollection(){return mMuEmcCollection;}

    UInt_t                 getEEmc() {return mEEmc;}
    UInt_t                 getHistControl() {return mHistControl;}
    
    void                   Browse(TBrowser* b); // StEvent staf will be visible in browser
    void                   pictureAllDetectors(Int_t print=0);                 
    void                   pictureForDetector(Int_t det, Int_t logy=1, Int_t print=0);   

    void                   printmEEmc();
    void                   printEEmcHits();
    void                   printSimFlags();

    void                   setEEmc(UInt_t  key) {mEEmc = key; if (Debug()) printmEEmc();}
    void                   setSimEtow(Bool_t a)   {mSimEtow = a;}
    void                   setSimEprs(Bool_t a)   {mSimEprs = a;}
    void                   setSimEsmd(Bool_t a)   {mSimEsmd = a;}
    void                   setAddPed(Bool_t a)    {mAddPed = a;}
    void                   setSmearPed(Bool_t a)  {mSmearPed = a;};
    void                   setDropBad(Bool_t a)   {mDropBad = a;};
    void                   setOverwrite(Bool_t a) {mOverwrite = a;};

    void                   setPrint(Bool_t a) {mPrint = a;}
    void                   setHistControl(UInt_t key) {mHistControl = key;}

    virtual const char*    GetCVS() const {static const char cvs[]="Tag $Name:  $ $Id: StEEmcSimulatorMaker.h,v 1.1 2005/05/23 19:41:23 wzhang Exp $ built "__DATE__" "__TIME__ ; return cvs;}

    ClassDef(StEEmcSimulatorMaker,0)  // Simulation maker for EEMC
};

#endif

////////////////////////////////////////////////////////////////////////////////
//
// $Id: StEEmcSimulatorMaker.h,v 1.1 2005/05/23 19:41:23 wzhang Exp $
// $Log: StEEmcSimulatorMaker.h,v $
// Revision 1.1  2005/05/23 19:41:23  wzhang
// First version
//
//
////////////////////////////////////////////////////////////////////////////////
