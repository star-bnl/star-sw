//////////////////////////////////////////////////////////////////////////
//
//
// StEmcTriggerMaker R. Fatemi (Oct 26, 2006)
//
// The structure of this class was first developed by J.Klay and Alex Suaide in 2001.
// It was originally designed to fill StEvent with the simulated L0 trigger response
// but to my understanding was never fully implemented 
//
// Early in 2005, using code originally developed by Alex Stopolsky to emulate the BEMC
// FEE output, I expanded the code to return full BEMC L0 trigger emulation. This code
// was motivated by the need to run the same trigger algorithm over data and simulation.
// All DSM outputs are stored (for data only) in StTriggerDetector class. The ultimate
// design vision is that StEmcTriggerMaker serves as access to the StBemcTrigger and
// StEemcTrigger classes which mock up the BEMC/EEMC FEE + L0 DSM trigger algorithms.
// Interface to L2 should also take place in this class.
//
// $Id: StEmcTriggerMaker.h,v 1.18 2007/05/12 12:45:53 rfatemi Exp $
//
// $Log: StEmcTriggerMaker.h,v $
// Revision 1.18  2007/05/12 12:45:53  rfatemi
// Added BHT2 for 2003, new access scheme extends back to 2003+2004, remove all access to StEmcPedestal tables
//
// Revision 1.17  2007/05/02 17:36:22  kocolosk
// added decoder wrapper method that correlates tower and trigger patch.
// Useful for HTTP in particular.
//
// Revision 1.16  2007/04/30 01:00:25  rfatemi
// Update for new trigger interface
//
// Revision 1.15  2007/04/24 15:53:18  kocolosk
// added new interface methods to get trigger thresholds and decisions based on trigId
//
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StEmcTriggerMaker
#define STAR_StEmcTriggerMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include <TH1.h>
#include <TH2.h>
#include "emc_def.h"
#include "StBemcTrigger.h"
#include "StEmcUtil/database/StBemcTables.h"
#include "StMessMgr.h"

#include <map>
using namespace std;

#define kNPatches 300
#define kNJet 12
#define kNTowers 4800
#define k12bits 4096

class StEvent;
class St_db_Maker;

class StEmcTriggerMaker : public StMaker
{
private:
    StBemcTrigger*    mBemcTrigger;

    int               isTrig[50];
    int               TowJetId[50];
    int               DsmAdc[50];
    int               numHT[12];//# of HT which pass HT trigger
    int               numJP[12];//# of JP which pass JP trigger
    int               numHTTP[6];//# of HT-TP which pass HTTP trigger
    int               HT12005array[kNTowers];//array of towers which pass trigger
    int               HT22005array[kNTowers];//array of towers which pass trigger
    int               JP12005array[kNJet];//array of JP's which pass trigger
    int               JP22005array[kNJet];//array of JP's which pass trigger
    int               JPSI2005adc[kNJet];//array of adc for highest tower in JP
    int               JPSI2005id[kNJet];//array of id for highest tower in JP
    int               BL12006arrayADC[kNJet/2];//array of level 1 DSM 5 bit ADC from 2x1 PATCH sum
    
    int mIs2003HT1;
    int mIs2003HT2;
    int mIs2004HT1;
    int mIs2004JP1;
    int mIs2004HT2;
    int mIs2004JP2;
    int mIs2005HT1;
    int mIs2005JP1;
    int mIs2005HT2;
    int mIs2005JP2;
    int mIs2005ADJ;
    int mIs2005JPSI;
    int mIs2006JP0[6];
    int mIs2006HT2[6];
    int mIs2006JP1[6];
    int mIs2006JPSI[6];
    int mIs2006HTTP[6];

    int HT1_ID_2003;
    int HT2_ID_2003;
    int HT1_ID_2004;
    int HT2_ID_2004;
    int JP1_ID_2004;
    int JP2_ID_2004;
    int HT1_ID_2005;
    int HT2_ID_2005;
    int JP1_ID_2005;
    int JP2_ID_2005;
    int ADJ_ID_2005;
    int HT2_ID_2006[6];
    int JP0_ID_2006[6];
    int JP1_ID_2006[6];

    int HT1_DSM_2003;
    int HT2_DSM_2003;
    int HT1_DSM_2004;
    int HT2_DSM_2004;
    int JP1_DSM_2004;
    int JP2_DSM_2004;
    int HT1_DSM_2005;
    int HT2_DSM_2005;
    int JP1_DSM_2005;
    int JP2_DSM_2005;
    int ADJ_DSM_2005;
    int HT2_DSM_2006[6];
    int JP0_DSM_2006[6];
    int JP1_DSM_2006[6];
    int BETOT_DSM_2006;

    int numHT1_2005;
    int numHT2_2005;
    int numJP1_2005;
    int numJP2_2005;
    int numADJ_2005;
    int HT1_2005_array[kNTowers];
    int HT2_2005_array[kNTowers];
    int JP1_2005_array[kNJet];
    int JP2_2005_array[kNJet];
    int ADJ_2005_array[kNJet];
    int JPSI_2005_ADC[kNJet];
    int JPSI_2005_ID[kNJet];
    int numHT2_2006[6];
    int numJP0_2006[6];
    int numJP1_2006[6];
    int HT2_2006_array[6][kNTowers];
    int JP0_2006_array[6][kNJet];
    int JP1_2006_array[6][kNJet];
    int JPSI_2006_ADC[6][kNJet];
    int JPSI_2006_ID[6][kNJet];
    int numHTTP_2006[6];
    int HTTP_2006_arrayTP[6][kNPatches];
    int HTTP_2006_arrayHT[6][kNPatches];
    int HTTP_2006_arrayTP_ADC[6][kNPatches];
    int HTTP_2006_arrayHT_ADC[6][kNPatches];
    int BL1_2006_arrayADC[kNJet/2];

protected:
public:
    StEmcTriggerMaker(const char *name="bemctrigger");
    virtual           ~StEmcTriggerMaker();

    virtual Int_t     Init();
    virtual Int_t     Make();
    virtual Int_t     Finish();

    void              fillStEvent(StEvent*);
    void              saveHistograms(char*);
    // void              setSaveStEvent(bool a) {mSaveStEvent = a;}
    void              set2006Trigger(int);

    ///1==Yes,0==No,-1==Don't Know.  Same convention holds for other methods where appropriate.
    int             isTrigger(int trigId);
    ///Tower ADC > threshold required to fire trigger.
    int             barrelTowerThreshold(int trigId, int softId=1);
    ///Trigger Patch ADC > threshold required to fire trigger.
    int             barrelTriggerPatchThreshold(int trigId, int patchId=1);
    ///Jet Patch ADC > threshold required to fire trigger.
    int             barrelJetPatchThreshold(int trigId, int patchId=1);
    ///Note: DO NOT use ordering of HT and TP to associate HT->TP for BHTTP trigger
    ///Use ::barrelTriggerPatchForTower to find TP associated with tower

    ///map contains (key,value) = (softId,ADC) of all towers above DSM threshold.  map is empty if threshold = 0,-1.
    map<int,int>    barrelTowersAboveThreshold(int trigId);
    ///map contains (key,value) = (patchId,ADC) of all TP above DSM threshold.  map is empty if threshold = 0,-1.
    map<int,int>    barrelTriggerPatchesAboveThreshold(int trigId);
    ///map contains (key,value) = (patchId,ADC) of all JP above DSM threshold.  map is empty if threshold = 0,-1.
    map<int,int>    barrelJetPatchesAboveThreshold(int trigId);
    
    int             barrelTriggerPatchForTower(int softId);

    int             endcapTowerThreshold(int trigId);
    int             endcapTriggerPatchThreshold(int trigId);
    int             endcapJetPatchThreshold(int trigId);

    map<int,int>    endcapTowersAboveThreshold(int trigId);
    map<int,int>    endcapTriggerPatchesAboveThreshold(int trigId);
    map<int,int>    endcapJetPatchesAboveThreshold(int trigId);

    ///DSM ADC threshold for ETOT trigger.  ADC > threshold required to fire trigger.
    int             totalEnergyThreshold(int trigId);
    ///ADC calculated for ETOT trigger.
    int             totalEnergy();


    int               is2003HT1() {return mIs2003HT1;}//1=true,0=false,-1=problem
    int               is2003HT2() {return mIs2003HT2;}
    int               is2004HT1() {return mIs2004HT1;}
    int               is2004HT2() {return mIs2004HT2;}
    int               is2004JP1() {return mIs2004JP1;}
    int               is2004JP2() {return mIs2004JP2;}
    int               is2005HT1() {return mIs2005HT1;}
    int               is2005HT2() {return mIs2005HT2;}
    int               is2005JP1() {return mIs2005JP1;}
    int               is2005JP2() {return mIs2005JP2;}
    int               is2005ADJ() {return mIs2005ADJ;}
    int               is2005JPSI() {return mIs2005JPSI;}

    int               get2003HT1_ID() {return HT1_ID_2003;}//tower/JP id of trigger
    int               get2003HT2_ID() {return HT2_ID_2003;}
    int               get2004HT1_ID() {return HT1_ID_2004;}
    int               get2004HT2_ID() {return HT2_ID_2004;}
    int               get2004JP1_ID() {return JP1_ID_2004;}
    int               get2004JP2_ID() {return JP2_ID_2004;}
    int               get2005HT1_ID() {return HT1_ID_2005;}
    int               get2005HT2_ID() {return HT2_ID_2005;}
    int               get2005JP1_ID() {return JP1_ID_2005;}
    int               get2005JP2_ID() {return JP2_ID_2005;}
    int               get2005ADJ_ID() {return ADJ_ID_2005;}

    int               get2003HT1_ADC() {return HT1_DSM_2003;}//6 bit DSM ADC
    int               get2003HT2_ADC() {return HT2_DSM_2003;}
    int               get2004HT1_ADC() {return HT1_DSM_2004;}
    int               get2004HT2_ADC() {return HT2_DSM_2004;}
    int               get2004JP1_ADC() {return JP1_DSM_2004;}
    int               get2004JP2_ADC() {return JP2_DSM_2004;}
    int               get2005HT1_ADC() {return HT1_DSM_2005;}
    int               get2005HT2_ADC() {return HT2_DSM_2005;}
    int               get2005JP1_ADC() {return JP1_DSM_2005;}
    int               get2005JP2_ADC() {return JP2_DSM_2005;}
    int               get2005ADJ_ADC() {return ADJ_DSM_2005;}

    void              get2005HT1_TOWS(int, int*);//array of tow ids passing HT1_2005 trig
    void              get2005HT2_TOWS(int, int*);//array of tow ids passing HT2_2005 trig
    int               get2005HT1_NTOWS() {return numHT1_2005;}//# tows passing HT1_2005 trig
    int               get2005HT2_NTOWS() {return numHT2_2005;}//# tows passing HT2_2005 trig

    void              get2005JP1_PATCHES(int, int*);//array of patches passing JP1_2005
    void              get2005JP2_PATCHES(int, int*);//array of patches passing JP2_2005
    int               get2005JP1_NPATCHES() {return numJP1_2005;}//# patches passing JP1_2005
    int               get2005JP2_NPATCHES() {return numJP2_2005;}//# patches passing JP2_2005

    void              get2005JPSI_ADC(int, int*);//array of HT adc for each JP
    void              get2005JPSI_ID(int, int*);//array of HT id for each JP
    void              get2006BL1_ADC(int, int*);//array of DSM_L1 ADC values


    StBemcTrigger*    getTrigger()
    {
      return mBemcTrigger;
    }
    
    void              setDbMaker(St_db_Maker *dbMk)
      {
        //mBemcTrigger->setDbMaker(dbMk);
	//This has been disabled so as to use StBemcTables but 
	//allow old code not to break
      }
    
    StBemcTables*     tables;
    
    void              setTableMaker(StBemcTables *bemcTab)
      {
        mBemcTrigger->setTableMaker(bemcTab);
      }

    int               trigPatch[300];
    
    ClassDef(StEmcTriggerMaker,0)
      };
#endif
