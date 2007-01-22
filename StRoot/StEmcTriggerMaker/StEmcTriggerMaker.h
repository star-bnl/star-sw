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

#define kNPatches 300
#define kNJet 12
#define kNTowers 4800
#define k12bits 4096

class StEvent;

class StEmcTriggerMaker : public StMaker
{
private:
    StBemcTrigger*    mBemcTrigger;
    bool              mSaveStEvent;

    int               isTrig[16];
    int               TowJetId[13];
    int               DsmAdc[14];
    int               numHT[7];//# of HT which pass HT trigger
    int               numJP[7];//# of JP which pass JP trigger
    int               numHTTP[7];//# of HT-TP which pass HTTP trigger
    int               HT12005array[kNTowers];//array of towers which pass trigger
    int               HT22005array[kNTowers];//array of towers which pass trigger
    int               JP12005array[kNJet];//array of JP's which pass trigger
    int               JP22005array[kNJet];//array of JP's which pass trigger
    int               JPSI2005adc[kNJet];//array of adc for highest tower in JP
    int               JPSI2005id[kNJet];//array of id for highest tower in JP
    int               HT12006array[kNTowers];//array of towers which pass trigger
    int               HT22006array[kNTowers];//array of towers which pass trigger
    int               JP12006array[kNJet];//array of JP's which pass trigger
    int               JP22006array[kNJet];//array of JP's which pass trigger
    int               JPSI2006id[kNJet];//array of id for highest tower in JP
    int               HTTP2006arrayHT[kNPatches];//array of HT which are part of a HTTP trigger
    int               HTTP2006arrayTP[kNPatches];//array of TP which are part of a HTTP trigger
    int               HTTP2006arrayHTADC[kNPatches];//array of HT ADC which are part of a HTTP trigger
    int               HTTP2006arrayTPADC[kNPatches];//array of TP ADC which are part of a HTTP trigger


    int mIs2003HT1;
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
    int mIs2006JP1;
    int mIs2006HT2;
    int mIs2006JP2;
    int mIs2006JPSI;
    int mIs2006HTTP;

    int HT1_ID_2003;
    int HT1_ID_2004;
    int HT2_ID_2004;
    int JP1_ID_2004;
    int JP2_ID_2004;
    int HT1_ID_2005;
    int HT2_ID_2005;
    int JP1_ID_2005;
    int JP2_ID_2005;
    int ADJ_ID_2005;
    int HT2_ID_2006;
    int JP1_ID_2006;
    int JP2_ID_2006;

    int HT1_DSM_2003;
    int HT1_DSM_2004;
    int HT2_DSM_2004;
    int JP1_DSM_2004;
    int JP2_DSM_2004;
    int HT1_DSM_2005;
    int HT2_DSM_2005;
    int JP1_DSM_2005;
    int JP2_DSM_2005;
    int ADJ_DSM_2005;
    int HT2_DSM_2006;
    int JP1_DSM_2006;
    int JP2_DSM_2006;
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
    int numHT2_2006;
    int numJP1_2006;
    int numJP2_2006;
    int HT2_2006_array[kNTowers];
    int JP1_2006_array[kNJet];
    int JP2_2006_array[kNJet];
    int JPSI_2006_ADC[kNJet];
    int JPSI_2006_ID[kNJet];
    int numHTTP_2006;
    int HTTP_2006_arrayTP[kNPatches];
    int HTTP_2006_arrayHT[kNPatches];
    int HTTP_2006_arrayTP_ADC[kNPatches];
    int HTTP_2006_arrayHT_ADC[kNPatches];

protected:
public:
    StEmcTriggerMaker(const char *name="bemctrigger");
    virtual           ~StEmcTriggerMaker();

    virtual Int_t     Init();
    virtual Int_t     Make();
    virtual Int_t     Finish();

    void              fillStEvent(StEvent*);
    //void              fillHistograms(StEvent*);
    void              saveHistograms(char*);
    void              setSaveStEvent(bool a) {mSaveStEvent = a;}
    void              setPrint(bool a) { LOG_INFO << "::setPrint() is obsolete.  Use logger config file to set verbosity instead." << endm; }///< Obsolete function; users can control messages with logger config file.

    int               is2003HT1() {return mIs2003HT1;}//1=true,0=false,-1=problem
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
    int               is2006HT2() {return mIs2006HT2;}
    int               is2006JP1() {return mIs2006JP1;}
    int               is2006JP2() {return mIs2006JP2;}
    int               is2006JPSI() {return mIs2006JPSI;}
    int               is2006HTTP() {return mIs2006HTTP;}

    int               get2003HT1_ID() {return HT1_ID_2003;}//tower/JP id of trigger
    int               get2004HT1_ID() {return HT1_ID_2004;}
    int               get2004HT2_ID() {return HT2_ID_2004;}
    int               get2004JP1_ID() {return JP1_ID_2004;}
    int               get2004JP2_ID() {return JP2_ID_2004;}
    int               get2005HT1_ID() {return HT1_ID_2005;}
    int               get2005HT2_ID() {return HT2_ID_2005;}
    int               get2005JP1_ID() {return JP1_ID_2005;}
    int               get2005JP2_ID() {return JP2_ID_2005;}
    int               get2005ADJ_ID() {return ADJ_ID_2005;}
    int               get2006HT2_ID() {return HT2_ID_2006;}
    int               get2006JP1_ID() {return JP1_ID_2006;}
    int               get2006JP2_ID() {return JP2_ID_2006;}

    int               get2003HT1_ADC() {return HT1_DSM_2003;}//6 bit DSM ADC
    int               get2004HT1_ADC() {return HT1_DSM_2004;}
    int               get2004HT2_ADC() {return HT2_DSM_2004;}
    int               get2004JP1_ADC() {return JP1_DSM_2004;}
    int               get2004JP2_ADC() {return JP2_DSM_2004;}
    int               get2005HT1_ADC() {return HT1_DSM_2005;}
    int               get2005HT2_ADC() {return HT2_DSM_2005;}
    int               get2005JP1_ADC() {return JP1_DSM_2005;}
    int               get2005JP2_ADC() {return JP2_DSM_2005;}
    int               get2005ADJ_ADC() {return ADJ_DSM_2005;}
    int               get2006HT2_ADC() {return HT2_DSM_2006;}
    int               get2006JP1_ADC() {return JP1_DSM_2006;}
    int               get2006JP2_ADC() {return JP2_DSM_2006;}
    int               get2006BETOT_ADC() {return BETOT_DSM_2006;}

    void              get2005HT1_TOWS(int, int*);//array of tow ids passing HT1_2005 trig
    void              get2005HT2_TOWS(int, int*);//array of tow ids passing HT2_2005 trig
    int               get2005HT1_NTOWS() {return numHT1_2005;}//# tows passing HT1_2005 trig
    int               get2005HT2_NTOWS() {return numHT2_2005;}//# tows passing HT2_2005 trig
    void              get2006HT2_TOWS(int, int*);//array of tow ids passing HT2_2006 trig
    int               get2006HT2_NTOWS() {return numHT2_2006;}//# tows passing HT2_2006 trig

    void              get2005JP1_PATCHES(int, int*);//array of patches passing JP1_2005
    void              get2005JP2_PATCHES(int, int*);//array of patches passing JP2_2005
    int               get2005JP1_NPATCHES() {return numJP1_2005;}//# patches passing JP1_2005
    int               get2005JP2_NPATCHES() {return numJP2_2005;}//# patches passing JP2_2005
    void              get2006JP1_PATCHES(int, int*);//array of patches passing JP1_2006
    void              get2006JP2_PATCHES(int, int*);//array of patches passing JP2_2006
    int               get2006JP1_NPATCHES() {return numJP1_2006;}//# patches passing JP1_2006
    int               get2006JP2_NPATCHES() {return numJP2_2006;}//# patches passing JP2_2006

    void              get2005JPSI_ADC(int, int*);//array of HT adc for each JP
    void              get2005JPSI_ID(int, int*);//array of HT id for each JP
    void              get2006JPSI_ADC(int, int*);//array of HT adc for each JP
    void              get2006JPSI_ID(int, int*);//array of HT id for each JP
    void              get2006HTTP_HT(int, int*);//array of HT for each HTTP
    void              get2006HTTP_TP(int, int*);//array of TP for each HTTP
    void              get2006HTTP_HT_ADC(int, int*);//array of HT ADC for each HTTP
    void              get2006HTTP_TP_ADC(int, int*);//array of TP ADC  for each HTTP
    int               get2006HTTP_NTP() {return numHTTP_2006;}//# HTTP passing trigger

    StBemcTrigger*    getTrigger()
    {
        return mBemcTrigger;
    }

    void              setDbMaker(St_db_Maker *dbMk)
    {
        mBemcTrigger->setDbMaker(dbMk);
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
