//
// $Id: StBemcTrigger.h,v 1.16 2007/05/14 16:23:44 rfatemi Exp $
//
//

#ifndef StMaker_H
#include "StMaker.h"
#endif

#ifndef STAR_StBemcTrigger
#define STAR_StBemcTrigger
#include "TObject.h"

#include "StMessMgr.h"

#define kNPatches 300
#define kNJet 12
#define kNTowers 4800
#define k12bits 4096
#define kN_sequences 25 //# TP inside a JP as defined by StEmcDecoder

class StEmcGeom;
class StEmcDecoder;
class StEvent;
class StBemcTables;

struct emcTrigger
{

  short    HTBits;                       /* High tower bits selection */
  short    PatchStatus[kNPatches];       /* online trigger patch masks */
  short    TowerStatus[kNTowers];        /* online single tower masks */
  short    HT[kNPatches];                /* trigger patch HT adc */
  short    HTID[kNPatches];              /* trigger patch HT id */
  short    Patch[kNPatches];             /* trigger patch adc */
  short    Jet[kNJet];                   /* jet patch adc */
  short    Et;                           /* total Et */
};

class StBemcTrigger: public TObject
{
private:

    StEmcGeom*     mGeo;
    StEmcDecoder*  mDecoder;
    StEvent*       mEvent;
    StBemcTables*  mTables;
    emcTrigger     mTrigger;

    int get2003Trigger();
    int get2004Trigger();
    int get2005Trigger();
    int get2006Trigger();
    void PatchMap();

    int mIsTrig[50];//1==true,0==false
    int mTowJetId[50];//JP_ID/HT_ID of trigger
    int mDsmAdc[50];//DSM ADC of trigger
    int mnumHT[12];//#towers in trigger
    int mnumJP[12];//#patches in trigger
    int mnumHTTP[6];//#HT+TP which fullf HTTP trigger
    int mHT12005array[kNTowers];//array of towers which pass trigger
    int mJP12005array[kNJet];//array of JP's which pass trigger
    int mHT22005array[kNTowers];//array of towers which pass trigger
    int mJP22005array[kNJet];//array of JP's which pass trigger
    int mJPSI2005adc[kNJet];//array of adc of HT in each JP
    int mJPSI2005id[kNJet];//array of id of HT in each JP
    int mHT22006array[6][kNTowers];//array of towers which pass trigger
    int mJP02006array[6][kNJet];//array of JP's which pass trigger
    int mJP12006array[6][kNJet];//array of JP's which pass trigger
    int mJPSI2006adc[6][kNJet];//array of adc of HT in each JP
    int mJPSI2006id[6][kNJet];//array of id of HT in each JP
    int mHTTP2006arrayHT[6][kNPatches];
    int mHTTP2006arrayHTADC[6][kNPatches];
    int mHTTP2006arrayTP[6][kNPatches];
    int mHTTP2006arrayTPADC[6][kNPatches];
    int mBL12006arrayADC[kNJet];

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
    int mIs2006HT2[6];
    int mIs2006JP0[6];
    int mIs2006JP1[6];
    int mIs2006JPSI[6];
    int mIs2006BHTTP[6];

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
    int JP_TP[12][25];
    int BETOT_DSM_2006;

    int numHT1_2005;
    int numHT2_2005;
    int numJP1_2005;
    int numJP2_2005;
    int numADJ_2005;
    int numHT2_2006[6];
    int numJP0_2006[6];
    int numJP1_2006[6];
    int numHTTP_2006[6];

    int HT1_2005_array[kNTowers];
    int HT2_2005_array[kNTowers];
    int JP1_2005_array[kNJet];
    int JP2_2005_array[kNJet];
    int JPSI_2005_ADC[kNJet];
    int JPSI_2005_ID[kNJet];
    int HT2_2006_array[6][kNTowers];
    int JP0_2006_array[6][kNJet];
    int JP1_2006_array[6][kNJet];
    int JPSI_2006_ADC[6][kNJet];
    int JPSI_2006_ID[6][kNJet];
    int BHTTP_2006_HT[6][kNPatches];
    int BHTTP_2006_HT_ADC[6][kNPatches];
    int BHTTP_2006_TP[6][kNPatches];
    int BHTTP_2006_TP_ADC[6][kNPatches];
    int BL1_ADC_2006[kNJet/2];

public:
       

    StBemcTrigger();
    virtual        ~StBemcTrigger();

    int     makeTrigger();
    void    zero();
    void    resetConf();
    void    setEvent(StEvent* e) { mEvent = e; }
    void    setPrint(bool a) {
                LOG_INFO << "::setPrint() is obsolete.  Use logger config file to set verbosity instead." << endm;
                    }///< Obsolete function; users can control messages with logger config file.
    void    setTableMaker(StBemcTables *tab) { mTables =tab; }
   
    //1==true, 0==false, -1==problems
    int*    isTrigEvent() { return mIsTrig; }
    //return JPID/towID -1==problems
    int*    getTowPatchId() { return mTowJetId; }
    //DSM 6bit ADC
    int*    getTowPatchDSM() { return mDsmAdc; }
    int*    getNHT() { return mnumHT; }
    int*    getNJP() { return mnumJP; }
    int*    getNHTTP() { return numHTTP_2006; }
    int*    getHT12005array() { return mHT12005array; }
    int*    getHT22005array() {	return mHT22005array; }
    int*    getJP12005array(){ return mJP12005array; }
    int*    getJP22005array() { return mJP22005array; }
    int*    getJPSI2005adc() { return mJPSI2005adc; }
    int*    getJPSI2005id() { return mJPSI2005id; }
    int*    getBL12006arrayADC() { return mBL12006arrayADC; }
    int    getHT22006array(int i,int j) { return mHT22006array[i][j]; }
    int    getJP02006array(int i,int j) { return mJP02006array[i][j]; }
    int    getJP12006array(int i,int j) { return mJP12006array[i][j]; }
    int    getJPSI2006adc(int i,int j) { return mJPSI2006adc[i][j]; }
    int    getJPSI2006id(int i,int j) { return mJPSI2006id[i][j]; }
    int    getHTTP2006arrayHT(int i, int j) { return mHTTP2006arrayHT[i][j]; }
    int    getHTTP2006arrayHTADC(int i, int j) { return mHTTP2006arrayHTADC[i][j]; }
    int    getHTTP2006arrayTP(int i, int j) { return mHTTP2006arrayTP[i][j]; }
    int    getHTTP2006arrayTPADC(int i, int j) { return mHTTP2006arrayTPADC[i][j]; }

    emcTrigger     getTrigger() { return mTrigger; }
    const StEmcDecoder* decoder() const { return mDecoder; }

    int     trgPatch[300];//just for testing purposes!

    ClassDef(StBemcTrigger, 1)
};

#endif


