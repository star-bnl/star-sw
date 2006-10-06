//
// $Id: StBemcTrigger.h,v 1.8 2006/10/06 19:48:53 rfatemi Exp $
//
//

#ifndef StMaker_H
#include "StMaker.h"
#endif

#ifndef STAR_StBemcTrigger
#define STAR_StBemcTrigger
#include "TObject.h"

#define kNPatches 300
#define kNJet 12
#define kNTowers 4800
#define k12bits 4096

class StEmcGeom;
class StEmcDecoder;
class StEvent;
class St_db_Maker;
class StBemcTables;

struct emcTrigger
{
  short    TowerPedestal[kNTowers];      /* single tower pedestals used for trigger (ID -1)*/
  short    TowerStatus[kNTowers];        /* single tower masks used for trigger  (ID -1)*/
  
  short    HTBits;                       /* High tower bits selection */
  short    PatchStatus[kNPatches];       /* trigger tower status */
  short    PatchLUT[kNPatches][k12bits]; /* Patch LUT */
  
  short    HT[kNPatches];/* HT adc */
  short    HTID[kNPatches];/* HT id */
  short    Patch[kNPatches];
  short    Jet[kNJet];
  short    Et;
};

class StBemcTrigger: public TObject
{
private:

    StEmcGeom*     mGeo;
    StEmcDecoder*  mDecoder;
    StEvent*       mEvent;
    St_db_Maker*   mDbMaker;
    StBemcTables*  mTables;
    emcTrigger     mTrigger;
    bool           mPrint;

    char           mHighTower[kNPatches];
    char           mPatch[kNPatches];
    char           mJetPatch[kNJet];
    char           mEt;

    int get2003Trigger();
    int get2004Trigger();
    int get2005Trigger();
    void PatchMap();

    int mIsTrig[11];//1==true,0==false
    int mTowJetId[10];//JP_ID/HT_ID of trigger
    int mDsmAdc[10];//DSM ADC of trigger
    int mnumHT[5];//#towers in trigger
    int mnumJP[5];//#patches in trigger
    int mHT12005array[kNTowers];//array of towers which pass trigger
    int mJP12005array[kNJet];//array of JP's which pass trigger
    int mHT22005array[kNTowers];//array of towers which pass trigger
    int mJP22005array[kNJet];//array of JP's which pass trigger
    int mJPSI2005adc[kNJet];//array of adc of HT in each JP
    int mJPSI2005id[kNJet];//array of id of HT in each JP

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
    int JP_TP[12][25];

    int numHT1_2005;
    int numHT2_2005;
    int numJP1_2005;
    int numJP2_2005;
    int numADJ_2005;

    int HT1_2005_array[kNTowers];
    int HT2_2005_array[kNTowers];
    int JP1_2005_array[kNJet];
    int JP2_2005_array[kNJet];

    int JPSI_2005_ADC[kNJet];
    int JPSI_2005_ID[kNJet];

public:
    StBemcTrigger();
    virtual        ~StBemcTrigger();

    int            makeTrigger();
    void           zero();
    void           resetConf();
    void           setEvent(StEvent* e)
    {
        mEvent = e;
    }
    void           setPrint(bool a)
    {
        mPrint = a;
    }
    void           setDbMaker(St_db_Maker* dbMaker)
    {
        mDbMaker = dbMaker;
    }
    void           setTableMaker(StBemcTables *tab)
    {
        mTables =tab;
    }

    //1==true, 0==false, -1==problems
    int*          isTrigEvent()
      {
        return mIsTrig;
      }
    //return JPID/towID -1==problems
    int*           getTowPatchId()
      {
        return mTowJetId;
      }
    //DSM 6bit ADC
    int*           getTowPatchDSM()
      {
        return mDsmAdc;
      }
    int*          getNHT()
      {
	return mnumHT;
      }
    int*         getNJP()
      {
	return mnumJP;
      }
    int*          getHT12005array()
      {
	return mHT12005array;
      }
   int*          getHT22005array()
      {
	return mHT22005array;
      }
    int*         getJP12005array()
      {
	return mJP12005array;
      }
    int*         getJP22005array()
      {
	return mJP22005array;
      }
    int*         getJPSI2005adc()
      {
	return mJPSI2005adc;
      }
    int*         getJPSI2005id()
      {
	return mJPSI2005id;

      }


    emcTrigger     getTrigger()
    {
        return mTrigger;
    }

    int            trgPatch[300];//just for testing purposes!

    ClassDef(StBemcTrigger, 1)
};

#endif


