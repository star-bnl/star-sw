//
// $Id: StBemcTrigger.h,v 1.10 2007/01/22 19:13:43 kocolosk Exp $
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

class StEmcGeom;
class StEmcDecoder;
class StEvent;
class St_db_Maker;
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
    St_db_Maker*   mDbMaker;
    StBemcTables*  mTables;
    emcTrigger     mTrigger;

    int get2003Trigger();
    int get2004Trigger();
    int get2005Trigger();
    int get2006Trigger();
    void PatchMap();

    int mIsTrig[16];//1==true,0==false
    int mTowJetId[13];//JP_ID/HT_ID of trigger
    int mDsmAdc[14];//DSM ADC of trigger
    int mnumHT[7];//#towers in trigger
    int mnumJP[7];//#patches in trigger
    int mnumHTTP[1];//#HT+TP which fullf HTTP trigger
    int mHT12005array[kNTowers];//array of towers which pass trigger
    int mJP12005array[kNJet];//array of JP's which pass trigger
    int mHT22005array[kNTowers];//array of towers which pass trigger
    int mJP22005array[kNJet];//array of JP's which pass trigger
    int mJPSI2005adc[kNJet];//array of adc of HT in each JP
    int mJPSI2005id[kNJet];//array of id of HT in each JP
    int mJP12006array[kNJet];//array of JP's which pass trigger
    int mHT22006array[kNTowers];//array of towers which pass trigger
    int mJP22006array[kNJet];//array of JP's which pass trigger
    int mJPSI2006adc[kNJet];//array of adc of HT in each JP
    int mJPSI2006id[kNJet];//array of id of HT in each JP
    int mHTTP2006arrayHT[kNPatches];
    int mHTTP2006arrayHTADC[kNPatches];
    int mHTTP2006arrayTP[kNPatches];
    int mHTTP2006arrayTPADC[kNPatches];

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
    int mIs2006BHTTP;

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
    int JP_TP[12][25];
    int BETOT_DSM_2006;

    int numHT1_2005;
    int numHT2_2005;
    int numJP1_2005;
    int numJP2_2005;
    int numADJ_2005;
    int numHT2_2006;
    int numJP1_2006;
    int numJP2_2006;
    int numHTTP_2006;

    int HT1_2005_array[kNTowers];
    int HT2_2005_array[kNTowers];
    int JP1_2005_array[kNJet];
    int JP2_2005_array[kNJet];
    int JPSI_2005_ADC[kNJet];
    int JPSI_2005_ID[kNJet];
    int HT2_2006_array[kNTowers];
    int JP1_2006_array[kNJet];
    int JP2_2006_array[kNJet];
    int JPSI_2006_ADC[kNJet];
    int JPSI_2006_ID[kNJet];
    int BHTTP_2006_HT[kNPatches];
    int BHTTP_2006_HT_ADC[kNPatches];
    int BHTTP_2006_TP[kNPatches];
    int BHTTP_2006_TP_ADC[kNPatches];

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
		LOG_INFO << "::setPrint() is obsolete.  Use logger config file to set verbosity instead." << endm;
    }///< Obsolete function; users can control messages with logger config file.
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
    int *        getNHTTP()
      {
	return mnumHTTP;
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
    int*          getHT22006array()
      {
	return mHT22006array;
      }
    int*         getJP12006array()
      {
	return mJP12006array;
      }
    int*         getJP22006array()
      {
	return mJP22006array;
      }
    int*         getJPSI2006adc()
      {
	return mJPSI2006adc;
      }
    int*         getJPSI2006id()
      {
	return mJPSI2006id;
      }
    int*         getHTTP2006arrayHT()
      {
	return mHTTP2006arrayHT;
      }
    int*         getHTTP2006arrayHTADC()
      {
	return mHTTP2006arrayHTADC;
      }
    int*         getHTTP2006arrayTP()
      {
	return mHTTP2006arrayTP;
      }
    int*         getHTTP2006arrayTPADC()
      {
	return mHTTP2006arrayTPADC;
      }

    emcTrigger     getTrigger()
    {
        return mTrigger;
    }

    int            trgPatch[300];//just for testing purposes!

    ClassDef(StBemcTrigger, 1)
};

#endif


