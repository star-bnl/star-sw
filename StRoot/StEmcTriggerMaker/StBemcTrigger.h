//
// $Id: StBemcTrigger.h,v 1.5 2005/09/01 19:00:20 suaide Exp $
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

    short    HT[kNPatches];
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

    int mIsTrig[10];//1==true,0==false
    int mTowJetId[10];//JP_ID/HT_ID of trigger
    int mDsmAdc[10];//DSM ADC of trigger

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


    emcTrigger     getTrigger()
    {
        return mTrigger;
    }

    int            trgPatch[300];//just for testing purposes!

    ClassDef(StBemcTrigger, 1)
};

#endif


