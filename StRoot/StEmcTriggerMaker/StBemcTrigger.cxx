///
// $Id: StBemcTrigger.cxx,v 1.16 2006/04/05 18:37:42 rfatemi Exp $
//
//

#include "StBemcTrigger.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StarRoot/TUnixTime.h"
#include "StDaqLib/EMC/StEmcDecoder.h"
#include "Stiostream.h"
#include "St_db_Maker/St_db_Maker.h"
#include "tables/St_emcPed_Table.h"
#include "StEmcUtil/database/StBemcTables.h"
ClassImp(StBemcTrigger);

//-------------------------------------------------------------------
StBemcTrigger::StBemcTrigger():TObject()
{
    mGeo=StEmcGeom::getEmcGeom("bemc");
    mEvent = NULL;
    mDecoder = NULL;
    mPrint = true;
    resetConf();
}
//----------------------------------------------------
StBemcTrigger::~StBemcTrigger()
{}

//----------------------------------------------------
void StBemcTrigger::resetConf(){

    PatchMap();

    mTrigger.HTBits = 3;

    for(int i = 0;i<kNTowers;i++)
    {
        mTrigger.TowerPedestal[i] = 0;
        mTrigger.TowerStatus[i] = 1;
    }


    for(int i = 0;i<kNPatches;i++)
    {
        mTrigger.PatchStatus[i] = 1;
    }

    mIs2003HT1=-1;
    mIs2004HT1=-1;
    mIs2004JP1=-1;
    mIs2004HT2=-1;
    mIs2004JP2=-1;
    mIs2005HT1=-1;
    mIs2005JP1=-1;
    mIs2005HT2=-1;
    mIs2005JP2=-1;
    mIs2005ADJ=-1;
    for (int i=0;i<10;i++)
    {
        mIsTrig[i]=-1;
    }

    HT1_ID_2003=-1;
    HT1_ID_2004=-1;
    HT2_ID_2004=-1;
    JP1_ID_2004=-1;
    JP2_ID_2004=-1;
    HT1_ID_2005=-1;
    HT2_ID_2005=-1;
    JP1_ID_2005=-1;
    JP2_ID_2005=-1;
    ADJ_ID_2005=-1;
    for (int i=0;i<10;i++)
    {
        mTowJetId[i] = -1;
    }

    HT1_DSM_2003=-1;
    HT1_DSM_2004=-1;
    HT2_DSM_2004=-1;
    JP1_DSM_2004=-1;
    JP2_DSM_2004=-1;
    HT1_DSM_2005=-1;
    HT2_DSM_2005=-1;
    JP1_DSM_2005=-1;
    JP2_DSM_2005=-1;
    ADJ_DSM_2005=-1;
    for (int i=0;i<10;i++)
    {
        mDsmAdc[i] = -1;
    }

}

//----------------------------------------------------
void StBemcTrigger::zero()
{
    for(int i=0;i<kNPatches;i++)
    {
        mTrigger.HT[i] = 0;
        mTrigger.Patch[i]= 0;
    }

    for(int i=0;i<kNJet;i++)
    {
        mTrigger.Jet[i]= 0;
    }

    mTrigger.Et = 0;

}


void StBemcTrigger::PatchMap()
{

    for (int i=0;i<20;i++)
    {
        JP_TP[0][i]=i+30;
        JP_TP[1][i]=i+50;
        JP_TP[2][i]=i+80;
        JP_TP[3][i]=i+100;
        JP_TP[4][i]=i+130;
        JP_TP[5][i]=i;
        if (i<10)
            JP_TP[6][i]=i+150;
        if (i>=10)
            JP_TP[6][i]=i+290-10;
        JP_TP[7][i]=i+170;
        JP_TP[8][i]=i+190;
        JP_TP[9][i]=i+220;
        JP_TP[10][i]=i+240;
        JP_TP[11][i]=i+270;
    }

    for (int i=20;i<25;i++)
    {
        int j=i-20;
        int keven=2*j;
        int kodd=2*j+1;
        JP_TP[0][i]=20+keven;
        JP_TP[1][i]=70+kodd;
        JP_TP[2][i]=70+keven;
        JP_TP[3][i]=120+kodd;
        JP_TP[4][i]=120+keven;
        JP_TP[5][i]=20+kodd;
        JP_TP[6][i]=160+kodd;
        JP_TP[7][i]=160+keven;
        JP_TP[8][i]=210+kodd;
        JP_TP[9][i]=210+keven;
        JP_TP[10][i]=260+kodd;
        JP_TP[11][i]=260+keven;
    }

    for (int i=0;i<12;i++)
    {
        cout<<"JP"<<i<<" is Sum of TP = ";
        for (int j=0;j<25;j++)
        {
            if (j!=24)
                cout<<JP_TP[i][j]<<"+";
            if (j==24)
                cout<<JP_TP[i][j]<<endl;
        }
    }


    /****************************************************************************
       http://www.nikhef.nl/~ogrebeny/emc/files/Towers%20Layout.xls documents
       the tower->TP->JP correpsondence. Using StEmcDecoder,StEmcGeom and 
       the above spreadsheet I defined my own JP#s and assigned correct TP's above.
       Unfortunately this was done before code was written to assoicate JP -> TP -> TOW

       JP      TOWERS                STAR PHI
       ---     --------          -----------------------------------------
       0        1-100,2101-2400           90
       1        101-500                   30
       2        501-900                  -30
       3        901-1300                 -90
       4        1301-1700               -150
       5        1701-2100                150
       6        2401-2500,4501-4800       
       7        2501-2900                 
       8        2901-3300                 
       9        3301-3700                 
       10       3701-4100                 
       11       4101-4500                                             
    **************************************************************************/

}


int StBemcTrigger::makeTrigger()
{

    get2003Trigger();
    get2004Trigger();
    get2005Trigger();


    //2003 HT == 1101
    mIsTrig[0]  =mIs2003HT1;
    mTowJetId[0]=HT1_ID_2003;
    mDsmAdc[0]  =HT1_DSM_2003;

    //2004 HT1 == 45201
    mIsTrig[1]  =mIs2004HT1;
    mTowJetId[1]=HT1_ID_2004;
    mDsmAdc[1]  =HT1_DSM_2004;

    //2004 HT2 == 45202
    mIsTrig[2]  =mIs2004HT2;
    mTowJetId[2]=HT2_ID_2004;
    mDsmAdc[2]  =HT2_DSM_2004;

    //2004 JP1 == 45206
    mIsTrig[3]  =mIs2004JP1;
    mTowJetId[3]=JP1_ID_2004;
    mDsmAdc[3]  =JP1_DSM_2004;

    //2004 JP2 == 45207
    mIsTrig[4]  =mIs2004JP2;
    mTowJetId[4]=JP2_ID_2004;
    mDsmAdc[4]  =JP2_DSM_2004;

    //2005 HT1 == 96201
    mIsTrig[5]  =mIs2005HT1;
    mTowJetId[5]=HT1_ID_2005;
    mDsmAdc[5]  =HT1_DSM_2005;

    //2005 HT2 == 96211
    mIsTrig[6]  =mIs2005HT2;
    mTowJetId[6]=HT2_ID_2005;
    mDsmAdc[6]  =HT2_DSM_2005;

    //2005 JP1 == 96221
    mIsTrig[7]  =mIs2005JP1;
    mTowJetId[7]=JP1_ID_2005;
    mDsmAdc[7]  =JP1_DSM_2005;

    //2005 JP2 == 96233
    mIsTrig[8]  =mIs2005JP2;
    mTowJetId[8]=JP2_ID_2005;
    mDsmAdc[8]  =JP2_DSM_2005;

    //2005 AJP == 96241
    mIsTrig[9]  =mIs2005ADJ;
    mTowJetId[9]=ADJ_ID_2005;
    mDsmAdc[9]  =ADJ_DSM_2005;

    for (int z=0;z<10;z++)
    {
        printf("Make i=%d, isTrig=%d, TowJetId=%d, DsmAdc=%d\n",z,mIsTrig[z],mTowJetId[z],mDsmAdc[z]);
    }

    return kStOK;
}


int StBemcTrigger::get2003Trigger()
{
    zero();

    const int HT1_TH_2003 = 8;

    if(!mEvent)
        return kStWarn;
    StEmcCollection *emc = mEvent->emcCollection();
    if(!emc)
        return kStWarn;


    TDataSet* db = 0;
    if(mDbMaker != 0)
        db = mDbMaker->GetDataBase("Calibrations/emc/y3bemc");
    else
    {
        if(mPrint)
            cout << "You *MUST* provide a pointer to St_db_Maker!" << endl
            << "In your macro say something like: myTrgMaker->setDbMaker(myDbMaker);"
            << endl;
        return kStWarn;
    }


    int adc12[kNTowers];
    int adc10[kNTowers];
    for(int i=0;i<kNTowers;i++)
    {
        adc12[i] = 0;
        adc10[i] = 0;
    }


    StEmcDetector* detector=emc->detector(kBarrelEmcTowerId);
    if(detector)
    {
        for(Int_t m=1;m<=120;m++)
        {
            StEmcModule* module = detector->module(m);
            if(module)
            {
                StSPtrVecEmcRawHit& rawHit=module->hits();
                for(UInt_t k=0;k<rawHit.size();k++)
                    if(rawHit[k])
                    {
                        Int_t did;
                        Int_t mod=rawHit[k]->module();
                        Int_t e=rawHit[k]->eta();
                        Int_t s=abs(rawHit[k]->sub());
                        mGeo->getId(mod,e,s,did);
                        if(mTrigger.TowerStatus[did-1]==1)
                        {
                            adc12[did-1]=rawHit[k]->adc();
                            adc10[did-1] = adc12[did-1]>>2;
                        }
                    }
            }
        }
    }

    else
    {
        return kStWarn;
    }



    TUnixTime unixTime(mEvent->time());
    Int_t dat=0,tim=0;
    unixTime.GetGTime(dat,tim);
    mDecoder = new StEmcDecoder(dat,tim);

    int HTmax=0;
    int HTmaxID=-1;
    for(int i = 0;i<kNPatches;i++)
        if(mTrigger.PatchStatus[i]==1)
        {
            int crate = 0;
            int seq  = 0;
            int HT = 0;//holder for highest tower in patch
            int HTID = -1;//id for highest tower in patch
            int id;//BEMC tower id

            mDecoder->GetCrateAndSequenceFromTriggerPatch(i,crate,seq);

            for(int j=seq;j<seq+16;j++)
            {
                int stat = mDecoder->GetTowerIdFromCrate(crate,j,id);
                if(stat==1)
                {
                    if(adc10[id-1]>=HT)
                    {
                        HT = adc10[id-1];
                        HTID = id;
                    }
                }
            }

            int SHIFT = mTrigger.HTBits;
            HT = HT >> SHIFT;   //shift by 3 bits -> now have 7 bits
            int HTL = HT & 0x1F;//strip off 5 LB
            int HTH = HT >> 5;//strip off top 2 HB
            int B5  = 0;
            if(HTH>0)
                B5 = 1; // IF top bits !=0 B5=1
            mTrigger.HT[i] = HTL+(B5<<5); // Or top bits
            if(mPrint)
                cout <<"Patch number "<<i<<" Tower id = "<<HTID
                <<" adc12 = "<<adc12[HTID-1]<<" adc10 = "<<adc10[HTID-1]
                <<" HT = "<<mTrigger.HT[i]<<endl;

            if (mTrigger.HT[i]>HTmax)
            {
                HTmax=mTrigger.HT[i];
                HTmaxID=HTID;
            }
        }

    if (HTmax>HT1_TH_2003)
    {
        mIs2003HT1=1;
        HT1_ID_2003=HTmaxID;
        HT1_DSM_2003=HTmax;
    }
    else
    {
        mIs2003HT1=0;
        HT1_ID_2003=HTmaxID;
        HT1_DSM_2003=HTmax;
    }

    delete mDecoder;
    return kStOK;
}


//----------------------------------------------------
int StBemcTrigger::get2004Trigger()
{
    zero();

    const int HT1_TH_2004 = 10;//bht0=10,bht1=20,bht2=30
    const int HT2_TH_2004 = 20;
    const int JP1_TH_2004 = 40;//bjp0=15,bjp1=40,bjp2=60
    const int JP2_TH_2004 = 60;
    const int pedestalTargetValue2004 = 8;

    if(!mEvent)
        return kStWarn;
    StEmcCollection *emc = mEvent->emcCollection();
    if(!emc)
        return kStWarn;


    //This code accesses offline database to get peds. These are NOT == peds
    //which were used during running. So NO ONLINE INFO is used in this trigger
    //reconstruction because the online database was not filled for 2004!
    TDataSet* db = 0;
    if(mDbMaker != 0)
        db = mDbMaker->GetDataBase("Calibrations/emc/y3bemc");
    else
    {
        if(mPrint)
            cout << "You *MUST* provide a pointer to St_db_Maker!" << endl
            << "In your macro say something like: myTrgMaker->setDbMaker(myDbMaker);"
            << endl;
        return kStWarn;
    }

    St_emcPed *emcPedestals = (St_emcPed*) db->Find("bemcPed");
    if (!emcPedestals)
        return kStWarn;
    emcPed_st *pedestalTable = emcPedestals->GetTable();
    if (!pedestalTable)
        return kStWarn;


    int adc12[kNTowers];
    int adc10[kNTowers];
    int adc08[kNTowers];
    int ped10[kNTowers];
    for(int i=0;i<kNTowers;i++)
    {
        adc12[i] = 0;
        adc10[i] = 0;
        adc08[i] = 0;
        ped10[i] = 0;
    }

    int ped12bit, val12bit,operation;// operation bit for pedestal subtraction
    // subtract: +1 (default)   add: 0

    StEmcDetector* detector=emc->detector(kBarrelEmcTowerId);
    if(detector)
    {
        for(Int_t m=1;m<=120;m++)
        {
            StEmcModule* module = detector->module(m);
            if(module)
            {
                StSPtrVecEmcRawHit& rawHit=module->hits();
                for(UInt_t k=0;k<rawHit.size();k++)
                    if(rawHit[k])
                    {
                        Int_t did;
                        Int_t mod=rawHit[k]->module();
                        Int_t e=rawHit[k]->eta();
                        Int_t s=abs(rawHit[k]->sub());
                        mGeo->getId(mod,e,s,did);
                        if(mTrigger.TowerStatus[did-1]==1)
                        {
                            adc12[did-1]=rawHit[k]->adc();
                            adc10[did-1] = adc12[did-1]>>2;
                            {
                                ped12bit = pedestalTable->AdcPedestal[did-1]/100;
                                operation = 1;
                                ped10[did-1] = ped12bit >> 2;
                                val12bit = ped12bit - pedestalTargetValue2004;

                                if(val12bit < 0)
                                {
                                    val12bit = -val12bit;
                                    operation = 0;
                                }
                                int val10bit = val12bit/4;

                                //this next line was in the 2004/2005 tcl code
                                //but it seems to be wrong because it results
                                //in ped10==1 at times (ex ADC=71&&ped=39)
                                if (val12bit - val10bit*4 > 2)
                                    val10bit+=1;
                                if(val10bit > 15)
                                {
                                    // can't subtract/add more than 15 on 10-bit level
                                    val10bit = val10bit - 4*((val10bit-11)/4);
                                }

                                if(operation==1)
                                {
                                    adc10[did-1] -= val10bit;
                                    ped10[did-1] -= val10bit;
                                }
                                else
                                {
                                    adc10[did-1] += val10bit;
                                    ped10[did-1] += val10bit;
                                }
                            }
                            adc08[did-1] = adc10[did-1]>>2;// adc10[],adc08[] are ped-adjusted
                        }
                    }
            }
        }
    }
    else
    {
        if(mPrint)
            cout << "StBemcTrigger::make2004Trigger() -- pointer to StEmcDetector is zero!" << endl;
        return kStWarn;
    }

    // making trigger patches and high towers
    TUnixTime unixTime(mEvent->time());
    Int_t dat=0,tim=0;
    unixTime.GetGTime(dat,tim);
    mDecoder = new StEmcDecoder(dat,tim);

    int HTmax=0;
    int HTmaxID=-1;
    for(int i = 0;i<kNPatches;i++)
        if(mTrigger.PatchStatus[i]==1)
        {
            int crate = 0;
            int seq  = 0;
            mDecoder->GetCrateAndSequenceFromTriggerPatch(i,crate,seq);
            int HT = 0;
            int PA = 0;
            int HTID = -1;
            int id;
            int patchPed = 0;
            for(int j=seq;j<seq+16;j++)
            {
                int stat = mDecoder->GetTowerIdFromCrate(crate,j,id);
                if(stat==1)
                {
                    if(adc10[id-1]>=HT)
                    {
                        HT = adc10[id-1];
                        HTID = id;

                    }
                    patchPed+= (ped10[id-1]>>2);
                    PA+=adc08[id-1];
                }
            }


            // now HT=10 bits and patch=12 bits
            // convert patch sum to 6 bits using LUT
            // TPsum= TPsum - 0 for 2004 because all TP peds=0
            // for 2005 all TP peds=16 so TPsum=TPsum-15
            // The fact that not all peds can be shifted to 2
            // on 10bit level require us to add up and then
            // subtract patchPed. But patchPed should==0 usually

            if(PA >= patchPed)
            {
                mTrigger.Patch[i] = PA - patchPed;
                if(mTrigger.Patch[i] > 62)
                    mTrigger.Patch[i] = 62;
            }

            // for HT need to OR top 2 bits and drop top bit
            // then drop bottom 3 bits
            int SHIFT = mTrigger.HTBits;
            HT = HT >> SHIFT;
            int HTL = HT & 0x1F;
            int HTH = HT >> 5;
            int B5  = 0;
            if(HTH>0)
                B5 = 1;
            mTrigger.HT[i] = HTL+(B5<<5);
            if(mPrint)
                cout <<"Patch number "<<i
                <<" Tower id = "<<HTID
                <<" adc12 = "<<adc12[HTID-1]<<" adc10 = "<<adc10[HTID-1]
                <<" adc08 = "<<adc08[HTID-1]
                <<" HT10 = "<<HT<<" PA12 = "<<PA
                <<" HT = "<<mTrigger.HT[i]<<" PA = "<<mTrigger.Patch[i]<<endl;

            if (mTrigger.HT[i]>HTmax)
            {
                HTmax=mTrigger.HT[i];
                HTmaxID=HTID;
            }
        }

    if (HTmax>HT1_TH_2004)
    {
        mIs2004HT1=1;
        HT1_ID_2004=HTmaxID;
        HT1_DSM_2004=HTmax;
    }
    else
    {
        mIs2004HT1=0;
        HT1_ID_2004=HTmaxID;
        HT1_DSM_2004=HTmax;
    }
    if (HTmax>HT2_TH_2004)
    {
        mIs2004HT2=1;
        HT2_ID_2004=HTmaxID;
        HT2_DSM_2004=HTmax;
    }
    else
    {
        mIs2004HT2=0;
        HT2_ID_2004=HTmaxID;
        HT2_DSM_2004=HTmax;
    }

    for (int i=0;i<300;i++)
    {
        trgPatch[i]=0;
        trgPatch[i]=mTrigger.Patch[i];
    }

    // making Jet trigger and Et
    int JPmax=0;
    int JPid=0;
    mTrigger.Et = 0;
    for(int i = 0;i<kNJet; i++)
    {
        int p0 = 0;
        int p1 = p0+25;

        mTrigger.Jet[i]= 0;
        for (int j=p0;j<p1;j++)
        {
            int k=JP_TP[i][j];
            mTrigger.Jet[i]+=mTrigger.Patch[k];
        }

        mTrigger.Et+=mTrigger.Jet[i];
        if (mTrigger.Jet[i]>JPmax)
        {
            JPmax=mTrigger.Jet[i];
            JPid=i;
        }
    }

    if (JPmax>JP1_TH_2004)
    {
        mIs2004JP1=1;
        JP1_ID_2004=JPid;
        JP1_DSM_2004=JPmax;
    }
    else
    {
        mIs2004JP1=0;
        JP1_ID_2004=JPid;
        JP1_DSM_2004=JPmax;
    }

    if (JPmax>JP2_TH_2004)
    {
        mIs2004JP2=1;
        JP2_ID_2004=JPid;
        JP2_DSM_2004=JPmax;
    }
    else
    {
        mIs2004JP2=0;
        JP2_ID_2004=JPid;
        JP2_DSM_2004=JPmax;
    }


    delete mDecoder;
    return kStOK;
}


//----------------------------------------------------
int StBemcTrigger::get2005Trigger()
{
    //for 2005 still need to test LUT subtraction
    //incorporate online peds. trigger masks, DSM masks + LUT tables

    zero();

    const int HT1_TH_2005 = 13;//bht0=5,bht1=13,bht2=17
    const int HT2_TH_2005 = 17;
    const int JP1_TH_2005 = 66;//bjp0=46,bjp1=66,bjp2=84
    const int JP2_TH_2005 = 84;
    const int pedestalTargetValue2005 = 24;


    if(!mEvent)
    {
        if(mPrint)
            cout << "StBemcTrigger::make2005Trigger() -- no StEvent!" << endl;
        return kStWarn;
    }
    StEmcCollection *emc = mEvent->emcCollection();
    if(!emc)
    {
        if(mPrint)
            cout << "StBemcTrigger::make2005Trigger() -- no StEmcCollection!" << endl;
        return kStWarn;
    }


    TDataSet* db = 0;
    if(mDbMaker != 0)
        db = mDbMaker->GetDataBase("Calibrations/emc/y3bemc");
    else
    {
        if(mPrint)
            cout << "StBemcTrigger::make2005Trigger() -- You *MUST* provide a pointer to St_db_Maker!" << endl
            << "    In your macro say something like: myTrgMaker->setDbMaker(myDbMaker);"
            << endl;
        return kStWarn;
    }


    St_emcPed *emcPedestals = (St_emcPed*) db->Find("bemcPed");
    if(!emcPedestals)
    {
        if(mPrint)
            cout << "StBemcTrigger::make2005Trigger() -- can't find BEMC pedestal table!" << endl;
        return kStWarn;
    }

    emcPed_st *pedestalTable = emcPedestals->GetTable();
    if(!pedestalTable)
    {
        if(mPrint)
            cout << "StBemcTrigger::make2005Trigger() -- can't find BEMC pedestal table!" << endl;
        return kStWarn;
    }

    int adc12[kNTowers];
    int adc10[kNTowers];
    int adc08[kNTowers];
    int ped10[kNTowers];
    for(int i=0;i<kNTowers;i++)
    {
        adc12[i] = 0;
        adc10[i] = 0;
        adc08[i] = 0;
        ped10[i] = 0;
    }

    int ped12bit, val12bit;
    int operation; // operation bit for pedestal subtraction
    // subtract: +1 (default)
    //      add: 0

    StEmcDetector* detector=emc->detector(kBarrelEmcTowerId);
    if(detector)
    {
        for(Int_t m=1;m<=120;m++)
        {
            StEmcModule* module = detector->module(m);
            if(module)
            {
                StSPtrVecEmcRawHit& rawHit=module->hits();
                for(UInt_t k=0;k<rawHit.size();k++)
                    if(rawHit[k])
                    {
                        Int_t did;
                        Int_t mod=rawHit[k]->module();
                        Int_t e=rawHit[k]->eta();
                        Int_t s=abs(rawHit[k]->sub());
                        mGeo->getId(mod,e,s,did);
                        if(mTrigger.TowerStatus[did-1]==1)
                        {
                            adc12[did-1]=rawHit[k]->adc();
                            adc10[did-1] = adc12[did-1]>>2;
                            {
                                // Trigger pedestals were subtracted online.
                                ped12bit = pedestalTable->AdcPedestal[did-1]/100;
                                operation = 1;
                                ped10[did-1] = ped12bit >> 2;
                                val12bit = ped12bit - pedestalTargetValue2005;
                                // this is by how much we want to change the ped

                                if(val12bit < 0)
                                {
                                    val12bit = -val12bit;
                                    operation = 0;
                                }
                                int val10bit = val12bit/4;
                                if(val12bit - val10bit*4 > 2)
                                    val10bit+=1;
                                if(val10bit > 15)
                                {
                                    // can't subtract/add more than 15 on 10-bit level
                                    val10bit = val10bit - 4*((val10bit-11)/4);
                                }

                                if(operation==1)
                                {
                                    adc10[did-1] -= val10bit;
                                    ped10[did-1] -= val10bit;
                                }
                                else
                                {
                                    adc10[did-1] += val10bit;
                                    ped10[did-1] += val10bit;
                                }
                            }
                            adc08[did-1] = adc10[did-1]>>2;// adc10[],adc08[] are ped-adj
                        }
                    }
            }
        }
    }
    else
    {
        if(mPrint)
            cout << "StBemcTrigger::make2005Trigger() -- pointer to StEmcDetector is zero!" << endl;
        return kStWarn;
    }

    // making trigger patches and high towers
    TUnixTime unixTime(mEvent->time());
    Int_t dat=0,tim=0;
    unixTime.GetGTime(dat,tim);
    mDecoder = new StEmcDecoder(dat,tim);

    float rped12bit;
    int HTmax=0;
    int HTmaxID=-1;
    for(int i = 0;i<kNPatches;i++)
        if(mTrigger.PatchStatus[i]==1)
        {
            int crate = 0;
            int seq  = 0;
            mDecoder->GetCrateAndSequenceFromTriggerPatch(i,crate,seq);
            mTables->getTriggerPedestal(crate,seq,rped12bit);
            //if (rped12bit!=0) cout<<"rped12bit="<<rped12bit<<endl;
            int HT = 0;
            int PA = 0;
            int HTID = -1;
            int id;
            int patchPed = 0;
            for(int j=seq;j<seq+16;j++)
            {
                int stat = mDecoder->GetTowerIdFromCrate(crate,j,id);
                if(stat==1)
                {
                    if(adc10[id-1]>=HT)
                    {
                        HT = adc10[id-1];
                        HTID = id;
                    }
                    patchPed+= (ped10[id-1]>>2);
                    PA+=adc08[id-1];
                }
		
		//printf("Patch %d Ped=%d PedSum=%d  ADC=%d ADCSum=%d\n",i,ped10[id-1]>>2,patchPed,adc08[id-1],PA);
	    }


            // now HT=10 bits and patch=12 bits
            // convert patch sum to 6 bits using LUT
            // during 2005 LUT's looked like this:
            // 0,0,0,...,0,1,2,3,...,63,63,63 -- total 4096 entries
            // <-- ped -->
            // the number of 0's is equal to patchPed_12bit
	    if(PA >= patchPed){

                mTrigger.Patch[i] = PA - (patchPed-1);
                if(mTrigger.Patch[i] > 62)  mTrigger.Patch[i] = 62;
            }
	    if(PA<patchPed) mTrigger.Patch[i]=1;

            // for HT need to OR top 2 bits and take correct bit window
            int SHIFT = mTrigger.HTBits;
            HT = HT >> SHIFT;
            int HTL = HT & 0x1F;
            int HTH = HT >> 5;
            int B5  = 0;
            if(HTH>0) B5 = 1;
            mTrigger.HT[i] = HTL+(B5<<5);
            if(mPrint)
	      cout <<"Patch number "<<i
		   <<" Tower id = "<<HTID
		   <<" adc12 = "<<adc12[HTID-1]<<" adc10 = "<<adc10[HTID-1]
		   <<" adc08 = "<<adc08[HTID-1]
		   <<" HT10 = "<<HT<<" PA12 = "<<PA
		   <<" HT = "<<mTrigger.HT[i]<<" PA = "<<mTrigger.Patch[i]<<endl;
	    
            if (mTrigger.HT[i]>HTmax)
	      {
                HTmax=mTrigger.HT[i];
                HTmaxID=HTID;
	      }
        }
    
    if (HTmax>HT1_TH_2005)
    {
        mIs2005HT1=1;
        HT1_ID_2005=HTmaxID;
        HT1_DSM_2005=HTmax;
    }
    else
    {
        mIs2005HT1=0;
        HT1_ID_2005=HTmaxID;
        HT1_DSM_2005=HTmax;
    }
    if (HTmax>HT2_TH_2005)
    {
        mIs2005HT2=1;
        HT2_ID_2005=HTmaxID;
        HT2_DSM_2005=HTmax;
    }
    else
    {
        mIs2005HT2=0;
        HT2_ID_2005=HTmaxID;
        HT2_DSM_2005=HTmax;
    }

    // making Jet trigger and Et
    int JPmax=0;
    int JPid=0;
    mTrigger.Et = 0;
    for(int i = 0;i<kNJet; i++)
    {
        int p0 = 0;
        int p1 = p0+25;

        mTrigger.Jet[i]= 0;
        for (int j=p0;j<p1;j++)
        {
            int k=JP_TP[i][j];
            mTrigger.Jet[i]+=mTrigger.Patch[k];
        }

        mTrigger.Et+=mTrigger.Jet[i];
        if (mTrigger.Jet[i]>JPmax)
        {
            JPmax=mTrigger.Jet[i];
            JPid=i;
        }
    }

    if (JPmax>JP1_TH_2005)
    {
        mIs2005JP1=1;
        JP1_ID_2005=JPid;
        JP1_DSM_2005=JPmax;
    }
    else
    {
        mIs2005JP1=0;
        JP1_ID_2005=JPid;
        JP1_DSM_2005=JPmax;
    }

    if (JPmax>JP2_TH_2005)
    {
        mIs2005JP2=1;
        JP2_ID_2005=JPid;
        JP2_DSM_2005=JPmax;
    }
    else
    {
        mIs2005JP2=0;
        JP2_ID_2005=JPid;
        JP2_DSM_2005=JPmax;
    }

    delete mDecoder;
    return kStOK;
}
