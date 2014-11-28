//
// $Id: StBemcTrigger.cxx,v 1.33 2009/12/03 14:15:59 rfatemi Exp $
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
#include "StEmcRawMaker/defines.h"

ClassImp(StBemcTrigger);

//-------------------------------------------------------------------
StBemcTrigger::StBemcTrigger():TObject()
{
    mGeo=StEmcGeom::getEmcGeom("bemc");
    mEvent = NULL;
    mDecoder = new StEmcDecoder();
    resetConf();
}
//----------------------------------------------------
StBemcTrigger::~StBemcTrigger()
{}

//----------------------------------------------------
void StBemcTrigger::resetConf(){

    PatchMap();

    mTrigger.HTBits = 3;
	
    //set all towers to status good because only offline status is used
    for(int i = 0;i<kNTowers;i++)   mTrigger.TowerStatus[i] = 1;
    //set all patches to status good because only offline status is used
    for(int i = 0;i<kNPatches;i++)  mTrigger.PatchStatus[i] = 1;
   

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
    mIs2005JPSI=-1;
    for ( int matrix=0;matrix<6;matrix++){
      mIs2006JP0[matrix]=-1;
      mIs2006HT2[matrix]=-1;
      mIs2006JP1[matrix]=-1;
      mIs2006JPSI[matrix]=-1;
      mIs2006BHTTP[matrix]=-1;
    }
    for (int i=0;i<50;i++)
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
    for (int matrix=0;matrix<6;matrix++){
      HT2_ID_2006[matrix]=-1;
      JP0_ID_2006[matrix]=-1;
      JP1_ID_2006[matrix]=-1;
    }
    for (int i=0;i<50;i++)
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
    for ( int matrix=0;matrix<6;matrix++){
      HT2_DSM_2006[matrix]=-1;
      JP0_DSM_2006[matrix]=-1;
      JP1_DSM_2006[matrix]=-1;
    }
    BETOT_DSM_2006=-1;
    for (int i=0;i<50;i++)
    {
        mDsmAdc[i] = -1;
    }
    
    for(int i=0;i<kNTowers;i++){
      mHT12005array[i]=-1;
      mHT22005array[i]=-1;
      for (int matrix=0;matrix<6;matrix++) mHT22006array[matrix][i]=-1;
    }
    for (int i=0;i<kNJet;i++){
      mJP12005array[i]=-1;
      mJP22005array[i]=-1;
      for (int matrix=0;matrix<6;matrix++) mJP02006array[matrix][i]=-1;
      for (int matrix=0;matrix<6;matrix++) mJP12006array[matrix][i]=-1;
    }

    for (int i=0;i<kNJet/2;i++) BL1_ADC_2006[i]=-1;

    for (int i=0;i<12;i++){
      mnumHT[i]=0;
      mnumJP[i]=0;
      if (i<6) mnumHTTP[i]=0;
    }

}

//----------------------------------------------------
void StBemcTrigger::zero()
{
    for(int i=0;i<kNPatches;i++)
    {
        mTrigger.HT[i] = 0;
        mTrigger.HTID[i]=0;
        mTrigger.Patch[i]= 0;
    }

    for(int i=0;i<kNJet;i++)
    {
        mTrigger.Jet[i]= 0;
    }

    mTrigger.Et = 0;
     
}


void StBemcTrigger::PatchMap()

  ///This is only used for trigger years before 2006 and is WRONG on the EAST BEMC for these years. 
  ///For year 2006 the TP->tower mapping is taken directly from EmcDecoder

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
		TString line("JP");
		line += i;
		line += " is sum of TP = ";
        for (int j=0;j<25;j++)
        {
            if (j!=24){
				line += JP_TP[i][j];
				line += "+";
			}
            if (j==24)
				line += JP_TP[i][j];
        }
		LOG_INFO << line << endm;
    }


    /****************************************************************************
       http://www.nikhef.nl/~ogrebeny/emc/files/Towers%20Layout.xls documents
       the tower->TP->JP correpsondence. Using StEmcDecoder,StEmcGeom and 
       the above spreadsheet I defined JP#s and assigned correct TP's above.

       JP      TOWERS               Clock Pos. looking from W -> vertex
       ---     --------          -----------------------------------------
       0        1-100,2101-2400           12
       1        101-500                   10
       2        501-900                   08
       3        901-1300                  06
       4        1301-1700                 04
       5        1701-2100                 02
       6        2401-2500,4501-4800       12
       7        2501-2900                 02
       8        2901-3300                 04
       9        3301-3700                 06
       10       3701-4100                 08
       11       4101-4500                 10                            
    **************************************************************************/


   /* mapping of renee's JP ids to StEmcDecoder's ids
    int renee[12];
    renee[0] = 5;
    renee[1] = 0;
    renee[2] = 1;
    renee[3] = 2;
    renee[4] = 3;
    renee[5] = 4;
    renee[6] = 7;
    renee[7] = 6;
    renee[8] = 11;
    renee[9] = 10;
    renee[10] = 9;
    renee[11] = 8;
    */

}


int StBemcTrigger::makeTrigger()
{
    get2003Trigger();
    get2004Trigger();
    get2005Trigger();
    get2006Trigger();

    //2003 HT1 == 1101+2201
    mIsTrig[0]  =mIs2003HT1;
    mTowJetId[0]=HT1_ID_2003;
    mDsmAdc[0]  =HT1_DSM_2003;

   //2003 HT2 == 2202
    mIsTrig[42]  =mIs2003HT2;
    mTowJetId[42]=HT2_ID_2003;
    mDsmAdc[42]  =HT2_DSM_2003;

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
    mnumHT[3] =numHT1_2005;
    for (int i=0;i<numHT1_2005;i++){
      mHT12005array[i]=HT1_2005_array[i];
    }

    //2005 HT2 == 96211
    mIsTrig[6]  =mIs2005HT2;
    mTowJetId[6]=HT2_ID_2005;
    mDsmAdc[6]  =HT2_DSM_2005;
    mnumHT[4] =numHT2_2005;
    for (int i=0;i<numHT2_2005;i++){
      mHT22005array[i]=HT2_2005_array[i];
    }

    //2005 JP1 == 96221
    mIsTrig[7]  =mIs2005JP1;
    mTowJetId[7]=JP1_ID_2005;
    mDsmAdc[7]  =JP1_DSM_2005;
    mnumJP[2]  =numJP1_2005;
    for (int i=0;i<numJP1_2005;i++){
      mJP12005array[i]=JP1_2005_array[i];
    }

    //2005 JP2 == 96233
    mIsTrig[8]  =mIs2005JP2;
    mTowJetId[8]=JP2_ID_2005;
    mDsmAdc[8]  =JP2_DSM_2005;
    mnumJP[3]   =numJP2_2005;
    for (int i=0;i<numJP2_2005;i++){
      mJP22005array[i]=JP2_2005_array[i];
    }

    //2005 AJP == 96241
    mIsTrig[9]  =mIs2005ADJ;
    mTowJetId[9]=ADJ_ID_2005;
    mDsmAdc[9]  =ADJ_DSM_2005;


    //2005 JPSI
    mIsTrig[10] =mIs2005JPSI;
    mTowJetId[10]=-1;
    mDsmAdc[10]=-1;
    for (int i=0;i<kNJet;i++){
      mJPSI2005adc[i]=JPSI_2005_ADC[i];
      mJPSI2005id[i]=JPSI_2005_ID[i];
    }
    


    for (int matrix=0;matrix<6;matrix++){
      
      //2006 HT2 
      mIsTrig[11+(matrix*5)]  =mIs2006HT2[matrix];
      mTowJetId[11+(matrix*5)]=HT2_ID_2006[matrix];
      mDsmAdc[11+(matrix*5)]  =HT2_DSM_2006[matrix];
      mnumHT[5+matrix] =numHT2_2006[matrix];
      for (int i=0;i<numHT2_2006[matrix];i++){
	mHT22006array[matrix][i]=HT2_2006_array[matrix][i];
      }
      
      //2006 JP1 
      mIsTrig[12+(matrix*5)]  =mIs2006JP0[matrix];
      mTowJetId[12+(matrix*5)]=JP0_ID_2006[matrix];
      mDsmAdc[12+(matrix*5)]  =JP0_DSM_2006[matrix];
      mnumJP[5+(matrix*2)]  =numJP0_2006[matrix];
      for (int i=0;i<numJP0_2006[matrix];i++){
	mJP02006array[matrix][i]=JP0_2006_array[matrix][i];
      }
      
      //2006 JP2 
      mIsTrig[13+(matrix*5)]  =mIs2006JP1[matrix];
      mTowJetId[13+(matrix*5)]=JP1_ID_2006[matrix];
      mDsmAdc[13+(matrix*5)]  =JP1_DSM_2006[matrix];
      mnumJP[6+(matrix*2)]   =numJP1_2006[matrix];
      for (int i=0;i<numJP1_2006[matrix];i++){
	mJP12006array[matrix][i]=JP1_2006_array[matrix][i];
      }
      
      //2006 JPSI
      mIsTrig[14+(matrix*5)] =mIs2006JPSI[matrix];
      mTowJetId[14+(matrix*5)]=-1;
      mDsmAdc[14+(matrix*5)]=-1;
      for (int i=0;i<kNJet;i++){
	mJPSI2006adc[matrix][i]=JPSI_2006_ADC[matrix][i];
	mJPSI2006id[matrix][i]=JPSI_2006_ID[matrix][i];
      }
      
      //2006 BHTTP
      mIsTrig[15+(matrix*5)]=mIs2006BHTTP[matrix];
      mTowJetId[15+(matrix*5)]=-1;
      mDsmAdc[15+(matrix*5)]=-1;
      mnumHTTP[matrix]=0;
      mnumHTTP[matrix]=numHTTP_2006[matrix];
      for (int i=0; i<numHTTP_2006[matrix];i++){
	mHTTP2006arrayHT[matrix][i]=BHTTP_2006_HT[matrix][i];
	mHTTP2006arrayHTADC[matrix][i]=BHTTP_2006_HT_ADC[matrix][i];
	mHTTP2006arrayTP[matrix][i]=BHTTP_2006_TP[matrix][i];
	mHTTP2006arrayTPADC[matrix][i]=BHTTP_2006_TP_ADC[matrix][i];
      }
    }

    //2006 BETOT
    mIsTrig[41]=-1;
    mTowJetId[41]=-1;
    mDsmAdc[41]=BETOT_DSM_2006;
    for (int i=0; i<(kNJet/2); i++){mBL12006arrayADC[i]=BL1_ADC_2006[i];}
    


    return kStOK;
}


int StBemcTrigger::get2003Trigger()
{
    zero();

    const int HT1_TH_2003 = 8;
    const int HT2_TH_2003 = 13;

    if(!mEvent)
        return kStWarn;
    StEmcCollection *emc = mEvent->emcCollection();
    if(!emc)
        return kStWarn;

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
        for(Int_t m=1;m<=60;m++)
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
    mDecoder->SetDateTime(dat,tim);
    mDecoder->SetFixTowerMapBug(true);


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
            if(HTH>0) B5 = 1; // IF top bits !=0 B5=1
            mTrigger.HT[i] = HTL+(B5<<5); // Or top bits
	    { LOG_DEBUG <<"Patch number "<<i<<" Tower id = "<<
		HTID<<" adc12 = "<<adc12[HTID-1]<<" adc10 = "<<
		adc10[HTID-1]<<" HT = "<<mTrigger.HT[i]<<endm; }
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

    if (HTmax>HT2_TH_2003)
    {
        mIs2003HT2=1;
        HT2_ID_2003=HTmaxID;
        HT2_DSM_2003=HTmax;
    }
    else
    {
        mIs2003HT2=0;
        HT2_ID_2003=HTmaxID;
        HT2_DSM_2003=HTmax;
    }


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
        for(Int_t m=1;m<=60;m++)
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
       			        //ped12bit = pedestalTable->AdcPedestal[did-1]/100;
			        /////////added to remove use of emcTables
				float NEWped=-1;
				float NEWrms=-1;
				mTables ->getPedestal(BTOW,did,0,NEWped,NEWrms);
				ped12bit=(int) NEWped;


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
        LOG_WARN << "StBemcTrigger::make2004Trigger() -- pointer to StEmcDetector is zero!" << endm;
        return kStWarn;
    }

    // making trigger patches and high towers
    TUnixTime unixTime(mEvent->time());
    Int_t dat=0,tim=0;
    unixTime.GetGTime(dat,tim);
    mDecoder->SetDateTime(dat,tim);
    mDecoder->SetFixTowerMapBug(true);

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
			{ LOG_DEBUG <<"Patch number "<<i<<" Tower id = "<<HTID<<" adc12 = "<<adc12[HTID-1]<<" adc10 = "
				<<adc10[HTID-1]<<" adc08 = "<<adc08[HTID-1]<<" HT10 = "<<HT<<" PA12 = "<<PA
				<<" HT = "<<mTrigger.HT[i]<<" PA = "<<mTrigger.Patch[i]<<endm; }
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


    return kStOK;
}


//----------------------------------------------------
int StBemcTrigger::get2005Trigger()
{
    //for 2005 still need to test LUT subtraction
    //incorporate online peds. trigger masks, DSM masks + LUT tables

    zero();


    const int JPSI_TH_2005 = 5;//bht0=5,bht1=13,bht2=17
    const int HT1_TH_2005 = 13;
    const int HT2_TH_2005 = 17;
    const int JP1_TH_2005 = 66;//bjp0=46,bjp1=66,bjp2=84
    const int JP2_TH_2005 = 84;
    const int pedestalTargetValue2005 = 24;


    if(!mEvent)
    {
        LOG_WARN << "StBemcTrigger::make2005Trigger() -- no StEvent!" << endm;
        return kStWarn;
    }
    StEmcCollection *emc = mEvent->emcCollection();
    if(!emc)
    {
        LOG_WARN << "StBemcTrigger::make2005Trigger() -- no StEmcCollection!" << endm;
        return kStWarn;
    }

    int adc12[kNTowers];
    int adc10[kNTowers];
    int adc08[kNTowers];
    int ped10[kNTowers];
    for(int i=0;i<kNTowers;i++){
        adc12[i] = 0;
        adc10[i] = 0;
        adc08[i] = 0;
        ped10[i] = 0;
	HT1_2005_array[i]=-1;
	HT2_2005_array[i]=-1;
        mHT12005array[i]=-1;
        mHT22005array[i]=-1;
    }

    mnumHT[3]=0;
    mnumHT[4]=0;
    mnumJP[2]=0;
    mnumJP[3]=0;
    mnumJP[4]=0;
    
    for (int i=0;i<kNJet;i++){
      JP1_2005_array[i]=-1;
      JP2_2005_array[i]=-1;
      mJP12005array[i]=-1;
      mJP22005array[i]=-1;
      mJPSI2005adc[i]=-1;
      mJPSI2005id[i]=-1;
     }

    numHT1_2005=0;
    numHT2_2005=0;
    numJP1_2005=0;
    numJP2_2005=0;
    numADJ_2005=0;
    
    

    int ped12bit, val12bit;
    int operation; // operation bit for pedestal subtraction
    // subtract: +1 (default)
    //      add: 0

    StEmcDetector* detector=emc->detector(kBarrelEmcTowerId);
    if(detector)
    {
        for(Int_t m=1;m<=60;m++)
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
                        if ((mTrigger.TowerStatus[did-1]==1)&&(mTables->status(BTOW,did,"calib")==1))
                        {
                            adc12[did-1]=rawHit[k]->adc();
                            adc10[did-1] = adc12[did-1]>>2;
                            {
                                // Trigger pedestals were subtracted online.
                                // ped12bit = pedestalTable->AdcPedestal[did-1]/100;
				
			        /////////added to remove use of emcTables
				float NEWped=-1;
				float NEWrms=-1;
				mTables ->getPedestal(BTOW,did,0,NEWped,NEWrms);
				ped12bit=(int) NEWped;

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
        LOG_WARN << "StBemcTrigger::make2005Trigger() -- pointer to StEmcDetector is zero!" << endm;
        return kStWarn;
    }

    // making trigger patches and high towers
    TUnixTime unixTime(mEvent->time());
    Int_t dat=0,tim=0;
    unixTime.GetGTime(dat,tim);
    mDecoder->SetDateTime(dat,tim);
    mDecoder->SetFixTowerMapBug(false);

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
	    mTrigger.HTID[i] = HTID;
		{ LOG_DEBUG <<"Patch number "<<i<<" Tower id = "<<mTrigger.HTID[i]<<" adc12 = "<<adc12[HTID-1]
			<<" adc10 = "<<adc10[HTID-1]<<" adc08 = "<<adc08[HTID-1]<<" HT10 = "<<HT<<" PA12 = "<<PA
			<<" HT = "<<mTrigger.HT[i]<<" PA = "<<mTrigger.Patch[i]<<endm; }
	    
            if (mTrigger.HT[i]>HTmax){
	      HTmax=mTrigger.HT[i];
	      HTmaxID=HTID;
	    }
	    
	    if (mTrigger.HT[i]>HT1_TH_2005){
	      HT1_2005_array[numHT1_2005]=HTID;
	      numHT1_2005++;
	      LOG_DEBUG<<HTID<<" Passed HT1 threshold="<<numHT1_2005<<"  "<<HT1_2005_array[numHT1_2005-1]<<endm;
	    }
	    
	    if (mTrigger.HT[i]>HT2_TH_2005){
	      HT2_2005_array[numHT2_2005]=HTID;
	      numHT2_2005++;
	      LOG_DEBUG<<HTID<<" Passed HT2 threshold="<<numHT2_2005<<"  "<<HT2_2005_array[numHT2_2005-1]<<endm;
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


        if (mTrigger.Jet[i]>JP1_TH_2005)
        {
	  JP1_2005_array[numJP1_2005]=i;
          numJP1_2005++;
        }

       if (mTrigger.Jet[i]>JP2_TH_2005)
        {
	  JP2_2005_array[numJP2_2005]=i;
          numJP2_2005++;
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


    // making Jpsi trigger
    int JpsiPatch[kNJet];
    for(int i = 0;i<kNJet; i++)
    {
        int p0 = 0;
        int p1 = p0+25;

        JPSI_2005_ADC[i]=0;
	JPSI_2005_ID[i]=0;
	JpsiPatch[i]=0;
        for (int j=p0;j<p1;j++)
        {
            int k=JP_TP[i][j];
	    if (mTrigger.HT[k]>JPSI_2005_ADC[i]) {
	      JPSI_2005_ADC[i]=mTrigger.HT[k];
	      JPSI_2005_ID[i]=mTrigger.HTID[k];
	    }
	    LOG_DEBUG<<"Jet id="<<i<<" Patch id="<<j<<" PatchHT="<<mTrigger.HT[k]<<" PatchHTID="<<mTrigger.HTID[k]<<" JPSI_2005_ADC="<<JPSI_2005_ADC[i]<<endm;
        }
	if  (JPSI_2005_ADC[i]>JPSI_TH_2005) {
	  JpsiPatch[i]=1;
	}
	LOG_DEBUG<<"Final JetPatchHT for JP"<<i<<" is TowID="<<JPSI_2005_ID[i]<<"  with ADC= "<<JPSI_2005_ADC[i]<<" and flag="<<JpsiPatch[i]<<endm;
    }

    if ((JpsiPatch[0]&&(JpsiPatch[2]||JpsiPatch[3]||JpsiPatch[4])) ||
	(JpsiPatch[1]&&(JpsiPatch[3]||JpsiPatch[4]||JpsiPatch[5])) ||
	(JpsiPatch[2]&&(JpsiPatch[4]||JpsiPatch[5])) ||
	(JpsiPatch[3]&&JpsiPatch[5]) )
      {
	
	mIs2005JPSI=1;
      }
    else
      {
	mIs2005JPSI=0;
      }
    

    return kStOK;
}



int StBemcTrigger::get2006Trigger()
{


  zero();

  //trigger matrix for 6 different settings in time
  const int HT0WEST_TH_2006[6]= {  5,  5,  5,  5,  5,  5};
  const int HT0EAST_TH_2006[6]= { 11, 11,  5,  5,  5,  5};
  const int HT1WEST_TH_2006[6]= { 12, 12, 16, 18, 16, 16};
  const int HT1EAST_TH_2006[6]= { 12, 12, 16, 18, 16, 16};
  const int HT2WEST_TH_2006[6]= { 22, 24, 24, 24, 24, 24};
  const int HT2EAST_TH_2006[6]= { 24, 24, 24, 24, 24, 24};
  const int HTTP0_TH_2006[6]  = {  1,  1,  1,  1,  1,  1};
  const int HTTP1_TH_2006[6]  = { 17, 17, 20, 20, 19, 19};
  const int HTTP2_TH_2006[6]  = { 31, 31, 31, 31, 31, 31};
  const int JP0_TH_2006[6]    = { 42, 42, 48, 49, 49, 49};
  const int JP1_TH_2006[6]    = { 58, 58, 58, 60, 60, 60};
  //const int JP2_TH_2006[6]    = {110,110,110,110,110,110};
  //const int BEMC_ETOT_2006[6] = {109,109,109,109,109,109};

  const int pedestalTargetValue2006 = 24;

 
    
    if(!mEvent)
    {
        LOG_WARN << "StBemcTrigger::make2006Trigger() -- no StEvent!" << endm;
        return kStWarn;
    }
    
    StEmcCollection *emc = mEvent->emcCollection();
    if(!emc)
    {
        LOG_WARN << "StBemcTrigger::make2006Trigger() -- no StEmcCollection!" << endm;
        return kStWarn;
    }


    int adc12[kNTowers];
    int adc10[kNTowers];
    int adc08[kNTowers];
    int ped10[kNTowers];
    for(int i = 0; i < kNTowers; ++i)
    {
    
        adc12[i] = 0;
        adc10[i] = 0;
        adc08[i] = 0;
        ped10[i] = 0;
	for ( int matrix=0; matrix<6; matrix++){
	  HT2_2006_array[matrix][i] = -1;
	  mHT22006array[matrix][i] = -1;
	}
    }

    for(int i = 0; i < kNJet; ++i)
      {
	for (int matrix=0;matrix<6;matrix++)
	  {
	    JP0_2006_array[matrix][i] = -1;
	    JP1_2006_array[matrix][i] = -1;
	    mJP02006array[matrix][i] = -1;
	    mJP12006array[matrix][i] = -1;
	    mJPSI2006adc[matrix][i] = -1;
	    mJPSI2006id[matrix][i] = -1;
	  }
      }

    
    for(int i = 0; i < kNPatches; ++i)
      {
	for (int matrix=0;matrix<6;matrix++)
	  {
	    BHTTP_2006_HT[matrix][i]=-1;
	    BHTTP_2006_HT_ADC[matrix][i]=-1;
	    BHTTP_2006_TP[matrix][i]=-1;
	    BHTTP_2006_TP_ADC[matrix][i]=-1;
	  }
      }

    for (int matrix=0;matrix<6;matrix++){
      numHT2_2006[matrix]=0;
      numJP0_2006[matrix]=0;
      numJP1_2006[matrix]=0;
      numHTTP_2006[matrix]=0;
    }
  
    int ped12bit, val12bit, operation; 
    // operation bit for pedestal subtraction
    // subtract: +1 (default)
    //      add: 0

    StEmcDetector* detector=emc->detector(kBarrelEmcTowerId);
    if(detector)
    {
        
        for(Int_t m = 1; m <= 120; ++m)
        {
         
            StEmcModule* module = detector->module(m);
            if(module)
            {
            
                StSPtrVecEmcRawHit& rawHit=module->hits();
                for(UInt_t k = 0; k < rawHit.size(); ++k)
                {
                
                    if(rawHit[k])
                    {
                    
                        Int_t did;
                        Int_t mod=rawHit[k]->module();
                        Int_t e=rawHit[k]->eta();
                        Int_t s=abs(rawHit[k]->sub());
                        mGeo->getId(mod,e,s,did);
                        
                        if ((mTrigger.TowerStatus[did-1]==1)&&(mTables->status(BTOW,did,"calib")==1))
                        {
                        
                            adc12[did-1]=rawHit[k]->adc();
                            adc10[did-1] = adc12[did-1]>>2;
                            
                            float NEWped=-1;
                            float NEWrms=-1;
                            mTables ->getPedestal(BTOW,did,0,NEWped,NEWrms);
                            ped12bit=(int) NEWped;
                            
                            operation = 1;
                            ped10[did-1] = ped12bit >> 2;
                            val12bit = ped12bit - pedestalTargetValue2006;
                            
                            if(val12bit < 0)
			      {
                                val12bit = -val12bit;
				operation = 0;
			      }
                            
                            int val10bit = val12bit/4;
                            if(val12bit - val10bit*4 > 2) val10bit+=1;
                            
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
                            
                            adc08[did-1] = adc10[did-1]>>2;// adc10[],adc08[] are pedestal-adjusted
                            
                        }
                        
                    }
                    
                }
                    
            }
            
        }
    
    }
    else
    {
        LOG_WARN << "StBemcTrigger::make2006Trigger() -- pointer to StEmcDetector is zero!" << endm;
        return kStWarn;
    }

    // making trigger patches and high towers
    TUnixTime unixTime(mEvent->time());
    Int_t dat=0,tim=0;
    unixTime.GetGTime(dat,tim);
    mDecoder->SetDateTime(dat,tim);
    mDecoder->SetFixTowerMapBug(false);

    float rped12bit;
    int HTmax=0;
    int HTmaxID=-1;
    
    for(int i = 0; i < kNPatches; ++i) 
    {

        if(mTrigger.PatchStatus[i]==1) 
        {
        
            int crate = 0;
            int seq  = 0;
            int HT = 0;
            int PA = 0;
            int HTID = -1;
            int id;
            int patchPed = 0;
            
            mDecoder->GetCrateAndSequenceFromTriggerPatch(i,crate,seq);
            mTables->getTriggerPedestal(crate,seq,rped12bit);
            
            //loop over each tower(j) in the trigger patch (i)
            for(int j = seq; j < seq + 16; ++j)
            {
            
                int stat = mDecoder->GetTowerIdFromCrate(crate,j,id);
                if(stat == 1)
                {
            
                    if(adc10[id-1]>=HT)
                    {
                        HT = adc10[id-1];
                        HTID = id;
                    }
                    
                    patchPed += ped10[id-1] >> 2;
                    PA += adc08[id-1];
                }
            
            }
	
            // now HT=10 bits and patch=12 bits
            // convert patch sum to 6 bits using LUT
            // during 2006 LUT's looked like this:
            // 0,0,0,...,0,1,2,3,...,63,63,63 -- total 4096 entries
            // <-- ped -->
            // the number of 0's is equal to patchPed_12bit
            if(PA >= patchPed)
            {
            
                mTrigger.Patch[i] = PA - (patchPed - 1);
                if(mTrigger.Patch[i] > 62)  mTrigger.Patch[i] = 62;
            }
            else
            {
                mTrigger.Patch[i] = 1;
            }
            
            // for HT need to:
            //1) drop lowest bits (depends on calibration)
            //2) take next 6 LSB as HT 6 bit adc
            //3) if 6 or higher bit ==1 need to set all bits high (63)
            HT = HT >>(mTrigger.HTBits - 1);
            int HTL = HT & 0x1F;//5 LSB
            int HTH = HT >> 5;  //>= 6 LSB
            int B5  = 0;
            if(HTH>0) B5 = 1;
            mTrigger.HT[i] = HTL+(B5<<5);
            mTrigger.HTID[i] = HTID;
            
            { 
            LOG_DEBUG <<"Patch number "<<i<<" Tower id = "<<mTrigger.HTID[i]<<" adc12 = "<<adc12[HTID-1]
            <<" adc10 = "<<adc10[HTID-1]<<" adc08 = "<<adc08[HTID-1]<<" HT10 = "<<HT<<" PA12 = "<<PA
            <<" HT = "<<mTrigger.HT[i]<<" PA = "<<mTrigger.Patch[i]<<endm; 
            }
            
            if(mTrigger.HT[i]>HTmax)
            {
                HTmax=mTrigger.HT[i];
                HTmaxID=HTID;
            }
    
	    for (int matrix=0; matrix<6; matrix++){

	      if (HTID>2400) {//East
		if(mTrigger.HT[i]>HT2EAST_TH_2006[matrix])
		  {
		    HT2_2006_array[matrix][numHT2_2006[matrix]]=HTID;
		    numHT2_2006[matrix]++;
		  }
	      }
	      
	      if (HTID<=2400) {//West
		if(mTrigger.HT[i]>HT2WEST_TH_2006[matrix])
		  {
		    HT2_2006_array[matrix][numHT2_2006[matrix]]=HTID;
		    numHT2_2006[matrix]++;
		  }
	      }
	    }
        }
    }
            
    //making HT trigger
    for (int matrix=0; matrix<6; matrix++){

      if (HTmaxID>2400){//East
	if (HTmax > HT2EAST_TH_2006[matrix])
	  {
	    mIs2006HT2[matrix]=1;
	    HT2_ID_2006[matrix]=HTmaxID;
	    HT2_DSM_2006[matrix]=HTmax;
	  }
	else
	  {
	    mIs2006HT2[matrix]=0;
	    HT2_ID_2006[matrix]=HTmaxID;
	    HT2_DSM_2006[matrix]=HTmax;
	  }
      }

      if (HTmaxID<=2400){//West
	if (HTmax > HT2WEST_TH_2006[matrix])
	  {
	    mIs2006HT2[matrix]=1;
	    HT2_ID_2006[matrix]=HTmaxID;
	    HT2_DSM_2006[matrix]=HTmax;
	  }
	else
	  {
	    mIs2006HT2[matrix]=0;
	    HT2_ID_2006[matrix]=HTmaxID;
	    HT2_DSM_2006[matrix]=HTmax;
	  }
      }
    }
    
    // making HTTP trigger
    int BHTTPcounter[6];
    for ( int matrix=0; matrix<6; matrix++){

      mIs2006BHTTP[matrix]=0;
      BHTTPcounter[matrix]=0;
      
      for(int i = 0; i < kNPatches; ++i)
	{
	  
	  int HTBIT = -1;
	  int TPBIT = -1;
        
	  if (kNPatches < 150){ //West
	    if (mTrigger.HT[i] < HT0WEST_TH_2006[matrix]) HTBIT = 0;
	    if (mTrigger.HT[i] > HT0WEST_TH_2006[matrix]) HTBIT = 1;
	    if (mTrigger.HT[i] > HT1WEST_TH_2006[matrix]) HTBIT = 2;
	    if (mTrigger.HT[i] > HT2WEST_TH_2006[matrix]) HTBIT = 3;
	  }
	  
	  if (kNPatches >= 150){//East
	    if (mTrigger.HT[i] < HT0EAST_TH_2006[matrix]) HTBIT = 0;
	    if (mTrigger.HT[i] > HT0EAST_TH_2006[matrix]) HTBIT = 1;
	    if (mTrigger.HT[i] > HT1EAST_TH_2006[matrix]) HTBIT = 2;
	    if (mTrigger.HT[i] > HT2EAST_TH_2006[matrix]) HTBIT = 3;
	  }
	
	  if (mTrigger.Patch[i] < HTTP0_TH_2006[matrix]) TPBIT = 0;
	  if (mTrigger.Patch[i] > HTTP0_TH_2006[matrix]) TPBIT = 1;
	  if (mTrigger.Patch[i] > HTTP1_TH_2006[matrix]) TPBIT = 2;
	  if (mTrigger.Patch[i] > HTTP2_TH_2006[matrix]) TPBIT = 3;
        
	  if( TPBIT >= 2 && HTBIT >= 2)
	    {
	      mIs2006BHTTP[matrix] = 1;
	      BHTTP_2006_TP[matrix][BHTTPcounter[matrix]] = i;
	      BHTTP_2006_TP_ADC[matrix][BHTTPcounter[matrix]] = mTrigger.Patch[i];
	      BHTTP_2006_HT[matrix][BHTTPcounter[matrix]] = mTrigger.HTID[i];
	      BHTTP_2006_HT_ADC[matrix][BHTTPcounter[matrix]] = mTrigger.HT[i];
	      BHTTPcounter[matrix]++;
	    }
	}
      numHTTP_2006[matrix]=BHTTPcounter[matrix];
    }
    

    // Clear jet patch variables
    int JPmax = 0;
    int JPid = 0;
    
    for(int i = 0; i < kNJet; ++i)
      {
        
        mTrigger.Jet[i] = 0;
        
        for(int sequence = 0; sequence < kN_sequences; ++sequence)
	  {
            
            int k = 0;
            mDecoder->GetTriggerPatchFromJetPatch(i, sequence, k);
            mTrigger.Jet[i] += mTrigger.Patch[k];
            
	  }
        
      }
    
    // Check jet patch triggers
    for(int i = 0; i < kNJet; ++i)
      {
	
        if(mTrigger.Jet[i] > JPmax)
	  {
            JPmax = mTrigger.Jet[i];
            JPid = i;
	  }
	
	for (int matrix=0;matrix<6;matrix++){
	
	  if(mTrigger.Jet[i] > JP0_TH_2006[matrix])
	    {
	      JP0_2006_array[matrix][numJP0_2006[matrix]] = i;
	      ++numJP0_2006[matrix];
	    }
	  
	  if(mTrigger.Jet[i] > JP1_TH_2006[matrix])
	    {
	      JP1_2006_array[matrix][numJP1_2006[matrix]] = i;
	      ++numJP1_2006[matrix];
	    }   
	}
      }
	

    for (int matrix=0;matrix<6;matrix++){
      
      if(JPmax > JP0_TH_2006[matrix])
	{
	  mIs2006JP0[matrix]=1;
	  JP0_ID_2006[matrix]=JPid;
	  JP0_DSM_2006[matrix]=JPmax;
	}
      else
	{
	  mIs2006JP0[matrix]=0;
	  JP0_ID_2006[matrix]=JPid;
	  JP0_DSM_2006[matrix]=JPmax;
	}
    
      if(JPmax > JP1_TH_2006[matrix])
	{
	  mIs2006JP1[matrix]=1;
	  JP1_ID_2006[matrix]=JPid;
	  JP1_DSM_2006[matrix]=JPmax;
	}
      else
	{
	  mIs2006JP1[matrix]=0;
	  JP1_ID_2006[matrix]=JPid;
	  JP1_DSM_2006[matrix]=JPmax;
	}
    }
    
    //making ETOT trigger
    //combine 2 JP to form 13 bit adc
    //drop 2 LSB 
    //check if bit 6 or higher == 1
    mTrigger.Et = 0;
    int EtotSum[6];
    for(int i = 0; i < 6; ++i) 
      {
	
	EtotSum[i] = 0;
	BL1_ADC_2006[i] = 0;
	Int_t j = 2*i;
	Int_t TempSum = 0;
	TempSum = mTrigger.Jet[j] + mTrigger.Jet[j+1];
	EtotSum[i]=TempSum >> 2;
	if (EtotSum[i]>31)  EtotSum[i]=31;
	mTrigger.Et += EtotSum[i];
	BL1_ADC_2006[i] = EtotSum[i];
	
      } 
    BETOT_DSM_2006=mTrigger.Et;
    
    //
    // Making Jpsi trigger
    // See http://www.star.bnl.gov/STAR/html/trg_l/TSL/Software/EMC.pdf
    //
    int JpsiPatch[6][kNJet];
    for (int matrix=0;matrix<6;matrix++)
      {
	for(int i = 0; i < kNJet; ++i)
	  {
	    
	    JPSI_2006_ADC[matrix][i]=0;
	    JPSI_2006_ID[matrix][i]=0;
	    JpsiPatch[matrix][i]=0;
	  
	    for(int sequence = 0; sequence < kN_sequences; ++sequence)
	      {
		
		int k = 0;
		mDecoder->GetTriggerPatchFromJetPatch(i, sequence, k);	    
		if(mTrigger.HT[k] > JPSI_2006_ADC[matrix][i]) 

		  {
		    JPSI_2006_ADC[matrix][i]=mTrigger.HT[k];
		    JPSI_2006_ID[matrix][i]=mTrigger.HTID[k];
		  }
	      }
	    
	    if(i < 6) 
	      {
		// BEMC West
		if(JPSI_2006_ADC[matrix][i] > HT0WEST_TH_2006[matrix]) JpsiPatch[matrix][i] = 1;
	      }
	    else 
	      {
		// BEMC East
		if(JPSI_2006_ADC[matrix][i] > HT0EAST_TH_2006[matrix]) JpsiPatch[matrix][i] = 1;
	      }
	    
	  }
	
	
	
	// The J/psi trigger fires if two opposite jet patches have high towers
	// above selected thresholds.
	// Follow convention in the DSM implementation where vector bits (0-5)
	// correspond to positions 2,4,6,8,10,12 o'clock.
	
	
	int ht_jpsi[6]={0,0,0,0,0,0};
	
	ht_jpsi[4] = JpsiPatch[matrix][0] || JpsiPatch[matrix][6];
	ht_jpsi[5] = JpsiPatch[matrix][1] || JpsiPatch[matrix][7];
	ht_jpsi[0] = JpsiPatch[matrix][2] || JpsiPatch[matrix][8];
	ht_jpsi[1] = JpsiPatch[matrix][3] || JpsiPatch[matrix][9];
	ht_jpsi[2] = JpsiPatch[matrix][4] || JpsiPatch[matrix][10];
	ht_jpsi[3] = JpsiPatch[matrix][5] || JpsiPatch[matrix][11];

	mIs2006JPSI[matrix] = ((ht_jpsi[0] && (ht_jpsi[2] || ht_jpsi[3] || ht_jpsi[4])) ||
			       (ht_jpsi[1] && (ht_jpsi[3] || ht_jpsi[4] || ht_jpsi[5])) ||
			       (ht_jpsi[2] && (ht_jpsi[4] || ht_jpsi[5])) || (ht_jpsi[3] &&  ht_jpsi[5]));
	
      }
    
    return kStOK;
}
