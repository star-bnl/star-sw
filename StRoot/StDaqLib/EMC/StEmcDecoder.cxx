#include "StEmcDecoder.h"
#include <time.h>
#include <stdlib.h>
#include <Stiostream.h>
#include <stdio.h>
#ifdef IN_PANITKIN
#include <TFile.h>
#endif
#include "StMessMgr.h"

//--------------------------------------------------------
/*!
Date and time should be in GMT
to get the correct electronics configuration for towers and SMD
*/
StEmcDecoder::StEmcDecoder(unsigned int date,unsigned int time, bool TowerMapBug)
{
    //cout <<"TIME USED FOR DECODER = "<<date<<" "<<time<<endl;
    this->SetFixTowerMapBug(TowerMapBug);
    this->SetDateTime(date,time);
}
//--------------------------------------------------------
StEmcDecoder::~StEmcDecoder()
{}
//--------------------------------------------------------
/*!
fixTowerBugIndexes method - fixes the indexes for tower map
correction
*/
void StEmcDecoder::fixTowerBugIndexes()
{
    for(int i=0;i<4800;i++)
    {
        int id = i+1;
        if(TowerBugFixIndex[i]!=id)
        {
            int newId = TowerBugFixIndex[i];
            TowerBugFixIndex[newId-1] = id;
        }
    }
    //for(int i=0;i<4800;i++) if(TowerBugFixIndex[i]!=i+1)
    //  cout <<"****** TOWER BUG - id_original = "<<i+1<<"   id_new = "<<TowerBugFixIndex[i]<<endl;
}
//--------------------------------------------------------
/*!
fixes the software indexes for the preshower based on 2006/2007 mapping indexes
many changes correspond to the tower mapping bugs
*/
void StEmcDecoder::fixPreshowerBugIndexes()
{
    for(int i=0; i<4800; i++) {
        int id = i+1;
        if(PreshowerBugFixIndex[i] != id) {
            int newId = PreshowerBugFixIndex[i];
            PreshowerBugFixIndex[newId-1] = id;
        }
    }
}
//--------------------------------------------------------
void StEmcDecoder::SetDateTime(unsigned int date, unsigned int time)
{
    this->Init(date, time);
}
//--------------------------------------------------------
bool StEmcDecoder::GetFixTowerMapBug(void) const
{
    return this->fixTowerMap;
}
//--------------------------------------------------------
void StEmcDecoder::SetFixTowerMapBug(bool fix)
{
    this->fixTowerMap = fix;
}
//--------------------------------------------------------
/*!
Init method - should be called from constructor
this method initializes the variables used in decoder in agreement with the
time stamp.
*/
void StEmcDecoder::Init(unsigned int date,unsigned int time)
{
    ////////////////////////////////////////////////////////////////////////
    // these vectors are for tower decoding ////////////////////////////////

    // initial position in the crate
    int Init_Crate_tmp[]={2260,2420,2580,2740,2900,3060,3220,3380,3540,3700,
                          3860,4020,4180,4340,4500,
                          2180,2020,1860,1700,1540,
                          1380,1220,1060,900,740,580,420,260,100,2340};
    for(int i=0;i<30;i++)
        Init_Crate[i]=Init_Crate_tmp[i];

    // which crate is connected to each TDC channel. See log book for details
    if(date <= 20011223)
    {
        int TDC_Crate_tmp[]= {18,17,16,30,29,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                              19,20,21,22,23,24,25,26,27};
        for(int i=0;i<30;i++)
            TDC_Crate[i]=TDC_Crate_tmp[i];
        goto FIXBUG;
    }
    if(date == 20011224 && time <=163000)
    {
        int TDC_Crate_tmp[]= {18,17,16,30,29,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                              19,20,21,22,23,24,25,26,27};
        for(int i=0;i<30;i++)
            TDC_Crate[i]=TDC_Crate_tmp[i];
        goto FIXBUG;
    }
    if(date == 20011224 && time >163000)
    {
        int TDC_Crate_tmp[]= {18,17,16,29,30,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                              19,20,21,22,23,24,25,26,27};
        for(int i=0;i<30;i++)
            TDC_Crate[i]=TDC_Crate_tmp[i];
        goto FIXBUG;
    }
    if(date == 20011225 && time <=073000)
    {
        int TDC_Crate_tmp[]= {18,17,16,29,30,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                              19,20,21,22,23,24,25,26,27};
        for(int i=0;i<30;i++)
            TDC_Crate[i]=TDC_Crate_tmp[i];
        goto FIXBUG;
    }
    if(date == 20011225 && time >073000)
    {
        int TDC_Crate_tmp[]= {18,17,16,30,29,28,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                              19,20,21,22,23,24,25,26,27};
        for(int i=0;i<30;i++)
            TDC_Crate[i]=TDC_Crate_tmp[i];
        goto FIXBUG;
    }
    if(date >= 20011226 && date < 20030701) // year 2002/2003 pp and dAu runs
    {
        int TDC_Crate_tmp[]= {18,17,16,30,29,28,27,26,25,24,23,22,21,20,19,1,2,3,4,
                              5,6,7,8,9,10,11,12,13,14,15};
        for(int i=0;i<30;i++)
            TDC_Crate[i]=TDC_Crate_tmp[i];
        goto FIXBUG;
    }
    if(date >= 20030701) // year 2004 AuAu and pp runs
    {
        int TDC_Crate_tmp[]= {18,17,16,30,29,28,27,26,25,24,23,22,21,20,19,
                              1,15,14,13,12,11,10,9,8,7,6,5,4,3,2};
        for(int i=0;i<30;i++)
            TDC_Crate[i]=TDC_Crate_tmp[i];
        goto FIXBUG;
    }

    ///////////////////////////////////////////////////////////////////////
    // these tables fixes the tower cabling misconnection for some towers

FIXBUG:
    for(int i=0;i<4800;i++)
        TowerBugFixIndex[i] = i+1;
    if(date >= 20040101 && date < 20050101)
    {
#include "TowerBug2004.txt"

    }
    if(date >= 20050101 && date < 20060101)
    {
#include "TowerBug2005.txt"

    }
    if(date >= 20060101 )
    {
#include "TowerBug2006.txt"
        fixTowerMap = true; // map is always corrected for  2006 and above
    }
    fixTowerBugIndexes();


    ///////////////////////////////////////////////////////////////////////
    // these tables are for SMD decoding //////////////////////////////////

    // which module is connected to each RDO and crate board
/*SMD:*/
    if(date <= 20011201)  // SMD modules connection before this date
    {
        int SmdModules_tmp[8][15]={
                                      {46,47,48,49,50,51,52,53,54,55,56,57,58,59,60},                //RDO 0
                                      {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15},                         //RDO 1
                                      {16,17,18,19,20,21,22,23,24,25,26,27,28,29,30},                //RDO 2
                                      {31,32,33,34,35,36,37,38,39,40,41,42,43,44,45},                //RDO 3
                                      {61,62,63,64,65,66,67,68,69,70,71,72,73,74,75},                //RDO 4
                                      {76,77,78,79,80,81,82,83,84,85,86,87,88,89,90},                //RDO 5
                                      {91,92,93,94,95,96,97,98,99,100,101,102,103,104,105},          //RDO 6
                                      {106,107,108,109,110,111,112,113,114,115,116,117,118,119,120}  //RDO 7
                                  };
        for(int i=0;i<8;i++)
            for(int j=0;j<15;j++)
                SmdModules[i][j]=SmdModules_tmp[i][j];
        goto FEE;
    }

    if(date >= 20011202 && date <20021001) // SMD modules connection after this date
    {
        int SmdModules_tmp[8][15]={
                                      {49,54,48,46,50,51,52,53,47,55,56,57,58,59,60},                //RDO 0
                                      {11,2,10,12,5,6,7,8,13,3,1,14,15,4,9},                         //RDO 1
                                      {16,17,18,19,20,21,22,23,24,25,26,27,28,29,30},                //RDO 2
                                      {31,32,33,34,35,36,37,38,39,40,41,42,43,44,45},                //RDO 3
                                      {61,62,63,64,65,66,67,68,69,70,71,72,73,74,75},                //RDO 4
                                      {76,77,78,79,80,81,82,83,84,85,86,87,88,89,90},                //RDO 5
                                      {91,92,93,94,95,96,97,98,99,100,101,102,103,104,105},          //RDO 6
                                      {106,107,108,109,110,111,112,113,114,115,116,117,118,119,120}  //RDO 7
                                  };
        for(int i=0;i<8;i++)
            for(int j=0;j<15;j++)
                SmdModules[i][j]=SmdModules_tmp[i][j];
        goto FEE;
    }

    if(date >= 20021001) // year 2002/2003 pp and dAu runs
    {
        int SmdModules_tmp[8][15]={
                                      {46,47,48,49,50,51,52,53,54,55,56,57,58,59,60},                //RDO 0
                                      {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15},                         //RDO 1
                                      {31,32,33,34,35,36,37,38,39,40,41,42,43,44,45},                //RDO 2
                                      {16,17,18,19,20,21,22,23,24,25,26,27,28,29,30},                //RDO 3
                                      {61,62,63,64,65,66,67,68,69,70,71,72,73,74,75},                //RDO 4
                                      {76,77,78,79,80,81,82,83,84,85,86,87,88,89,90},                //RDO 5
                                      {91,92,93,94,95,96,97,98,99,100,101,102,103,104,105},          //RDO 6
                                      {106,107,108,109,110,111,112,113,114,115,116,117,118,119,120}  //RDO 7
                                  };
        for(int i=0;i<8;i++)
            for(int j=0;j<15;j++)
                SmdModules[i][j]=SmdModules_tmp[i][j];
        goto FEE;
    }

FEE:
    int FEE1_tmp[4]={1,4,3,2};
    int FEE2_tmp[4]={2,1,4,3};
    int FEE3_tmp[4]={3,2,1,4};
    for(int i=0;i<4;i++)
    {
        FEE1[i]=FEE1_tmp[i];
        FEE2[i]=FEE2_tmp[i];
        FEE3[i]=FEE3_tmp[i];
    }
    int connector1_tmp[20]={1,1,2,3,4,4,5,6,7,7,8,9,10,10,11,12,13,13,14,15};
    int connector2_tmp[20]={1,2,2,3,4,5,5,6,7,8,8,9,10,11,11,12,13,14,14,15};
    int connector3_tmp[20]={1,2,3,3,4,5,6,6,7,8,9,9,10,11,12,12,13,14,15,15};
    for(int i=0;i<20;i++)
    {
        connector1[i]=connector1_tmp[i];
        connector2[i]=connector2_tmp[i];
        connector3[i]=connector3_tmp[i];
    }

    ///////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////
    // these tables are for PSD decoding //////////////////////////////////

    // which PMT Box is connected to each RDO and crate board
    if(date >= 0 && date < 20060101) // year 2002/2003 pp and dAu runs
    {
        int PsdModules_tmp[4][15]={
                                      {11,12,13,14,15,16,17,18,19,20,21,22,23,24,25},                //RDO 8
                                      {1,2,3,4,5,6,7,8,9,10,26,27,28,29,30},                         //RDO 9
                                      {31,32,33,34,35,36,37,38,39,40,41,42,43,44,45},                //RDO 10
                                      {46,47,48,49,50,51,52,53,54,55,56,57,58,59,60}                 //RDO 11
                                  };               
        for(int i=0;i<4;i++)
            for(int j=0;j<15;j++)
                PsdModules[i][j]=PsdModules_tmp[i][j];
        goto PSDTables;
    }
    if(date >= 20060101) // year 2006
    {
        int PsdModules_tmp[4][15]={
                                      {11,12,13,14,15,16,17,18,19,20,21,22,23,24,25},                //RDO 8
                                      {26,27,28,29,30,1,2,3,4,5,6,7,8,9,10},                         //RDO 9
                                      {54,55,56,57,58,59,60,31,32,33,34,35,36,37,38},                //RDO 10
                                      {39,40,41,42,43,44,45,46,47,48,49,50,51,52,53}                 //RDO 11
                                  };               
        for(int i=0;i<4;i++)
            for(int j=0;j<15;j++)
                PsdModules[i][j]=PsdModules_tmp[i][j];
        goto PSDTables;
    }

PSDTables:
    if(date < 20040801 ) // this is the WRONG PSD MAP. I is kept while production is runnning but will be removed after it is done
    {
        int PsdOffset_tmp[40] = {20,21,22,23,0,1,2,3,24,25,26,27,4,5,6,7,28,29,30,31,
                                 8,9,10,11,32,33,34,35,12,13,14,15,36,37,38,39,16,17,18,19};
        for(int i=0;i<40;i++)
            PsdOffset[i] = PsdOffset_tmp[i];
    }
    else
    {
        int PsdOffset_tmp[40] = {36,37,38,39,16,17,18,19,32,33,34,35,12,13,14,15,28,29,30,31,
                                 8,9,10,11,24,25,26,27,4,5,6,7,20,21,22,23,0,1,2,3};
        for(int i=0;i<40;i++)
            PsdOffset[i] = PsdOffset_tmp[i];
    }
    int PsdStart_tmp[60] = {2261,2181,2101,2021,1941,1861,1781,1701,1621,1541,1461,1381,1301,1221,1141,1061,
                            981,901,821,741,661,581,501,421,341,261,181,101,21,2341,
                            4661,4741,2421,2501,2581,2661,2741,2822,2901,2981,3061,3141,3221,3301,3381,3461,
                            3541,3621,3701,3782,3861,3941,4021,4101,4181,4261,4341,4421,4501,4581};
    for(int i=0;i<60;i++)
        PsdStart[i] = PsdStart_tmp[i];

    //fix preshower softIds based on 2006/7 mapping studies
    for(int i=0; i<4800; i++) {
        PreshowerBugFixIndex[i] = i+1;
    }
    if(date >= 20060101) //use mapping determined from Run 7 for Run 6 data as well
    {
        #include "PreshowerBug2007.txt"
        if(date >= 20080101) fixPreshowerMap = true;
    }
    fixPreshowerBugIndexes();
    
    ///////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////
    // these tables are for TRIGGER decoding //////////////////////////////

    if(date>=0)
    {
        int TriggerPatch_tmp[30] = {180,170,160,150,290,
                                    280,270,260,250,240,
                                    230,220,210,200,190,
                                    30, 20, 10,  0,140,
                                    130,120,110,100, 90,
                                    80, 70, 60, 50, 40};
        //    int TriggerPatch_tmp[30] = {150,160,170,180,190,
        //                                200,210,220,230,240,
        //                                250,260,270,280,290,
        //                                 30, 20, 10,  0,140,
        //                                130,120,110,100, 90,
        //                                 80, 70, 60, 50, 40};
        int TriggerSequence_tmp[10] = {0,1,2,3,4,5,6,7,8,9};
        for(int i=0;i<30;i++)
            TriggerPatch[i] = TriggerPatch_tmp[i];
        for(int i=0;i<10;i++)
            TriggerSequence[i] = TriggerSequence_tmp[i];

        int JPSTART[12] =           {0,30,50,80,100,130,150,180,200,230,250,280};
        int JPEXTRA[12][5] =       {{21, 23, 25, 27, 29},
                                    {20, 22, 24, 26, 28},
                                    {71, 73, 75, 77, 79},
                                    {70, 72, 74, 76, 78},
                                    {121, 123, 125, 127, 129},
                                    {120, 122, 124, 126, 128},
                                    {170, 172, 174, 176, 178},
                                    {171, 173, 175, 177, 179},
                                    {220, 222, 224, 226, 228},
                                    {221, 223, 225, 227, 229},
                                    {270, 272, 274, 276, 278},
									{271, 273, 275, 277, 279}};
		
        for(int jetPatch = 0;jetPatch < 12;jetPatch++)
        {
            for(int seq = 0;seq < 25;seq++)
            {
                int triggerPatch;
                if ((jetPatch % 2) == 0)
                {
                    triggerPatch = (seq < 20) ? (JPSTART[jetPatch] + seq) : (JPEXTRA[jetPatch][seq - 20]);
                }
                else
                {
                    triggerPatch = (seq < 5) ? (JPEXTRA[jetPatch][seq]) : (JPSTART[jetPatch] + seq - 5);
                }
                JetPatchFromTriggerPatch[triggerPatch] = jetPatch;
                JetPatchSeqFromTriggerPatch[triggerPatch] = seq;
                TriggerPatchFromJetPatchAndSeq[(jetPatch * 25) + seq] = triggerPatch;
            }
        }

    }


    // reverse order for tower
    for(int i=0;i<30;i++)
        Crate_TDC[TDC_Crate[i]-1]=i;
    for(int RDO=0;RDO<4800;RDO++)
    {
        int id=0;
        if(GetTowerIdFromDaqId(RDO,id)==1)
            if(id>0)
                ReverseOrder[id-1]=RDO;
    }

    // reverse order for SMD
    int det,m,e,s;
    for(int RDO=0;RDO<8;RDO++)
        for(int index=0;index<4800;index++)
        {
            int status=GetSmdCoord(RDO,index,det,m,e,s);
            if(status==1 && m>0 && e>0 && s>0)
            {
                if(det==3)
                {
                    SmdeRDO[m-1][e-1]=RDO;
                    SmdeIndex[m-1][e-1]=index;
                }
                if(det==4)
                {
                    SmdpRDO[m-1][e-1][s-1]=RDO;
                    SmdpIndex[m-1][e-1][s-1]=index;
                }
            }
        }

    // reverse order for PSD
    int id;
    for(int RDO=0;RDO<4;RDO++)
        for(int index=0;index<4800;index++)
        {
            int status=GetPsdId(RDO,index,id);
            if (id>0 && id<4801)
            {
                if(status==1)
                {
                    PsdRDO[id-1] = RDO;
                    PsdIndex[id-1]=index;
                }
                else
                {
                    PsdRDO[id-1]  = -1;
                    PsdIndex[id-1]= -1;
                }
            }
        }

    return;
}
//--------------------------------------------------------
/*!
GetTowerBugCorrectionShift method - returns the id shift with respect to
the tower index (software) in the original map
\param id_original is the soft_id in the original map
\param id_shift is the shift that should be applied to the id. In this case, id_corrected = Id_original+id_shift
*/
int StEmcDecoder::GetTowerBugCorrectionShift(int id_original,int& id_shift) const
{
    int id_new = TowerBugFixIndex[id_original - 1];
    id_shift = id_new-id_original;
    return 1;
}
//--------------------------------------------------------
/*!
GetPreshowerBugCorrectionShift method - returns the id shift with respect to
the preshower index (software) in the original map
\param id_original is the soft_id in the original map
\param id_shift is the shift that should be applied to the id. In this case, id_corrected = Id_original+id_shift
*/
int StEmcDecoder::GetPreshowerBugCorrectionShift(int id_original,int& id_shift) const
{
    int id_new = PreshowerBugFixIndex[id_original - 1];
    id_shift = id_new-id_original;
    return 1;
}
//--------------------------------------------------------
/*!
Copy of StEmcGeom version
\param TowerId is the software id for towers
\param module is the module number
\param eta is the eta division for towers
\param sub is the sub division for towers
*/
int StEmcDecoder::GetTowerBin(const int TowerId,int &module,int &eta,int &sub) const
{
    int rid=TowerId;

    if(rid<1 || rid>4800)
        return 0;

    int idw;
    module   = (rid - 1) / 40 + 1; // Module number
    idw = (rid - 1) % 40;     // change from 0 to 39
    sub   = idw/20 + 1;
    eta   = idw%20+1;
    //  e   = 20 - idw%20;
    return 1;                   // zero is bad
}
//--------------------------------------------------------
/*!
\param TDC is the TDC channel number
\param crate is the crate number
*/
int StEmcDecoder::GetTowerCrateFromTDC(int TDC, int& crate) const
{
    if(TDC>=0 && TDC<30)
    {
        crate = TDC_Crate[TDC];
        return 1;
    }
    return 0;
}
//--------------------------------------------------------
/*!
\param crate is the crate number
\param TDC is the TDC channel number
*/
int StEmcDecoder::GetTowerTDCFromCrate(int crate, int& TDC) const
{
    if(crate>0 && crate<31)
    {
        TDC = Crate_TDC[crate-1];
        return 1;
    }
    return 0;
}
//--------------------------------------------------------
/*!
\param RDO is the DAQ id
\param TDC is the TDC channel number
*/
int StEmcDecoder::GetTowerTDCFromDaqId(int RDO, int& TDC) const
{
    if(RDO>=0 && RDO<=4799)
    {
        TDC=RDO%30;
        return 1;
    }
    return 0;
}
//--------------------------------------------------------
/*!
\param RDO is the DAQ id
\param crate is the crate number
\param crate_sequency is the position of the tower inside the crate
*/
int StEmcDecoder::GetTowerCrateFromDaqId(int RDO, int& crate, int& crate_sequency) const
{
    int tdc=RDO%30;
    crate_sequency=RDO/30;
    if(GetTowerCrateFromTDC(tdc, crate)==1)
        return 1; // 1 is ok
    return 0;
}
//--------------------------------------------------------
int StEmcDecoder::Getjose_towerWest(int start,int crate_seq) const
{
    int card=crate_seq/32;
    int card_seq=31-(crate_seq%32);
    int channel_seq=card_seq/4;
    int channel=card_seq-(channel_seq*4)+1;
    int jose_tower=start+channel_seq*20+card*4+(5-channel);
    if(jose_tower>2400)
        jose_tower-=2400;
    return jose_tower;
}
//--------------------------------------------------------
int StEmcDecoder::Getjose_towerEast(int start,int crate_seq) const
{
    int card=crate_seq/32;
    int card_seq=31-(crate_seq%32);
    int channel_seq=card_seq/4;
    int channel=card_seq-(channel_seq*4)+1;
    int jose_tower=start+channel_seq*20+card*4+(5-channel);
    if(jose_tower<=2400)
        jose_tower+=2400;
    return jose_tower;
}
//--------------------------------------------------------
/*!
\param RDO is the DAQ id
\param TowerId is the software id for towers
*/
int StEmcDecoder::GetTowerIdFromDaqId(int RDO,int& TowerId) const
{
    //RDO from 0 to 4799
    if(RDO<0 || RDO>4799)
        return 0;  //0 is bad

    int crate_seq=0;
    int Crate;
    GetTowerCrateFromDaqId(RDO,Crate,crate_seq);
    return GetTowerIdFromCrate(Crate,crate_seq,TowerId);
}
//--------------------------------------------------------
/*!
\param TowerId is the software id for towers
\param RDO is the DAQ id
*/
int StEmcDecoder::GetDaqIdFromTowerId(int TowerId,int& RDO) const
{
    if(TowerId<1 || TowerId >4800)
        return 0; // 0 is bad

    RDO = ReverseOrder[TowerId-1];

    return 1;
}
//--------------------------------------------------------
/*!
\param crate is the crate number
\param crate_sequency is the position of the tower inside the crate
\param TowerId is the software id for towers
*/
int StEmcDecoder::GetTowerIdFromCrate(int crate,int crate_sequency, int& TowerId) const
{
    if(crate>15 && crate<31)
    {
        int start=Init_Crate[crate-1];
        TowerId=Getjose_towerWest(start,crate_sequency);
        // this is to fix the tower connection for some towers
        if(fixTowerMap)
            TowerId=TowerBugFixIndex[TowerId-1];
        return 1;
    }
    if(crate>0 && crate<16)
    {
        int start=Init_Crate[crate-1];
        TowerId=Getjose_towerEast(start,crate_sequency);
        // this is to fix the tower connection for some towers
        if(fixTowerMap)
            TowerId=TowerBugFixIndex[TowerId-1];
        return 1;
    }
    return 0;
}
//--------------------------------------------------------
/*!
\param TDC is the crate number
\param tdc_sequency is the position of the tower inside the crate
\param TowerId is the software id for towers
*/
int StEmcDecoder::GetTowerIdFromTDC(int TDC,int tdc_sequency, int& TowerId) const
{

    int Crate;
    int crate_seq=tdc_sequency;
    if(GetTowerCrateFromTDC(TDC, Crate)==0)
        return 0;
    return GetTowerIdFromCrate(Crate,crate_seq,TowerId);
}
//--------------------------------------------------------
/*!
\param CRATE is the crate number
\param crate_seq is the position of the tower inside the crate (0 <= crate_seq <=159)
\param patchId is the software id for towers
*/
int StEmcDecoder::GetTriggerPatchFromCrate(int CRATE,int crate_seq, int& patchId) const
{
    if(CRATE<1 || CRATE>30)
        return 0;
    int S = TriggerPatch[CRATE-1];
    int N = crate_seq/16;
    patchId = S+TriggerSequence[N];
    return 1;
}
//--------------------------------------------------------
/*!
\param PATCH is the software id for towers
\param CRATE is the crate number
\param crate_seq is the position of the tower inside the crate (0 <= crate_seq <=159)
*/
int StEmcDecoder::GetCrateAndSequenceFromTriggerPatch(int PATCH,int& CRATE,int& crate_seq) const
{
    if(PATCH<0 || PATCH>299)
        return 0;
    int P = PATCH/10;
    P*=10;
    for(int i=0;i<30;i++)
        if(TriggerPatch[i]==P)
        {
            CRATE = i+1;
            break;
        }
    int S = PATCH%10;
    for(int i=0;i<10;i++)
        if(TriggerSequence[i]==S)
        {
            crate_seq = i*16;
            break;
        }
    return 1;
}
//--------------------------------------------------------
/*!
\param jetPatch is the jet patch id (0 <= jetPatch <= 11)
\param jetPatch_seq is the trigger patch position within the jet patch (0 <= jetPatch_seq <= 24)
\param triggerPatch is the trigger patch id (0 <= triggerPatch <=299)
*/
int StEmcDecoder::GetTriggerPatchFromJetPatch(int jetPatch, int jetPatch_seq, int &triggerPatch) const
{
    if (jetPatch<0 || jetPatch>=12 || jetPatch_seq<0 || jetPatch_seq>=25)
        return 0;
    triggerPatch = TriggerPatchFromJetPatchAndSeq[(jetPatch * 25) + jetPatch_seq];
    return 1;
}
//--------------------------------------------------------
/*!
\param jetPatch is the jet patch id (0 <= jetPatch <= 11)
\param jetPatch_seq is the trigger patch position within the jet patch (0 <= jetPatch_seq <= 24)
\param triggerPatch is the trigger patch id (0 <= triggerPatch <=299)
*/
int StEmcDecoder::GetJetPatchAndSequenceFromTriggerPatch(int triggerPatch, int &jetPatch, int &jetPatch_seq) const
{
    if (triggerPatch<0 || triggerPatch>=300)
        return 0;
    jetPatch = JetPatchFromTriggerPatch[triggerPatch];
    jetPatch_seq = JetPatchSeqFromTriggerPatch[triggerPatch];
    return 1;
}

int StEmcDecoder::GetDSMFromTriggerPatch(int patchId, int &dsmModule) const {
    if(patchId < 0 || patchId >= 300) return 0;
    
    dsmModule = patchId/10;
    return 1;
}

int StEmcDecoder::GetTriggerPatchesFromDSM(int dsmModule, int *triggerPatches) const {
    if(dsmModule < 0 || dsmModule >= 30) return 0;
    
    for(int i=0; i<10; i++) {
        triggerPatches[i] = dsmModule*10 + i;
    }
    
    return 1;
}

//--------------------------------------------------------
/*!
\param detector is detector number (3 = SMDE, 4 = SMDP)
\param module is the module number
\param eta is the eta division for towers
\param sub is the sub division for towers
\param RDO is the SMD fiber number
\param index is the position in the fiber
*/
int StEmcDecoder::GetSmdRDO(int detector,int module, int eta, int sub, int& RDO, int& index) const
{
    if(module<1 || module >120)
        return 0;
    if(detector==3)
    {
        if(eta<1 || eta>150)
            return 0;
        RDO=SmdeRDO[module-1][eta-1];
        index=SmdeIndex[module-1][eta-1];
        return 1;
    }
    if(detector==4)
    {
        if(eta<1 || eta>10)
            return 0;
        if(sub<1 || sub>15)
            return 0;
        RDO=SmdpRDO[module-1][eta-1][sub-1];
        index=SmdpIndex[module-1][eta-1][sub-1];
        return 1;
    }
    return 0;
}
//--------------------------------------------------------
/*!
\param RDO is the SMD fiber number
\param index is the position in the fiber
\param detector is detector number (3 = SMDE, 4 = SMDP)
\param module is the module number
\param eta is the eta division for towers
\param sub is the sub division for towers
\param print enables/disables some printout information
*/
int StEmcDecoder::GetSmdCoord(int RDO,int index, int& detector, int& module,int& eta,int& sub,bool print) const
{
    int wire, A_value;
    return GetSmdCoord(RDO,index,detector,module,eta,sub,wire,A_value,print);
}
//--------------------------------------------------------
/*!
\param RDO is the SMD fiber number
\param index is the position in the fiber
\param detector is detector number (3 = SMDE, 4 = SMDP)
\param module is the module number
\param eta is the eta division for towers
\param sub is the sub division for towers
\param wire is the SMD-wire number
\param A_value is the A_value in the FEE card
\param print enables/disables some printout information
*/
int StEmcDecoder::GetSmdCoord(int RDO,int index, int& detector, int& module,int& eta,int& sub,int& wire, int& A_value,bool print) const
{

    if(RDO<0 || RDO>7)
        return 0;
    if(index <0 || index >4799)
        return 0;

    char line[300];

    if(print)
        sprintf(line,"%2d    %4d    ",RDO,index);

    int daq_smd=index;

    int category=daq_smd/1600;
    wire=(daq_smd-category*1600)/20+1;
    int A_step=daq_smd%4;
    int S_step=daq_smd%20;
    A_value=0;
    int S_value=0;
    int SCA = S_step/5+1;

    if(print)
        sprintf(line,"%s%3d    %3d    %4d    %3d    Sca%1d    ",line,category,wire,A_step,S_step,SCA);

    if(category==0)
    {
        A_value=FEE1[A_step];
        S_value=connector1[S_step];
    }

    if(category==1)
    {
        A_value=FEE2[A_step];
        S_value=connector2[S_step];
    }

    if(category==2)
    {
        A_value=FEE3[A_step];
        S_value=connector3[S_step];
    }

    if(print)
        sprintf(line,"%sA%1d    S%02d    ",line,A_value,S_value);

    //detector no
    int det=0;
    int half=0;

    if(A_value==1)
    {
        det=3;
        half=2;
    }
    if(A_value==2)
    {
        det=4;
        half=1;
    }
    if(A_value==3)
    {
        det=4;
        half=2;
    }
    if(A_value==4)
    {
        det=3;
        half=1;
    }
    if(print)
        sprintf(line,"%s%2d    %2d    ",line,det,half);

    int mod=0;
    int mod_stat=getSmdModule(RDO, S_value, mod);
    int pin=0;
    if(mod_stat)
    {
        //Get strip no
        int dummy=checkDummy(wire);
        if(print)
            sprintf(line,"%s%3d    %1d    ",line,mod,dummy);
        int stat=getSmdPin(det,half,wire,pin);
        if(stat)
        {/*nothing*/
        }
        if(dummy==0)
        {
            /*cout <<"RDO = "<<RDO
                 <<"  idx = "<<index
                 <<"  AS = "<<A_step
                 <<"  SS = "<<S_step
                 <<"  A = "<<A_value
                 <<"  S = "<<S_value
                 <<"  cat = "<<category
                 <<"  wire = "<<wire
                 <<"  det = "<<det
                 <<"  half = "<<half
                 <<"  pin = "<<pin;*/

            detector = det;
            module = mod;
            if(detector == 3) // bsmde
            {
                eta = 151-pin;
                sub = 1;
                //cout <<" m = "<<module<<"  e = "<<eta <<"  s = "<<sub;
            }
            if(detector == 4) // bsmdp
            {
                int stat=getSmdpStrip(pin,eta,sub);
                if(stat!=1)
                    return 0;
                //cout <<" m = "<<module<<"  e = "<<eta <<"  s = "<<sub;
            }
            if(print)
                sprintf(line,"%s%3d    %3d    %2d",line,pin,eta,sub);
            if(print)
                cout<<line<<endl;
            return 1;
        }
        if(print)
            sprintf(line,"%s%3d    %3d    %2d",line,pin,0,0);
        if(print)
            cout <<line<<endl;
    }
    return 0;
}
//--------------------------------------------------
/*!
\param RDO is the SMD fiber number
\param S_value is the connector in the SMD crate
\param module is the module number
*/
int StEmcDecoder::getSmdModule(int RDO, int S_value,int& module) const

{
    module=SmdModules[RDO][S_value-1];
    return 1;
}
//--------------------------------------------------
/*!
\param fiberno is the fiber number in each SMD crate connector
*/
int StEmcDecoder::checkDummy(int fiberno) const
{
    int dummy=0;
    if(fiberno==1 || fiberno==17 || fiberno==33 || fiberno==49 || fiberno==65)
    {
        dummy=1;
    }
    else
    {
        dummy=0;
    }
    return dummy;
}
//--------------------------------------------------
/*!
\param detector is detector number (3 = SMDE, 4 = SMDP)
\param half is a internal variable
\param fiberno is the fiber number in each SMD crate connector
\param pin is the pin number
*/
int StEmcDecoder::getSmdPin(int detector,int half,int fiberno,int& pin) const

{
    int gap=(fiberno-1)/16;
    if(half==2)
    {
        pin=(fiberno-1)*2 - gap*2-1;
    }
    if(half==1)
    {
        pin=(fiberno-(gap+1))*2;
    }
    return 1;
}
//--------------------------------------------------
/*!
Converts the pin number in eta and sub divisions for SMDP
\param pin is the pin number
\param eta is the eta division for towers
\param sub is the sub division for towers
*/
int StEmcDecoder::getSmdpStrip(int pin,int& eta,int& sub) const
{
    eta=(pin-1)%10+1;
    sub=15-(pin-1)/10;
    return 1; // 0 is bad
}
//--------------------------------------------------------
/*!
\param RDO is the PSD fiber number
\param index is the position in the fiber
\param id is the software Id
\param print enables/disables some printout information
 
The Pre Shower and SMD crates are identical and they share almost
the same decoder.
*/
int StEmcDecoder::GetPsdId(int RDO,int index, int& id,bool print) const
{
    int wire, A_value, PMT;
    return GetPsdId(RDO,index,id,PMT,wire,A_value,print);
}
//--------------------------------------------------------
/*!
\param RDO is the PSD fiber number
\param index is the position in the fiber
\param id is the software Id
\param PMT is the PMT box number
\param wire is the equivalent mux-wire in the SMD/PSD crate
\param A_value is the A_value in the FEE card
\param print enables/disables some printout information
 
The Pre Shower and SMD crates are identical and they share almost
the same decoder.
*/
int StEmcDecoder::GetPsdId(int RDO,int index, int& id, int& PMTBox, int& wire, int& A_value,bool print) const
{
    id=0;
    if(RDO<0 || RDO>3)
        return 0;
    if(index <0 || index >4799)
        return 0;
    char line[300];

    if(print)
        sprintf(line,"RDO=%1d  index=%4d",RDO,index);

    int daq_smd=index;

    int category=daq_smd/1600;
    wire=(daq_smd-category*1600)/20+1;
    int A_step=daq_smd%4;
    int S_step=daq_smd%20;
    A_value=0;
    int S_value=0;
    int SCA = S_step/5+1;
    PMTBox = 0;

    if(print)
        sprintf(line,"%s cat=%2d  wire=%3d  As=%4d  Ss=%3d  SCA=%1d",line,category,wire,A_step,S_step,SCA);

    if(category==0)
    {
        A_value=FEE1[A_step];
        S_value=connector1[S_step];
    }
    if(category==1)
    {
        A_value=FEE2[A_step];
        S_value=connector2[S_step];
    }
    if(category==2)
    {
        A_value=FEE3[A_step];
        S_value=connector3[S_step];
    }

    if(print)
        sprintf(line,"%s  Slot=%02d  A%1d",line,S_value,A_value);

    if(A_value==3 || A_value==4 || wire>40)
    {
        if(print)
            cout<<line<<endl;
        return 0;
    }
    int half;
    if(A_value==1)
        half=40;
    else if(A_value==2)
        half=0;
	else {
		LOG_WARN << "Bad A_value -- " << A_value << " -- result for PSD softId is wrong" << endm;
		half = -10000;
	}

    PMTBox = PsdModules[RDO][S_value-1];
    int start  = PsdStart[PMTBox-1];
    int offset = PsdOffset[wire-1];

    id = start+offset+half;

    if(PMTBox==30 && id>2400)
        id-=2400;
    if(PMTBox==32 && id>4800)
        id-=2400;
        
    if(fixPreshowerMap) id = PreshowerBugFixIndex[id-1];
    
    if(print)
        sprintf(line,"%s  PMTB=%2d  start=%4d  offset=%2d  half=%2d  SoftId=%4d",line,PMTBox,start,offset,half,id);
    if(print)
        cout<<line<<endl;

    return 1;
}
//--------------------------------------------------------
/*!
\param id is the software Id
\param RDO is the PSD fiber number
\param index is the position in the fiber
*/
int StEmcDecoder::GetPsdRDO(int id, int& RDO,int& index) const
{
    RDO=0;
    index=0;
    if(id<1 || id>4800)
        return 0;
        
    if(fixPreshowerMap) id = PreshowerBugFixIndex[id-1];
    
    RDO = PsdRDO[id-1];
    index = PsdIndex[id-1];
    return 1;
}
//--------------------------------------------------
void StEmcDecoder::PrintTowerMap(ofstream *out) const
{
    *out <<"TDC channels connections\n";
    *out <<"-----------------------------------------------------------\n";
    for(int i=0;i<30;i++)
        *out <<"  TDC channel "<<i<<" connected to crate "<<TDC_Crate[i]<<endl;
    *out <<endl;
    *out <<"Tower MAP\n";
    *out <<"-----------------------------------------------------------\n";
    for(int daq =0;daq<4800;daq++)
    {
        int towerId,tdc,crate,position,m,e,s;
        GetTowerIdFromDaqId(daq,towerId);
        GetTowerTDCFromDaqId(daq,tdc);
        GetTowerCrateFromDaqId(daq,crate,position);
        GetTowerBin(towerId,m,e,s);
        *out <<"  daq id = "<<daq<<"  TDC channel = "<<tdc
        <<"  Crate = "<<crate<<"  position in crate = "<<position
        <<"  software id = "<<towerId<<"  m = "<<m<<"  e = "<<e<<"  s = "<<s<<endl;
    }
    *out <<endl;
}
//--------------------------------------------------
void StEmcDecoder::PrintSmdMap(ofstream *out) const
{
    *out <<"Modules connected to SMD crate\n";
    *out <<"-----------------------------------------------------------\n";

    for(int i=0;i<8;i++)
    {
        *out <<"SMD CRATE number "<<i+1<<endl;
        for(int j=0;j<15;j++)
            *out <<"  channel "<<j<<" is connected to module "<<SmdModules[i][j]<<endl;
    }
    *out <<"\nSMD MAP\n";
    *out <<"-----------------------------------------------------------\n";

    for(int i=0;i<8;i++)
    {
        *out <<"SMD CRATE number "<<i+1<<endl;
        for(int index =0;index<4800;index++)
        {
            int det,m,e,s;
            int status = GetSmdCoord(i,index,det,m,e,s);
            *out <<"  RDO = "<<i<<"  index = "<<index;
            if(status == 1)
                *out <<" detector = "<<det<<"  mod = "<<m<<"  eta = "<<e<<"  sub = "<<s<<endl;
            else
                *out <<"  dummy connection\n";
        }
    }
    *out <<endl;
}
//--------------------------------------------------
void StEmcDecoder::PrintPsdMap(ofstream *out) const
{
    *out <<"PMT Boxes connected to PSD crate\n";
    *out <<"-----------------------------------------------------------\n";

    for(int i=0;i<4;i++)
    {
        *out <<"PSD CRATE number "<<i+1<<endl;
        for(int j=0;j<15;j++)
            *out <<"  channel "<<j<<" is connected to PMT Box "<<PsdModules[i][j]<<endl;
    }
    *out <<"\nPSD MAP\n";
    *out <<"-----------------------------------------------------------\n";

    for(int i=0;i<4;i++)
    {
        *out <<"PSD CRATE number "<<i+1<<endl;
        for(int index =0;index<4800;index++)
        {
            int id;
            int status = GetPsdId(i,index,id);
            *out <<"  RDO = "<<i<<"  index = "<<index;
            if(status == 1)
                *out <<" id = "<<id<<endl;
            else
                *out <<"  dummy connection\n";
        }
    }
    *out <<endl;
}
//--------------------------------------------------
int StEmcDecoder::GetCrateFromTowerId(int softId, int &crate, int &sequence) const
{
	int RDO;
	if(GetDaqIdFromTowerId(softId,RDO)) {
		return GetTowerCrateFromDaqId(RDO,crate,sequence);
	}
	return 0;
}
//--------------------------------------------------
int StEmcDecoder::GetTDCFromTowerId(int softId, int &TDC) const
{
	int RDO;
	if(GetDaqIdFromTowerId(softId,RDO)) {
		return GetTowerTDCFromDaqId(RDO,TDC);
	}
	return 0;
}
//--------------------------------------------------
int StEmcDecoder::GetTriggerPatchFromTowerId(int softId, int &patchId) const
{
	int crate, sequence;
	if(GetCrateFromTowerId(softId,crate,sequence)) {
		return GetTriggerPatchFromCrate(crate,sequence,patchId);
	}
	return 0;
}
//--------------------------------------------------
int StEmcDecoder::GetJetPatchFromTowerId(int softId, int &jetPatch) const
{
	int triggerPatch, sequence;
	if(GetTriggerPatchFromTowerId(softId,triggerPatch)) {
		return GetJetPatchAndSequenceFromTriggerPatch(triggerPatch,jetPatch,sequence);
	}
	return 0;
}
//--------------------------------------------------
int StEmcDecoder::GetTowerIdFromBin(int m, int e, int s, int &softId) const
{
	if( (m<1) || (m>120) ) return 0;
	if( (e<1) || (e>20)  ) return 0;
	if( (s<1) || (s>2)   ) return 0;
	softId = 40*(m-1) + 20*(s-1) + e;
	return 1;
}

// $Id: StEmcDecoder.cxx,v 2.51 2007/10/09 18:02:24 kocolosk Exp $
//
// $Log: StEmcDecoder.cxx,v $
// Revision 2.51  2007/10/09 18:02:24  kocolosk
// two extra support functions for TP <=> DSM module mapping
//
// Revision 2.50  2007/09/11 13:30:13  kocolosk
// removed ClassImp that was left in by accident
//
// Revision 2.49  2007/09/11 02:41:37  kocolosk
// added code to fix preshower swaps in 2006 and beyond
//
// Revision 2.48  2007/08/07 19:44:07  perev
// Gene scalers added
//
// Revision 2.47  2007/06/01 17:47:41  jml
// Attempt to fix panitkin plot compile
//
// Revision 2.46  2007/04/09 23:35:13  kocolosk
// 2.45 didn't get it quite right ... thanks to Oleksandr who identified the correct fix for east side JP mapping
//
// Revision 2.45  2007/04/09 13:23:23  kocolosk
// fixed TP -> JP mapping in East barrel
//
// Revision 2.44  2007/04/04 17:35:11  kocolosk
// Added methods GetCrateFromTowerId, GetTDCFromTowerId, GetTDCFromTowerId, GetTriggerPatchFromTowerId, GetJetPatchFromTowerId, and GetTowerIdFromBin.  Also implemented const-correctness and used meaningful argument names in method declarations to improve readability.
//
