 /***************************************************************************
 *
 * $Id: StTriggerData2009.h,v 2.19 2012/04/30 15:19:11 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2009
 ***************************************************************************
 *
 * Description:  Concrete implementation of StTriggerData for 2009.
 *
 ***************************************************************************
 *
 * $Log: StTriggerData2009.h,v $
 * Revision 2.19  2012/04/30 15:19:11  ullrich
 * Added access function for l2sum (Akio)
 *
 * Revision 2.18  2011/02/15 18:56:09  ullrich
 * New access fct for ZDCSMD, new ZDCSMD map, spinBit() modified.
 *
 * Revision 2.17  2011/01/18 23:06:07  ullrich
 * New function mtdgemAtAddress added. vpdADC, vpdTDC, vpdADCHighThr, vpdTDCHighThr, vpdEarliestTDC, and vpdEarliestTDCHighThr updated.
 *
 * Revision 2.16  2010/06/01 22:27:30  ullrich
 * Reduce print-out for case when muDst is read.
 *
 * Revision 2.15  2010/04/07 14:43:00  ullrich
 * Added streamer and added new access function for BBC large tile earliest TAC and difference
 *
 * Revision 2.14  2010/01/13 17:55:47  ullrich
 * Better mErrorFlags, abort, and debug flag handling, updated MTD DSM access function for run10, clean up compiler warning messages.
 *
 * Revision 2.13  2010/01/08 22:44:37  ullrich
 * Updates needed to add StFmsCollection and related classes.
 *
 * Revision 2.12  2009/08/24 22:39:13  ullrich
 * Flag corruption in new member mErrorFlag.
 *
 * Revision 2.11  2009/08/14 15:06:10  ullrich
 * Added checks of trigger data banks.
 *
 * Revision 2.10  2009/06/16 15:44:26  ullrich
 * Added fmsADC() method.
 *
 * Revision 2.9  2009/05/15 18:16:15  ullrich
 * Updates for pp2pp and ToF.
 *
 * Revision 2.8  2009/05/05 20:53:16  ullrich
 * Updates for MTD.
 *
 * Revision 2.7  2009/03/19 02:46:01  ullrich
 * Add 2nd argument (pre/post) to vpdEarliestTDC().
 *
 * Revision 2.6  2009/03/04 02:01:30  ullrich
 * New access functions for ZDC DSM layer-1 and layer-2 data.
 *
 * Revision 2.5  2009/02/23 22:31:09  ullrich
 * Fixed problem when running over 2009 data (solution by Pibero) and new VPD access functions.
 *
 * Revision 2.4  2009/02/13 23:04:50  ullrich
 * Updates necessary for use in Online QA (P) plots.
 *
 * Revision 2.3  2009/02/11 23:33:55  jeromel
 * Modifications by Akio to support getDsm0_BEMCE and getDsm0_BEMCW as well as
 * getDsm1_BEMC. However, use of const=0 impose implementation (was not done
 * in years < 2009). Added methods with return 0.
 *
 * Revision 2.2  2009/01/20 18:10:15  ullrich
 * Bug fix and new ZDC access functions.
 *
 * Revision 2.1  2009/01/14 17:56:14  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#ifndef StTriggerData2009_hh
#define StTriggerData2009_hh

#include "StTriggerData.h"
#include "StDaqLib/TRG/trgStructures2009.h"

class StTriggerData2009 : public StTriggerData {
public:
    StTriggerData2009();
    StTriggerData2009(const TriggerDataBlk2009* data, int run);
    StTriggerData2009(const TriggerDataBlk2009* data, int run, int bs, int dbg=0);
    ~StTriggerData2009();
    
    void readData() {readData(0,0);}
    void readData(const TriggerDataBlk2009* data, int bs);
    void dump() const;  //dump data into text

    // Versison and data type information
    unsigned int version() const;           
    unsigned int numberOfPreXing() const;   
    unsigned int numberOfPostXing() const;  
    
    // Generic trigger informations
    unsigned int   eventNumber() const;
    unsigned int   token() const;
    unsigned int   triggerWord() const;
    unsigned int   actionWord() const;      
    unsigned short busyStatus() const;
    unsigned short dsmInput() const;
    unsigned short trgToken() const;
    unsigned short dsmAddress() const;
    unsigned short mAddBits() const;
    unsigned short bcData(int channel) const;
    
    // L2 offsets
    int  L2ResultsOffset(StL2AlgorithmId id) const;  
    bool isL2Triggered(StL2TriggerResultType id) const;
    unsigned long long l2sum() const;

    // Bunch and spin bits
    unsigned int bunchCounterHigh() const;
    unsigned int bunchCounterLow() const;
    unsigned int bunchId48Bit() const;
    unsigned int bunchId7Bit() const;
    unsigned int spinBit() const;
    unsigned int spinBitYellowFilled() const;
    unsigned int spinBitYellowUp() const;
    unsigned int spinBitYellowDown() const;
    unsigned int spinBitYellowUnpol() const;
    unsigned int spinBitBlueFilled() const;
    unsigned int spinBitBlueUp() const;
    unsigned int spinBitBlueDown() const;
    unsigned int spinBitBlueUnpol() const;
    
    // High Level Trigger info
    unsigned short tcuBits() const;
    unsigned short lastDSM(int address) const;
    unsigned short bemcLayer1DSM(int channel, int prepost=0) const;
    unsigned short eemcLayer1DSM(int channel, int prepost=0) const;
    unsigned short emcLayer2DSM(int channel) const;
    unsigned short fpdLayer1DSMRaw(StBeamDirection eastwest, int channel, int prepost=0) const;
    //    unsigned short fpdLayer1DSM(StBeamDirection eastwest, int module, int board, int prepost=0) const;
    unsigned short fpdLayer2DSMRaw(int channel) const;
    //  unsigned short fpdLayer2DSM(StBeamDirection eastwest, int module) const;
    
    // CTB
    //  unsigned short ctbRaw(int address, int prepost=0) const;
    //  unsigned short ctb(int pmt, int prepost=0) const;
    //  unsigned short ctbTraySlat(int tray, int slat, int prepost=0) const;    
    //  unsigned short ctbSum(int prepost=0) const;
    
    // BBC
    unsigned short bbcADC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short bbcTDC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short bbcADCSum(StBeamDirection eastwest, int prepost=0) const;
    unsigned short bbcADCSumLargeTile(StBeamDirection eastwest, int prepost=0) const;
    unsigned short bbcEarliestTDC(StBeamDirection eastwest, int prepost=0) const;
    unsigned short bbcTimeDifference() const;
    unsigned short bbcEarliestTDCLarge(StBeamDirection eastwest, int prepost=0) const;
    unsigned short bbcTimeDifferenceLarge() const;
    
    // FPD
    unsigned short fpd(StBeamDirection eastwest, int module, int pmt, int prepost=0) const; 
    unsigned short fpdSum(StBeamDirection eastwest, int module) const;
 
    // FMS
    unsigned short nQTdata(int prepost=0) const;
    unsigned int*  QTdata(int prepost=0) const;    
    unsigned short fmsADC(int crt, int adr, int ch, int prepost=0) const;
    unsigned short fmsTDC(int crt, int adr, int ch, int prepost=0) const;

    //ZDC
    bool zdcPresent(int prepost=0) const;
    unsigned short zdcAtChannel(int channel, int prepost=0) const;
    unsigned short zdcAtAddress(int address, int prepost=0) const;
    unsigned short zdcUnAttenuated(StBeamDirection eastwest, int prepost=0) const;
    unsigned short zdcAttenuated(StBeamDirection eastwest, int prepost=0) const;
    unsigned short zdcADC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short zdcTDC(StBeamDirection eastwest, int prepost=0) const;
    unsigned short zdcPmtTDC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short zdcHardwareSum(int prepost=0) const;
    //ZDC DSM L1
    unsigned short zdcEarliestTDC(StBeamDirection eastwest, int prepost=0) const;
    bool zdcSumADCaboveThreshold(StBeamDirection eastwest, int prepost=0) const;
    bool zdcFrontADCaboveThreshold(StBeamDirection eastwest, int prepost=0) const;
    bool zdcBackADCaboveThreshold(StBeamDirection eastwest, int prepost=0) const;
    unsigned short zdcTruncatedSum(StBeamDirection eastwest, int prepost=0) const;
    //ZDC DSM L2
    bool zdcSumADCaboveThresholdL2(StBeamDirection eastwest) const;
    bool zdcFrontADCaboveThresholdL2(StBeamDirection eastwest) const;
    bool zdcBackADCaboveThresholdL2(StBeamDirection eastwest) const;
    unsigned short zdcTimeDifference() const;
    //ZDC Last DSM
    bool zdcSumADCaboveThresholdL3(StBeamDirection eastwest) const;
    bool zdcFrontADCaboveThresholdL3(StBeamDirection eastwest) const;
    bool zdcBackADCaboveThresholdL3(StBeamDirection eastwest) const;
    bool zdcTimeDifferenceInWindow() const;
    
    //ZDCSMD
    bool zdcSMDPresent(int prepost=0) const;
    unsigned short zdcSMD(StBeamDirection eastwest, int verthori, int strip, int prepost=0) const; 
    unsigned short zdcSMDHighestStrip(StBeamDirection eastwest, int verthori, int prepost=0) const;   

    // EMC
    unsigned char bemcHighTower(int patch_id, int prepost=0) const;
    unsigned char bemcJetPatch (int patch_id, int prepost=0) const;
    unsigned char eemcHighTower(int patch_id, int prepost=0) const;
    unsigned char eemcJetPatch (int patch_id, int prepost=0) const;
    unsigned char bemcHighestTowerADC(int prepost=0) const;
    unsigned char eemcHighestTowerADC(int prepost=0) const;

    // VPD
    unsigned short vpdADC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short vpdTDC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short vpdADCHighThr(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short vpdTDCHighThr(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short vpdEarliestTDC(StBeamDirection eastwest, int prepost=0) const;
    unsigned short vpdEarliestTDCHighThr(StBeamDirection eastwest, int prepost=0) const;
    unsigned short vpdTimeDifference() const;

    //MTD
    unsigned short mtdAtAddress(int address, int prepost=0) const;
    unsigned short mtdAdc(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short mtdTdc(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned char  mtdDsmAtCh(int ch, int prepost=0) const;
    bool           mtdDsmHit(int pmt, int prepost=0) const;
    unsigned short mtdgemAtAddress(int address, int prepost=0) const;

    //TOF
    unsigned short tofAtAddress(int address, int prepost=0) const;
    unsigned short tofTrayMultiplicity(int tray, int prepost=0) const;
    unsigned short tofMultiplicity(int prepost=0) const;

    //PP2PP
    unsigned short pp2ppADC(StBeamDirection eastwest, int vh, int udio, int ch, int prepost=0) const;
    unsigned short pp2ppTAC(StBeamDirection eastwest, int vh, int udio, int ch, int prepost=0) const;
    unsigned long  pp2ppDSM(int prepost=0) const;

    // Experts only
    char*                getTriggerStructure();
    TriggerDataBlk2009*  getTriggerStructure2009();  
    int                  getRawSize() const;

    unsigned char*  getDsm0_BEMCE(int prepost=0) const;
    unsigned char*  getDsm0_BEMCW(int prepost=0) const;
    unsigned short* getDsm1_BEMC(int prepost=0) const;
    unsigned char*  getDsm0_EEMC(int prepost=0) const;
    unsigned short* getDsm1_EEMC(int prepost=0) const;
    unsigned short* getDsm2_EMC()  const;

    unsigned short*  getDsm3()      const;
    unsigned char*   getDsm_FMS(int prepost=0) const;
    //unsigned char*   getDsm01_FMS(int prepost=0) const;
    //unsigned char*   getDsm02_FMS(int prepost=0) const;
    unsigned short*  getDsm1_FMS(int prepost=0) const;
    unsigned short*  getDsm2_FMS() const;
    unsigned int     l2ResultLength() const;
    const unsigned int*  l2Result() const;
    
    // StFmsHitMaker only!!!
    void killFMS();

protected:
    TriggerDataBlk2009 *mData;
  
    EvtDescData2009*  EvtDesc;  //!
    L1_DSM_Data2009*  L1_DSM;   //!
    TrgSumData2009*   TrgSum;   //!
    BELayerBlock2009* mBC1[11]; //!
    QTBlock2009*      mMXQ[11];	//!
    MIXBlock2009*     mMIX[11];	//!
    BWestBlock2009*   mBCW[11];	//!
    BEastBlock2009*   mBCE[11];	//!
    QTBlock2009*      mFEQ[11];	//!
    BBCBlock2009*     mBBC[11];	//!
    QTBlock2009*      mBBQ[11];	//!
    FMSBlock2009*     mFMS[11];	//!
    QTBlock2009*      mQT1[11];	//!
    QTBlock2009*      mQT2[11];	//!
    QTBlock2009*      mQT3[11];	//!
    QTBlock2009*      mQT4[11];	//!
    unsigned short mxq[11][16][32],feq[11][16][32],bbq[11][16][32],qt1[11][16][32],qt2[11][16][32],qt3[11][16][32],qt4[11][16][32];       //!
    unsigned char tmxq[11][16][32],tfeq[11][16][32],tbbq[11][16][32],tqt1[11][16][32],tqt2[11][16][32],tqt3[11][16][32],tqt4[11][16][32]; //!

    void swapOfflen(TrgOfflen2009* offlen);
    void swapDataBlk(TriggerDataBlk2009* TrgData);
    void swapEvtDesc(EvtDescData2009* EvtDesc);
    void swapL1_DSM(L1_DSM_Data2009* L1_DSM);
    void swapTrgSum(TrgSumData2009* TrgSum);
    void swapRawDetOfflen(TrgOfflen2009* offlen);
    void swapRawDet(DataBlock2009* data, int name, int hlength, int bs);

    ClassDef(StTriggerData2009,1) 
};

inline void StTriggerData2009::swapOfflen(TrgOfflen2009* offlen)
{
    swapI((unsigned int*)&offlen->offset);
    swapI((unsigned int*)&offlen->length);
}

inline void StTriggerData2009::swapDataBlk(TriggerDataBlk2009 *TrgData)
{
    swapI((unsigned int*)&TrgData->FormatVersion);
    swapI((unsigned int*)&TrgData->totalTriggerLength);
    swapI((unsigned int*)&TrgData->eventNumber);
    swapOfflen(&TrgData->EventDesc_ofl);
    swapOfflen(&TrgData->L1_DSM_ofl);
    swapOfflen(&TrgData->Summary_ofl);
    swapIn((unsigned int*)TrgData->PrePostList,10);
}

inline void StTriggerData2009::swapEvtDesc(EvtDescData2009* EvtDesc)
{
    swapIn((unsigned int*)&EvtDesc->length,3);
    swapSCC((unsigned int*)&EvtDesc->actionWdDetectorBitMask);
    swapSSn((unsigned int*)&EvtDesc->TrgToken,12);
}

inline void StTriggerData2009::swapL1_DSM(L1_DSM_Data2009* L1_DSM)
{
    swapI((unsigned int*)&L1_DSM->length);
    swapSSn((unsigned int*)L1_DSM->TOF,16+8*6);
}

inline void StTriggerData2009::swapTrgSum(TrgSumData2009* TrgSum)
{
    swapIn((unsigned int*)&TrgSum->length,1+2+2+32+64+64);
}

inline void StTriggerData2009::swapRawDetOfflen(TrgOfflen2009* offlen)
{
    int i;
    for (i=0; i<y9MAX_OFFLEN; i++) { 
        swapOfflen(&offlen[i]); 
        if (mDebug>0) printf("Offlen id=%2d offset=%d length=%d\n", i, offlen[i].offset, offlen[i].length);
    }
}

inline void StTriggerData2009::swapRawDet(DataBlock2009* data, int name, int hlength,int bs)
{
    BELayerBlock2009* bc1;
    MIXBlock2009* mix;
    BBCBlock2009 *bbc;
    QTBlock2009* qtdata;
    int header_length = 8;
    if(bs) swapI((unsigned int*)&data->length);
    switch(name){
    case y9MXQ_CONF_NUM : case y9FEQ_CONF_NUM : case y9BBQ_CONF_NUM : 
    case y9QT1_CONF_NUM : case y9QT2_CONF_NUM : case y9QT3_CONF_NUM : case y9QT4_CONF_NUM :
        header_length = 12; break;
    }
    if (hlength != data->length + header_length){
        mErrorFlag = mErrorFlag | (1 << name);
        printf("StTriggerData2009: Error reading Block=%2d [%1c%1c%1c%1c] length %d != %d + %d\n",
	     name,data->name[0],data->name[1],data->name[2],data->name[3],
	     hlength,data->length,header_length);      
        printf("StTriggerData2009: Droping the data block =%2d [%1c%1c%1c%1c] with ErrorFlag=0x%x\n",
	     name,data->name[0],data->name[1],data->name[2],data->name[3],mErrorFlag);
        data=0;
        return;
    }
    if (bs){
        switch(name){
        case y9BC1_CONF_NUM :
            bc1 = (BELayerBlock2009*) data;
            swapSSn((unsigned int*)bc1->BEMClayer1,48);
            swapSSn((unsigned int*)bc1->EEMClayer1,16);
            break;
        case y9MIX_CONF_NUM :
            mix = (MIXBlock2009*) data;
            swapSSn((unsigned int*)mix->FPDEastNSLayer1,8);
            swapSSn((unsigned int*)mix->TOFLayer1,8+48);
            break;
        case y9BCW_CONF_NUM :
            //only char
            break;
        case y9BCE_CONF_NUM :
            //only char
            break;
        case y9BBC_CONF_NUM :
            bbc = (BBCBlock2009*) data;
            swapSSn((unsigned int*)bbc->BBClayer1,16+8+8);
            break;
        case y9FMS_CONF_NUM :
            //only char
            break;
        case y9MXQ_CONF_NUM :
        case y9FEQ_CONF_NUM :
        case y9BBQ_CONF_NUM :
        case y9QT1_CONF_NUM :
        case y9QT2_CONF_NUM :
        case y9QT3_CONF_NUM :
        case y9QT4_CONF_NUM :
            qtdata = (QTBlock2009*) data;
            swapI((unsigned int*)&qtdata->dataLoss);
            swapIn(qtdata->data, qtdata->length/4);
            break;
        }
    }
    if(mDebug>0) 
        printf("Read id=%2d name=%1c%1c%1c%1c length=%d\n",
	     name,data->name[0],data->name[1],data->name[2],data->name[3],data->length);
}

#endif
