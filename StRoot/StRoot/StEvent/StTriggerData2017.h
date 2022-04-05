/***************************************************************************
 *
 * $Id: StTriggerData2017.h,v 2.5 2017/10/13 20:14:26 ullrich Exp $
 *
 * Author: Akio Ogawa, Dec 2016
 ***************************************************************************
 *
 * Description:  Concrete implementation of StTriggerData for 2017.
 *
 ***************************************************************************
 *
 * $Log: StTriggerData2017.h,v $
 * Revision 2.5  2017/10/13 20:14:26  ullrich
 * Added access fct epdADC() and epdTDC().
 *
 * Revision 2.4  2017/05/30 15:59:14  ullrich
 * Added bbcTDC5bit() method.
 *
 * Revision 2.3  2017/05/13 00:59:54  ullrich
 * Added bbcBB101() and bbcBB102().
 *
 * Revision 2.2  2016/12/15 16:30:08  ullrich
 * Updates from Jeff.
 *
 * Revision 2.1  2016/12/08 18:58:29  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#ifndef StTriggerData2017_hh
#define StTriggerData2017_hh

#include "StTriggerData.h"
#include "StDaqLib/TRG/trgStructures2017.h"

class StTriggerData2017 : public StTriggerData {
    
public:
    StTriggerData2017();
    StTriggerData2017(const TriggerDataBlk2017* data, int run);
    StTriggerData2017(const TriggerDataBlk2017* data, int run, int bs, int dbg=0);
    ~StTriggerData2017();
    
    void readData() {readData(0,0);}
    void readData(const TriggerDataBlk2017* data, int bs);
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

    unsigned short getTrgDetMask() const;
    unsigned int   getTrgCrateMask() const;

    // L2 offsets
    int  L2ResultsOffset(StL2AlgorithmId id) const;  
    bool isL2Triggered(StL2TriggerResultType id) const;
    unsigned long long l2sum() const;
    
    // Bunch and spin bits
    unsigned int tcuCounter() const;
    unsigned int rccCounter(int crate) const;
    unsigned long long bunchCounter() const;
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
    unsigned short tpcMaskDSM(int channel) const;
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
    unsigned short bbcTDC5bit(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short bbcADCSum(StBeamDirection eastwest, int prepost=0) const;
    unsigned short bbcADCSumLargeTile(StBeamDirection eastwest, int prepost=0) const;
    unsigned short bbcEarliestTDC(StBeamDirection eastwest, int prepost=0) const;
    unsigned short bbcTimeDifference() const;
    unsigned short bbcTacSum() const;
    unsigned short bbcEarliestTDCLarge(StBeamDirection eastwest, int prepost=0) const;
    unsigned short bbcTimeDifferenceLarge() const;
    unsigned short bbcBB101(int ch, int prepost=0) const;
    unsigned short bbcBB102(int ch, int prepost=0) const;
    
    // FPD
    unsigned short fpd(StBeamDirection eastwest, int module, int pmt, int prepost=0) const; 
    unsigned short fpdSum(StBeamDirection eastwest, int module) const;
    
    // FMS
    unsigned short nQTdata(int prepost=0) const;
    unsigned int*  QTdata(int prepost=0) const;    
    unsigned short fmsADC(int crt, int adr, int ch, int prepost=0) const;
    unsigned short fmsTDC(int crt, int adr, int ch, int prepost=0) const;
    
    //EPD
    unsigned short epdADC(int crt, int adr, int ch, int prepost=0) const;
    unsigned short epdTDC(int crt, int adr, int ch, int prepost=0) const;

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

    unsigned short bbcVP101(int ch, int prepost = 0) const;

    //MXQ crate
    unsigned short mxqAtSlotAddress(int address, int prepost=0, int slot=0) const;

    //MTD
    unsigned short mtdQtAtCh(int qtid, int address, int prepost) const;
    unsigned short mtdAtAddress(int address, int prepost=0) const;
    unsigned short mtdgemAtAddress(int address, int prepost=0) const;
    unsigned short mtd3AtAddress(int address, int prepost=0) const;
    unsigned short mtdAdc(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short mtdTdc(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned char  mtdDsmAtCh(int ch, int prepost=0) const;
    bool           mtdDsmHit(int pmt, int prepost=0) const;
    unsigned short mtdVpdTacDiff() const;
    unsigned short mtd4AtAddress(int address, int prepost=0) const;
    
    //TOF
    unsigned short tofAtAddress(int address, int prepost=0) const;
    unsigned short tofTrayMultiplicity(int tray, int prepost=0) const;
    unsigned short tofMultiplicity(int prepost=0) const;
    unsigned short dsmTF201Ch(int ch) const;
    
    //PP2PP
    unsigned short pp2ppADC(StBeamDirection eastwest, int vh, int udio, int ch, int prepost=0) const;
    unsigned short pp2ppTAC(StBeamDirection eastwest, int vh, int udio, int ch, int prepost=0) const;
    unsigned long  pp2ppDSM(int prepost=0) const;
    
    // Experts only
    char*                getTriggerStructure();
    TriggerDataBlk2017*  getTriggerStructure2017();  
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
    TriggerDataBlk2017 *mData;
    
    EvtDescData2017*  EvtDesc;  //!
    L1_DSM_Data2017*  L1_DSM;   //!
    TrgSumData2017*   TrgSum;   //!
    BELayerBlock2017* mBC1[11]; //!
    QTBlock2017*      mMXQ[11];	//!
    MIXBlock2017*     mMIX[11];	//!
    BWestBlock2017*   mBCW[11];	//!
    BEastBlock2017*   mBCE[11];	//!
    QTBlock2017*      mEPQ[11];	//!
    BBCBlock2017*     mBBC[11];	//!
    QTBlock2017*      mBBQ[11];	//!
    FMSBlock2017*     mFMS[11];	//!
    QTBlock2017*      mQT1[11];	//!
    QTBlock2017*      mQT2[11];	//!
    QTBlock2017*      mQT3[11];	//!
    QTBlock2017*      mQT4[11];	//!
    QTBlock2017*      mFQ1[11];	//!
    QTBlock2017*      mFQ2[11];	//!
    unsigned short mxq[11][16][32],epq[11][16][32],bbq[11][16][32],qt1[11][16][32],qt2[11][16][32],qt3[11][16][32],qt4[11][16][32],       
   	fq1[11][16][32],fq2[11][16][32]; //!
    unsigned char tmxq[11][16][32],tepq[11][16][32],tbbq[11][16][32],tqt1[11][16][32],tqt2[11][16][32],tqt3[11][16][32],tqt4[11][16][32],
   	tfq1[11][16][32],tfq2[11][16][32]; //!
    
    
    void swapOfflen(TrgOfflen2017* offlen);
    void swapDataBlk(TriggerDataBlk2017* TrgData);
    void swapEvtDesc(EvtDescData2017* EvtDesc);
    void swapL1_DSM(L1_DSM_Data2017* L1_DSM);
    void swapTrgSum(TrgSumData2017* TrgSum);
    void swapRawDetOfflen(TrgOfflen2017* offlen);
    void swapRawDet(DataBlock2017* data, int name, int hlength, int bs);
    
    ClassDef(StTriggerData2017,1) 
};

inline void StTriggerData2017::swapOfflen(TrgOfflen2017* offlen)
{
    swapI((unsigned int*)&offlen->offset);
    swapI((unsigned int*)&offlen->length);
}

inline void StTriggerData2017::swapDataBlk(TriggerDataBlk2017 *TrgData)
{
    swapI((unsigned int*)&TrgData->FormatVersion);
    swapI((unsigned int*)&TrgData->totalTriggerLength);
    swapI((unsigned int*)&TrgData->eventNumber);
    swapOfflen(&TrgData->EventDesc_ofl);
    swapOfflen(&TrgData->L1_DSM_ofl);
    swapOfflen(&TrgData->Summary_ofl);
    swapIn((unsigned int*)TrgData->PrePostList,10);
}

inline void StTriggerData2017::swapEvtDesc(EvtDescData2017* EvtDesc)
{
    swapIn((unsigned int*)&EvtDesc->length,3);
    swapSCC((unsigned int*)&EvtDesc->actionWdDetectorBitMask);
    swapSSn((unsigned int*)&EvtDesc->TrgToken,12);
}

inline void StTriggerData2017::swapL1_DSM(L1_DSM_Data2017* L1_DSM)
{
    swapI((unsigned int*)&L1_DSM->length);
    swapSSn((unsigned int*)L1_DSM->TOF,16+8*7);
}

inline void StTriggerData2017::swapTrgSum(TrgSumData2017* TrgSum)
{
    swapIn((unsigned int*)&TrgSum->length,1+2+2+32+64+64);
}

inline void StTriggerData2017::swapRawDetOfflen(TrgOfflen2017* offlen)
{
    int i;
    for (i=0; i<y17MAX_OFFLEN; i++) { 
        swapOfflen(&offlen[i]); 
        if (mDebug>0) printf("Offlen id=%2d offset=%d length=%d\n", i, offlen[i].offset, offlen[i].length);
    }
}

#endif
