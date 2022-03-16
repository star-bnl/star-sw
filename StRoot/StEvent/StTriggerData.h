/***************************************************************************
 *
 * $Id: StTriggerData.h,v 2.55 2019/06/25 15:50:15 ullrich Exp $
 *
 * Author: Akio Ogawa & Mirko Planinic, Feb 2003
 ***************************************************************************
 *
 * Description: abstract class for trigger data
 *
 ***************************************************************************
 *
 * $Log: StTriggerData.h,v $
 * Revision 2.55  2019/06/25 15:50:15  ullrich
 * Improved QT board error reports/handling. Added EPD access functions. (Akio)
 *
 * Revision 2.54  2019/01/07 15:50:12  ullrich
 * Added StTriggerData2019.
 *
 * Revision 2.53  2018/06/06 18:03:59  ullrich
 * Added fcts: epdNHits, vpdADCSum, vpdMeanTimeDifference (Akio)
 *
 * Revision 2.52  2018/02/22 16:47:20  ullrich
 * Changes for blind analysis and EPD
 *
 * Revision 2.51  2017/10/13 20:13:53  ullrich
 * Added access fct epdADC() and epdTDC().
 *
 * Revision 2.50  2017/05/30 15:59:14  ullrich
 * Added bbcTDC5bit() method.
 *
 * Revision 2.49  2017/05/18 17:09:43  ullrich
 * Changes to decoding of Qt data.
 *
 * Revision 2.48  2017/02/20 16:32:12  ullrich
 * Added bbcVP101
 *
 * Revision 2.47  2016/12/15 16:30:06  ullrich
 * Updates from Jeff.
 *
 * Revision 2.46  2016/06/07 15:51:34  akio
 * Making code better based on Coverity reports
 *
 * Revision 2.45  2016/02/11 14:22:02  ullrich
 * Add fcts to access MTD DSM and QT info.
 *
 * Revision 2.44  2013/11/13 19:17:01  ullrich
 * Added mtd4AtAddress() and dsmTF201Ch(). (Akio)
 *
 * Revision 2.43  2013/02/12 19:40:32  ullrich
 * Add two new methods: mxqAtSlotAddress and mtd3AtAddress (Llope).
 *
 * Revision 2.42  2012/05/07 14:42:58  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
 * Revision 2.41  2012/04/30 15:19:11  ullrich
 * Added access function for l2sum (Akio)
 *
 * Revision 2.40  2012/02/01 17:00:07  ullrich
 * Fixed bug concerning seg failt when MIX DSM not in run and added new arg to MtdVpdTacDiff()
 *
 * Revision 2.39  2011/11/04 19:20:38  ullrich
 * Added tpcMaskDSM() - Akio.
 *
 * Revision 2.38  2011/10/17 15:36:37  fisyak
 * increment version no.
 *
 * Revision 2.37  2011/02/15 18:56:09  ullrich
 * New access fct for ZDCSMD, new ZDCSMD map, spinBit() modified.
 *
 * Revision 2.36  2011/01/18 23:06:07  ullrich
 * New function mtdgemAtAddress added. vpdADC, vpdTDC, vpdADCHighThr, vpdTDCHighThr, vpdEarliestTDC, and vpdEarliestTDCHighThr updated.
 *
 * Revision 2.35  2010/06/01 22:18:44  ullrich
 * Change member debug to mDebug.
 *
 * Revision 2.34  2010/04/07 14:43:19  ullrich
 * Added new access function for BBC large tile earliest TAC and difference
 *
 * Revision 2.33  2010/01/13 17:55:39  ullrich
 * Better mErrorFlags, abort, and debug flag handling, updated MTD DSM access function for run10, clean up compiler warning messages.
 *
 * Revision 2.32  2010/01/08 22:44:37  ullrich
 * Updates needed to add StFmsCollection and related classes.
 *
 * Revision 2.31  2009/08/24 22:38:28  ullrich
 * New data member mErrorFlag and referring access fct.
 *
 * Revision 2.30  2009/06/16 15:44:26  ullrich
 * Added fmsADC() method.
 *
 * Revision 2.29  2009/05/15 18:16:15  ullrich
 * Updates for pp2pp and ToF.
 *
 * Revision 2.28  2009/05/05 20:53:16  ullrich
 * Updates for MTD.
 *
 * Revision 2.27  2009/03/19 02:46:01  ullrich
 * Add 2nd argument (pre/post) to vpdEarliestTDC().
 *
 * Revision 2.26  2009/03/04 02:01:30  ullrich
 * New access functions for ZDC DSM layer-1 and layer-2 data.
 *
 * Revision 2.25  2009/02/23 22:31:09  ullrich
 * Fixed problem when running over 2009 data (solution by Pibero) and new VPD access functions.
 *
 * Revision 2.24  2009/02/13 23:04:50  ullrich
 * Updates necessary for use in Online QA (P) plots.
 *
 * Revision 2.23  2009/02/11 23:33:55  jeromel
 * Modifications by Akio to support getDsm0_BEMCE and getDsm0_BEMCW as well as
 * getDsm1_BEMC. However, use of const=0 impose implementation (was not done
 * in years < 2009). Added methods with return 0.
 *
 * Revision 2.22  2009/01/20 18:10:14  ullrich
 * Bug fix and new ZDC access functions.
 *
 * Revision 2.21  2009/01/14 17:54:45  ullrich
 * Modified to cope with necessary changes for 2009.
 *
 * Revision 2.20  2008/03/12 15:56:41  ullrich
 * Add new methods tofAtAddress() and tofMultiplicity().
 *
 * Revision 2.19  2007/07/02 17:03:02  ullrich
 * Add two new members mtdAdc() and mtdTdc() (Akio).
 *
 * Revision 2.18  2007/05/15 16:31:26  ullrich
 * Add virtual function mtdAtAddress().
 *
 * Revision 2.17  2007/04/03 20:10:49  ullrich
 * Added access function for VPD data.
 *
 * Revision 2.16  2006/09/20 00:44:56  ullrich
 * Modified method to return length of L2 results.
 *
 * Revision 2.15  2006/09/19 22:53:55  ullrich
 * Added access method to L2 results.
 *
 * Revision 2.14  2006/09/13 23:59:55  ullrich
 * Added new data member mRun. Removed arg run from ctb(), ctbTraySlat(), zdcSMD()
 *
 * Revision 2.13  2006/08/21 19:41:50  ullrich
 * Add run number as argument to ctb(), ctbTray(), and zdcSMD(). Used 2005 only. (Akio)
 *
 * Revision 2.12  2006/05/05 16:01:41  ullrich
 * Added isL2Triggered().
 *
 * Revision 2.11  2006/03/22 20:58:21  ullrich
 * Added interface to L2 results (offsets).
 *
 * Revision 2.10  2004/11/30 19:19:11  ullrich
 * Added new access function for EEMC data (Akio).
 *
 * Revision 2.9  2004/10/20 18:56:22  ullrich
 * Add method getRawSize().
 *
 * Revision 2.8  2004/08/03 17:22:16  ullrich
 * Major update by Akio and Marco.
 *
 * Revision 2.7  2004/07/20 18:02:26  jeromel
 * Updates from Akio to fix CTB issues.
 *
 * Revision 2.6  2004/02/11 01:39:51  ullrich
 * Use enumeration StBeamDirector for east/west. Add member for ZDC vertex.
 *
 * Revision 2.5  2004/01/28 00:29:49  ullrich
 * Methods to retrieve ZDC data added.
 *
 * Revision 2.4  2003/12/23 21:58:28  ullrich
 * Modifications to handle StTruggerData2004.
 *
 * Revision 2.3  2003/07/16 19:58:31  perev
 * Cleanup of StTriggerData2003 at all
 *
 * Revision 2.2  2003/05/21 03:58:44  ullrich
 * Added more methods to retrieve spin bits.
 *
 * Revision 2.1  2003/04/16 17:47:41  ullrich
 * Initial Revision.
 *
 **************************************************************************
 *
 * prepost argument should go between -5 and 5, and 0 is triggered crossing
 *
 **************************************************************************/

#ifndef StTriggerData_hh
#define StTriggerData_hh

#include "StObject.h"
#include "StEnumerations.h"
#include "StMessMgr.h"

class StTriggerData : public StObject {
public:
    StTriggerData();
    virtual ~StTriggerData();
    
    virtual void readData() {};
    virtual void dump() const = 0;   // dump data into text
    virtual void setDebug(unsigned int); 
    virtual void blindRunInfo();     // for run18 blinding analysis
       
    // version and data type information   
    virtual int year() const;                          // year of the data
    virtual unsigned int version() const = 0;          // TrgDataType Version Number 
    virtual unsigned int numberOfPreXing() const = 0;  // # of pre xing data for detectors
    virtual unsigned int numberOfPostXing() const = 0; // # of post xing data for detectors
    virtual unsigned int errorFlag() const;            // error flag
  
    // generic trigger infomations
    virtual unsigned int eventNumber() const;
    virtual unsigned int token() const = 0;
    virtual unsigned int triggerWord() const = 0;
    virtual unsigned int actionWord() const = 0;  
    virtual unsigned short busyStatus() const;
    virtual unsigned short dsmInput() const;
    virtual unsigned short trgToken() const;
    virtual unsigned short dsmAddress() const;
    virtual unsigned short mAddBits() const;
    virtual unsigned short bcData(int channel) const;

    virtual unsigned short getTrgDetMask() const;
    virtual unsigned int   getTrgCrateMask() const;

    //L2 results offsets 
    virtual int L2ResultsOffset(StL2AlgorithmId id) const;  
    bool isL2Triggered(StL2TriggerResultType id) const;
    virtual unsigned long long l2sum() const;

    // bunch and spin bits
    virtual unsigned int tcuCounter() const;
    virtual unsigned int rccCounter(int crate) const;
    virtual unsigned long long bunchCounter() const;
    virtual unsigned int bunchCounterHigh() const;
    virtual unsigned int bunchCounterLow() const;
    virtual unsigned int bunchId48Bit() const;
    virtual unsigned int bunchId7Bit() const;
    virtual unsigned int spinBit() const;
    virtual unsigned int spinBitYellowFilled() const;
    virtual unsigned int spinBitYellowUp() const;
    virtual unsigned int spinBitYellowDown() const;
    virtual unsigned int spinBitYellowUnpol() const;
    virtual unsigned int spinBitBlueFilled() const;
    virtual unsigned int spinBitBlueUp() const;
    virtual unsigned int spinBitBlueDown() const;
    virtual unsigned int spinBitBlueUnpol() const;
  
    // high level DSM infos
    virtual unsigned short tcuBits() const = 0;
    virtual unsigned short lastDSM(int channel) const;
    virtual unsigned short vertexDSM(int channel) const;
    virtual unsigned short ctbLayer1DSM(int channel) const;
    virtual unsigned short ctbLayer2DSM(int channel) const;
    virtual unsigned short bemcLayer1DSM(int channel, int prepost=0) const;
    virtual unsigned short eemcLayer1DSM(int channel, int prepost=0) const;
    virtual unsigned short emcLayer2DSM(int channel) const;
    virtual unsigned short tpcMaskDSM(int channel) const;
    virtual unsigned short fpdLayer1DSMRaw(StBeamDirection eastwest, int channel, int prepost=0) const;
    virtual unsigned short fpdLayer1DSM(StBeamDirection eastwest, int module, int board, int prepsot=0) const;
    virtual unsigned short fpdLayer2DSMRaw(int channel) const;
    virtual unsigned short fpdLayer2DSM(StBeamDirection eastwest, int module) const;

    // CTB
    virtual unsigned short ctbRaw(int address, int prepost=0) const;
    virtual unsigned short ctb(int pmt, int prepost=0) const;
    virtual unsigned short ctbTraySlat(int tray, int slat, int prepost=0) const;
    virtual unsigned short ctbSum(int prepost=0) const;

    // MWC
    virtual unsigned short mwc(int sector, int prepost=0) const;

    // ZDC 
    virtual bool zdcPresent(int prepost=0) const;
    virtual unsigned short zdcAtChannel(int channel, int prepost=0) const;
    virtual unsigned short zdcAtAddress(int address, int prepost=0) const;
    virtual unsigned short zdcUnAttenuated(StBeamDirection eastwest, int prepost=0) const;
    virtual unsigned short zdcAttenuated(StBeamDirection eastwest, int prepost=0) const;
    virtual unsigned short zdcADC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    virtual unsigned short zdcTDC(StBeamDirection eastwest, int prepost=0) const;
    virtual unsigned short zdcPmtTDC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    virtual unsigned short zdcHardwareSum(int prepost=0) const;
    // ZDC DSM L1
    virtual unsigned short zdcEarliestTDC(StBeamDirection eastwest, int prepost=0) const;
    virtual bool zdcSumADCaboveThreshold(StBeamDirection eastwest, int prepost=0) const;
    virtual bool zdcFrontADCaboveThreshold(StBeamDirection eastwest, int prepost=0) const;
    virtual bool zdcBackADCaboveThreshold(StBeamDirection eastwest, int prepost=0) const;
    virtual unsigned short zdcTruncatedSum(StBeamDirection eastwest, int prepost=0) const;
    // ZDC DSM L2
    virtual bool zdcSumADCaboveThresholdL2(StBeamDirection eastwest) const;
    virtual bool zdcFrontADCaboveThresholdL2(StBeamDirection eastwest) const;
    virtual bool zdcBackADCaboveThresholdL2(StBeamDirection eastwest) const;
    virtual unsigned short zdcTimeDifference() const;
    // ZDC DSM L3
    virtual bool zdcSumADCaboveThresholdL3(StBeamDirection eastwest) const;
    virtual bool zdcFrontADCaboveThresholdL3(StBeamDirection eastwest) const;
    virtual bool zdcBackADCaboveThresholdL3(StBeamDirection eastwest) const;
    virtual bool zdcTimeDifferenceInWindow() const;

    //ZDCSMD
    virtual bool zdcSMDPresent(int prepost=0) const;
    virtual unsigned short zdcSMD(StBeamDirection eastwest, int verthori, int strip, int prepost=0) const;
    virtual unsigned short zdcSMDHighestStrip(StBeamDirection eastwest, int verthori, int prepost=0) const;

    // EMC
    virtual unsigned char bemcHighTower(int patch_id, int prepost=0) const;
    virtual unsigned char bemcJetPatch (int patch_id, int prepost=0) const;
    virtual unsigned char eemcHighTower(int patch_id, int prepost=0) const;
    virtual unsigned char eemcJetPatch (int patch_id, int prepost=0) const;
    virtual unsigned char bemcHighestTowerADC(int prepost=0) const;
    virtual unsigned char eemcHighestTowerADC(int prepost=0) const;

    // BBC bbcTDC
    virtual unsigned short bbcADC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    virtual unsigned short bbcTDC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    virtual unsigned short bbcTDC5bit(StBeamDirection eastwest, int pmt, int prepost=0) const;
    virtual unsigned short bbcADCSum(StBeamDirection eastwest, int prepost=0) const;
    virtual unsigned short bbcADCSumLargeTile(StBeamDirection eastwest, int prepost=0) const;
    virtual unsigned short bbcEarliestTDC(StBeamDirection eastwest, int prepost=0) const;
    virtual unsigned short bbcTimeDifference() const;
    virtual unsigned short bbcTacSum() const;
    virtual unsigned short bbcEarliestTDCLarge(StBeamDirection eastwest, int prepost=0) const;
    virtual unsigned short bbcTimeDifferenceLarge() const;
    virtual unsigned short bbcBB101(int ch, int prepost=0) const;
    virtual unsigned short bbcBB102(int ch, int prepost=0) const;
  
    // FPD  module #: north=0, south=1, top=2, bottom=3, north preshower=4, south preshower=5
    virtual unsigned short fpd(StBeamDirection eastwest, int module, int pmt, int prepost=0) const; 
    virtual unsigned short fpdSum(StBeamDirection eastwest, int module) const;
  
    // FMS 
    virtual unsigned short nQTdata(int prepost=0) const;
    virtual unsigned int*  QTdata(int prepost=0) const;
    virtual unsigned short fmsADC(int crt, int adr, int ch, int prepost=0) const;
    virtual unsigned short fmsTDC(int crt, int adr, int ch, int prepost=0) const;

    //EPD
    virtual unsigned short epdEarliestTDC(StBeamDirection eastwest, int prepost=0) const;
    virtual unsigned short epdTimeDifference() const;
    virtual bool           epdHitLayer2(StBeamDirection eastwest) const;
    virtual unsigned short epdLayer1(int ch, int prepost=0) const;
    virtual unsigned short epdLayer1a(int ch, int prepost=0) const;
    virtual unsigned short epdLayer1b(int ch, int prepost=0) const;
    virtual unsigned short epdLayer0t(int ch, int prepost=0) const;
    virtual unsigned short epdLayer0a(int ch, int prepost=0) const;
    virtual unsigned char  epdLayer0h(int ch, int prepost=0) const;
    virtual unsigned short epdADC(int crt, int adr, int ch, int prepost=0) const;
    virtual unsigned short epdTDC(int crt, int adr, int ch, int prepost=0) const;    
    virtual unsigned short epdNHits(StBeamDirection eastwest, int prepost=0) const;    
    virtual unsigned short epdNHitsQT(int crate, int qt, int mult12, int prepost=0) const;
    virtual unsigned short epdLayer0aMult(int ch, int prepost=0) const;
    virtual unsigned short epdLayer0hMult(int ch, int mult12, int prepost=0) const;
    virtual unsigned short epdLayer1bMult(StBeamDirection eastwest, int ring, int prepost=0) const;
    virtual unsigned short epdMultTotal(int prepost=0) const;
    virtual unsigned short epdMultDiff(int prepost=0) const;

    // VPD
    virtual unsigned short vpdADC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    virtual unsigned short vpdTDC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    virtual unsigned short vpdADCHighThr(StBeamDirection eastwest, int pmt, int prepost=0) const;
    virtual unsigned short vpdTDCHighThr(StBeamDirection eastwest, int pmt, int prepost=0) const;
    virtual unsigned short vpdEarliestTDC(StBeamDirection eastwest, int prepost=0) const;
    virtual unsigned short vpdEarliestTDCHighThr(StBeamDirection eastwest, int prepost=0) const;
    virtual unsigned short vpdADCSum(StBeamDirection eastwest, int prepost=0) const;
    virtual unsigned short vpdTimeDifference() const;
    virtual float          vpdMeanTimeDifference(int prepost=0) const;
    virtual unsigned short bbcVP101(int ch, int prepost = 0) const;

    //MXQ crate
    virtual unsigned short mxqAtSlotAddress(int address, int prepost=0, int slot=0) const;

    //MTD
    virtual unsigned short mtdQtAtCh(int qtid, int address, int prepost) const;
    virtual unsigned short mtdAtAddress(int address, int prepost=0) const;
    virtual unsigned short mtdgemAtAddress(int address, int prepost=0) const;
    virtual unsigned short mtd3AtAddress(int address, int prepost=0) const;
    virtual unsigned short mtdAdc(StBeamDirection eastwest, int pmt, int prepost=0) const;
    virtual unsigned short mtdTdc(StBeamDirection eastwest, int pmt, int prepost=0) const;
    virtual unsigned char  mtdDsmAtCh(int ch, int prepost=0) const;
    virtual bool           mtdDsmHit(int pmt, int prepost=0) const;
    virtual unsigned short mtdVpdTacDiff() const;
    virtual unsigned short mtd4AtAddress(int address, int prepost=0) const;

    //TOF
    virtual unsigned short tofAtAddress(int address, int prepost=0) const;
    virtual unsigned short tofTrayMultiplicity(int tray, int prepost=0) const;
    virtual unsigned short tofMultiplicity(int prepost=0) const;
    virtual unsigned short dsmTF201Ch(int ch) const;

    //PP2PP
    virtual unsigned short pp2ppADC(StBeamDirection eastwest, int vh, int udio, int ch, int prepost=0) const;
    virtual unsigned short pp2ppTAC(StBeamDirection eastwest, int vh, int udio, int ch, int prepost=0) const;
    virtual unsigned long  pp2ppDSM(int prepost=0) const;

    // auxiliary information
    float zdcVertexZ() const;
    void  setZdcVertexZ(float);
    
    // Experts only!
    virtual char* getTriggerStructure() = 0;
    virtual int getRawSize() const = 0;
    virtual unsigned char*  getDsm0_BEMCE(int prepost=0) const =0;
    virtual unsigned char*  getDsm0_BEMCW(int prepost=0) const =0;
    virtual unsigned short* getDsm1_BEMC(int prepost=0)  const =0;
    virtual unsigned char*  getDsm0_EEMC(int prepost=0)  const =0;
    virtual unsigned short* getDsm1_EEMC(int prepost=0)  const =0;
    virtual unsigned short* getDsm2_EMC()                const =0;
    virtual unsigned short* getDsm3()                    const =0;
    virtual unsigned char*  getDsm_FMS(int prepost=0)    const;
    virtual unsigned char*  getDsm01_FMS(int prepost=0)  const;
    virtual unsigned char*  getDsm02_FMS(int prepost=0)  const;
    virtual unsigned short* getDsm1_FMS(int prepost=0)   const;
    virtual unsigned short* getDsm2_FMS() const;
    virtual unsigned int    l2ResultLength() const = 0;  // Length of raw info
    virtual const unsigned int* l2Result() const = 0;  // Pointer to raw info

    // StFmsHitMaker only!!!
    virtual void killFMS();

protected:
    int prepostAddress(int prepost) const; //get pre&post xsing addess, return negative if bad.
        
    // Service routine to decode EMC layer0 DSM info into 12bit input values
    unsigned short decodeEmc12bit(const int dsm, const int channel, const unsigned char *raw) const;
  
    // Byte swapping functions
    void swapI(unsigned int *);
    void swapSCC(unsigned int *);
    void swapSS(unsigned int *);
    void swapIn(unsigned int *, unsigned int);
    void swapSSn(unsigned int *, unsigned int);
    void swapSCCn(unsigned int *, unsigned int);

    // QT data decoding
    enum {MaxQTData = 529}; //!
    void decodeQT(unsigned int ndata, unsigned int *data, unsigned short adc[16][32], unsigned char tac[16][32]);

protected:
    int   mYear;
    float mZdcVertexZ;    
    int   mRun;
    unsigned int mErrorFlag;

    unsigned int mDebug; //!

    ClassDef(StTriggerData,7) 
};

inline void StTriggerData::swapI(unsigned int *var){
    *var = 
        (*var & 0xff000000) >> 24 |
        (*var & 0x00ff0000) >> 8  |
        (*var & 0x0000ff00) << 8  |
        (*var & 0x000000ff) << 24 ;
}

inline void StTriggerData::swapSCC(unsigned int *var){
    *var =
        (*var & 0x0000ff00) >> 8 |
        (*var & 0x000000ff) << 8 |
        (*var & 0xffff0000);
}

inline void StTriggerData::swapSS(unsigned int *var){
    *var = 
        (*var & 0xff000000) >> 8 |
        (*var & 0x00ff0000) << 8 |
        (*var & 0x0000ff00) >> 8 |
        (*var & 0x000000ff) << 8;
}

inline void StTriggerData::swapIn(unsigned int *var, unsigned int n)  {for(unsigned int i=0; i<n; i++)   {swapI(var++);} }
inline void StTriggerData::swapSSn(unsigned int *var, unsigned int n) {for(unsigned int i=0; i<n/2; i++) {swapSS(var++);} }
inline void StTriggerData::swapSCCn(unsigned int *var, unsigned int n){for(unsigned int i=0; i<n; i++)   {swapSCC(var++);} }

#endif
  
