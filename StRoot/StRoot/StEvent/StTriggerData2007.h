 /***************************************************************************
 *
 * $Id: StTriggerData2007.h,v 2.6 2009/03/19 02:46:01 ullrich Exp $
 *
 * Author: Akio Ogawa, Feb 2007
 ***************************************************************************
 *
 * Description:  Concrete implementation of StTriggerData for 2007.
 *
 ***************************************************************************
 *
 * $Log: StTriggerData2007.h,v $
 * Revision 2.6  2009/03/19 02:46:01  ullrich
 * Add 2nd argument (pre/post) to vpdEarliestTDC().
 *
 * Revision 2.5  2009/02/11 23:33:55  jeromel
 * Modifications by Akio to support getDsm0_BEMCE and getDsm0_BEMCW as well as
 * getDsm1_BEMC. However, use of const=0 impose implementation (was not done
 * in years < 2009). Added methods with return 0.
 *
 * Revision 2.4  2007/07/02 17:04:32  ullrich
 * Add two new members mtdAdc() and mtdTdc() (Akio).
 *
 * Revision 2.3  2007/04/24 14:51:59  ullrich
 * Fixed bug in VPD unpacking (Akio).
 *
 * Revision 2.2  2007/04/03 20:10:50  ullrich
 * Added access function for VPD data.
 *
 * Revision 2.1  2007/02/22 20:31:24  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTriggerData2007_hh
#define StTriggerData2007_hh

#include "StTriggerData.h"

struct TrgDataType2007;

class StTriggerData2007 : public StTriggerData {
public:
    StTriggerData2007();
    StTriggerData2007(const TrgDataType2007*, int run);
    ~StTriggerData2007();
    
    void dump() const;  //dump data into text
    
    // versison and data type information
    unsigned int version() const;           
    unsigned int numberOfPreXing() const;   
    unsigned int numberOfPostXing() const;  
    
    // generic Trigger infomations
    unsigned int   token() const;
    unsigned int   triggerWord() const;
    unsigned int   actionWord() const;      
    unsigned short busyStatus() const;
    unsigned short dsmInput() const;
    unsigned short trgToken() const;
    unsigned short dsmAddress() const;
    unsigned short mAddBits() const;
    unsigned short bcData(int channel) const;
    
    //L2 offsets
    int  L2ResultsOffset(StL2AlgorithmId id) const;  
    bool isL2Triggered(StL2TriggerResultType id) const;
    
    // bunch and spin bits
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
    unsigned short fpdLayer1DSM(StBeamDirection eastwest, int module, int board, int prepost=0) const;
    unsigned short fpdLayer2DSMRaw(int channel) const;
    unsigned short fpdLayer2DSM(StBeamDirection eastwest, int module) const;
    
    // CTB
    unsigned short ctbRaw(int address, int prepost=0) const;
    unsigned short ctb(int pmt, int prepost=0) const;
    unsigned short ctbTraySlat(int tray, int slat, int prepost=0) const;    
    unsigned short ctbSum(int prepost=0) const;
    
    // BBC
    unsigned short bbcADC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short bbcTDC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short bbcADCSum(StBeamDirection eastwest, int prepost=0) const;
    unsigned short bbcADCSumLargeTile(StBeamDirection eastwest, int prepost=0) const;
    unsigned short bbcEarliestTDC(StBeamDirection eastwest, int prepost=0) const;
    unsigned short bbcTimeDifference() const;
    
    // FPD
    unsigned short fpd(StBeamDirection eastwest, int module, int pmt, int prepost=0) const; 
    unsigned short fpdSum(StBeamDirection eastwest, int module) const;
 
    // FMS
    unsigned short nQTdata(int prepost=0) const;
    unsigned int*  QTdata(int prepost=0) const;    

    //ZDC
    unsigned short zdcAtChannel(int channel, int prepost=0) const;
    unsigned short zdcAtAddress(int address, int prepost=0) const;
    unsigned short zdcUnAttenuated(StBeamDirection eastwest, int prepost=0) const;
    unsigned short zdcAttenuated(StBeamDirection eastwest, int prepost=0) const;
    unsigned short zdcADC(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short zdcTDC(StBeamDirection eastwest, int prepost=0) const;
    unsigned short zdcHardwareSum(int prepost=0) const;
    
    //ZDCSMD
    unsigned short zdcSMD(StBeamDirection eastwest, int verthori, int strip, int prepost=0) const;
    
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
    unsigned short vpdEarliestTDC(StBeamDirection eastwest, int prepost=0) const;
    unsigned short vpdTimeDifference() const;

    //MTD
    unsigned short mtdAtAddress(int address, int prepost=0) const;
    unsigned short mtdAdc(StBeamDirection eastwest, int pmt, int prepost=0) const;
    unsigned short mtdTdc(StBeamDirection eastwest, int pmt, int prepost=0) const;

    // experts only
    char*                getTriggerStructure();
    TrgDataType2007*     getTriggerStructure2007();  
    int                  getRawSize() const;

    unsigned char*  getDsm0_BEMCE(int prepost=0) const { return 0;}
    unsigned char*  getDsm0_BEMCW(int prepost=0) const { return 0;}
    unsigned char*  getDsm0_EEMC(int prepost=0) const;  
    unsigned short* getDsm1_BEMC(int prepost=0) const { return 0;}

    unsigned short* getDsm1_EEMC(int prepost=0) const;
    unsigned short* getDsm2_EMC()  const;
    unsigned short* getDsm3()      const;
    unsigned char*  getDsm_FMS(int prepost=0) const;
    unsigned char*  getDsm01_FMS(int prepost=0) const;
    unsigned char*  getDsm02_FMS(int prepost=0) const;
    unsigned short* getDsm1_FMS(int prepost=0) const;
    unsigned short* getDsm2_FMS() const;
    unsigned int    l2ResultLength() const;
    const unsigned int*  l2Result() const;
    
protected:
    TrgDataType2007 *mData;

    ClassDef(StTriggerData2007,1) 
};

#endif
