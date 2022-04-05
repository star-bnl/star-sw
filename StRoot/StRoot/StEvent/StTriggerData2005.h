/***************************************************************************
 *
 * $Id: StTriggerData2005.h,v 2.9 2009/02/11 23:33:55 jeromel Exp $
 *
 * Author: Akio Ogawa, Oct 2004
 ***************************************************************************
 *
 * Description:  Concrete implementation of StTriggerData.
 *               For year 2005 only.
 *
 ***************************************************************************
 *
 * $Log: StTriggerData2005.h,v $
 * Revision 2.9  2009/02/11 23:33:55  jeromel
 * Modifications by Akio to support getDsm0_BEMCE and getDsm0_BEMCW as well as
 * getDsm1_BEMC. However, use of const=0 impose implementation (was not done
 * in years < 2009). Added methods with return 0.
 *
 * Revision 2.8  2006/09/20 00:44:56  ullrich
 * Modified method to return length of L2 results.
 *
 * Revision 2.7  2006/09/19 22:53:55  ullrich
 * Added access method to L2 results.
 *
 * Revision 2.6  2006/09/13 23:59:55  ullrich
 * Added new data member mRun. Removed arg run from ctb(), ctbTraySlat(), zdcSMD()
 *
 * Revision 2.5  2006/08/21 19:41:51  ullrich
 * Add run number as argument to ctb(), ctbTray(), and zdcSMD(). Used 2005 only. (Akio)
 *
 * Revision 2.4  2006/05/04 19:05:51  ullrich
 * Added stuff to handle L2 results data.
 *
 * Revision 2.3  2006/03/22 20:58:21  ullrich
 * Added interface to L2 results (offsets).
 *
 * Revision 2.2  2004/11/30 19:19:12  ullrich
 * Added new access function for EEMC data (Akio).
 *
 * Revision 2.1  2004/11/02 21:18:39  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTriggerData2005_hh
#define StTriggerData2005_hh

#include "StTriggerData.h"

struct TrgDataType2005;

class StTriggerData2005 : public StTriggerData {
public:
    StTriggerData2005();
    StTriggerData2005(const TrgDataType2005*, int run);
    ~StTriggerData2005();
  
    void dump() const;  //dump data into text
  
    // versison and data type information
    unsigned int version() const;           
    unsigned int numberOfPreXing() const;   
    unsigned int numberOfPostXing() const;  
  
    // generic Trigger infomations
    unsigned int token() const;
    unsigned int triggerWord() const;
    unsigned int actionWord() const;      
    unsigned short busyStatus() const;
    unsigned short dsmInput() const;
    unsigned short trgToken() const;
    unsigned short dsmAddress() const;
    unsigned short mAddBits() const;
    unsigned short bcData(int channel) const;

    //L2 offsets
    int L2ResultsOffset(StL2AlgorithmId id) const;  
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

    // MWC
    unsigned short mwc(int pmt, int prepost=0) const;
            
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

    // experts only
    char* getTriggerStructure();
    TrgDataType2005* getTriggerStructure2005();  
    int getRawSize() const;

    unsigned      char* getDsm0_BEMCE(int prepost=0) const { return 0;}
    unsigned      char* getDsm0_BEMCW(int prepost=0) const { return 0;}
    unsigned      char * getDsm0_EEMC(int prepost=0) const;
    unsigned short int*  getDsm1_BEMC(int prepost=0) const { return 0;}
    unsigned short int * getDsm1_EEMC(int prepost=0) const;
    unsigned short int * getDsm2_EMC()  const;
    unsigned short int * getDsm3()      const;
    unsigned int         l2ResultLength() const;
    const unsigned int*  l2Result() const;
    
protected:
    TrgDataType2005 *mData;
    
    ClassDef(StTriggerData2005,1) 
};

#endif
