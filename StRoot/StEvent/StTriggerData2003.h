/***************************************************************************
 *
 * $Id: StTriggerData2003.h,v 2.14 2009/02/11 23:33:55 jeromel Exp $
 *
 * Author: Akio Ogawa, Feb 2003
 ***************************************************************************
 *
 * Description:  Concrete implementation of StTriggerData.
 *               For year 2003 only.
 *
 ***************************************************************************
 *
 * $Log: StTriggerData2003.h,v $
 * Revision 2.14  2009/02/11 23:33:55  jeromel
 * Modifications by Akio to support getDsm0_BEMCE and getDsm0_BEMCW as well as
 * getDsm1_BEMC. However, use of const=0 impose implementation (was not done
 * in years < 2009). Added methods with return 0.
 *
 * Revision 2.13  2006/09/20 00:44:55  ullrich
 * Modified method to return length of L2 results.
 *
 * Revision 2.12  2006/09/19 22:53:55  ullrich
 * Added access method to L2 results.
 *
 * Revision 2.11  2006/09/13 23:59:55  ullrich
 * Added new data member mRun. Removed arg run from ctb(), ctbTraySlat(), zdcSMD()
 *
 * Revision 2.10  2006/08/21 19:41:51  ullrich
 * Add run number as argument to ctb(), ctbTray(), and zdcSMD(). Used 2005 only. (Akio)
 *
 * Revision 2.9  2004/11/30 19:19:12  ullrich
 * Added new access function for EEMC data (Akio).
 *
 * Revision 2.8  2004/10/20 18:56:22  ullrich
 * Add method getRawSize().
 *
 * Revision 2.7  2004/08/03 17:22:16  ullrich
 * Major update by Akio and Marco.
 *
 * Revision 2.6  2004/07/20 18:02:26  jeromel
 * Updates from Akio to fix CTB issues.
 *
 * Revision 2.5  2004/06/29 22:37:35  ullrich
 * Added missing access function for ZDC. Currently same as 2004.
 *
 * Revision 2.4  2004/02/11 01:39:52  ullrich
 * Use enumeration StBeamDirector for east/west. Add member for ZDC vertex.
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
 **************************************************************************/
#ifndef StTriggerData2003_hh
#define StTriggerData2003_hh

#include "StTriggerData.h"

struct TrgDataType2003;

class StTriggerData2003 : public StTriggerData {
public:
    StTriggerData2003();
    StTriggerData2003(const TrgDataType2003*, int run);
    ~StTriggerData2003();
  
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
    
    // EMC
    unsigned char bemcHighTower(int patch_id, int prepost=0) const;
    unsigned char bemcJetPatch (int patch_id, int prepost=0) const;
    unsigned char eemcHighTower(int patch_id, int prepost=0) const;
    unsigned char eemcJetPatch (int patch_id, int prepost=0) const;
    unsigned char bemcHighestTowerADC(int prepost=0) const;
    unsigned char eemcHighestTowerADC(int prepost=0) const;
 
    // experts only
    char* getTriggerStructure();
    TrgDataType2003* getTriggerStructure2003();  
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
    TrgDataType2003 *mData;
    
    ClassDef(StTriggerData2003,1) 
};

#endif
