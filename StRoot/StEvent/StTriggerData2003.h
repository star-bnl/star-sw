/***************************************************************************
 *
 * $Id: StTriggerData2003.h,v 2.2 2003/05/21 03:58:44 ullrich Exp $
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
    StTriggerData2003(char*);
    StTriggerData2003(TrgDataType2003*);
  
    void dump() const;  //dump data into text
  
    // versison and data type information
    unsigned int version() const;           
    unsigned int numberOfPreXing() const;   
    unsigned int numberOfPostXing() const;  
  
    // generic Trigger infomations
    unsigned int token() const;
    unsigned int triggerWord() const;
    unsigned int actionWord() const;      
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
    
    // CTB
    unsigned short ctb(int pmt, int prepost=0) const;
    
    // MWC
    unsigned short mwc(int pmt, int prepost=0) const;
            
    // BBC
    unsigned short bbcADC(int eastwest, int pmt, int prepost=0) const;
    unsigned short bbcTDC(int eastwest, int pmt, int prepost=0) const;
    unsigned short bbcADCSum(int eastwest, int prepost=0) const;
    unsigned short bbcADCSumLargeTile(int eastwest, int prepost=0) const;
    unsigned short bbcEarliestTDC(int eastwest, int prepost=0) const;
    unsigned short bbcTimeDifference() const;

    // FPD
    unsigned short fpd(int eastwest, int module, int pmt, int prepost=0) const; 
    unsigned short fpdSum(int eastwest, int module) const;
    
    // experts only
    char* getTriggerStructure();
    TrgDataType2003* getTriggerStructure2003();  
    
protected:
    TrgDataType2003 *mData;
    
    ClassDef(StTriggerData2003,1) 
};

#endif





