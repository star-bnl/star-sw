/***************************************************************************
 *
 * $Id: StTriggerData2004.h,v 2.1 2003/12/23 21:56:52 ullrich Exp $
 *
 * Author: Akio Ogawa, Feb 2004
 ***************************************************************************
 *
 * Description:  Concrete implementation of StTriggerData.
 *               For year 2004 only.
 *
 ***************************************************************************
 *
 * $Log: StTriggerData2004.h,v $
 * Revision 2.1  2003/12/23 21:56:52  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#ifndef StTriggerData2004_hh
#define StTriggerData2004_hh

#include "StTriggerData.h"

struct TrgDataType2004;

class StTriggerData2004 : public StTriggerData {
public:
    StTriggerData2004();
    StTriggerData2004(const TrgDataType2004*);
virtual ~StTriggerData2004();
  
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
   
    //ZDCSMD
    unsigned short zdcSMD(int eastwest, int verthori, int strip, int prepost=0) const;

    // experts only
    char* getTriggerStructure();
    TrgDataType2004* getTriggerStructure2004();  
    
protected:
    TrgDataType2004 *mData;
    
    ClassDef(StTriggerData2004,1) 
};

#endif





