/***************************************************************************
 *
 * $Id: StTriggerData.h,v 2.2 2003/05/21 03:58:44 ullrich Exp $
 *
 * Author: Akio Ogawa & Mirko Planinic, Feb 2003
 ***************************************************************************
 *
 * Description: abstract class for trigger data
 *
 ***************************************************************************
 *
 * $Log: StTriggerData.h,v $
 * Revision 2.2  2003/05/21 03:58:44  ullrich
 * Added more methods to retrieve spin bits.
 *
 * Revision 2.1  2003/04/16 17:47:41  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTriggerData_hh
#define StTriggerData_hh

#include "StObject.h"

class StTriggerData : public StObject {
public:
    StTriggerData();
    ~StTriggerData();
    
    virtual void dump() const = 0;   //dump data into text
    
    // version and data type information
    virtual int year() const;                          // year of the data
    virtual unsigned int version() const = 0;          // TrgDataType Version Number 
    virtual unsigned int numberOfPreXing() const = 0;  // # of pre xing data for detectors
    virtual unsigned int numberOfPostXing() const = 0; // # of post xing data for detectors
  
    // generic trigger infomations
    virtual unsigned int token() const = 0;
    virtual unsigned int triggerWord() const = 0;
    virtual unsigned int actionWord() const = 0;  
    virtual unsigned int busyStatus() const;
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
    virtual unsigned short emcLayer2DSM(int channel) const;
    virtual unsigned short fpdLayer2DSM(int channel) const;

    // CTB
    virtual unsigned short ctb(int pmt, int prepost=0) const;

    // MWC
    virtual unsigned short mwc(int sector, int prepost=0) const;

    // ZDC  east=0, west=1
    virtual unsigned short zdcUnAttenuated(int eastwest, int prepost=0) const;
    virtual unsigned short zdcAttenuated(int eastwest, int prepost=0) const;
    virtual unsigned short zdcADC(int eastwest, int pmt, int prepost=0) const;
    virtual unsigned short zdcTDC(int eastwest, int prepost=0) const;

    // EMC
    virtual unsigned short bemcHighTower(int eta, int phi, int prepost=0) const;
    virtual unsigned short bemcJetPatch (int eta, int phi, int prepost=0) const;
    virtual unsigned short eemcHighTower(int eta, int phi, int prepost=0) const;
    virtual unsigned short eemcJetPatch (int eta, int phi, int prepost=0) const;
    
    // BBC east=0, west=1
    virtual unsigned short bbcADC(int eastwest, int pmt, int prepost=0) const;
    virtual unsigned short bbcTDC(int eastwest, int pmt, int prepost=0) const;
    virtual unsigned short bbcADCSum(int eastwest, int prepost=0) const;
    virtual unsigned short bbcADCSumLargeTile(int eastwest, int prepost=0) const;
    virtual unsigned short bbcEarliestTDC(int eastwest, int prepost=0) const;
    virtual unsigned short bbcTimeDifference() const;
  
    // FPD east=0, west=1. north=0, south=1, top=2, bottom=3, north preshower=4, south preshower=5
    virtual unsigned short fpd(int eastwest, int module, int pmt, int prepost=0) const; 
    virtual unsigned short fpdSum(int eastwest, int module) const;
  
    // Experts only!
    virtual char* getTriggerStructure() = 0;
  
protected:
    int mYear;
    int prepostAddress(int prepost) const; //get pre&post xsing addess, return negative if bad.
        
    ClassDef(StTriggerData,1) 
};

//
//  Inline functions. Most of them return a default value (zero). Not all 
//  of them will be overwritten by classes inheriting from StTriggerData.
//
inline int StTriggerData::year() const {return mYear;}
inline unsigned int StTriggerData::busyStatus() const {return 0;}
inline unsigned int StTriggerData::bunchCounterHigh() const {return 0;}
inline unsigned int StTriggerData::bunchCounterLow() const {return 0;}
inline unsigned int StTriggerData::bunchId48Bit() const {return 0;}
inline unsigned int StTriggerData::bunchId7Bit() const {return 0;}
inline unsigned int StTriggerData::spinBit() const {return 0;}
inline unsigned int StTriggerData::spinBitYellowFilled() const {return 0;}
inline unsigned int StTriggerData::spinBitYellowUp() const {return 0;}
inline unsigned int StTriggerData::spinBitYellowDown() const {return 0;}
inline unsigned int StTriggerData::spinBitYellowUnpol() const {return 0;}
inline unsigned int StTriggerData::spinBitBlueFilled() const {return 0;}
inline unsigned int StTriggerData::spinBitBlueUp() const {return 0;}
inline unsigned int StTriggerData::spinBitBlueDown() const {return 0;}
inline unsigned int StTriggerData::spinBitBlueUnpol() const {return 0;}
inline unsigned short StTriggerData::lastDSM(int channel) const {return 0;};
inline unsigned short StTriggerData::vertexDSM(int channel) const {return 0;}
inline unsigned short StTriggerData::ctbLayer1DSM(int channel) const {return 0;}
inline unsigned short StTriggerData::ctbLayer2DSM(int channel) const {return 0;}
inline unsigned short StTriggerData::emcLayer2DSM(int channel) const {return 0;}
inline unsigned short StTriggerData::fpdLayer2DSM(int channel) const {return 0;}
inline unsigned short StTriggerData::ctb(int pmt, int prepost) const {return 0;}
inline unsigned short StTriggerData::mwc(int sector, int prepost) const {return 0;}
inline unsigned short StTriggerData::zdcUnAttenuated(int eastwest, int prepost) const {return 0;}
inline unsigned short StTriggerData::zdcAttenuated(int eastwest, int prepost) const {return 0;}
inline unsigned short StTriggerData::zdcADC(int eastwest, int pmt, int prepost) const {return 0;}
inline unsigned short StTriggerData::zdcTDC(int eastwest, int prepost) const {return 0;}
inline unsigned short StTriggerData::bemcHighTower(int eta, int phi, int prepost) const {return 0;}
inline unsigned short StTriggerData::bemcJetPatch (int eta, int phi, int prepost) const {return 0;}
inline unsigned short StTriggerData::eemcHighTower(int eta, int phi, int prepost) const {return 0;}
inline unsigned short StTriggerData::eemcJetPatch (int eta, int phi, int prepost) const {return 0;}
inline unsigned short StTriggerData::bbcADC(int eastwest, int pmt, int prepost) const {return 0;}
inline unsigned short StTriggerData::bbcTDC(int eastwest, int pmt, int prepost) const {return 0;}
inline unsigned short StTriggerData::bbcADCSum(int eastwest, int prepost) const {return 0;}
inline unsigned short StTriggerData::bbcADCSumLargeTile(int eastwest, int prepost) const {return 0;}
inline unsigned short StTriggerData::bbcEarliestTDC(int eastwest, int prepost) const {return 0;}
inline unsigned short StTriggerData::bbcTimeDifference() const {return 0;}
inline unsigned short StTriggerData::fpd(int eastwest, int module, int pmt, int prepost) const {return 0;} 
inline unsigned short StTriggerData::fpdSum(int eastwest, int module) const {return 0;}
  
#endif

