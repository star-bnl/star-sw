/***************************************************************************
 *
 * $Id: 
 *
 * Author: Herbert Ward 
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ SC reader functions
 *
 ***************************************************************************
 *
 * $Log: 
 *
 **************************************************************************/
#ifndef _StSCReader_
#define _StSCReader_

class TDataSet;
struct sc_t;

class StSCReader
{
  public:
    StSCReader(sc_t *daqLegacy, unsigned int utime);
    virtual ~StSCReader();
    char thereIsSCData(); // returns FALSE if there is no SC data in the .daq file
    virtual int close();
    virtual int Update();
    double getCTBWest();
    double getCTBEast();
    double getCTBOrTOFp();
    double getTOFp();
    double getZDCWest();
    double getZDCEast();
    double getZDCX();
    double getZDCWestNoKiller();
    double getZDCEastNoKiller();
    double getZDCXNoKiller();
    double getMult();
    double getL0();
    double getBBCX();
    double getBBCXCTB();
    double getBBCWest();
    double getBBCEast();
    double getBBCYellowBkg();
    double getBBCBlueBkg();
    double getPVPDWest();
    double getPVPDEast();
    unsigned int getValid();
    unsigned int getTime();
    int getTimelag();
    float getMagField();
    TDataSet* getSCTable(unsigned long runno=0);
  protected: 
    void FillTime(unsigned int utime);

    sc_t *fSC;
    short flipBBCBkg;
    short useNoKillers;
};

#endif
