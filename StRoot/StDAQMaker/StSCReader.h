/***************************************************************************
 *
 *  
 *
 * Author: Herbert Ward 
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ SC reader functions
 *
 ***************************************************************************
 *
 *  
 *
 **************************************************************************/
#ifndef _StSCReader_
#define _StSCReader_


class SC_Reader;
class StDAQReader;
class TDataSet;

class StSCReader
{
  public:
    StSCReader(StDAQReader *rd);
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
    TDataSet* getSCTable(unsigned long runno=0);
  protected:
    SC_Reader *fSCImpReader;
    StDAQReader* fDAQReader;
};

#endif
