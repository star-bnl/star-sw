/***************************************************************************
 *
 * $Id: 
 *
 * Author: Herbert Ward 
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ SSD reader functions
 *
 ***************************************************************************
 *
 * $Log: 
 *
 **************************************************************************/
#ifndef _StSSDReader_
#define _StSSDReader_


class SSD_Reader;
class StDAQReader;

class StSSDReader
{
  public:
    StSSDReader(StDAQReader *rd);
    virtual ~StSSDReader();
    virtual int close();
    virtual int Update();
    int getSsdData(int ladder,char eastWest,int channel,int& data,int& pedestal,int& noise);
  protected:
    SSD_Reader *fSSDImpReader;
    StDAQReader* fDAQReader;
};

#endif
