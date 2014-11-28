/***************************************************************************
 *
 * $Id: 
 *
 * Author: Herbert Ward and Subhasis
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ EMC reader functions
 *
 ***************************************************************************
 *
 * $Log: 
 *
 **************************************************************************/
#ifndef _StEMCReader_
#define _StEMCReader_


class EMC_Reader;
class StDAQReader;

class StEMCReader
{
 public:
  StEMCReader(StDAQReader *rd);
  virtual ~StEMCReader();
  virtual int close();
  virtual int Update();
  int getTowerADC(int mod,   int e, int s, unsigned short& ADC );
  int getTowerADC(int index,               unsigned short& ADC );
  int getSMD_ADC (int index, int fiber,    unsigned short& ADC );
  int getSMDE_ADC(int mod,   int e,        unsigned short& ADC );
  int getSMDP_ADC(int mod,   int bin,int s,unsigned short& ADC );
  int getSMD_TIMEBIN(int fiber,            unsigned int& TimeBin);
  int NSmdHits();
  int NTowerHits();
  EMC_Reader* getBemcReader(); // From Alex P. Suaide at Wayne State University, Aug 28 2002.

 protected:
  EMC_Reader *fEMCImpReader;
  StDAQReader* fDAQReader;
};

#endif
