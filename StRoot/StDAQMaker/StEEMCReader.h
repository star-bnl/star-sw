/***************************************************************************
 *
 *  
 *
 * Author: Herbert Ward 
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ EEMC reader functions
 *
 ***************************************************************************
 *
 *  
 *
 **************************************************************************/
#ifndef _StEEMCReader_
#define _StEEMCReader_


class EEMC_Reader;
class StDAQReader;

class StEEMCReader
{
 public:

  StEEMCReader(StDAQReader *rd);
  int getEemcData2004(int crate,int channel); // For use only by EEMC personnel (ie, Jan Balewski).
  int getTowerAdc(int crate,int channel);     // For use only by EEMC personnel (ie, Jan Balewski).
  int getEEmcData(int crate, int channel, int mapping=-1);
  virtual ~StEEMCReader();
  virtual int close();
  virtual int Update();

 protected:
  EEMC_Reader *fEEMCImpReader;
  StDAQReader* fDAQReader;
};

#endif
