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
  int getTowerAdc(int crate,int channel); // For use only by EEMC personnel (ie, Jan Balewski).
  virtual ~StEEMCReader();
  virtual int close();
  virtual int Update();

 protected:
  EEMC_Reader *fEEMCImpReader;
  StDAQReader* fDAQReader;
};

#endif
