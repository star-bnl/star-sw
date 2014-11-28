/***************************************************************************
 *
 * $Id: 
 *
 * Author: Herbert Ward 
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ EEMC reader functions
 *
 ***************************************************************************
 *
 * $Log: 
 *
 **************************************************************************/
#ifndef _StEEMCReader_
#define _StEEMCReader_
#ifdef __APPLE__
#include <sys/types.h>
#endif


class EEMC_Reader;
class StDAQReader;

class StEEMCReader
{
 public:

  StEEMCReader(StDAQReader *rd);

  u_short *getEemcHeadBlock(int fiber, char type);
  u_short *getEemcDataBlock(int fiber, char type);

  u_short getEemcHead(int fiber, int channel, char type);
  u_short getEemcData(int fiber, int channel, char type);
  int isEemcBankIn( char type) ;
   
  virtual ~StEEMCReader();
  virtual int close();
  virtual int Update();

 protected:
  EEMC_Reader *fEEMCImpReader;
  StDAQReader* fDAQReader;
};

#endif
