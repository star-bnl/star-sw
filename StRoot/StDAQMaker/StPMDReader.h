/***************************************************************************
 *
 * Author: Susanta and Subhasis
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ PMD reader functions
 *
 ***************************************************************************
 *
 **************************************************************************/
#include "StDaqLib/PMD/PMD_Reader.hh"

#ifndef _StPMDReader_
#define _StPMDReader_


class PMD_Reader;
class StDAQReader;

class StPMDReader
{
 public:
  StPMDReader(StDAQReader *rd);
  virtual ~StPMDReader();
  virtual int close();
  virtual int Update();
  void getPMD_ADC();
  int NPMDHits();
  int getNoOfChannelsInCramBlock(int, int, int );   // sec/Crate_No, Cram, Blk , Return No. Of channels in that Cram Block 
  int getAllPmdCpvData(int *); // get ADC values, Return total No. of Channel
  int getAllPmdCpvDataChannelByChannel(int, int, int, int); // Sector/Crate No.,Crams,Block,Channel, Return ADC Value 
  int getNoOfChannelsInPmdChain(int );   // Chain No.  , Return No. Of channels in that chain
  int getPmdChainData(int, int *); // Chain No., ADC Value, Return No. Of channels in that chain
  int getNoOfChannelsInCpvChain(int );   // Chain No.  , Return No. Of channels in that chain
  int getCpvChainData(int, int *); // Chain No., ADC Value, Return No. Of channels in that chain

 protected:
  Bank_DATA mPmd;
  Bank_DATA *mPmdp;
  StDAQReader* fDAQReader;
  PMD_Reader *fPMDImpReader;
};

#endif
