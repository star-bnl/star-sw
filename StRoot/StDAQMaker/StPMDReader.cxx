/***************************************************************************
 *$Id: StPMDReader.cxx,v 1.3 2004/05/14 21:44:31 perev Exp $
 * 
 * StPMDReader.cxx
 * Author: Susanta and Subhasis 
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ PMD reader classes
 **************************************************************************
 *$Log: StPMDReader.cxx,v $
 *Revision 1.3  2004/05/14 21:44:31  perev
 *Check non existing PMDImpReader added
 *
 *Revision 1.2  2003/12/10 10:31:01  subhasis
 *loop for No of channels read is changed to PMD_CRAMS_CH_MAX
 *
 **************************************************************************/
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <assert.h>
#include "StDAQReader.h"
#include "StPMDReader.h"
#include "StDaqLib/PMD/PMD_Reader.hh"

typedef EventInfo DAQEventInfo;
  
StPMDReader::StPMDReader(StDAQReader *daqr) {
  fDAQReader = daqr;
  fPMDImpReader = ::getPMDReader(daqr->getEventReader());
  
  getPMD_ADC();

}


StPMDReader::~StPMDReader() {
}

int StPMDReader::close() {
  return 1;
}

int StPMDReader::Update() {
  delete fPMDImpReader;
  fPMDImpReader = ::getPMDReader(fDAQReader->getEventReader());
  getPMD_ADC();
 // close();
 return 1;
}

/* int StPMDReader::getPMD_ADCD(int SEC,int CRAM, int BLOCK,int CHANNEL, unsigned short& ADC )
 {

   if(!fPMDImpReader->getPMD_ADCD(SEC,CRAM,BLOCK,CHANNEL,ADC))return 0;
   return 1; //1 is good
 }
*/
int StPMDReader::NPMDHits()
 {
   return (fPMDImpReader) ? fPMDImpReader->NPMDHits():0;
 }

void  StPMDReader::getPMD_ADC()
{
  if (fPMDImpReader) {mPmd = fPMDImpReader->getPMD_ADC();}
  else               {memset(&mPmd,0,sizeof(mPmd));     }
}

int StPMDReader::getNoOfChannelsInCramBlock(int sec, int cram, int blk)
{
 return mPmd.no_of_channels_in_cram_blk[sec][cram][blk] ;
}

int StPMDReader::getAllPmdCpvData(int *adc)
{
	int no_ch=0;
	for(int sec=0; sec<PMD_SECTOR; sec++){
		for(int cram=0; cram< PMD_CRAMS_MAX; cram++){
			for(int blk=0; blk <PMD_CRAMS_BLOCK; blk++ ){
				for( int ch=0; ch < PMD_CRAMS_CH_MAX; ch++, adc++){
					*adc=mPmd.adc[sec][cram][blk][ch];
					no_ch++;
				}
			}
		}
	}
	return no_ch;
}

int StPMDReader::getAllPmdCpvDataChannelByChannel(int sec, int cram, int blk, int channel)
{
	return mPmd.adc[sec][cram][blk][channel];
}

int StPMDReader::getNoOfChannelsInPmdChain(int chain_no)
{
        int sec, cram, blk;
        cram=chain_no; blk=0; // PMD is in Block0
	if (chain_no < 12 )
		sec=0;
	else
		sec=1;
	  return mPmd.no_of_channels_in_cram_blk[sec][cram][blk] ;
}

int StPMDReader::getPmdChainData(int chain_no, int *data)
{
        int sec, cram, blk, channel;
        cram=chain_no; blk=0;   //PMD is in Block0
	if (chain_no < 12 )
		sec=0;
	else
		sec=1;
	  for(channel=0; channel < PMD_CRAMS_CH_MAX; channel++ , data++){
	    *data=  mPmd.adc[sec][cram][blk][channel];
	 }  

	  return mPmd.no_of_channels_in_cram_blk[sec][cram][blk] ;
}


int StPMDReader::getNoOfChannelsInCpvChain(int chain_no)
{
        int sec, cram, blk;
        cram=chain_no; blk=1; // CPV is in Block1
	if (chain_no < 12 )
		sec=0;
	else
		sec=1;
	  return mPmd.no_of_channels_in_cram_blk[sec][cram][blk] ;
}

int StPMDReader::getCpvChainData(int chain_no, int *data)
{
        int sec, cram, blk, channel;
        cram=chain_no; blk=1;   //CPV is in Block1
	if (chain_no < 12 )
		sec=0;
	else
		sec=1;
	  for(channel=0; channel <PMD_CRAMS_CH_MAX; channel++ , data++){
	    *data=  mPmd.adc[sec][cram][blk][channel];
	 }  

	  return mPmd.no_of_channels_in_cram_blk[sec][cram][blk] ;
}

int StPMDReader::getAllPmdCpvPed(int *ped)
{
	int no_ch=0;
	for(int sec=0; sec<PMD_SECTOR; sec++){
		for(int cram=0; cram< PMD_CRAMS_MAX; cram++){
			for(int blk=0; blk <PMD_CRAMS_BLOCK; blk++ ){
				for( int ch=0; ch < PMD_CRAMS_CH_MAX; ch++, ped++){
					*ped=mPmd.ped[sec][cram][blk][ch];
	if(mPmd.ped[sec][cram][blk][ch]>0)no_ch++;
				}
			}
		}
	}
	return no_ch;
}
int StPMDReader::getAllPmdCpvRms(int *rms)
{
	int no_ch=0;
	for(int sec=0; sec<PMD_SECTOR; sec++){
		for(int cram=0; cram< PMD_CRAMS_MAX; cram++){
			for(int blk=0; blk <PMD_CRAMS_BLOCK; blk++ ){
				for( int ch=0; ch < PMD_CRAMS_CH_MAX; ch++, rms++){
					*rms=mPmd.rms[sec][cram][blk][ch];
	if(mPmd.rms[sec][cram][blk][ch]>0)no_ch++;
					no_ch++;
				}
			}
		}
	}
	return no_ch;
}
