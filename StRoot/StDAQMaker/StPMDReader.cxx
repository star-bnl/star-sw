/***************************************************************************
 *
 * Author: Susanta and Subhasis 
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ PMD reader classes
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
  fPMDImpReader=0;
   fDAQReader = daqr;
  delete fPMDImpReader;
  fPMDImpReader = ::getPMDReader(daqr->getEventReader());
  
  getPMD_ADC();

}


StPMDReader::~StPMDReader() {
}

int StPMDReader::close() {
//  delete fEMCImpReader; fEMCImpReader=0;
  return 1;
}

int StPMDReader::Update() {
  delete fPMDImpReader;
  fPMDImpReader = ::getPMDReader(fDAQReader->getEventReader());
  getPMD_ADC();
 // close();
 cout<<" PMD Updated**"<<endl;
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
   return fPMDImpReader->NPMDHits();
 }

void  StPMDReader::getPMD_ADC()
{
	mPmd= fPMDImpReader->getPMD_ADC();
}

int StPMDReader::getNoOfChannelsInCramBlock(int sec, int cram, int blk)
{
 return mPmd.no_of_channels_in_cram_blk[sec][cram][blk] ;
}

int StPMDReader::getAllPmdCpvData(int *adc)
{
	int no_ch=0;
	for(int sec=0; sec<2; sec++){
		for(int cram=0; cram< PMD_CRAMS_MAX; cram++){
			for(int blk=0; blk <2; blk++ ){
				for( int ch=0; ch < mPmd.no_of_channels_in_cram_blk[sec][cram][blk]; ch++, adc++){
					*adc=mPmd.adc[sec][cram][blk][ch];
					no_ch++;
	//	cout << sec <<":" << cram <<":" <<blk <<":"<<ch<<":"<< *adc << endl;
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
	  for(channel=0; channel < mPmd.no_of_channels_in_cram_blk[sec][cram][blk]; channel++ , data++){
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
	  for(channel=0; channel < mPmd.no_of_channels_in_cram_blk[sec][cram][blk]; channel++ , data++){
	    *data=  mPmd.adc[sec][cram][blk][channel];
	 }  

	  return mPmd.no_of_channels_in_cram_blk[sec][cram][blk] ;
}
