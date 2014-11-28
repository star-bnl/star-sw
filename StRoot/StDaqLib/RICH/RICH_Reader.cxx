/***************************************************************************
 * $id: Rich Event Reader.cxx
 * Author: M.J. LeVine and Jon Gans
 ***************************************************************************
 * Description: common definitions for RICH (dummy placeholder)
 *      
 *
 *   change log
 *                               GetNumOfChannels() returns total number of channels
 * 02-Jul-99 MJL add navigation code to get to RICHP bank
 * 08-Jul-99 MJL completely change definition - RICH_Reader is independent 
 *               class which is handed a pointer at the constructor invocation
 * 12-Mar-00 XZB Token check, EventNumber=Token, consistancy check, BankId
 * 21-Apr-00 xzb add in RichEventReader for standalone data file 
 ***************************************************************************
 * $Log: Opens Event From File, Fills Struct 
 *
 **************************************************************************/

#include "RICH_Reader.hh"
#include "RichEventReader.hh"
#define MAX_ADC 0xFFF 

using namespace OLDEVP;
using namespace RICH_READER;

void RICH_Reader::ProcessEvent(const Bank_RICP * RichPTR)
{
  int cramBlock, cramBank, dataDWord ;
  unsigned int *secPTR;
  RICCRAMP * RicCramP;
  RICDATAD * RicDataD;
  unsigned short channelNum,ADC, Token, SeqWc1, SeqWc2, BankId;
  
  Token = RichPTR->header.Token;
  if (Token==0){
    cout<<"hack: do not know how to deal with token==0"<<endl;
    return;
  }
  BankId=RICH_CRAM_BANKS; // Init to out of range
  mTheRichArray.NumOfChannels=0;
  mTheRichArray.EventNumber = Token;
  for (cramBlock = 0; cramBlock < 2 * RICH_NUM_CRAMS ; cramBlock++) {
                // for each C-RAM block
    if(RichPTR->CramPTR[cramBlock].len > 0)
      {
	RicCramP = (RICCRAMP *)( (unsigned long *)RichPTR + RichPTR->CramPTR[cramBlock].off);
	// Must Make RichPTR and pointer to unsigned int so can add to the offet 
	RicCramP->swap();
	
	for(cramBank=0; cramBank < RICH_CRAM_BANKS ; cramBank++) { 
	  // for each bank in each block
	  if(RicCramP->banks[cramBank].len > 0)
	    {
	      RicDataD = (RICDATAD *)((unsigned long *)RicCramP+RicCramP->banks[cramBank].off);
	      RicDataD->swap();
	      if (BankId==RICH_CRAM_BANKS){
		BankId=RicDataD->header.BankId;
	      }
	      else{
		if(BankId!=RicDataD->header.BankId){
		  printf("Mismatch BankType or more than one BankType Present!\n");
		  return;
		}
	      }
	      mTheRichArray.BankType = name2str(RicDataD->header.BankType); // move to global bankType
	      if (RicDataD->header.Token!=Token){
		mTheRichArray.EventNumber=0;
		printf("Token mismatch: RICP %d !=RICD %d\n",Token, RicDataD->header.Token);
	      }
	      // must use this function as header.BankType does not have terminating NULL
	      // also must Un-swap the banktype as char arrays are not effected by endian changes
	      secPTR = &(RicDataD->chanADC[0]);
	      
	      // go From 0, to the number of ADC/ChannelNumber pairs in back
	      // RicDataD->header.BankLength is total length of Bank
	      // sizeof(BANK_HEADER) is the number of words of the header
	      // their differnce must be the number of ADC/ChannelNumber pairs
	      // NOTE: for completness should probably subtract sizeof(RicDataD->header)/4 instead of
	      // sizeof(BANK_HEADER)/4 however, i am also develuping this code in ROOT
	      // and it has problems with inheritence, so am leave the code like this for portability
	      
	      SeqWc1=RicDataD->header.BankLength - (INT32)sizeof(Bank_Header)/4 ;
	      SeqWc2=0;
	      mTheRichArray.NumOfChannels +=SeqWc1;
	      for (dataDWord = 0; (INT32)dataDWord < SeqWc1; dataDWord++)
		{ 
		  channelNum = (secPTR[ dataDWord ] >> 16) & 0x3ff; // Channel in High Bits
		  ADC = secPTR[ dataDWord ] & MAX_ADC;         // ADC in Low Bits
		  if (channelNum>MAX_CHANNEL_NUM||ADC>MAX_ADC){
		    printf("Wrong Data Word: %x\n",secPTR[dataDWord]);
		  }
		  else{
		    if(mTheRichArray.RichMatrix[ channelNum / 6 ][ cramBlock * 6 + channelNum % 6] ==0){
		    mTheRichArray.RichMatrix[ channelNum / 6 ][ cramBlock * 6 + channelNum % 6] = ADC ; // put in array that
		    SeqWc2++;
		    }
		    else
		      printf("Channel =%d of block %d has been occupied\n",channelNum, cramBlock);
		  }
		                                                                            // represents actual
		                                                                           // geometry of pad plane
		}
	      if (SeqWc1!=SeqWc2) {
		printf("Mismatch Word Counts %d !=%d\n",SeqWc1, SeqWc2);
	      }
	    }
	}
      } 
  }
}



RICH_Reader::RICH_Reader(EventReader *er, Bank_RICP *pRICP)
{
  
  pBankRICP = pRICP; //copy into class data member for use by other methods
  ercpy = er; // squirrel away pointer eventreader for our friends
  if (!pBankRICP->test_CRC())  {
    printf("CRC error in RICP: %s %d\n",__FILE__,__LINE__) ;
  }
  
  if (pBankRICP->swap() < 0) {
    printf("swap error in RICP: %s %d\n",__FILE__,__LINE__) ;
  }
  
  pBankRICP->header.CRC = 0;
  //do whatever else needs to be done
    int Token = pBankRICP->header.Token;
  Bank_DATAP *dp = (Bank_DATAP *)ercpy->getDATAP(); 
  if(Token !=dp->header.Token){
    printf("Token mismatch between global %d and RICH %d\n",dp->header.Token,Token);
  }
  
  // Stuff jon is adding
  
  // Initialize RichDATA array to 0's
  for(int i = 0 ; i <RICH_PAD ; i++) 
      for(int j = 0 ; j < RICH_ROW; j++) 
      mTheRichArray.RichMatrix[i][j] = 0;
  
  
  mTheRichArray.ByteSwapped = 0x04030201;  // This is to signal Endianness if struct is saved
  
  
  ProcessEvent(pBankRICP);    // switch locations in little endian machines
  
 }

RICH_Reader::RICH_Reader(RichEventReader *er, Bank_RICP *pRICP)
{
  
  pBankRICP = pRICP; //copy into class data member for use by other methods
  if (!pBankRICP->test_CRC())  {
    printf("CRC error in RICP: %s %d\n",__FILE__,__LINE__) ;
  }
  
  if (pBankRICP->swap() < 0) {
    printf("swap error in RICP: %s %d\n",__FILE__,__LINE__) ;
  }
  
  pBankRICP->header.CRC = 0;
  //do whatever else needs to be done

  // Stuff jon is adding
  
  // Initialize RichDATA array to 0's
  for(int i = 0 ; i <RICH_PAD ; i++) 
      for(int j = 0 ; j < RICH_ROW; j++) 
      mTheRichArray.RichMatrix[i][j] = 0;
  
  
  mTheRichArray.ByteSwapped = 0x04030201;  // This is to signal Endianness if struct is saved
  
  
  ProcessEvent(pBankRICP);    // switch locations in little endian machines
  
 }
  
unsigned short RICH_Reader::GetADCFromCoord(int x,int y)
{
  return mTheRichArray.RichMatrix[x][RICH_ROW-y-1]; // swapping the coordinate
}

unsigned short RICH_Reader::GetADCFromCramChannel(int cramBlock,int channelNum){
  return mTheRichArray.RichMatrix[ channelNum / 6 ][ cramBlock * 6 + channelNum % 6  ];}

unsigned int RICH_Reader::GetEventNumber(){
  return mTheRichArray.EventNumber ;}

unsigned int RICH_Reader::GetNumOfChannels(){
  return mTheRichArray.NumOfChannels ;}

const char * RICH_Reader::GetBankType(){
  return mTheRichArray.BankType;}

int RICH_Reader::IsByteSwapped(){
  return mTheRichArray.ByteSwapped; }
  
