/***************************************************************************
 * $id: Rich Event Reader.cxx
 * Author: M.J. LeVine and Jon Gans
 ***************************************************************************
 * Description: common definitions for RICH (dummy placeholder)
 *      
 *
 *   change log
 * 02-Jul-99 MJL add navigation code to get to RICHP bank
 * 08-Jul-99 MJL completely change definition - RICH_Reader is independent 
 *               class which is handed a pointer at the constructor invocation
 ***************************************************************************
 *  Opens Event From File, Fills Struct 
 *
 **************************************************************************/

#include "RICH_Reader.hh"

void RICH_Reader::ProcessEvent(const Bank_RICP * RichPTR)
{
  int cramBlock, cramBank, dataDWord ;
  unsigned int *secPTR;
  RICCRAMP * RicCramP;
  RICDATAD * RicDataD;
  unsigned short channelNum,ADC;
  unsigned int lengthOfRicDataD;
  
  for (cramBlock = 0; cramBlock < 2 * MAX_NUM_CRAMS ; cramBlock++) {
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
	      
	      jon.BankType = name2str(RicDataD->header.BankType); // move to global bankType
	      
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
	      
	      for (dataDWord = 0; dataDWord < RicDataD->header.BankLength - sizeof(Bank_Header)/4 ; dataDWord++)
		{ 
		  channelNum = (secPTR[ dataDWord ] >> 16) & 0x3ff; // Channel in High Bits
		  ADC = secPTR[ dataDWord ] & 0xfff;                // ADC in Low Bits
		  jon.RichMatrix[ channelNum / 6 ][ cramBlock * 6 + channelNum % 6] = ADC ; // put in array that
		                                                                            // represents actual
		                                                                           // geometry of pad plane
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
  
  // Stuff jon is adding
  
  // Initialize RichDATA array to 0's
  for(int i = 0 ; i < 96 ; i++)
    for(int j = 0 ; j < 24 ; j++)
      jon.RichMatrix[i][j] = 0;
  
  
  jon.ByteSwapped = 0x04030201;  // This is to signal Endianness if struct is saved
  
  
  ProcessEvent(pBankRICP);    // switch locations in little endian machines
  
 }
  
unsigned short RICH_Reader::GetADCFromCoord(int x,int y){
  return jon.RichMatrix[x][y];}

unsigned short RICH_Reader::GetADCFromCramChannel(int cramBlock,int channelNum){
  return jon.RichMatrix[ cramBlock / 6 ][ cramBlock * 6 + channelNum % 6  ];}

unsigned int RICH_Reader::GetEventNumber(){
  return jon.EventNumber ;}

const char * RICH_Reader::GetBankType(){
  return jon.BankType;}

int RICH_Reader::IsByteSwapped(){
  return jon.ByteSwapped; }
  

















