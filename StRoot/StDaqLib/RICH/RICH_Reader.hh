 /***************************************************************************
 * $id: Rich Event Reader
 * Author: Jon Gans and M.J. LeVine
 ***************************************************************************
 * Description: common definitions for RICH (dummy placeholder)
 *      
 *
 *   change log
 * 02-Jul-99 MJL add navigation code to get to RICHP bank
 * 08-Jul-99 MJL completely change definition - RICH_Reader is independent 
 *               class which is handed a pointer at the constructor invocation
 * 22-Nov-99 MJL fixed bug in array size  unsigned short RichMatrix[][]
 * 12-Mar-00  XZB GetNumOfChannels() returns total number of channels
 * 21-Apr-00 xzb Add in RichEventReader for standalone data file 
 ***************************************************************************
 * $Log: Opens Event From File, Fills Struct 
 *
 **************************************************************************/
#ifndef RICH_READER_HH
#define RICH_READER_HH

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "StDaqLib/GENERIC/swaps.hh"
#include "StDaqLib/RICH/RichEventReader.hh"

#define RICH_CRAM_BANKS 8   /* data banks lowest level */
#define RICH_NUM_CRAMS  8
#define MAX_NUM_CRAMS   8
#define MAX_CHANNEL_NUM 960
#define RICH_NUM_ROWS_PER_CRAM 6
#define RICH_PAD  MAX_CHANNEL_NUM/RICH_NUM_ROWS_PER_CRAM 
#define RICH_ROW 2*RICH_NUM_CRAMS*RICH_NUM_ROWS_PER_CRAM
//#define MAX_CHANNEL_NUM 575  

namespace RICH_READER {
struct offlen {
  unsigned int off;
  unsigned int len;		
} ;	
}
struct Bank_RICP: public Bank
{
   struct RICH_READER::offlen CramPTR[2*MAX_NUM_CRAMS] ; /* number of CRAM blocks */
  struct RICH_READER::offlen Reserved[2];
  
} ;

struct RICCRAMP: public Bank 
{ 
  struct RICH_READER::offlen banks[RICH_CRAM_BANKS];
};


struct RICDATAD: public Bank{
  
  unsigned int chanADC[1];  // This is the first word of the Channel/ADC array
                 // there are many more following it, specified by the
                 // total amount of words, including this first one is
                 // RICDATAD->header.BankLenth - sizeof(RICHDATAD->header)/4
};

struct RichDATA{
  char * BankType; // Will be filled with a 9 char array (8 letters + NULL)
  unsigned int ByteSwapped ; // Should be 0x04030201
  unsigned int EventNumber; //Token number
  unsigned int NumOfChannels; //Total number of channels
  unsigned short RichMatrix[RICH_PAD] [RICH_ROW] ; // Matrix of ADC's in Physical Positions
};




class StRichReaderInterface {
public:
    virtual ~StRichReaderInterface(){}

    virtual unsigned short GetADCFromCoord(int, int)       = 0;
    virtual unsigned short GetADCFromCramChannel(int, int) = 0;
    virtual unsigned int GetEventNumber()                  = 0;
};


class RICH_Reader : public StRichReaderInterface {
   void ProcessEvent(const Bank_RICP * RichPTR);

public:

RICH_Reader(EventReader *er, Bank_RICP *pRICP);
RICH_Reader(RichEventReader *er, Bank_RICP *pRICP);

~RICH_Reader(){}; 

  unsigned short GetADCFromCoord(int x,int y);
  unsigned short GetADCFromCramChannel(int cramBlock,int channelNum);
  unsigned int   GetEventNumber();  //Token number actaully
  unsigned int GetNumOfChannels();
    
    const char * GetBankType();
    
    int IsByteSwapped();

protected:

    // copy of EventReader pointer
    EventReader *ercpy;

    // Bank Pointers
    struct Bank_RICP *pBankRICP;
    
    RichDATA mTheRichArray;
                                                                      // in tic, each row is 96 channels
                                                                              // and it takes 6 rows to fit each cramBank
};

RICH_Reader *getRICHReader(EventReader *er);

#endif












