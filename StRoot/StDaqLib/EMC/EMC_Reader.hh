/***************************************************************************
 *  
 * Authors: Herbert Ward, Subhasis Chattopadhya, Mike Levine
 ***************************************************************************
 * Description: common definitions for EMC Bank and EMC_Reader
 ***************************************************************************
 *  
 *
 **************************************************************************/
#ifndef EMC_READER_HH
#define EMC_READER_HH
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "StDaqLib/GENERIC/swaps.hh"


struct Bank_EMCP: public Bank
{
  struct Pointer EMCSecPointer[6] ; /* BEMC (tower,prs,smd), EEMC */
};

struct Bank_EMCSECP: public Bank
{
  struct Pointer FiberPointer[8] ; /* No of fibere to daq from each subdetector, can be maximum 8 */
};

struct Bank_EMCRBP: public Bank
{
  struct Pointer EMCADCR;  /* RAW DATA */
  struct Pointer EMCADCD ; /* one tower block*/
  struct Pointer EMCPEDR; 
  struct Pointer EMCRMSR ; /* one tower block*/
};

struct Bank_TOWERADCR: public Bank
{
        int dummy ;                 // 4 bytes of junk - should read as  0x00c0ffee
        short fiberHeader[64] ;     // 128 bytes of standard FEE header
        short TDCHeader[120] ;      // TDC header - error flag etc
        short fiberData[4800] ;     // stuff on the fiber - (Raw data)
};

struct BTOWERDATA
{
  char * BankType;                            // Will be filled with a 9 char array (8 letters + NULL)
  unsigned int   DetFlag;                     // DAQ Detector flag for BEMC, PRS, SMDE or SMDP
  unsigned int   EventNumber;                 // Token number
  unsigned int   PedFlag;                     // Pedestal subtracted or not (??)
  unsigned int   ReceivedByteCount;           // Total number of Bytes 
  unsigned int   NTowerHits;                  // Total number of valid channels
  unsigned int   TDCErrorFlag;                // Error from TDC
  unsigned int   NTDCChannels;                // Total number of valid TDC channels      

  unsigned short TDCData[30][160];            // data from each TDC channel
  unsigned short TDCError[30];                // vector with TDC error for each TDC channel
  unsigned short TDCToken[30];                // vector with crate token for each TDC channel
  unsigned short TDCTrigger[30];              // vector with trigger number for each TDC channel
  unsigned short TDCCrateId[30];              // vector with crate Id for each TDC channel
  unsigned short TDCCount[30];                // vector with byte count for each TDC channel
  
  unsigned short TowerMatrix[120][20][2];     // Matrix of ADC's in Physical Positions
  unsigned short TowerADCArray[4800];         // Matrix of ADC's as obtained from daq
};

// similar banks for ADCR, ADCD, PED and RMS

struct Bank_BTOWERADCR: public BTOWERDATA{};
struct Bank_BTOWERADCD: public BTOWERDATA{};
struct Bank_BTOWERPEDR: public BTOWERDATA{};
struct Bank_BTOWERRMSR: public BTOWERDATA{};

struct Bank_SMDADCR: public Bank  
{   
        int dummy ;                // 4 bytes of junk - should read as  0x00c0ffee
        short fiberHeader[128] ;   // 256 bytes (64 words of 32 bits) of standard FEE header the same for event pool
        short fiberData[4800] ;    // stuff on the fiber - (Raw data)
};

struct BSMDDATA
{
  char            *BankType;                    // Will be filled with a 9 char array (8 letters + NULL)
  unsigned int    DetFlag;                      // Daq detector flag
  unsigned int    EventNumber;                  // Token number
  unsigned int    PedFlag;                      // Pedestal subtracted or not (??)
  unsigned int    SMDErrorFlag;                 // Error from TDC
  unsigned int    ReceivedByteCount;            // Total number of channels
  unsigned int    NSmdHits;                     // Total number of channels
  unsigned int    TimeBin[8];                   // I do not know if 8 fibers will have different time bin
  unsigned short  SMDADCArray[8][4800] ;        // Matrix of ADC's as obtained from daq
  unsigned short  SmdE_ADCMatrix[120][150] ;    // Matrix of ADC's in Physical Positions for SMD_Eta
  unsigned short  SmdP_ADCMatrix[120][10][15] ; // Matrix of ADC's in Physical Positions for SMD_Phi
};


struct Bank_BSMDADCR: public BSMDDATA{};
struct Bank_BSMDADCD: public BSMDDATA{};
struct Bank_BSMDPEDR: public BSMDDATA{};
struct Bank_BSMDRMSR: public BSMDDATA{};



class EMC_Reader 
{
      void              ProcessEvent(const Bank_EMCP *EmcPTR);

  public:
                        EMC_Reader(EventReader *er, Bank_EMCP *pEMCP);
                        ~EMC_Reader() {}; 

      Bank_BTOWERADCR&  getBTOWERADCR();
      int               getTowerADC(int,int,int,unsigned short&); 
      int               getTowerADC(int,unsigned short&);         
      int               NTowerHits();

      Bank_BSMDADCR&    getSMD_ADCR();

      int               getSMD_ADC(int,int,unsigned short&);
      int               getSMDE_ADC(int,int,unsigned short&);     
      int               getSMDP_ADC(int,int,int,unsigned short&);
      int               getSMD_TIMEBIN(int,unsigned int&);
      int               NSmdHits();

  protected:

      // copy of EventReader pointer
      EventReader       *ercpy;
      Bank_EMCP         *pBankEMCP;

      // Data Banks
      Bank_BTOWERADCR   mTheTowerAdcR;
      Bank_BTOWERADCD   mTheTowerAdcD;
      Bank_BTOWERPEDR   mTheTowerPedR;
      Bank_BTOWERRMSR   mTheTowerRMSR;

      Bank_BSMDADCR     mTheSmdAdcR;
      Bank_BSMDADCD     mTheSmdAdcD;
      Bank_BSMDPEDR     mTheSmdPedR;
      Bank_BSMDRMSR     mTheSmdRMSR;
      bool              mTowerPresent;
      bool              mSmdPresent;
};

EMC_Reader *getEMCReader(EventReader *er);


#endif
