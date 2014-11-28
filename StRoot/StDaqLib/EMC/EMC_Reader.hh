/***************************************************************************
 * $Id: 
 * Authors: Herbert Ward, Subhasis Chattopadhya, Mike Levine
 ***************************************************************************
 * Description: common definitions for EMC Bank and EMC_Reader
 ***************************************************************************
 * $Log: 
 *
 **************************************************************************/

/*!\class EMC_Reader
\author Subhasis, Herbert Ward and Alexandre A. P. Suaide

Main EMC reader for towers and SMD.
*/ 

#ifndef EMC_READER_HH
#define EMC_READER_HH
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "StDaqLib/GENERIC/swaps.hh"
#include "StDaqLib/TRG/TRG_Reader.hh"

struct Bank_EMCP: public Bank
{
  struct Pointer EMCSecPointer[6] ; /* BEMC (tower,prs,smd), EEMC */
};

struct Bank_EMCSECP: public Bank
{
  struct Pointer FiberPointer[12] ; /* No of fibere to daq from each subdetector, can be maximum 12 */
};

struct Bank_EMCRBP: public Bank
{
  struct Pointer EMCADCR;  /* RAW DATA */
  struct Pointer EMCADCD ; /* one tower block*/
  struct Pointer EMCPEDR; 
  struct Pointer EMCRMSR ; /* one tower block*/
};

/*!\struct Bank_TOWERADCR
\author Subhasis, Herbert Ward and Alexandre A. P. Suaide

Structure that defines Tower data format.
*/ 
struct Bank_TOWERADCR: public Bank
{
        int dummy ;                 ///< 4 bytes of junk - should read as  0x00c0ffee
        short fiberHeader[64] ;     ///< 128 bytes of standard FEE header
        short TDCHeader[120] ;      ///< TDC header - error flag etc
        short fiberData[4800] ;     ///< stuff on the fiber - (Raw data)
};

/*!\struct BTOWERDATA
\author Subhasis, Herbert Ward and Alexandre A. P. Suaide

Structure that defines Tower data format after decodification.
*/ 
struct BTOWERDATA
{
  char * BankType;                            ///< Will be filled with a 9 char array (8 letters + NULL)
  unsigned int   DetFlag;                     ///< DAQ Detector flag for BEMC, PRS, SMDE or SMDP
  unsigned int   EventNumber;                 ///< Token number
  unsigned int   PedFlag;                     ///< Pedestal subtracted or not (??)
  unsigned int   ReceivedByteCount;           ///< Total number of Bytes 
  unsigned int   NTowerHits;                  ///< Total number of valid channels
  unsigned int   TDCErrorFlag;                ///< Error from TDC
  unsigned int   NTDCChannels;                ///< Total number of valid TDC channels      

  unsigned short TDCData[30][160];            ///< data from each TDC channel
  unsigned short TDCError[30];                ///< vector with TDC error for each TDC channel
  unsigned short TDCToken[30];                ///< vector with crate token for each TDC channel
  unsigned short TDCTrigger[30];              ///< vector with trigger number for each TDC channel
  unsigned short TDCCrateId[30];              ///< vector with crate Id for each TDC channel
  unsigned short TDCCount[30];                ///< vector with byte count for each TDC channel
  
  unsigned short TowerMatrix[120][20][2];     ///< Matrix of ADC's in Physical Positions
  unsigned short TowerADCArray[4800];         ///< Matrix of ADC's as obtained from daq
  unsigned short TDCHeader[120];              ///< This is the TDC event header
};

// similar banks for ADCR, ADCD, PED and RMS

struct Bank_BTOWERADCR: public BTOWERDATA{};
struct Bank_BTOWERADCD: public BTOWERDATA{};
struct Bank_BTOWERPEDR: public BTOWERDATA{};
struct Bank_BTOWERRMSR: public BTOWERDATA{};

/*!\struct Bank_SMDADCR
\author Subhasis, Herbert Ward and Alexandre A. P. Suaide

Structure that defines SMD data format.
*/ 
struct Bank_SMDADCR: public Bank  
{   
        int dummy ;                ///< 4 bytes of junk - should read as  0x00c0ffee
        short fiberHeader[128] ;   ///< 256 bytes (64 words of 32 bits) of standard FEE header the same for event pool
        short fiberData[4800] ;    ///< stuff on the fiber - (Raw data)
};

/*!\struct BSMDDATA
\author Subhasis, Herbert Ward and Alexandre A. P. Suaide

Structure that defines SMD data format after decodification
*/ 
struct BSMDDATA
{
  char            *BankType;                    ///< Will be filled with a 9 char array (8 letters + NULL)
  unsigned int    DetFlag;                      ///< Daq detector flag
  unsigned int    EventNumber;                  ///< Token number
  unsigned int    PedFlag;                      ///< Pedestal subtracted or not (??)
  unsigned int    SMDErrorFlag;                 ///< Error from TDC
  unsigned int    ReceivedByteCount;            ///< Total number of channels
  unsigned int    NSmdHits;                     ///< Total number of channels
  unsigned int    TimeBin[12];                   ///< Time bin for each SMD fiber. Now it has 12 fibers for the PSD detector
  unsigned short  SMDADCArray[12][4800] ;        ///< Matrix of ADC's as obtained from daq
  unsigned short  SmdE_ADCMatrix[120][150] ;    ///< Matrix of ADC's in Physical Positions for SMD_Eta
  unsigned short  SmdP_ADCMatrix[120][10][15] ; ///< Matrix of ADC's in Physical Positions for SMD_Phi
  unsigned short  SmdHeader[12][128];            ///< SMDHeader for each fiber
  unsigned short  HasData[12]; ///< 1 if there is data for that fiber
};


struct Bank_BSMDADCR: public BSMDDATA{};
struct Bank_BSMDADCD: public BSMDDATA{};
struct Bank_BSMDPEDR: public BSMDDATA{};
struct Bank_BSMDRMSR: public BSMDDATA{};



class EMC_Reader 
{
      void              ProcessEvent(const Bank_EMCP *EmcPTR, const Bank_TRGP *TrgPTR);///<Process EMC (tower+SMD) event

  public:
                        EMC_Reader(EventReader *er, Bank_EMCP *pEMCP, Bank_TRGP *pTRGP);///<EMC_Reader constructor
                        ~EMC_Reader() {}; ///<EMC_Reader destructor

      Bank_BTOWERADCR&  getBTOWERADCR();
			bool              isTowerPresent();
      int               getTowerADC(int,int,int,unsigned short&); ///<Get ADC value for one tower with given module, eta and sub
      int               getTowerADC(int,unsigned short&); ///<Get ADC value for one tower with given Daq Id    
      int               NTowerHits();///<Return number of valid hits on towers

      Bank_BSMDADCR&    getSMD_ADCR();
			bool              isSmdPresent();
      int               getSMD_ADC(int,int,unsigned short&);///<Get SMD ADC for a given index and fiber (RDO) number
      int               getSMDE_ADC(int,int,unsigned short&); ///<Get ADC for SMDE with given module and stip number    
      int               getSMDP_ADC(int,int,int,unsigned short&);///<Get ADC for SMDP with given module, eta and sub
      int               getSMD_TIMEBIN(int,unsigned int&);///<Get SMD time bin (capacitor number) for a given fiber (RDO)
      int               NSmdHits();///<Return number of valid hits on SMD

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
