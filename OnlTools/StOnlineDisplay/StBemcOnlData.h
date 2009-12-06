#ifndef StBemcOnlData_hh
#define StBemcOnlData_hh
#include "TDataSet.h"

/*!
  \class StBemcOnlData
  \author Alexandre A. P. Suaide

  This class contains all information from one EMC event.
*/

class StEmcDecoder;

class StBemcOnlData: public TDataSet
{
 protected:
  enum {MAXSMDRDO=8, TDCCHANNELS=30, EMCTRIGGERPATCH=300, TOWERCHANNELS=4800, SMDCHANNELS=18000};
  StEmcDecoder *mDecoder; //!
 
 public: 
  StBemcOnlData(char* name="bemcData");
  virtual	~StBemcOnlData();
  Bool_t  getTDCStatus(Int_t);          ///< Returns only if a given TDC channel should be active or not in this run. DOES NOT return if its data is valid or not
  Bool_t  getSMDStatus(Int_t);          ///< Returns only if a given SMD crate should be active or not in this run. DOES NOT return if its data is valid or not
  Bool_t  checkTDC(Int_t);              ///< check if data for TDC channel is ok
  void    validateData();               ///< Validates EMC data. Should be used only if headers are properly filled
  void    invalidateTDC(int tdc);       ///< 
  void    printTower();                 ///< Print tower data
  void    printSMD();                   ///< Print SMD data
  void    zeroAll();
  
  // global event data
  Int_t   RunNumber;
  Int_t   TriggerWord;
  Int_t   TriggerCmd;
  Int_t   PhysWord;
  Int_t   EventDate;
  Int_t   EventTime;
  Int_t   NGlobalTracks;
  Float_t Vertex[3];
  Long_t  ZDCSum;
  Long_t  CTBSum;
  
  // tower data -----------------------------------------------------
  Bool_t  TowerPresent;                 ///< Check to see if there is tower information in the event
  Bool_t  ValidTowerEvent;              ///< Check to see if the event is valid
  Short_t EventNumber;                  ///< Token number 
  Short_t TowerByteCount;               ///< Total number of Bytes
  Short_t NTowerHits;                   ///< Total number of valid channels
  Short_t TDCErrorFlag;                 ///< Error from TDC
  Short_t NTDCChannels;                 ///< Total number of valid TDC channels

  Short_t TDCError[TDCCHANNELS];        ///< vector with TDC error for each TDC channel
  Short_t TDCToken[TDCCHANNELS];        ///< vector with crate token for each TDC channel
  Short_t TDCTrigger[TDCCHANNELS];      ///< vector with trigger number for each TDC channel
  Short_t TDCCrateId[TDCCHANNELS];      ///< vector with crate Id for each TDC channel
  Short_t TDCCount[TDCCHANNELS];        ///< vector with byte count for each TDC channel

  Float_t TDCGhost[TDCCHANNELS];        ///< vector with the threshold for each TDC channel to remove ghost pedestals
  Short_t TDCGhostMode[TDCCHANNELS];    ///< Mode for ghost pedestal removal
  Bool_t  TowerRemoveGhost;             ///< If kTRUE, removes the entire event in case of a ghost pedestal
  Short_t NumberGhost;                  ///< Number of ghost TDC channel
  Short_t NumberBadHeader;              ///< Number of bad headers
  
  Short_t TowerADC[TOWERCHANNELS];      ///< Matrix of ADC's as obtained from daq
  Float_t TowerEnergy[TOWERCHANNELS];   ///< tower energy
  Char_t  TowerStatus[TOWERCHANNELS];   ///< tower status
					
  Char_t  HighTower[EMCTRIGGERPATCH];   ///< high tower trigger data
  Char_t  Patch[EMCTRIGGERPATCH];       ///< Patch trigger data
	
	// smd data -----------------------------------------------------
  Bool_t  SMDPresent;                   ///< Check to see if there is SMD information in the event
  Bool_t  ValidSMDEvent;                ///< check to see if the event is valid
  Short_t SMDErrorFlag;                 ///< Error from TDC
  Short_t SMDByteCount;                 ///< Total byte count for SMD
  Short_t NSmdHits;                     ///< Total number of channels
  Char_t  TimeBin[MAXSMDRDO];           ///< Time bin for each SMD fiber
  Short_t SmdeADC[SMDCHANNELS];         ///< Matrix of ADC's in Physical Positions for SMD_Eta
  Short_t SmdpADC[SMDCHANNELS];         ///< Matrix of ADC's in Physical Positions for SMD_Phi
  Float_t SmdeEnergy[SMDCHANNELS];      ///< SMD-eta energy 
  Float_t SmdpEnergy[SMDCHANNELS];      ///< SMD-phi energy
  Char_t  SmdeStatus[SMDCHANNELS];      ///< SMD-eta status 
  Char_t  SmdpStatus[SMDCHANNELS];      ///< SMD-phi status
  
	// psd data -----------------------------------------------------
  Bool_t  PSDPresent;                   ///< Check to see if there is PSD information in the event
  Bool_t  ValidPSDEvent;                ///< check to see if the event is valid
  Short_t PSDErrorFlag;                 ///< Error from TDC
  Short_t PSDByteCount;                 ///< Total byte count for PSD
  Short_t NPsdHits;                     ///< Total number of channels
  Short_t PsdADC[TOWERCHANNELS];        ///< Matrix of ADC's in Physical Positions for PSD
  Float_t PsdEnergy[TOWERCHANNELS];     ///< SMD-eta energy 
  Char_t  PsdStatus[TOWERCHANNELS];     ///< SMD-eta status 
  
  ClassDef(StBemcOnlData,0) 
};
#endif
