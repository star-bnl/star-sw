#ifndef StBemcData_hh
#define StBemcData_hh
#include "TDataSet.h"
/*!\class StBemcData
\author Alexandre A. P. Suaide

This class contains all information from one EMC event.
*/

class StEmcDecoder;

class StBemcData: public TDataSet
{
 protected:
          StEmcDecoder *mDecoder;
 
 public: 
									StBemcData(char* name="bemcData");
					virtual	~StBemcData();
					Bool_t  getTDCStatus(Int_t);          ///< Returns only if a given TDC channel should be active or not in this run. DOES NOT return if its data is valid or not
					Bool_t  getSMDStatus(Int_t);          ///< Returns only if a given SMD crate should be active or not in this run. DOES NOT return if its data is valid or not
					Bool_t  checkTDC(Int_t);              ///< check if data for TDC channel is ok
          void    validateData();               ///< Validates EMC data. Should be used only if headers are properly filled
					void    printTower();                 ///< Print tower data
					void    printSMD();                   ///< Print SMD data
          void    zeroAll();
					
					// global event data
					Int_t   RunNumber;
					Int_t   TriggerWord;
					Int_t   EventDate;
					Int_t   EventTime;
					Int_t   NGlobalTracks;
          Float_t Vertex[3];
	
					// tower data -----------------------------------------------------
					Bool_t  TowerPresent;                 ///< Check to see if there is tower information in the event
					Bool_t  ValidTowerEvent;              ///< Check to see if the event is valid
					Short_t EventNumber;                  ///< Token number 
  				Short_t TowerByteCount;               ///< Total number of Bytes
  				Short_t NTowerHits;                   ///< Total number of valid channels
  				Short_t TDCErrorFlag;                 ///< Error from TDC
  				Short_t NTDCChannels;                 ///< Total number of valid TDC channels

  				Short_t TDCError[30];                 ///< vector with TDC error for each TDC channel
  				Short_t TDCToken[30];                 ///< vector with crate token for each TDC channel
  				Short_t TDCTrigger[30];               ///< vector with trigger number for each TDC channel
  				Short_t TDCCrateId[30];               ///< vector with crate Id for each TDC channel
  				Short_t TDCCount[30];                 ///< vector with byte count for each TDC channel

  				Short_t TowerADC[4800];               ///< Matrix of ADC's as obtained from daq
					Float_t TowerEnergy[4800];            ///< tower energy
					Char_t  TowerStatus[4800];            ///< tower status
					
					Char_t  HighTower[300];               ///< high tower trigger data
					Char_t  Patch[300];                   ///< Patch trigger data
	
					// smd data -----------------------------------------------------
					Bool_t  SMDPresent;                   ///< Check to see if there is SMD information in the event
					Bool_t  ValidSMDEvent;                ///< check to see if the event is valid
  				Short_t SMDErrorFlag;                 ///< Error from TDC
  				Short_t SMDByteCount;                 ///< Total byte count for SMD
  				Short_t NSmdHits;                     ///< Total number of channels
  				Char_t  TimeBin[8];                   ///< Time bin for each SMD fiber
  				Short_t SmdeADC[18000];               ///< Matrix of ADC's in Physical Positions for SMD_Eta
  				Short_t SmdpADC[18000];               ///< Matrix of ADC's in Physical Positions for SMD_Phi
					Float_t SmdeEnergy[18000];            ///< SMD-eta energy 
					Float_t SmdpEnergy[18000];            ///< SMD-phi energy
					Char_t  SmdeStatus[18000];            ///< SMD-eta status 
					Char_t  SmdpStatus[18000];            ///< SMD-phi status
					
	ClassDef(StBemcData,1) 
};
#endif
