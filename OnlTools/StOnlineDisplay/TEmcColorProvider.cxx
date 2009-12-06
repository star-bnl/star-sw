#include "TEmcColorProvider.h"
#include "DAQ_BTOW/daq_btow.h"
#include "EmcChecker.h"

//________________________________________________________________
Int_t TEmcColorProvider::NextAttribute() 
{ 
   Int_t colorResponce = 0;
   Int_t tdc;
   Int_t channel;
   int towerId = fIndex+1;fIndex++;
   if (fDataSource && fEmcChecker && 
       (fEmcChecker->GetTowerTDCChannel(towerId,tdc,channel) ) ) {
//           UInt_t colorCode = 50.*(ReportValue((fDataSource[daqId]))/4095.) ;
      UInt_t colorCode = ReportValue((*fDataSource)->adc[tdc][channel]);
#ifdef DEBUGDATA        
      if ( towerId >  1*40 && towerId <=   2*40)  return kBlue;   // STAR has no East-end emc tower yet !!!
      if ( towerId > 28*40 && towerId <=  29*40) return kGreen;   // STAR has no East-end emc tower yet !!!
      if ( towerId > 43*40 && towerId <=  44*40) return kYellow;  // STAR has no East-end emc tower yet !!!
      if ( towerId > 58*40 && towerId <=  59*40) return kRed;     // STAR has no East-end emc tower yet !!!

      if ( towerId >  61*40 && towerId <=  62*40)  return kBlue;  // STAR has no East-end emc tower yet !!!
      if ( towerId >  72*40 && towerId <=  73*40)  return kGreen; // STAR has no East-end emc tower yet !!!
      if ( towerId > 103*40 && towerId <= 104*40) return kYellow; // STAR has no East-end emc tower yet !!!
      if ( towerId > 116*40 && towerId <= 117*40) return kRed;    // STAR has no East-end emc tower yet !!!
      return kCyan; //0
#endif
//           return colorCode+51;
//           for (i=0;i<ncolors;i++) fPalette.fArray[i] = 51+i;
      if (colorCode) {
  	// If edep less then MIP (~300 MeV), 60GeV <-> 4096 ADC counts
         if ( colorCode < 20)
            colorResponce = kBlue;
	// If edep large then MIP but less then 1 GeV 
         else if ( colorCode < 68 )
            colorResponce = kGreen;
	// If between 1 GeV and lowest HT threshold (4 GeV for Run7)
         else if ( colorCode < 256) 
            colorResponce = kYellow;
	// If above lowest HT thershold
         else  colorResponce = kRed;
#if 0       
      if (colorCode - pedCorrection <= 0) colorResponce = 0;
         else if (colorCode < 20) 
             colorResponce = kBlue;
         else if ( ( 21 <= colorCode)  && (colorCode < 40) )
             colorResponce = kGreen;
         else if ( ( 41 <= colorCode)  && (colorCode < 60) )
             colorResponce = kYellow;
          else if ( ( 61 <= colorCode)  /* && (colorCode < 80) */ )
             colorResponce = kRed;    
#endif       
      } 
   }
       // if (fIndex >= 4800) ResetCounter();
   return colorResponce;
}
