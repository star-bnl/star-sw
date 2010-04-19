#include "TEmcSizeProvider.h"
#include "St_emcOnlineStatus_Table.h"
#include "EmcChecker.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_BTOW/daq_btow.h"

#include <QMessageBox>
#include <QDebug>

//!  TEmcSizeProvider is class to provide the dimensions to visualize the EMC towers
//________________________________________________________________
TEmcSizeProvider::TEmcSizeProvider(btow_t **src,void **available,Int_t *len) : TDataProvider(src,available, len)
  , fScale(4095),fIndex(), fThreshold(200),fEmcChecker(),fHeader(),fBemcOnlineStatus(),fUsePedestals (kTRUE)
{ 
   fEmcChecker = new EmcChecker(); 
   fScale /= 100;
}

//______________________________________________________________________________
TEmcSizeProvider::~TEmcSizeProvider()
{
    delete fEmcChecker; fEmcChecker = 0;
}

///
///      
//______________________________________________________________________________
char   * TEmcSizeProvider::GetObjectInfo(Int_t towerId, Int_t /* py */) const
{ 
   TString info = fObjectInfo + Form("<p><b>EmcTower</b> %d" ,towerId);
   if (fBemcOnlineStatus) {
    //    int SoftId;
    //    int PatchMaskHT;   /* Patch unmasked in HT? */
    //    int PatchMaskSum;  /* Patch unmasked in sum? */
    //    int TriggerPatch;  /* TriggerPatch */
   emcOnlineStatus_st  &row = *fBemcOnlineStatus->GetTable(towerId-1);
           info  = Form("<p><b>EmcTower</b> %d" ,towerId         );
           info += Form("<br><b>HT</b> %d"      ,row.PatchMaskHT );
           info += Form("<br><b>Sum</b> %d"     ,row.PatchMaskSum);
           info += Form("<br><b>Trigger</b> %d" ,row.TriggerPatch);
           info += Form("<br><b>Pedestal</b> %d",row.Pedestal    );
   } 
  // ((TEmcSizeProvider*)this)->fObjectInfo = info;
   return (char *)info.Data();
}

//!  ComputerScale returns the extra factor each tower size should be mulitplied to
//______________________________________________________________________________
void TEmcSizeProvider::ComputerScale() 
{
   fScale = 1; // 4095/10; /* Web page value */
}

//! iterates of the array of towers of provides the size of the "next" tower
//______________________________________________________________________________
Int_t TEmcSizeProvider::NextAttribute() 
{ 
   Int_t  channel = 0;
   Int_t  tdc = -1;
   Int_t report = 0;
   int towerId = fIndex+1;fIndex++;
#ifdef DEBUGDATA       
   if ( towerId > 61*40 && towerId <=  62*40)  return 2; // STAR has no East-end emc tower yet !!!
   if ( towerId > 72*40 && towerId <=  73*40)  return 31; // STAR has no East-end emc tower yet !!!
   if ( towerId > 103*40 && towerId <= 104*40) return 44; // STAR has no East-end emc tower yet !!!
   if ( towerId > 116*40 && towerId <= 117*40) return 49; // STAR has no East-end emc tower yet !!!

   if ( towerId > 1*40 && towerId <=   2*40)  return 2; // STAR has no East-end emc tower yet !!!
   if ( towerId > 28*40 && towerId <=  29*40) return 31; // STAR has no East-end emc tower yet !!!
   if ( towerId > 43*40 && towerId <=  44*40) return 44; // STAR has no East-end emc tower yet !!!
   if ( towerId > 58*40 && towerId <=  59*40) return 49; // STAR has no East-end emc tower yet !!!

   return 99;
   if (  towerId <= 1200)                        return  100; // 0  
   if ( (towerId > 2400) && ( towerId < 3600) )  return  100; // 0
   if ( (towerId >= 1200) && (towerId < 2400) )  return 20;
   else return 80;
#endif
//       if ( towerId > 71*40 && towerId <=  103*40) return 0; // STAR has no East-end emc tower yet !!!
   if (   fDataSource 
       && fEmcChecker 
       && (fEmcChecker->GetTowerTDCChannel(towerId,tdc,channel) ) ) 
   { 
      if (fEmcChecker->getTDCStatus(tdc) )
         report = ReportValue((*fDataSource)->adc[tdc][channel]);
   } 
   // if (fIndex >= 4800) ResetCounter();
   return report;
}

//!  ReportValue provides the szie fo the box to visualize the EMC towers ADC value
/*!
     Report the "size" value" based on the input  /a val parameter
*/
//_____________________________________________________________________________
Int_t TEmcSizeProvider::ReportValue(UShort_t val) 
{
   UShort_t ped=0;
   Bool_t flag=true;
   if (fBemcOnlineStatus) {
      ped = (UShort_t) fBemcOnlineStatus->GetTable(fIndex-1)->Pedestal;
      flag =  (Bool_t) fBemcOnlineStatus->GetTable(fIndex-1)->TowerMask;
   }
   Int_t final = 0;
#ifndef UPSILON
   if (flag) {
      final = val;
      if (UsePedestals()) final -= ped;
      if (final <= 10) final = 0;           
   }
#else
   if (val >190) final = val-190+70;
#endif
   return final;
//        return (val - fThreshold > 0 ? 0: Int_t((val - fThreshold)/fScale));
}

//!     Reset  that /a date and /a time of EmbChecker  object if present
//______________________________________________________________________________
void TEmcSizeProvider::ResetEmcDecoder(int date, int time) 
{
   if (fEmcChecker) 
      fEmcChecker->ResetEmcDecoder(date,time);
}

//!  ResetCounter method reset the counter the NextAttribute method sizes to provide the next tower size
//______________________________________________________________________________
void TEmcSizeProvider::ResetCounter()
{ 
   fIndex = 0; 
   // Copycat from StEmcOnl.cxx::decodeEmc
   btow_t *d = *fDataSource;
   if (d) {
       for (int i=0;i<30;i++) {
          fEmcChecker->TDCCount  [i]  =  d->preamble[i][0];
          fEmcChecker->TDCError  [i]  =  d->preamble[i][1];
          fEmcChecker->TDCToken  [i]  =  d->preamble[i][2];
          fEmcChecker->TDCTrigger[i]  = (d->preamble[i][3]&0xF00) >> 8;
          fEmcChecker->TDCCrateId[i]  =  d->preamble[i][3]&0x0FF;
      }
   }
}
