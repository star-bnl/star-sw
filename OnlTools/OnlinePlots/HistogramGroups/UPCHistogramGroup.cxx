#include "UPCHistogramGroup.h"

#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include "daqFormats.h"
#  include "cfgutil.h"
#else
#  include "StEvent/StTriggerData.h"
#  include "TriggerData.h"
#endif
#include <iostream>
#include "TMapFile.h"
#include "EvpUtil.h"



ClassImp(UPCHistogramGroup) ;

UPCHistogramGroup::UPCHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector) : HistogramGroup(group,subGroup,trigger,detector) {

  h_zdc_time_east = new TH1D( pre("h_zdc_time_east"), "ZDC Time East",256,0.,256.);
  h_zdc_time_west = new TH1D( pre("h_zdc_time_west"), "ZDC Time West",256,0.,256.);
  h_zdc_timediff_east_west = new TH1D( pre("h_zdc_timediff_east_west"),"ZDC Time (West - East)",100,-100.,100.);
  h_zdc_time_east_vs_west = new TH2D( pre("h_zdc_time_east_vs_west"),"ZDC Time East vs West",256,0.,256.,256,0.,256);
  h_zdc_unatt_east = new TH1D( pre("ZDCUnattenuated East (Channel 4)"),"ZDCUnattenuated East (Channel 4)",  150,-0.5,149.5);
  h_zdc_unatt_west = new TH1D( pre("ZDC Unattenuated West (Channel 0)"),"ZDC Unattenuated West (Channel 0)",150,-0.5,149.5);


}

UPCHistogramGroup::~UPCHistogramGroup() {
  //cout << __PRETTY_FUNCTION__ << endl;
  delete h_zdc_time_east;
  delete h_zdc_time_west;
  delete h_zdc_timediff_east_west;
  delete h_zdc_time_east_vs_west;
  delete h_zdc_unatt_east;
  delete h_zdc_unatt_west;
}

void UPCHistogramGroup::reset() {
  h_zdc_time_east->Reset();
  h_zdc_time_west->Reset();
  h_zdc_timediff_east_west->Reset();
  h_zdc_time_east_vs_west->Reset();
  h_zdc_unatt_east->Reset();
  h_zdc_unatt_west->Reset();
}


void UPCHistogramGroup::draw(TCanvas* cc) {
  //cout << __PRETTY_FUNCTION__ << endl;
  cc->cd();
  cc->Clear();
  cc->Divide(2,3);
  cc->cd(1);
  h_zdc_time_east->Draw();
  cc->cd(2);
  h_zdc_time_west->Draw();
  cc->cd(3);
  h_zdc_timediff_east_west->Draw();
  cc->cd(4);
  h_zdc_time_east_vs_west->Draw();  
  cc->cd(5);
  h_zdc_unatt_east->Draw();
  cc->cd(6);
  h_zdc_unatt_west->Draw();
  cc->Update();
} 

#include <stdlib.h>

bool UPCHistogramGroup::fill(evpReader* evp, char* datap) { 
#ifndef NEW_DAQ_READER
  int ret = trgReader(datap) ;
  if(ret <= 0) {
    fprintf(stderr,"TRG RAW: problems in data (%d) - continuing...",ret) ;
    return false;
  }  
  // ZDC Timing information
  unsigned int zdcTime_east = trg.ZDC[8];
  unsigned int zdcTime_west = trg.ZDC[9];
  unsigned int zdcAdc_east  = trg.ZDC[4];
  unsigned int zdcAdc_west  = trg.ZDC[0];
#else
  StTriggerData* trgd = TriggerData::Instance(datap);
  if(!trgd) return false;
  unsigned int zdcTime_east = trgd->zdcTDC(east);
  unsigned int zdcTime_west = trgd->zdcTDC(west);
  unsigned int zdcAdc_east  = trgd->zdcUnAttenuated(east);
  unsigned int zdcAdc_west  = trgd->zdcUnAttenuated(west);
#endif
  int zdcTime_difference = zdcTime_west - zdcTime_east;
  h_zdc_time_east->Fill( zdcTime_east );
  h_zdc_time_west->Fill( zdcTime_west );
  h_zdc_timediff_east_west->Fill( zdcTime_difference );
  h_zdc_time_east_vs_west->Fill( zdcTime_east, zdcTime_west );  
  h_zdc_unatt_east->Fill( zdcAdc_east );
  h_zdc_unatt_west->Fill( zdcAdc_west );
  return true;
}



 

