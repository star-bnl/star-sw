#include "UPCHistogramZdcGroup.h"

#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include "daqFormats.h"
#  include "cfgutil.h"
#else
#  include "DAQ_READER/daqReader.h"
#  include "DAQ_TRG/trgReader.h"
#  include "DAQ_READER/cfgutil.h"
#endif
#include <iostream>
#include <stdlib.h>
#include "TMapFile.h"
#include "EvpUtil.h"


ClassImp(UPCHistogramZdcGroup) ;

UPCHistogramZdcGroup::UPCHistogramZdcGroup()
{
   // For ROOT I/O 
}

UPCHistogramZdcGroup::UPCHistogramZdcGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector) {
  h_zdce_sum = new TH1D( pre("h_zdce_sum"), "ZDC East Unattenuated Sum", 150, -0.5, 149.5);
  h_zdce_sum->SetXTitle("Sum [ADC Ch.]");
  h_zdce_sum->SetYTitle("Count");
  h_zdcw_sum = new TH1D( pre("h_zdcw_sum"), "ZDC West Unattenuated Sum", 150, -0.5, 149.5);
  h_zdcw_sum->SetXTitle("Sum [ADC Ch.]");
  h_zdcw_sum->SetYTitle("Count");
  h_zdce_sum_vs_ctb_sum = new TH2D( pre("h_zdce_sum_vs_ctb_sum"), "ZDC East Unattenuated Sum vs. CTB ADC Sum",
					      210, -0.5, 209.5, 150, -0.5, 149.5);
  h_zdce_sum_vs_ctb_sum->SetXTitle("CTB Sum [ADC Ch.]");
  h_zdce_sum_vs_ctb_sum->SetYTitle("ZDC Sum [ADC Ch.]");
  h_zdcw_sum_vs_ctb_sum = new TH2D( pre("h_zdcw_sum_vs_ctb_sum"), "ZDC West Unattenuated Sum vs. CTB ADC Sum",
					      210, -0.5, 209.5, 150, -0.5, 149.5);
  h_zdcw_sum_vs_ctb_sum->SetXTitle("CTB Sum [ADC Ch.]");
  h_zdcw_sum_vs_ctb_sum->SetYTitle("ZDC Sum [ADC Ch.]");
}


UPCHistogramZdcGroup::~UPCHistogramZdcGroup() {
  delete h_zdce_sum;
  delete h_zdcw_sum;
  delete h_zdce_sum_vs_ctb_sum;
  delete h_zdcw_sum_vs_ctb_sum;
}


void UPCHistogramZdcGroup::reset() {
  h_zdce_sum->Reset();
  h_zdcw_sum->Reset();
  h_zdce_sum_vs_ctb_sum->Reset();
  h_zdcw_sum_vs_ctb_sum->Reset();
}


void UPCHistogramZdcGroup::draw(TCanvas* cc) {
  cc->cd();
  cc->Clear();
  cc->Divide(2, 2);
  cc->cd(1);
  h_zdce_sum->Draw();
  cc->cd(2);
  h_zdcw_sum->Draw();
  cc->cd(3);
  h_zdce_sum_vs_ctb_sum->Draw("COLZ");
  cc->cd(4);
  h_zdcw_sum_vs_ctb_sum->Draw("COLZ");
  cc->Update();
} 


bool UPCHistogramZdcGroup::fill(evpReader* evp, char* datap) { 
#ifndef NEW_DAQ_READER
  int ret = trgReader(datap);
  if(ret <= 0) {
    fprintf(stderr,"TRG RAW: problems in data (%d) - continuing...",ret) ;
    return false;
  }  
  unsigned int zdcEastUnattSum = trg.ZDC[4];
  unsigned int zdcWestUnattSum = trg.ZDC[0];
  h_zdce_sum->Fill(zdcEastUnattSum);
  h_zdcw_sum->Fill(zdcWestUnattSum);
  TrgSumData* sumData = static_cast<TrgSumData*>(trg.trg_sum);
  if(!sumData) {
    fprintf(stderr, "TRG RAW: cannot get TrgSumData data - continuing...");
    return false;
  }
  L0_DSM_Data* dsmData = &sumData->DSMdata;
  // bytes in DSM array have to be swapped in blocks of 64 bits
  //     array offset | 0 1 2 3 4 5 6 7
  //     DSM channel  | 3 2 1 0 7 6 5 4  
  unsigned int ctbAdcSum = dsmData->lastDSM[3];  // ch 0 of last DSM
  h_zdce_sum_vs_ctb_sum->Fill(ctbAdcSum, zdcEastUnattSum);
  h_zdcw_sum_vs_ctb_sum->Fill(ctbAdcSum, zdcWestUnattSum);
  return true;
#else
  return false;
#endif
}
