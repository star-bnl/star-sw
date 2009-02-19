#include "UPCHistogramCtbGroup.h"

#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include "daqFormats.h"
#  include "cfgutil.h"
#else
#  include "StEvent/StTriggerData.h"
#  include "TriggerData.h"
#endif
#include <iostream>
#include <sstream>
#include <stdlib.h>
#include "TMapFile.h"
#include "EvpUtil.h"
#include "TLatex.h"
#include "TLine.h"


ClassImp(UPCHistogramCtbGroup) ;


// CTB byte mapping
const unsigned char UPCHistogramCtbGroup::mCtbMap[2][120] = {
  { 109, 108, 107, 106, 105,   7,   6,   5,   4,   3,
      2,   1,   0,  15,  14,  13,  12,  11,  10,   9,
     39,  38,  37,  36,  35,  34,  33,  32,  47,  46,
     45,  44,  43,  42,  41,  71,  70,  69,  68,  67,
     66,  65,  64,  79,  78,  77,  76,  75,  74,  73,
    103, 102, 101, 100,  99,  98,  97,  96, 111, 110,
    141, 140, 139, 138, 137, 167, 166, 165, 164, 163,
    162, 161, 160, 175, 174, 173, 172, 171, 170, 169,
    199, 198, 197, 196, 195, 194, 193, 192, 207, 206,
    205, 204, 203, 202, 201, 231, 230, 229, 228, 227,
    226, 225, 224, 239, 238, 237, 236, 235, 234, 233,
    135, 134, 133, 132, 131, 130, 129, 128, 143, 142 },
  { 125, 124, 123, 122, 121,  23,  22,  21,  20,  19,
     18,  17,  16,  31,  30,  29,  28,  27,  26,  25,
     55,  54,  53,  52,  51,  50,  49,  48,  63,  62,
     61,  60,  59,  58,  57,  87,  86,  85,  84,  83,
     82,  81,  80,  95,  94,  93,  92,  91,  90,  89,
    119, 118, 117, 116, 115, 114, 113, 112, 127, 126,
    157, 156, 155, 154, 153, 183, 182, 181, 180, 179,
    178, 177, 176, 191, 190, 189, 188, 187, 186, 185,
    215, 214, 213, 212, 211, 210, 209, 208, 223, 222,
    221, 220, 219, 218, 217, 247, 246, 245, 244, 243,
    242, 241, 240, 255, 254, 253, 252, 251, 250, 249,
    151, 150, 149, 148, 147, 146, 145, 144, 159, 158 }
};


UPCHistogramCtbGroup::UPCHistogramCtbGroup() {
  // For ROOT I/O
  h_ctb_adc_sum = 0;
  h_ctb_count_vs_tray = 0;
  h_ctb_cdb = 0;
  h_ctb_cdb_zoom = 0;
}

UPCHistogramCtbGroup::UPCHistogramCtbGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector) {
  h_ctb_adc_sum = new TH1D( pre("h_ctb_adc_sum"), "CTB ADC Sum", 210, -0.5, 209.5);
  h_ctb_adc_sum->SetXTitle("Sum [ADC Ch.]");
  h_ctb_adc_sum->SetYTitle("Count");
  h_ctb_count_vs_tray = new TH1D( pre("h_ctb_count_vs_tray"), "CTB Counts (ADC > 0)", 240, -0.25, 119.75);
  h_ctb_count_vs_tray->SetXTitle("Tray + 0.5 * Slat");
  h_ctb_count_vs_tray->SetYTitle("Count");
  h_ctb_cdb = new TH2D( pre("h_ctb_cdb"), "CTB ADC Spectra", 256, -0.5, 255.5, 256, -0.5, 255.5);
  h_ctb_cdb->SetXTitle("16 * CDB + Channel");
  h_ctb_cdb->SetYTitle("Amplitude [ADC Ch.]");
  h_ctb_cdb_zoom = new TH2D( pre("h_ctb_cdb_zoom"), "CTB ADC Spectra", 256, -0.5, 255.5, 41, -0.5, 40.5);
  h_ctb_cdb_zoom->SetXTitle("16 * CDB + Channel");
  h_ctb_cdb_zoom->SetYTitle("Amplitude [ADC Ch.]");
}


UPCHistogramCtbGroup::~UPCHistogramCtbGroup() {
  delete h_ctb_adc_sum;
  delete h_ctb_count_vs_tray;
  delete h_ctb_cdb;
  delete h_ctb_cdb_zoom;
}


void UPCHistogramCtbGroup::reset() {
  h_ctb_adc_sum->Reset();
  h_ctb_count_vs_tray->Reset();
  h_ctb_cdb->Reset();
  h_ctb_cdb_zoom->Reset();
}


void UPCHistogramCtbGroup::putCdbLabels(TH1* h)
{
  gPad->Update();
  int nmb = h->GetNbinsX() / 16;
  TLatex label;
  label.SetTextAlign(21);
  label.SetTextSize(0.03);
  for (int i = 0; i < nmb; ++i) {
    stringstream l;
    l << i;
    label.DrawLatex(i * 16 + 7.5, 0.9 * gPad->GetUymax(), l.str().c_str());
  }
  TLine line;
  line.SetLineColor(16);
  for (int i = 1; i < nmb; ++i)
    line.DrawLine(i * 16 - 0.5, gPad->GetUymin(), i * 16 - 0.5, gPad->GetUymax());
}


void UPCHistogramCtbGroup::draw(TCanvas* cc) {
  cc->cd();
  cc->Clear();
  cc->Divide(2, 2);
  cc->cd(1);
  h_ctb_adc_sum->Draw();
  cc->cd(2);
  h_ctb_count_vs_tray->Draw();
  cc->cd(3);
  h_ctb_cdb->Draw("COLZ");
  putCdbLabels(h_ctb_cdb);
  cc->cd(4);
  h_ctb_cdb_zoom->Draw("COLZ");
  putCdbLabels(h_ctb_cdb_zoom);
  cc->Update();
} 


bool UPCHistogramCtbGroup::fill(evpReader* evp, char* datap) { 
#ifndef NEW_DAQ_READER
  int ret = trgReader(datap);
  if(ret <= 0) {
    fprintf(stderr,"TRG RAW: problems in data (%d) - continuing...", ret);
    return false;
  }  
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
#else
  StTriggerData* trgd = TriggerData::Instance(datap);
  if(!trgd) return false;
  unsigned int ctbAdcSum = trgd->lastDSM(3);
#endif
  h_ctb_adc_sum->Fill(ctbAdcSum);

#ifndef NEW_DAQ_READER
  //TRGD* trgData = static_cast<struct TRGD*>(trg.trgd);
  //if(trgData==0)return true;//not compatible in 2008
  for (int tray = 0; tray < 120; ++tray)
    for (int slat = 0; slat < 2; ++slat) {
      unsigned int adcValue = trg.CTB[mCtbMap[slat][tray]];
      if (adcValue > 0)
  	h_ctb_count_vs_tray->Fill(tray + 0.5 * slat);
    }
  for (int i = 0; i < 16; ++i)      // loop over CDBs
    for (int j = 0; j < 16; ++j) {  // loop over CDB bytes
      unsigned int adcValue = trg.CTB[i * 16 + j];
      int          channel  = (j / 8) * 8 + 7 - j % 8;  // calculate CDB channel number
      h_ctb_cdb->Fill     (i * 16 + channel, adcValue);
      h_ctb_cdb_zoom->Fill(i * 16 + channel, adcValue);
    }
#else
  //no more CTB after run8
#endif

  return true;
}
