#include "VPDHistogramGroup.h"

#include <iostream>
#include <sstream>
#include <stdlib.h>

#include "TVirtualPad.h"
#include "TLine.h"
#include "TLatex.h"

#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include "daqFormats.h"
#  include "cfgutil.h"
#else
#  include "DAQ_READER/daqReader.h"
#  include "DAQ_TRG/trgReader.h"
#  include "DAQ_READER/cfgutil.h"
#endif
#include "TMapFile.h"
#include "EvpUtil.h"
#include "HistoHandler.h"


ClassImp(VPDHistogramGroup) ;


VPDHistogramGroup::VPDHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector) {
  for (int i = 0; i < 4; ++i) {
    stringstream n;
    n << "h_vpd_cdb_vp00" << i + 1;
    stringstream t;
    t << "VP00" << i + 1 << " ADC Spectra";
    h_vpd_cdb[i] = new TH2D( pre(n.str().c_str()), t.str().c_str(), 16, -0.5, 15.5, 256, -0.5, 255.5);
    h_vpd_cdb[i]->SetXTitle("CDB Channel #");
    h_vpd_cdb[i]->SetYTitle("ADC Value");
  }
  h_vpd_tac_east_vs_tac_west = new TH2D( pre("h_vpd_tac_east_vs_tac_west"), "VPD TAC East vs. TAC West", 257, -1.5, 255.5, 257, -1.5, 255.5);
  h_vpd_tac_east_vs_tac_west->SetXTitle("TAC West");
  h_vpd_tac_east_vs_tac_west->SetYTitle("TAC East");
  h_vpd_vertex_vs_l3_vertex = new TH2D( pre("h_vpd_vertex_vs_l3_vertex"), "VPD TAC Difference vs. L3 Vertex z-Position", 200, -200, 200, 512, -0.5, 511.5);
  h_vpd_vertex_vs_l3_vertex->SetXTitle("L3 Vertex z-Position [cm]");
  h_vpd_vertex_vs_l3_vertex->SetYTitle("VPD TAC Difference");
}


VPDHistogramGroup::~VPDHistogramGroup() {
  for (int i = 0; i < 4; ++i)
    delete h_vpd_cdb[i];
  delete h_vpd_tac_east_vs_tac_west;
  delete h_vpd_vertex_vs_l3_vertex;
}


void VPDHistogramGroup::reset() {
  for (int i = 0; i < 4; ++i)
    h_vpd_cdb[i]->Reset();
  h_vpd_tac_east_vs_tac_west->Reset();
  h_vpd_vertex_vs_l3_vertex->Reset();
}


void VPDHistogramGroup::draw(TCanvas* cc) {
  TLine  line;
  line.SetLineColor(16);
  TLatex label;
  label.SetTextAlign(23);  // center, top
  label.SetTextSize(0.06);
  label.SetTextColor(16);
  cc->cd();
  cc->Clear();
  cc->Divide(2, 3);
  for (unsigned i = 0; i < 4; ++i) {
    cc->cd(i + 1);
    h_vpd_cdb[i]->Draw("COLZ");
    gPad->Update();
    line.DrawLine(7.5, gPad->GetUymin(), 7.5, gPad->GetUymax());
    label.DrawLatex( 3.5, 0.9 * gPad->GetUymax(), "ADC");
    label.DrawLatex(11.5, 0.9 * gPad->GetUymax(), "TAC");
  }
  cc->cd(5);
  h_vpd_tac_east_vs_tac_west->Draw("COLZ");
  cc->cd(6);
  h_vpd_vertex_vs_l3_vertex->Draw("COLZ");
  cc->Update();
} 


bool VPDHistogramGroup::fill(evpReader* evp, char* datap) { 
#ifndef NEW_DAQ_READER
  int ret = trgReader(datap);
  if(ret <= 0) {
    fprintf(stderr, "TRG RAW: problems in data (%d) - continuing...", ret);
    return false;
  }
  for (int i = 0; i < 4; ++i)       // loop over CDBs
    for (int j = 0; j < 16; ++j) {  // loop over CDB bytes
      unsigned int adcValue = trg.VPD[i * 16 + j];
      int          channel  = (j / 8) * 8 + 7 - j % 8;  // calculate CDB channel number
      h_vpd_cdb[i]->Fill(channel, adcValue);
    }
  TrgSumData* sumData = static_cast<TrgSumData*>(trg.trg_sum);
  if(!sumData) {
    fprintf(stderr, "TRG RAW: cannot get TrgSumData data - continuing...");
    return false;
  }
  L0_DSM_Data* dsmData = &sumData->DSMdata;
  // get MaxTacs from layer 1 DSM information
  // bytes in DSM array have to be swapped in blocks of 64 bits
  //     array offset | 0 1 2 3 4 5 6 7
  //     DSM channel  | 3 2 1 0 7 6 5 4  
  // east channels 0 through 7
  unsigned int maxTacEast1      =  dsmData->VPD[2] & 0xFF;       // 1st byte of ch 1 of L1_VP101
  bool         maxTacEast1Valid = (dsmData->VPD[2] >> 8) & 0x1;  // bit 8 of ch 1 of L1_VP101
  // east channels 8 through 15
  unsigned int maxTacEast2      =  dsmData->VPD[0] & 0xFF;       // 1st byte of ch 3 of L1_VP101
  bool         maxTacEast2Valid = (dsmData->VPD[0] >> 8) & 0x1;  // bit 8 of ch 3 of L1_VP101
  // west channels 0 through 7
  unsigned int maxTacWest1      =  dsmData->VPD[6] & 0xFF;       // 1st byte of ch 5 of L1_VP101
  bool         maxTacWest1Valid = (dsmData->VPD[6] >> 8) & 0x1;  // bit 8 of ch 5 of L1_VP101
  // west channels 8 through 15
  unsigned int maxTacWest2      =  dsmData->VPD[4] & 0xFF;       // 1st byte of ch 7 of L1_VP101
  bool         maxTacWest2Valid = (dsmData->VPD[4] >> 8) & 0x1;  // bit 8 of ch 7 of L1_VP101
  int maxTacEast = -1;
  if (maxTacEast1Valid && maxTacEast2Valid)
    maxTacEast = (maxTacEast1 > maxTacEast2) ? maxTacEast1 : maxTacEast2;
  else if (maxTacEast1Valid)
    maxTacEast = maxTacEast1;
  else if (maxTacEast2Valid)
    maxTacEast = maxTacEast2;
  int maxTacWest = -1;
  if (maxTacWest1Valid && maxTacWest2Valid)
    maxTacWest = (maxTacWest1 > maxTacWest2) ? maxTacWest1 : maxTacWest2;
  else if (maxTacWest1Valid)
    maxTacWest = maxTacWest1;
  else if (maxTacWest2Valid)
    maxTacWest = maxTacWest2;
  h_vpd_tac_east_vs_tac_west->Fill(maxTacWest, maxTacEast);
  // get pointer to L3 data buffer and read L3 data
  L3_P* l3Data = reinterpret_cast<L3_P*>(HistoHandler::l3Buffer());
  if (!l3Data || (l3Reader(l3Data) <= 0) ) {
    fprintf(stderr, "L3 RAW: cannot get L3 data - continuing...");
    return false;
  }
  // get TAC difference from layer 2 DSM information
  unsigned int tacDiff = dsmData->CTB[4] & 0x1FF;  // lower 9 bits of ch 7 of L1_CB201
  // ???? really 9 bits
//   TrgSumData *tsd = (TrgSumData *)trg.trg_sum;
//   int l2tacdiff=tsd->DSMdata.VTX[3]%512 - 256;
  h_vpd_vertex_vs_l3_vertex->Fill(l3.zVertex, tacDiff);
  return true;
#else
  return false;
#endif
}
