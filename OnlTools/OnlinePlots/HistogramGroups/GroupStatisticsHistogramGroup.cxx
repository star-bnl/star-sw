#include "GroupStatisticsHistogramGroup.h"

#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include "daqFormats.h"
#  include "cfgutil.h"
#else
#  include "DAQ_READER/daqReader.h"
#  include "DAQ_READER/cfgutil.h"
#endif
#include <iostream>
#include "TMapFile.h"
#include "EvpUtil.h"
#include "TLatex.h"
#include "rtsSystems.h"


ClassImp(GroupStatisticsHistogramGroup) ;

GroupStatisticsHistogramGroup::GroupStatisticsHistogramGroup() {
  // For ROOT I/O
  hEventGroups = 0;
  hDetectorGroups = 0;
}

GroupStatisticsHistogramGroup::GroupStatisticsHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector) : HistogramGroup(group,subGroup,trigger,detector) {
  hEventGroups = new TH1D( pre("hEventGroups"), "event groups",32,-0.5,31.5);
  hDetectorGroups = new TH1D( pre("hDetectorGroups"), "detector groups",32,-0.5,31.5);
}

GroupStatisticsHistogramGroup::~GroupStatisticsHistogramGroup() {
  delete hEventGroups;
  delete hDetectorGroups;
}

void GroupStatisticsHistogramGroup::reset() {
  hEventGroups->Reset();
  hDetectorGroups->Reset();
}


void GroupStatisticsHistogramGroup::draw(TCanvas* cc) {
  cc->Clear();
  cc->Divide(1,2);
  cc->cd(1);
  hEventGroups->Draw();
  drawEvpGroupLabels();
  cc->cd(2);
  hDetectorGroups->Draw();
  drawDetectorLabels();
  cc->Update();
} 

void  GroupStatisticsHistogramGroup::drawEvpGroupLabels() {
  TLatex label;
  label.SetTextAlign(12);  // center, top
  label.SetTextAngle(90);
  label.SetTextSize(0.04);
  label.SetTextColor(4);
  //double max = hist->GetMaximum();
  for (unsigned i = 0; i < 32; ++i) {
    //  label.DrawLatex( (double)i, 0., rts2name(i) );
  } 
}

void  GroupStatisticsHistogramGroup::drawDetectorLabels() {
  TLatex label;
  label.SetTextAlign(12);  // center, top
  label.SetTextAngle(90);//Align(23);  // center, top
  label.SetTextSize(0.05);
  label.SetTextColor(4);
  //double max = hist->GetMaximum();
  for (unsigned i = 0; i < 32; ++i) {
    label.DrawLatex( (double)i, 0., rts2name(i) );
  } 
}

#include <bitset>
using  namespace std;
bool GroupStatisticsHistogramGroup::fill(evpReader* evp, char* datap) { 
  bitset<32> trg(evp->evpgroups);
  bitset<32> det(evp->detectors);
  for ( int i=0; i<32; i++) {
    hEventGroups->Fill( i, trg.test(i) );
    hDetectorGroups->Fill( i, det.test(i) );
  }
  return true;
}



 

