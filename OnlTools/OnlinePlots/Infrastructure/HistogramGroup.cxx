#include "HistogramGroup.h"

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
#include "TMapFile.h"
#include "EvpUtil.h"



ClassImp(HistogramGroup) ;

HistogramGroup::HistogramGroup()
{ }

HistogramGroup::HistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)  {

    mGroupName = TString(group);
    mSubGroupName = TString(subGroup);
    mTriggerName = TString(trigger);
    mDetectorName = TString(detector);
    char tmp[1024];
    sprintf(tmp,"%s#%s#%s#%s",group,subGroup,trigger,detector);
    mId = TString(tmp);
    SetName(tmp);
  
  mTriggerBits = EvpUtil::evpgroupmask( (char*)trigger );
  mDetectorBits = EvpUtil::detmask( (char*)detector );
  mActive = false;
}


char* HistogramGroup::pre(const char* a) {
  static char name[1024];
  //  sprintf(name,"%s###%s",mId.Data(),a);
  //  return name;
  return (char*)a;
}

bool HistogramGroup::testBits(unsigned int trigger, unsigned int detector) {
  //cout << hex;
  //cout << "t: " << trigger << " " <<  mTriggerBits  << " " << (trigger&mTriggerBits) << endl;
  //cout << "d: " << detector << " " << mDetectorBits  << " " << (detector&mDetectorBits) << endl;
  //cout << dec;
  return (trigger&mTriggerBits) && (detector&mDetectorBits);
}

bool HistogramGroup::operator<(const HistogramGroup& hg) const {
    return string(id()) < string(hg.id());
  }


void HistogramGroup::draw(TCanvas* cc) { }




 

