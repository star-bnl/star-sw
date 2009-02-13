#include "L2UpsilonMassHistogramGroup.h"

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



ClassImp(L2UpsilonMassHistogramGroup) ;

L2UpsilonMassHistogramGroup::L2UpsilonMassHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector) : HistogramGroup(group,subGroup,trigger,detector) {

 hEnergyL0 = new TH1D( pre("hEnergyL0"), "energy L0",100,0.,25.);
 hEnergyL2 = new TH1D( pre("hEnergyL2"), "energy L2",100,0.,25.);
 hMass     = new TH1D( pre("hMass"), "inv. mass",100,0.,20.);
 hCosTheta = new TH1D( pre("hCosTheta"), "cos(theta)",100,-1.,1.);


}

L2UpsilonMassHistogramGroup::~L2UpsilonMassHistogramGroup() {
  //cout << __PRETTY_FUNCTION__ << endl;
  delete hEnergyL0;
  delete hEnergyL2;
  delete hMass;
  delete hCosTheta; 
}

void L2UpsilonMassHistogramGroup::reset() {
  hEnergyL0->Reset();
  hEnergyL2->Reset();
  hMass->Reset();
  hCosTheta->Reset(); 
}


void L2UpsilonMassHistogramGroup::draw(TCanvas* cc) {
  //cout << __PRETTY_FUNCTION__ << endl;
  cc->cd();
  cc->Clear();
  cc->Divide(2,2);
  cc->cd(1);
  hEnergyL0->Draw();
  cc->cd(2);
  hEnergyL2->Draw();
  cc->cd(3);
  hMass->Draw();
  cc->cd(4);
  hCosTheta->Draw();
  cc->Update();
} 

#include <stdlib.h>
#include "L2UpsilonResult.h"


bool L2UpsilonMassHistogramGroup::fill(evpReader* evp, char* datap) { 
  L2UpsilonResult L2;
  L2UpsilonResult* mL2 = &L2;
  if (!mL2) return false;

#ifndef NEW_DAQ_READER
  int ret = trgReader(datap) ;
  if(ret <= 0) {
    fprintf(stderr,"TRG RAW: problems in data (%d) - continuing...",ret) ;
    return false;
  }  

  TRGD *trgd = (TRGD *)trg.trgd;
  if ( !trgd ) return false;;

  memcpy(mL2,trgd->sum.L2Result+L2RESULTS_2008_OFFSET_UPS,sizeof(L2UpsilonResult) );
  mL2->swap();
#else
  StTriggerData* trgd = TriggerData::Instance(datap);
  if(!trgd) return false;
  const unsigned int* l2result = trgd->l2Result();
  //here we got l2result array... but there is no L2RESULTS_2009_OFFSET defined anywhere.
  //please contact akio@bnl.gov  
#endif

  if (!mL2) {
    cout << " No pointer to L2 result" << endl;
    return false;
  }
  hEnergyL0->Fill( 1.e-3 * mL2->energyOfClusterL0);
  hEnergyL2->Fill( 1.e-3 * mL2->energyOfClusterL2);
  hMass->Fill( 1.e-3 * mL2->invMass );
  hCosTheta->Fill( mL2->cosTheta );
  return true;
}

/*************************************************************************************
 $Id: L2UpsilonMassHistogramGroup.cxx,v 1.2 2009/02/13 22:23:04 dkettler Exp $
 *************************************************************************************
 $Log: L2UpsilonMassHistogramGroup.cxx,v $
 Revision 1.2  2009/02/13 22:23:04  dkettler
 Trigger data changes

 Revision 1.1  2009/01/23 16:07:56  jeromel
 Import from online/RTS/src/

 Revision 1.4  2008/12/19 15:51:03  dkettler
 Added new daqReader

 Revision 1.3  2007/11/27 11:52:05  psoren
 L2RESULTS_2007 --> L2RESULTS_2008

 Revision 1.2  2007/04/03 13:18:55  laue
 *** empty log message ***

 Revision 1.1  2007/03/21 17:06:07  laue
 Some new histogram groups.
 Some groups just moved from the Infrastructure folder to this new folder

*************************************************************************************/




 

