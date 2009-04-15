#include "L2UpsilonTowersHistogramGroup.h"
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




ClassImp(L2UpsilonTowersHistogramGroup) ;

L2UpsilonTowersHistogramGroup::L2UpsilonTowersHistogramGroup() {
  // For ROOT I/O
  hTriggerTowerIdL0 = 0;
  hTriggerTowerIdL2 = 0;
  hNumberOfTowersL0 = 0;
  hNumberOfTowersL2 = 0;
  hEtaPhiL0 = 0;
  hEtaPhiL2 = 0;
}

L2UpsilonTowersHistogramGroup::L2UpsilonTowersHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector) : HistogramGroup(group,subGroup,trigger,detector) {

  hTriggerTowerIdL0 = new TH1D( pre("hTriggerTowerIdL0"), "trigger tower id L0",4800,0.,4800.);
  hTriggerTowerIdL2 = new TH1D( pre("hTriggerTowerIdL2"), "trigger tower id L2",4800,0.,4800.);
  hNumberOfTowersL0 = new TH1D( pre("hNumberOfTowersL0"), "number of towers L0",25,0.,25);
  hNumberOfTowersL2 = new TH1D( pre("hNumberOfTowersL2"), "number of towers L2",100,0.,100);
  hEtaPhiL0 =  new TH2D( pre("hEtaPhiL0"), "phi vs eta L0",40,-1.3,1.3, 120,-3.14145927,+3.14145927);
  hEtaPhiL2 =  new TH2D( pre("hEtaPhiL2"), "phi vs eta L2",40,-1.3,1.3, 120,-3.14145927,+3.14145927);
}

L2UpsilonTowersHistogramGroup::~L2UpsilonTowersHistogramGroup() {
 delete  hTriggerTowerIdL0;
 delete  hTriggerTowerIdL2;
 delete  hNumberOfTowersL0;
 delete  hNumberOfTowersL2;
 delete  hEtaPhiL0;
 delete  hEtaPhiL2;
}

void L2UpsilonTowersHistogramGroup::reset() {
 hTriggerTowerIdL0->Reset();
 hTriggerTowerIdL2->Reset();
 hNumberOfTowersL0->Reset();
 hNumberOfTowersL2->Reset();
 hEtaPhiL0->Reset();
 hEtaPhiL2->Reset();
}


void L2UpsilonTowersHistogramGroup::draw(TCanvas* cc) {
  cc->cd();
  cc->Clear();
  cc->Divide(2,3);
  cc->cd(1);
  hTriggerTowerIdL0->Draw();
  cc->cd(2);
  hTriggerTowerIdL2->Draw();
  cc->cd(3);
  hNumberOfTowersL0->Draw();
  cc->cd(4);
  hNumberOfTowersL2->Draw();
  cc->cd(5);
  hEtaPhiL0->Draw("colz");
  cc->cd(6);
  hEtaPhiL2->Draw("colz");
  cc->Update();
} 

#include <stdlib.h>
#include "L2UpsilonResult.h"
#include "BemcGeometry.h"
bool L2UpsilonTowersHistogramGroup::fill(evpReader* evp, char* datap) { 
  L2UpsilonResult L2;
  L2UpsilonResult* mL2 = &L2;
  if  (!mL2) return false;

#ifndef NEW_DAQ_READER
  int ret = trgReader(datap) ;
  if(ret <= 0) {
    fprintf(stderr,"TRG RAW: problems in data (%d) - continuing...",ret) ;
    return false;
  }  
  TRGD *trgd = (TRGD *)trg.trgd;
  if ( !trgd ) return false;
  
  memcpy(mL2,trgd->sum.L2Result+L2RESULTS_2008_OFFSET_UPS,sizeof(L2UpsilonResult) );
  mL2->swap();
#else
  StTriggerData* trgd = TriggerData::Instance(datap);
  if(!trgd) return false;
  const unsigned int* l2result = trgd->l2Result();
  //here we got l2result array... but there is no L2RESULTS_2009_OFFSET defined anywhere.
  //please contact akio@bnl.gov
#endif

  hTriggerTowerIdL0->Fill( mL2->triggerTowerL0 );
  hTriggerTowerIdL2->Fill( mL2->triggerTowerL2 );
  hNumberOfTowersL0->Fill( mL2->numberOfTowersL0 );
  hNumberOfTowersL2->Fill( mL2->numberOfTowersL2 );

  //BemcGeometry* geom = BemcGeometry::instance();
  
  //hEtaPhiL0->Fill( geom->eta(mL2->triggerTowerL0), geom->phi(mL2->triggerTowerL0) );
  //hEtaPhiL2->Fill( geom->eta(mL2->triggerTowerL2), geom->phi(mL2->triggerTowerL2) );
    
  return true;
}

/*************************************************************************************
 $Id: L2UpsilonTowersHistogramGroup.cxx,v 1.4 2009/04/15 21:07:31 dkettler Exp $
 *************************************************************************************
 $Log: L2UpsilonTowersHistogramGroup.cxx,v $
 Revision 1.4  2009/04/15 21:07:31  dkettler
 Avoid segfaults

 Revision 1.3  2009/02/19 22:32:05  genevb
 More thorough default constructor implementations

 Revision 1.2  2009/02/13 22:23:04  dkettler
 Trigger data changes

 Revision 1.1  2009/01/23 16:07:56  jeromel
 Import from online/RTS/src/

 Revision 1.4  2008/12/19 15:51:04  dkettler
 Added new daqReader

 Revision 1.3  2007/11/27 11:52:06  psoren
 L2RESULTS_2007 --> L2RESULTS_2008

 Revision 1.2  2007/04/03 13:18:55  laue
 *** empty log message ***

 Revision 1.1  2007/03/21 17:06:07  laue
 Some new histogram groups.
 Some groups just moved from the Infrastructure folder to this new folder

*************************************************************************************/




 

