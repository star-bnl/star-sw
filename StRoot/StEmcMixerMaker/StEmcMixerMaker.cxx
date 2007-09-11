#include "StEmcMixerMaker.h"
#include <Stiostream.h>
#include <math.h>
#include "SystemOfUnits.h"
#include <stdlib.h>
#include <string.h>
#include "TMath.h"
#include "TFile.h"
#include "Stypes.h"
#include "math_constants.h"
#include "StEmcSimulatorMaker/StEmcSimulatorMaker.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "tables/St_emcStatus_Table.h"
#include "tables/St_smdStatus_Table.h"
                                                  
ClassImp(StEmcMixerMaker)

//_____________________________________________________________________________
StEmcMixerMaker::StEmcMixerMaker(const char *name):StMaker(name)
{
  mAddHits = kTRUE;
  mClear = kTRUE;
  mUseDB = kTRUE;
  mEmbedAll = kTRUE;
  mFakeTrackEmbed = kFALSE;
  for(Int_t i=0;i<NDETECTORS;i++) mGeom[i]=StEmcGeom::instance(i+1);
}
//_____________________________________________________________________________
StEmcMixerMaker::~StEmcMixerMaker()
{
}
//_____________________________________________________________________________
Int_t StEmcMixerMaker::Init()
{
  m_hit_1 = new TH1F("old_hit","old_hit",100,0.,30.);
  m_hit_2 = new TH1F("new_hit","new_hit",100,0.,30.);
  m_edep_1 = new TH1F("EDEP1","edep1",100,0.,30.);
  m_edep_2 = new TH1F("EDEP2","edep2",100,0.,30.);
  return StMaker::Init();
}  
//_____________________________________________________________________________
Int_t StEmcMixerMaker::Make()
{
  if(!getEvents()) return kStWarn;
  
  if(mUseDB) getDB(); // get status tables from DB. If not, set all status to 1 (valid channel)
  else 
    for(Int_t i=0;i<NDETECTORS;i++) 
      for(Int_t j=0;j<(EMCCHANNELS+(MAXCHANNELS-EMCCHANNELS)*(i>1));j++) 
        mStatus[i][j] = 1;
  
  clearPoints(); // clear EMC points
  clearClusters();  // clear EMC clusters

  if(mAddHits){ if(addHits()!=kStOk) { LOG_WARN <<" error in addhits***"<<endm; return kStWarn; } }
  if(mFakeTrackEmbed) addTracks(); 
  return kStOK;
}

//_____________________________________________________________________________
Int_t StEmcMixerMaker::Finish() 
{
  return StMaker::Finish();
}
//--------------------------------------------------------------------
/*!
This method adds the hits from the the second StEmcCollection from mEvent2
into the first StEmcCollection in mEvent1 for all EMC subdetectors
*/
Int_t StEmcMixerMaker::addHits()
{   
  StEmcCollection* emccol1=(StEmcCollection*)mEvent1->emcCollection(); 
  StEmcCollection* emccol2=(StEmcCollection*)mEvent2->emcCollection();
  Int_t stat_h2_all[MAXCHANNELS];
  Int_t stat_h2[MAXCHANNELS];

// Loop over all four sub detectors
  Float_t old_edep_tot=0.0;
  Float_t new_edep_tot=0.0;
  for(Int_t i=0; i<NDETECTORS; i++) 
  {
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    StEmcDetector* detector1=emccol1->detector(id);
    StEmcDetector* detector2=emccol2->detector(id);
    if(!detector1){ LOG_WARN <<"detector1 not loaded"<<endm; }
	if(!detector2){ LOG_WARN <<"detector2 not loaded"<<endm; }
   
    Float_t edep1_tot=0;
    Float_t edep2_tot=0;
 
    if(detector1 && detector2) for(UInt_t j=1;j<=NMODULES;j++) // loop over all modules
    {
      StEmcModule* module1 = detector1->module(j);
      StEmcModule* module2 = detector2->module(j);
      StSPtrVecEmcRawHit& rawHit1=module1->hits();
      StSPtrVecEmcRawHit& rawHit2=module2->hits();

      for(UInt_t k1=0;k1<rawHit1.size();k1++) {edep1_tot+=rawHit1[k1]->energy();}
      for(UInt_t k1=0;k1<rawHit2.size();k1++) {edep2_tot+=rawHit2[k1]->energy();}

      for(UInt_t is=0;is<rawHit2.size();is++) {stat_h2_all[is]=0;stat_h2[is]=0;}
 
      for(UInt_t k1=0;k1<rawHit1.size();k1++)  
      {
        Int_t m1, e1, s1; 
        m1=(Int_t)rawHit1[k1]->module();
        e1=(Int_t)rawHit1[k1]->eta();
        s1=abs(rawHit1[k1]->sub());                                                   
        for(UInt_t is=0;is<rawHit2.size();is++) {stat_h2[is]=0;}

        Float_t edep_add=0.0;
        UInt_t adc_add=0; 
        for(UInt_t k2=0;k2<rawHit2.size();k2++) if(checkHit(i,rawHit2[k2]))
        {
          if(stat_h2[k2]==1)continue;
          Int_t m2, e2, s2;
          m2=(Int_t)rawHit2[k2]->module();
          e2=(Int_t)rawHit2[k2]->eta();
          s2=abs(rawHit2[k2]->sub());          
          if(m1==m2 && e1==e2 && s1==s2)
          {
            stat_h2[k2]=1;
            stat_h2_all[k2]=1;
            edep_add=rawHit2[k2]->energy();
            adc_add=rawHit2[k2]->adc();
          }
          if(stat_h2[k2]==1)break;
        }
        Float_t new_edep=rawHit1[k1]->energy()+edep_add;
        UInt_t new_adc=rawHit1[k1]->adc()+adc_add;
        Float_t oldE=rawHit1[k1]->energy();
        old_edep_tot+=rawHit1[k1]->energy();
        rawHit1[k1]->setAdc(new_adc);
        rawHit1[k1]->setEnergy(new_edep);
        new_edep_tot+=new_edep;
        UInt_t calib = rawHit1[k1]->calibrationType();
        while(calib>127) calib-=128;
        if(edep_add>0) {
          LOG_DEBUG <<"EMBEDDED HIT -> det = "<<i+1<<"  m = "<<m1<<"  e = "<<e1<<"  s = "<<s1
                           <<"  calib = "<<rawHit1[k1]->calibrationType()
                           <<"  new calib = "<<calib
                           <<"  oldE = "<<oldE<<" EADD = "<<edep_add
                           <<"  newE = "<<rawHit1[k1]->energy()<<endm;
		}
        rawHit1[k1]->setCalibrationType(calib);
      }
      
      //Add remainig hits
      if(mEmbedAll) for(UInt_t k2=0;k2<rawHit2.size();k2++) if(checkHit(i,rawHit2[k2]))
      {
        if(stat_h2_all[k2]==0)
        { 
          StEmcRawHit* hit = new StEmcRawHit(id, rawHit2[k2]->module(),
                                             rawHit2[k2]->eta(), rawHit2[k2]->sub(),
                                             rawHit2[k2]->adc(), rawHit2[k2]->energy()); 
          detector1->addHit(hit); 
          new_edep_tot+=rawHit2[k2]->energy();
        }
      }   
    } 
    m_hit_1->Fill(old_edep_tot);
    m_hit_2->Fill(new_edep_tot);
    m_edep_1->Fill(edep1_tot);
    m_edep_2->Fill(edep2_tot);
  }
  return kStOK;
}
//-------------------------------------------------------------------
/*!
This method clears the Emc points in the StEmcCollection from mEvent1
*/
void StEmcMixerMaker::clearPoints()
{
  StEmcCollection* emccol=(StEmcCollection*)mEvent1->emcCollection();
  if(emccol)
  {
    StSPtrVecEmcPoint& pvec = emccol->barrelPoints();
    if(mClear)pvec.clear();  
  }
}
//-------------------------------------------------------------------
/*!
This method clears the Emc clusters in the StEmcCollection from mEvent1
*/
void StEmcMixerMaker::clearClusters()
{
  StEmcCollection* emccol=(StEmcCollection*)mEvent1->emcCollection();
  if(emccol) for(Int_t i=0; i<NDETECTORS; i++)
  {
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    StEmcDetector* detector=emccol->detector(id);
    if(detector)
    {
      if(detector->cluster())
      {
        StSPtrVecEmcCluster& cluster=detector->cluster()->clusters();
        if(mClear)cluster.clear();  
      }
    }
  }
}
//-------------------------------------------------------------------
/*!
This method gets the two StEvent, and sets the global date to be
the date of the first event (mEvent1). mEvent1 is got using the standard
way of getting StEvent pointer. Because of this it should be the first
one loaded on the memory. The second StEvent pointer (mEvent2) is got
from a StIOMaker with name "embedIO" or StEmcSimulatorMaker
*/
Bool_t StEmcMixerMaker::getEvents()
{
  // the default event should be the one where the data will be embedded
  // for this, it should be created before the second event
  // if there is no EMC collection in the event there is no reason for
  // embedding.
  mEvent1 = (StEvent*)GetInputDS("StEvent");
  if(!mEvent1) return kFALSE;
  if(!mEvent1->emcCollection()) return kFALSE; // should have EMC data

  // getting second event. First try StEmcSimulatorMaker
  // if there is no EMC simulator, try to get from a second StEvent in the memory
  // if the event source is the EMC simulator, StEmcMixerMaker owns the StEvent
  // that is created to hold the StEmcCollection from simulator
  // This maker adds the StEvent to .data so it will be properly deleted
  // during Clear() action
  StEmcSimulatorMaker *sim = (StEmcSimulatorMaker*)GetMaker("EmcSimulator");
  if(sim)
  {
    StEmcCollection *ecol = sim->getEmcCollection();
    if(ecol)
    {
      mEvent2 = new StEvent();
      mEvent2->setEmcCollection(ecol);
      AddData(mEvent2);
    } else { LOG_WARN <<"No second event to embed"<<endm; return kFALSE; }
  }
  else // no EMC simulator. Events come from another source
  {
    StMaker *m = GetMaker("embedIO");
	if(!m) { LOG_WARN <<"No embedIO maker"<<endm; return kFALSE; }
    mEvent2 = (StEvent*)m->GetInputDS("StEvent");
    if(!mEvent2) { LOG_WARN <<"No second event to embed"<<endm; return kFALSE; }
    if(!mEvent2->emcCollection()) { LOG_WARN <<"No second event to embed"<<endm; return kFALSE; }
  }
  
  LOG_DEBUG <<"Event 1 = "<<mEvent1<<"   Event 2 ="<<mEvent2<<endm;
  
  // StEvent pointers should be different.
  if(mEvent1==mEvent2) { LOG_DEBUG <<"Identical events"<<endm; return kFALSE; }
  return kTRUE;
}
//-------------------------------------------------------------------
void StEmcMixerMaker::printHits(StEvent *event)
{
  const TString detname[] = {"bemc", "bprs", "bsmde", "bsmdp","eemc", "eprs", "esmde", "esmdp"};
  StEmcCollection* emccol=(StEmcCollection*)event->emcCollection();
  
  for(Int_t i=0; i<NDETECTORS; i++)
  {  
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    StEmcDetector* detector=emccol->detector(id);
    LOG_INFO <<"****************** hits in detector "<< detname[i].Data()<<endm;
    if(detector) for(UInt_t j=1;j<=NMODULES;j++) 
    {
      StEmcModule* module = detector->module(j);
      StSPtrVecEmcRawHit& rawHit=module->hits();
      if(rawHit.size()>0) { LOG_INFO <<"Number of hits for module "<<j<<" = "<<rawHit.size()<<endm; }
      for(UInt_t k=0;k<rawHit.size();k++) {
        LOG_INFO <<"Hit number = "<<k<<"  module = " << rawHit[k]->module()<<"  eta = "<<rawHit[k]->eta() << "  sub = "<< rawHit[k]->sub()<< "  adc = "<< rawHit[k]->adc() <<"  energy = "<<rawHit[k]->energy()<<endm;
	  }
    }
  }
  
}
//-------------------------------------------------------------------
/*!
This method gets the Status tables from database. Only channels with 
good status are embedded. This option can be turned off, however.
*/
void StEmcMixerMaker::getDB()
{
  TDataSet *Db=GetDataBase("Calibrations/emc/y3bemc");
  Int_t valid[NDETECTORS]; for(Int_t i=0;i<NDETECTORS;i++) valid[i]=0;
  St_emcStatus* s0 = (St_emcStatus*)Db->Find("bemcStatus");
  if(s0)
  {
    emcStatus_st* st=s0->GetTable();
    for(Int_t i=0;i<EMCCHANNELS;i++)  mStatus[0][i] = st[0].Status[i];
  } else for(Int_t i=0;i<EMCCHANNELS;i++) mStatus[0][i] = 0;
   
  Db=GetDataBase("Calibrations/emc/y3bprs");
  St_emcStatus* s1 = (St_emcStatus*)Db->Find("bprsStatus");
  if(s1)
  {
    emcStatus_st* st=s1->GetTable();
    for(Int_t i=0;i<EMCCHANNELS;i++)  mStatus[1][i] = st[0].Status[i];
  } else for(Int_t i=0;i<EMCCHANNELS;i++) mStatus[1][i] = 0;
    
  Db=GetDataBase("Calibrations/emc/y3bsmde");
  St_smdStatus* s2 = (St_smdStatus*)Db->Find("bsmdeStatus");
  if(s2)
  {
    smdStatus_st* st=s2->GetTable();
    for(Int_t i=0;i<SMDCHANNELS;i++)  mStatus[2][i] = st[0].Status[i];
  } else for(Int_t i=0;i<SMDCHANNELS;i++) mStatus[2][i] = 0;
    
  Db=GetDataBase("Calibrations/emc/y3bsmdp");
  St_smdStatus* s3 = (St_smdStatus*)Db->Find("bsmdpStatus");
  if(s3)
  {
    smdStatus_st* st=s3->GetTable();
    for(Int_t i=0;i<SMDCHANNELS;i++)  mStatus[3][i] = st[0].Status[i];
  } else for(Int_t i=0;i<SMDCHANNELS;i++) mStatus[3][i] = 0;
  
  for(Int_t i=0;i<NDETECTORS;i++) 
    for(Int_t j=0;j<(EMCCHANNELS*(i<2)+SMDCHANNELS*(i>1));j++) 
      if(mStatus[i][j]==1) valid[i]++;
  
  LOG_DEBUG <<"Date = "<<GetDate()<<"  time = "<<GetTime()<<endm;
  for(Int_t i=0;i<NDETECTORS;i++) { LOG_DEBUG <<"Number of valid channels for detector "<<i<<" = "<<valid[i]<<endm; }
}
//-------------------------------------------------------------------
/*!
This method check if the hit is a valid one based on the status
tables. 
\param d is the detector number
\param h is the StEmcRawHit pointer
*/
Bool_t StEmcMixerMaker::checkHit(Int_t d,StEmcRawHit* h)
{
  if(!h) return kFALSE;
  Int_t m = (Int_t)h->module();
  Int_t e = (Int_t)h->eta();
  Int_t s = abs(h->sub());     
  Int_t id;
  mGeom[d]->getId(m,e,s,id);
  if(id>0) if(mStatus[d][id-1]==1) return kTRUE;
  return kFALSE;     
}
//--------------------------------------------------------------------
/*!
This method adds the tracks of mEvent2 into mEvent1. This is a
fake track embedding. This is not a true track embedding. Should
not be used for final efficiencies calculations
*/
Int_t StEmcMixerMaker::addTracks()
{
  // this just adds the tracks from one StEvent into the tracks of another StEvent.
  StSPtrVecTrackNode& Node1 = mEvent1->trackNodes();
  StSPtrVecTrackNode& Node2 = mEvent2->trackNodes();
  for (unsigned int j=0; j < Node2.size(); j++)
  {
    StGlobalTrack*   gTrack = (StGlobalTrack*)(Node2[j]->track(global));
    StPrimaryTrack*  pTrack = (StPrimaryTrack*)(Node2[j]->track(primary));
    StTrackNode *node = new StTrackNode();
    if(gTrack) node->addTrack(new StGlobalTrack(*gTrack));
    if(pTrack) node->addTrack(new StPrimaryTrack(*pTrack));
    Node1.push_back(node);
  } 
  return 0;
}
