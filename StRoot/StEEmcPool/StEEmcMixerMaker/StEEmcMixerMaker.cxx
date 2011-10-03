#include <stdlib.h>
#include <string.h>
#include "TFile.h"

#include <Stiostream.h>
#include <StMessMgr.h>  
#include "StEventTypes.h"
#include "StEvent.h"
#include "StChain.h"
                                                  
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"

#include "StEEmcSimulatorMaker/StEEmcSimulatorMaker.h"
#include "StEEmcMixerMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h" 

static const TString eemcDetname[] = { "etow", "eprs", "esmdu", "esmdv" };

ClassImp(StEEmcMixerMaker)

//_____________________________________________________________________________
StEEmcMixerMaker::StEEmcMixerMaker(const char *name):StMaker(name)
{
  mAddHits        = kTRUE;
  mClear          = kTRUE;
  mEmbedAll       = kTRUE;
  mFakeTrackEmbed = kFALSE;
  mDoPrint        = kTRUE;
  mAdcToE         = kTRUE;

// MC data may have a ped added in StEEmcSlowMaker. Such an extra ped must be 
// subtracted when two ADCs (MC + data) are summed up in merging.  
  for(int i = 0; i < NDETECTORS; i++) mMinusPed[i] = kFALSE;


  scale(1.0);

  /// Initialize default thresholds, nsigma above ped
  threshold(3.0,0); /// towers
  threshold(3.0,1); /// prs
  threshold(3.0,2); /// smdu
  threshold(3.0,3); /// smdv

}

//_____________________________________________________________________________
StEEmcMixerMaker::~StEEmcMixerMaker() { }

//_____________________________________________________________________________
Int_t StEEmcMixerMaker::Init()
{
  mEEDb=(StEEmcDbMaker*)GetMaker("eemcDb");
  assert( mEEDb);

  m_hit_1 =  new TH1F("old_hit","old_hit",100,0.,30.);
  m_hit_2 =  new TH1F("new_hit","new_hit",100,0.,30.);
  m_edep_1 = new TH1F("EDEP1","edep1",100,0.,30.);
  m_edep_2 = new TH1F("EDEP2","edep2",100,0.,30.);

  mMuUtil = new StMuEmcUtil();
  return StMaker::Init();
}  

//_____________________________________________________________________________
Int_t StEEmcMixerMaker::Make()
{
  if(!getEvents()) return kStWarn;
  
  getDB();          // get info from DB
  clearPoints();    // clear EEMC points
  clearClusters();  // clear EEMC clusters

// to convert ADC to energy with calibration data in mEEDb.   
   if(mAdcToE) adcToEnergy();


  if(Debug()>1) {
    printf("-------------- print mEvent1 after energy converted -----------\n");
    printHits(mEvent1);
    printf("-------------- print mEvent2 after energy converted -----------\n");
    printHits(mEvent2);
  }

  if(mAddHits) if(addHits()!=kStOk) { 
    if(mDoPrint) gMessMgr->Warning()<<" error in addhits***"<<endm; 
    return kStWarn; 
  }

  if(Debug()) {
    printf("-------------- print mEvent1 after mixing ----------------\n");
    printHits(mEvent1);
  }

  if(mFakeTrackEmbed) addTracks(); 
  return kStOK;
}

//_____________________________________________________________________________
Int_t StEEmcMixerMaker::Finish() { return StMaker::Finish(); }

//-------------------------------------------------------------------
/*!
This method gets the two inputs and sets the global date to be the date of 
the first event (mEvent1). The first mEvent1 is got using the standard
way of getting StEvent pointer. Because of this it should be the first
one loaded on the memory. The second StEvent pointer (mEvent2) is 
converted from a StEmcCollection owned by StEEmcSimulatorMaker.
*/

Bool_t StEEmcMixerMaker::getEvents()
{
  // The default event should be the one where the data will be embedded
  // for this, it should be created before the second event
  // if there is no EMC collection in the event there is no reason for
  // embedding.

// First, read data
  mEvent1 = (StEvent*)GetInputDS("StEvent");
  if(!mEvent1) return kFALSE;
  if(!mEvent1->emcCollection()) return kFALSE; // should have EMC data

  if(Debug()) {
    printf("-------------- print mEvent1 ----------------\n");
    printHits(mEvent1);
  }

// Second, get MC from StEEmcSimulatorMaker

  // If the event source is the EEMC simulator, StEEmcMixerMaker owns the 
  // StEvent that is created to hold the StEmcCollection from simulator.
  // This maker adds the StEvent to .data so it will be properly deleted
  // during Clear() action
  StEEmcSimulatorMaker *sim = (StEEmcSimulatorMaker*)GetMaker("EEmcSimulator");
  if(sim)
  {
    StEmcCollection *ecol = sim->getEmcCollection();
    if(ecol)
    {
      mEvent2 = new StEvent();
      mEvent2->setEmcCollection(ecol);
      AddData(mEvent2);
    } else { if(mDoPrint) gMessMgr->Warning() <<"No second event to embed"<<endm; return kFALSE; }
  }
   else { if(mDoPrint) gMessMgr->Warning() <<"No EEmcSimulator found"<<endm; return kFALSE; }

/* MuDst stuff. Later, the maker may be expanded for MuDst.

  StMuDst* muDst;
  muDst = (StMuDst*)GetInputDS("MuDst");
  assert(muDst);
  StMuEmcCollection* muEmc = muDst->muEmcCollection();

// check MuDst eemc data
  if(Debug()) {
    printf("--------------  Info of MC MuDst (event2)  ----------------\n");
    printf("NEndcapTowser =  %d\n", muEmc->getNEndcapTowerADC());
    printf("NEndcapPrsHits =  %d\n", muEmc->getNEndcapPrsHits());
    printf("NEndcapSmdU =  %d\n", muEmc->getNEndcapSmdHits('U'));
    printf("NEndcapSmdV =  %d\n", muEmc->getNEndcapSmdHits('V'));
  }

  if(!muEmc) {
    if(mDoPrint) gMessMgr->Warning() <<"No StMuEmcCollection"<<endm; 
    return kFALSE; 
  }

// convert MuDst-emc to StEvent-emc 
  mEvent2 = new StEvent();
  StEmcCollection *emcCol = mMuUtil->getEmc(muEmc);


  mEvent2->setEmcCollection(emcCol);
*/
  if(Debug()) {
    printf("-------------- print MC mEvent2 ----------------\n");
    printHits(mEvent2);
  }

  if(mDoPrint) 
    gMessMgr->Info()<<"Event 1 = "<<mEvent1<<"   Event 2 ="<<mEvent2<<endm;
  
  // StEvent pointers should be different.
  if(mEvent1==mEvent2) { 
      if(mDoPrint) gMessMgr->Warning() <<"Identical events"<<endm; 
      return kFALSE;
  }
  return kTRUE;
}

//-------------------------------------------------------------------
/*!
This method gets status, gain, ped, and sigma from database. Only channels with 
good status are embedded. 
*/
void StEEmcMixerMaker::getDB()
{
  Int_t nHits[NDETECTORS][4];
  Int_t valid[NDETECTORS];
  for(Int_t i=0;i<NDETECTORS;i++) {
     for (Int_t j=0;j<4;j++)  nHits[i][j]=0;
     valid[i]=0;
     for(Int_t j=0;j<(TOWCHANNELS+(PRECHANNELS-TOWCHANNELS)*(i>0)+
                      (MAXCHANNELS-PRECHANNELS)*(i>1)); j++)
           mStatus[i][j] = 0;
  } 
/*
  StMuDst* muDst;
  muDst = (StMuDst*)GetInputDS("MuDst");
  assert(muDst);
  StMuEmcCollection* muEmc = muDst->muEmcCollection();
*/
  StMuEmcCollection* muEmc = mMuUtil->getMuEmc(mEvent2->emcCollection());
  if(!muEmc) {
    if(mDoPrint) gMessMgr->Warning() <<"getDB(): No StMuEmcCollection"<<endm; 
    return; 
  } 

  int rid;
  nHits[Etow][0]=muEmc->getNEndcapTowerADC();
  for (Int_t i=0; i< muEmc->getNEndcapTowerADC(); i++) {
     int sec,eta,sub,val; //muDst  ranges:sec:1-12, sub:1-5, eta:1-12
     muEmc->getEndcapTowerADC(i,val,sec,sub,eta);
     assert(sec>0 && sec<=kEEmcNumSectors);// total corruption of muDst
     nHits[Etow][1]++;

     if (sec<mEEDb->mfirstSecID || sec>mEEDb->mlastSecID) continue;
     //Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R 
     const EEmcDbItem *x=mEEDb->getTile(sec,'A'+sub-1,eta,'T');
     if(x==0) {
      printf("DB: x = 0\n");
      if(mDoPrint) gMessMgr->Info()<<"Hit number = "<<i<<"  sector = " << sec
      <<"  eta = "<<eta << "  sub = "<< sub<< "  adc = " << val <<endm;
      continue;
     } 
     nHits[Etow][2]++;
     if(x->fail) {
      printf("DB: x->fail\n");
      if(mDoPrint) gMessMgr->Info()<<"Hit number = "<<i<<"  sector = " << sec
      <<"  eta = "<<eta << "  sub = "<< sub<< "  adc = " << val <<endm;
      continue;
     } 
    nHits[Etow][3]++;
    mMuUtil->getEndcapId(Etow+1+DETOFFSET,sec,eta,sub,rid);
    if(rid < 1 || rid > 720) 
       printf ("TOWER: Invalid Id = %d sec eta sub = %d %d %d \n", 
                                                     rid, sec, eta, sub); 
    mStatus[Etow][rid-1] = 1;  
    mPed[0][rid-1] = x->ped;
    mSigPed[0][rid-1] = x->sigPed;
    mGain[0][rid-1] = x->gain;
  } // end of towers
  
  nHits[Eprs][0]=muEmc->getNEndcapPrsHits();
  for ( Int_t i = 0; i < muEmc->getNEndcapPrsHits(); i++ ) {
    Int_t pre,sec,eta,sub;
    muEmc->getEndcapPrsHit(i,sec,sub,eta,pre);
    nHits[Eprs][1]++;

   if (sec<mEEDb->mfirstSecID || sec>mEEDb->mlastSecID) continue;
    /// Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R 
    const EEmcDbItem *x=mEEDb-> getTile(sec,sub-1+'A', eta, pre-1+'P');
    if(x==0) {
      printf("DB: x = 0\n");
      if(mDoPrint) gMessMgr->Info()<<"Hit number = "<<i<<"  sector = " << sec
      <<"  eta = "<<eta << "  sub = "<< sub<< "  pre = " << pre <<endm;
      continue;
    } 
    nHits[Eprs][2]++;
    if(x->fail) {
      printf("DB: x->fail\n");
      if(mDoPrint) gMessMgr->Info()<<"Hit number = "<<i<<"  sector = " << sec
      <<"  eta = "<<eta << "  sub = "<< sub<< "  pre = " << pre <<endm;
      continue;
    } 
    nHits[Eprs][3]++;

    mMuUtil->getEndcapId(Eprs+1+DETOFFSET,sec,eta,sub+(pre-1)*5,rid);
    if(rid < 1 || rid > 2160)  
       printf ("PRE: Invalid Id = %d sec eta sub pre = %d %d %d %d\n", 
                                                  rid, sec, eta, sub, pre); 
    else 
    mStatus[Eprs][rid-1] = 1;  
    mPed[1][rid-1] = x->ped;
    mSigPed[1][rid-1] = x->sigPed;
    mGain[1][rid-1] = x->gain;
  } //end of pre/post

  char uv='U';
  for(uv='U'; uv<='V'; uv++) {
    nHits[uv-'U'+2][0]=muEmc->getNEndcapSmdHits(uv);
    int sec,strip;
    for (Int_t i=0; i<muEmc->getNEndcapSmdHits(uv); i++) {
       muEmc->getEndcapSmdHit(uv,i,sec,strip);
       nHits[uv-'U'+2][1]++;
    if (sec<mEEDb->mfirstSecID || sec>mEEDb->mlastSecID) continue;
       const EEmcDbItem *x=mEEDb->getByStrip(sec,uv,strip);
    
    if(x==0) {
      printf("DB: x = 0\n");
      if(mDoPrint) gMessMgr->Info()<<"Hit number = "<<i<<"  sector = " << sec
      <<"  strip = "<<strip << "  Layer = " << uv <<endm;
      continue;
    } 
    nHits[uv-'U'+2][2]++;
    if(x->fail) {
      printf("DB: x->fail\n");
      if(mDoPrint) gMessMgr->Info()<<"Hit number = "<<i<<"  sector = " << sec
      <<"  strip = "<<strip << "  Layer = " << uv <<endm;
      continue;
    } 
    nHits[uv-'U'+2][3]++;
    mMuUtil->getEndcapId(Esmdv+1+DETOFFSET,sec,strip,1,rid);
    if(rid < 1 || rid > 3600) 
       printf ("SMD: Invalid Id = %d sec strip layer = %d %d %c \n", 
                                                       rid, sec, strip, uv); 
    else
    mStatus[2+uv-'U'][rid-1] = 1;  
    mPed[2+uv-'U'][rid-1] = x->ped;
    mSigPed[2+uv-'U'][rid-1] = x->sigPed;
    mGain[2+uv-'U'][rid-1] = x->gain;
    }
  } // end of smd 

  for(Int_t i=0;i<NDETECTORS;i++) { 
      for(Int_t j=0;j<(TOWCHANNELS+(PRECHANNELS-TOWCHANNELS)*(i>0)+
                      (MAXCHANNELS-PRECHANNELS)*(i>1)); j++) {
          if(mStatus[i][j]==1) valid[i]++;
      }
  }
  if(mDoPrint) cout <<"Date = "<<GetDate()<<"  time = "<<GetTime()<<endl;
  if(mDoPrint) {
    for(Int_t i=0;i<NDETECTORS;i++) {
       cout <<"# of nHits[4] (eeDb-stat) and valid-chans for " 
      << eemcDetname[i]
      << " = " << nHits[i][0] << " " << nHits[i][1] << " " << nHits[i][2] 
      << " " << nHits[i][3]  << " " << valid[i]<<endl;
    }
  }
}

//-------------------------------------------------------------------
/*
This method converts ADC to energy and sets energy in hits for two StEvents. 
*/
void StEEmcMixerMaker::adcToEnergy() {
  
  if(Debug()>2) {
    printf("-------------- print mEvent1 before energy converted ----------\n");
    printHits(mEvent1);
    printf("-------------- print mEvent2 before energy converted ----------\n");
    printHits(mEvent2);
  }

// loop over two inputs (mEvent1 and mEvent2)
  for(UInt_t ievt = 0; ievt<2; ievt++) {

// loop over detectors 
    for(UInt_t i=0; i<NDETECTORS; i++) {
      StEmcCollection* emccol;
      if(ievt==0) emccol=(StEmcCollection*)mEvent1->emcCollection();
      else emccol=(StEmcCollection*)mEvent2->emcCollection();
      StDetectorId id = static_cast<StDetectorId>(i+kEndcapEmcTowerId);
      StEmcDetector* detector=emccol->detector(id);

// loop over sectors 
      for(int j=1;j<=kEEmcNumSectors;j++) { 
        StEmcModule* sector = detector->module(j);
        StSPtrVecEmcRawHit& rawHit=sector->hits();
        for(UInt_t k=0;k<rawHit.size();k++) {
           if(checkHit(i,rawHit[k])) {
              UInt_t adc;
              Float_t energy;
              Float_t ped = getPed(i,rawHit[k]);
              Float_t gain = getGain(i,rawHit[k]);
              Float_t threshold = ped + getSigPed(i,rawHit[k]) * mSigmaPed[i];

              adc = rawHit[k]->adc();
              if(gain > 0 && ped >= 0 && (adc >= threshold)) {
                if(ievt == 0) 
                  energy = float(adc - ped + 0.5)/gain;
                else
// No offset 0.5 for MC in StEEmcSlowMaker and StEEmcSimulatorMaker as for data 
                  energy = float(adc - ped)/gain;
                if(energy >  0) rawHit[k]->setEnergy(energy);
              }
           }
        }
      }
    }
  } 
} 

//--------------------------------------------------------------------
/*!
This method adds the hits from the the second StEmcCollection in mEvent2
into the first StEmcCollection in mEvent1 for all EEMC subdetectors
*/
Int_t StEEmcMixerMaker::addHits()
{   
  StEmcCollection* emccol1=(StEmcCollection*)mEvent1->emcCollection(); 
  StEmcCollection* emccol2=(StEmcCollection*)mEvent2->emcCollection();
  Int_t stat_h2_all[MAXCHANNELS];
  Int_t stat_h2[MAXCHANNELS];

  Float_t old_edep_tot=0.0;
  Float_t new_edep_tot=0.0;

/* 
 For EEMC, module = sector(1-12). 
 Tower (detector-name eemc): sub(1-5), eta(1-12)  
 Pre/Post (detector-name eprs): sub(1-15), eta(1-12)  
          Pre1 sub(1-5), Pre2: sub(6-10), Post: sub(11-15)
 Smd U & V (detector-name: esmdu and esmdv): sub=-1, eta=strip(1-288) 
*/                   

// Loop over all four sub detectors
  for(Int_t i=0; i<NDETECTORS; i++) 
  {
    StDetectorId id = static_cast<StDetectorId>(i+kEndcapEmcTowerId);
    StEmcDetector* detector1=emccol1->detector(id);
    StEmcDetector* detector2=emccol2->detector(id);
    if(!detector1) if(mDoPrint) 
         gMessMgr->Warning()<<"detector1 not loaded"<<endm;
    if(!detector2) 
         if(mDoPrint) gMessMgr->Warning()<<"detector2 not loaded"<<endm;
   
    Float_t edep1_tot=0;
    Float_t edep2_tot=0;

// loop over sectors
    if(detector1 && detector2) for(int j=1;j<=kEEmcNumSectors;j++) 
    {
      StEmcModule* sector1 = detector1->module(j);
      StEmcModule* sector2 = detector2->module(j);
      StSPtrVecEmcRawHit& rawHit1=sector1->hits();
      StSPtrVecEmcRawHit& rawHit2=sector2->hits();

      for(UInt_t k1=0;k1<rawHit1.size();k1++) edep1_tot+=rawHit1[k1]->energy();
      for(UInt_t k2=0;k2<rawHit2.size();k2++) {
           stat_h2_all[k2]=0;
           stat_h2[k2]=0;
           edep2_tot+=rawHit2[k2]->energy();
      }

      for(UInt_t k1=0;k1<rawHit1.size();k1++)  
      {
        Int_t sec1, eta1, sub1; 
        sec1=(Int_t)rawHit1[k1]->module();
        eta1=(Int_t)rawHit1[k1]->eta();
        sub1=abs(rawHit1[k1]->sub());                                                   
        for(UInt_t is=0;is<rawHit2.size();is++) {stat_h2[is]=0;}

        Float_t edep_add=0.0;
        UInt_t adc_add=0; 
        for(UInt_t k2=0;k2<rawHit2.size();k2++) if(checkHit(i,rawHit2[k2]))
        {
          if(stat_h2[k2]==1)continue;
          Int_t sec2, eta2, sub2; 
          Float_t ped2;
          sec2=(Int_t)rawHit2[k2]->module();
          eta2=(Int_t)rawHit2[k2]->eta();
          sub2=abs(rawHit2[k2]->sub());          
          if(sec1==sec2 && eta1==eta2 && sub1==sub2)
          {
            stat_h2[k2]=1;
            stat_h2_all[k2]=1;
            edep_add=rawHit2[k2]->energy();
            ped2 = (mMinusPed[i])?getPed(i,rawHit2[k2]):0.0;
            adc_add= UInt_t(rawHit2[k2]->adc() - ped2); 
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
        if(mDoPrint && edep_add>0) { 
          if(i < 2) 
            gMessMgr->Info() <<"EMBEDDED HIT -> " << eemcDetname[i]  
                    <<"  sec = " <<sec1<<"  eta = "<<eta1<<"  sub = "<<sub1
                    <<" ADCADD = "<<adc_add
                    <<"  newADC = "<<rawHit1[k1]->adc()
                    <<"  oldE = "<<oldE<<" EADD = "<<edep_add
                    <<"  newE = "<<rawHit1[k1]->energy()<<endm;
          else 
            gMessMgr->Info() <<"EMBEDDED HIT -> " << eemcDetname[i]  
                    <<"  sec = " <<sec1<<"  strip = "<<eta1
                    <<" ADCADD = "<<adc_add
                    <<"  newADC = "<<rawHit1[k1]->adc()
                    <<"  oldE = "<<oldE<<" EADD = "<<edep_add
                    <<"  newE = "<<rawHit1[k1]->energy()<<endm;
        }
      }
      
      //Add remainig hits
      if(mEmbedAll) for(UInt_t k2=0;k2<rawHit2.size();k2++) if(checkHit(i,rawHit2[k2]))
      {
        if(stat_h2_all[k2]==0)
        { 
/*
 We initialize sub = 0 for SMD even rawHit2[k2]->sub() always = -1. 
 Initializing with a negative sub will screw up eta value of a new StEmcRawHit! 
*/
          StEmcRawHit* hit = new StEmcRawHit(id, rawHit2[k2]->module(),
                                 rawHit2[k2]->eta(), 
                                 (rawHit2[k2]->sub()>=0)?rawHit2[k2]->sub():0,
                                 rawHit2[k2]->adc(), rawHit2[k2]->energy()); 
          detector1->addHit(hit); 
          new_edep_tot+=rawHit2[k2]->energy();

          if(mDoPrint) { 
            if(i < 2) 
              gMessMgr->Info() <<"No Matching HIT ->  " <<  eemcDetname[i]
                           <<"  sec = " <<rawHit2[k2]->module()
                           <<"  eta = "<<rawHit2[k2]->eta()
                           <<"  sub = "<<rawHit2[k2]->sub()
                           <<"  energy = "<<rawHit2[k2]->energy()<<endm;
            else
              gMessMgr->Info() <<"No Matching HIT ->  " <<  eemcDetname[i]
                           <<"  sec = " <<rawHit2[k2]->module()
                           <<"  strip = "<<rawHit2[k2]->eta()
                           <<"  energy = "<<rawHit2[k2]->energy()<<endm;
          }
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
This method clears the EEmc points in the StEmcCollection from mEvent1
*/
void StEEmcMixerMaker::clearPoints()
{
  StEmcCollection* emccol=(StEmcCollection*)mEvent1->emcCollection();
  if(emccol)
  {
    StSPtrVecEmcPoint& pvec = emccol->endcapPoints();
    if(mClear)pvec.clear();  
  }
}

//-------------------------------------------------------------------
/*!
This method clears the EEmc clusters in the StEmcCollection from mEvent1
*/
void StEEmcMixerMaker::clearClusters()
{
  StEmcCollection* emccol=(StEmcCollection*)mEvent1->emcCollection();
  if(emccol) for(Int_t i=0; i<NDETECTORS; i++)
  {
    StDetectorId id = static_cast<StDetectorId>(i+kEndcapEmcTowerId);
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
This method gets peds. 
\param h is the StEmcRawHit pointer
*/
Float_t StEEmcMixerMaker::getPed(Int_t d, StEmcRawHit* h)
{
  if(!h) return kFALSE;
  Int_t m = (Int_t)h->module();
  Int_t e = (Int_t)h->eta();
  Int_t s = abs(h->sub());     
  Int_t rid;
// EEMC detector (d) in getEndcapId()  goes from 5-8 
// 5:TOW, 6:PRE, 7:SMDU, 8 SMDV 
  mMuUtil->getEndcapId(d+1+DETOFFSET,m,e,s,rid);
  return mPed[d][rid-1];
}

//-------------------------------------------------------------------
/*!
This method gets sigPeds. 
\param h is the StEmcRawHit pointer
*/
Float_t StEEmcMixerMaker::getSigPed(Int_t d, StEmcRawHit* h)
{
  if(!h) return kFALSE;
  Int_t m = (Int_t)h->module();
  Int_t e = (Int_t)h->eta();
  Int_t s = abs(h->sub());     
  Int_t rid;
// EEMC detector (d) in getEndcapId()  goes from 5-8 
// 5:TOW, 6:PRE, 7:SMDU, 8 SMDV 
  mMuUtil->getEndcapId(d+1+DETOFFSET,m,e,s,rid);
  return mSigPed[d][rid-1];
}

//-------------------------------------------------------------------
/*!
This method gets gain. 
\param h is the StEmcRawHit pointer
*/
Float_t StEEmcMixerMaker::getGain(Int_t d, StEmcRawHit* h)
{
  if(!h) return kFALSE;
  Int_t m = (Int_t)h->module();
  Int_t e = (Int_t)h->eta();
  Int_t s = abs(h->sub());     
  Int_t rid;
// EEMC detector (d) in getEndcapId()  goes from 5-8 
// 5:TOW, 6:PRE, 7:SMDU, 8 SMDV 
  mMuUtil->getEndcapId(d+1+DETOFFSET,m,e,s,rid);
  return mGain[d][rid-1];
}

//-------------------------------------------------------------------
/*!
This method checks if the hit is a valid one. 
\param d is the detector number
\param h is the StEmcRawHit pointer
*/
Bool_t StEEmcMixerMaker::checkHit(Int_t d, StEmcRawHit* h)
{
  if(!h) return kFALSE;
  Int_t m = (Int_t)h->module();
  Int_t e = (Int_t)h->eta();
  Int_t s = abs(h->sub());     
  Int_t rid;
// EEMC detector (d) in getEndcapId()  goes from 5-8 
// 5:TOW, 6:PRE, 7:SMDU, 8 SMDV 
  mMuUtil->getEndcapId(d+1+DETOFFSET,m,e,s,rid);
  if(mStatus[d][rid-1] == 1) return kTRUE;
  return kFALSE;     
}

//--------------------------------------------------------------------
/*!
This method adds the tracks of mEvent2 into mEvent1. This is a
fake track embedding. This is not a true track embedding. Should
not be used for final efficiencies calculations
*/
Int_t StEEmcMixerMaker::addTracks()
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

//-------------------------------------------------------------------
void StEEmcMixerMaker::printHits(StEvent *event)
{
  StEmcCollection* emccol=(StEmcCollection*)event->emcCollection();
  
  for(Int_t i=0; i<NDETECTORS; i++)
  {  
    StDetectorId id = static_cast<StDetectorId>(i+kEndcapEmcTowerId);
    StEmcDetector* detector=emccol->detector(id);
    gMessMgr->Info()<<"******* StEEmcMixerMaker:: hits in detector "
                                 << eemcDetname[i].Data()<<endm;
    if(detector) for(int j=1;j<=kEEmcNumSectors;j++) 
    {
      StEmcModule* sector = detector->module(j);
      StSPtrVecEmcRawHit& rawHit=sector->hits();
      if(rawHit.size()>0) 
         if(mDoPrint) gMessMgr->Info()<<"Number of hits for module "
                                      <<j<<" = "<<rawHit.size()<<endm;
      for(UInt_t k=0;k<rawHit.size();k++)
        if(i < 2) // for Tower and Prs
          gMessMgr->Info()<<"Hit number = "<<k
                     <<"  sector = " << rawHit[k]->module()
                     <<"  eta = "<<rawHit[k]->eta() 
                     <<"  sub = "<< rawHit[k]->sub()
                     <<"  adc = "<< rawHit[k]->adc() 
                     <<"  energy = "<<rawHit[k]->energy()<<endm;
        else 
          gMessMgr->Info()<<"Hit number = "<<k
                     <<"  sector = " << rawHit[k]->module()
                     <<"  strip = "<<rawHit[k]->eta() 
                     <<"  adc = "<< rawHit[k]->adc() 
                     <<"  energy = "<<rawHit[k]->energy()<<endm;
    }
  }
}

///////////////////////////////////////////////////////////////////////////
//
// $Id: StEEmcMixerMaker.cxx,v 1.1.1.1 2005/05/31 18:53:25 wzhang Exp $
// $Log: StEEmcMixerMaker.cxx,v $
// Revision 1.1.1.1  2005/05/31 18:53:25  wzhang
// First version
//
//
///////////////////////////////////////////////////////////////////////////
