//
// $Id: StBemcTrigger.cxx,v 1.13 2004/08/04 17:53:08 suaide Exp $
//
//    

#include "StBemcTrigger.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StarRoot/TUnixTime.h"  
#include "StDaqLib/EMC/StEmcDecoder.h"
#include "Stiostream.h"
ClassImp(StBemcTrigger);

//-------------------------------------------------------------------
StBemcTrigger::StBemcTrigger():TObject()
{   
  mGeo=StEmcGeom::getEmcGeom("bemc");
  mEvent = NULL;
  mDecoder = NULL;
  mPrint = true;
  resetConf();
}
//----------------------------------------------------    
StBemcTrigger::~StBemcTrigger()
{ 
}
//----------------------------------------------------    
void StBemcTrigger::resetConf()
{
  for(int i = 0;i<kNTowers;i++)
  {
    mTrigger.TowerPedestal[i] = 0;
    mTrigger.TowerStatus[i] = 1;
  }
  mTrigger.HTBits = 3;
  int ped = 15;
  for(int i = 0;i<kNPatches;i++)
  {
    mTrigger.PatchStatus[i] = 1;
    for(int j=0;j<k12bits;j++)
    {
      if(j<ped)              mTrigger.PatchLUT[i][j] = 0;
      if(j>=ped && j<ped+63) mTrigger.PatchLUT[i][j] = j-ped;
      if(j>=ped+63)          mTrigger.PatchLUT[i][j] = 63;
    }
  }
}

//----------------------------------------------------    
void StBemcTrigger::zero()
{
  for(int i=0;i<kNPatches;i++)
  {
    mTrigger.HT[i] = 0;
    mTrigger.Patch[i]= 0;
  }
  for(int i=0;i<kNJet;i++)
  {
    mTrigger.Jet[i]= 0;
  }
  mTrigger.Et = 0;
}
//----------------------------------------------------    
void StBemcTrigger::makeTrigger()
{    
  
  zero();
  if(!mEvent) return;
  StEmcCollection *emc = mEvent->emcCollection();
  if(!emc) return;
  
  int adc12[kNTowers];
  int adc10[kNTowers];
  int adc08[kNTowers];
  for(int i=0;i<kNTowers;i++) adc12[i] = 0;
  
  StEmcDetector* detector=emc->detector(kBarrelEmcTowerId);
  if(detector) 
  {
    for(Int_t m=1;m<=120;m++)
    {
      StEmcModule* module = detector->module(m);
      if(module)
      {
        StSPtrVecEmcRawHit& rawHit=module->hits();
        for(UInt_t k=0;k<rawHit.size();k++) if(rawHit[k])
        {
          Int_t did;
          Int_t mod=rawHit[k]->module();
          Int_t e=rawHit[k]->eta(); 
          Int_t s=abs(rawHit[k]->sub());
          mGeo->getId(mod,e,s,did);
          if(mTrigger.TowerStatus[did-1]==1) 
            adc12[did-1]=rawHit[k]->adc()-mTrigger.TowerPedestal[did-1];
        }
      }
    }
  } else return;
  
  // convert to 10 and 8 bits
  for(int i=0;i<kNTowers;i++) 
  {
    adc10[i] = adc12[i]>>2;
    adc08[i] = adc10[i]>>2;
  }
  
  // making trigger patches and high towers
  
  TUnixTime unixTime(mEvent->time());
  Int_t dat=0,tim=0;
  unixTime.GetGTime(dat,tim);
  mDecoder = new StEmcDecoder(dat,tim);
  
  for(int i = 0;i<kNPatches;i++) if(mTrigger.PatchStatus[i]==1)
  {
    int crate = 0;
    int seq  = 0;    
    mDecoder->GetCrateAndSequenceFromTriggerPatch(i,crate,seq);
    int HT = 0;
    int PA = 0;
    int HTID = -1;
    int id;
    for(int j=seq;j<seq+16;j++)
    {
      int stat = mDecoder->GetTowerIdFromCrate(crate,j,id);
      if(stat==1)
      {
        if(adc10[id-1]>=HT) {HT = adc10[id-1]; HTID = id;}
        PA+=adc08[id-1];
      }
    }
    // at this point we have the high tower with 10 bits and
    // the patch with 12 bits
    
    // conversion to 6 bits ...
    // patch sum is easy. Just need to look in a LUT
    mTrigger.Patch[i] = mTrigger.PatchLUT[i][PA];
    
    // high tower is different. Need to shift bits and make an
    // or of the top bits
    int SHIFT = mTrigger.HTBits;
    HT = HT >> SHIFT;
    int HTL = HT & 0x1F;
    int HTH = HT >> 5;
    int B5  = 0;
    if(HTH>0) B5 = 1;
    mTrigger.HT[i] = HTL+(B5<<5);
    if(mPrint) cout <<"Patch number "<<i
                    <<" Tower id = "<<HTID
                    <<" adc12 = "<<adc12[HTID-1]<<" adc10 = "<<adc10[HTID-1]
                    <<" adc08 = "<<adc08[HTID-1] 
                    <<" HT10 = "<<HT<<" PA12 = "<<PA
                    <<" HT = "<<mTrigger.HT[i]<<" PA = "<<mTrigger.Patch[i]<<endl;
  }   
  
  // making Jet trigger and Et
  mTrigger.Et = 0;
  for(int i = 0;i<kNJet; i++)
  {
    int p0 = i*30;
    int p1 = (i+1)*30;
    mTrigger.Jet[i]= 0;
    for(int j = p0;j<p1;j++) mTrigger.Jet[i]+=mTrigger.Patch[j];
    mTrigger.Et+=mTrigger.Jet[i];
  }
    
  delete mDecoder;
  return;
}
