//
// $Id: StBemcTrigger.cxx,v 1.9 2001/10/18 19:25:34 suaide Exp $
//
//    

#include "StBemcTrigger.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "StEmcUtil/StEmcGeom.h"
  
ClassImp(StBemcTrigger);
// these vectors are for tower decoding ////////////////////////////////
int sh =-0;
int Init_Crate[]={2260+sh,2420+sh,2580+sh,2740+sh,2900+sh,3060+sh,3220+sh,3380+sh,3540+sh,3700+sh,
                  3860+sh,4020+sh,4180+sh,4340+sh,4500+sh,
                  2180,2020,1860,1700,1540,1380,1220,1060,900,740,
                  580,420,260,100,2340};

int TDC_Crate[]= {18,17,16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,19,20,21,
                  22,23,24,25,26,27,28,29,30};

int Crate_TDC[30];
////////////////////////////////////////////////////////////////////////
int Getjose_towerWest(int start,int crate_seq)
{
  int card=crate_seq/32;
  int card_seq=31-(crate_seq%32);
  int channel_seq=card_seq/4;
  int channel=card_seq-(channel_seq*4)+1;
  int jose_tower=start+channel_seq*20+card*4+(5-channel);
  if(jose_tower>2400)jose_tower-=2400;
  return jose_tower;
}
int Getjose_towerEast(int start,int crate_seq)
{
  int card=crate_seq/32;
  int card_seq=31-(crate_seq%32);
  int channel_seq=card_seq/4;
  int channel=card_seq-(channel_seq*4)+1;
  int jose_tower=start+channel_seq*20+card*4+(5-channel);
  if(jose_tower<2400)jose_tower+=2400;
  return jose_tower;
}
int GetTowerIdFromCrate(int Crate,int crate_sequency, int& id)
{
  {
    int start=Init_Crate[Crate-1];
    if(Crate>15) id=Getjose_towerWest(start,crate_sequency);
    else id=Getjose_towerEast(start,crate_sequency);
    //cout <<"  Crate = "<<Crate<<"  seq = "<<crate_sequency<<"  id = "<<id<<endl;
    return 1;
  }
  return 0;
}
/////////////////////////////////////////////////////////////////////////


StEmcGeom* geo;

//-------------------------------------------------------------------
StBemcTrigger::StBemcTrigger():StEmcTrigger()
{   


  for (int i=0;i<30;i++) Crate_TDC[TDC_Crate[i]-1]=i;



  EmcTrigger=new St_emcTrigger("BemcTrigger",1);
  EmcTrigger->SetNRows(1);
  
  PatchTrigger=new St_emcPatchTrigger("BemcPatch",300);
  PatchTrigger->SetNRows(300);
  
  JetTrigger=new St_emcJetTrigger("BemcJet",14);
  JetTrigger->SetNRows(14);
      
  //geo=new StEmcGeom("bemc");
  geo=StEmcGeom::getEmcGeom("bemc");
}
//----------------------------------------------------    
StBemcTrigger::~StBemcTrigger()
{ 
  delete geo;
  delete EmcTrigger;
  delete PatchTrigger;
  delete JetTrigger;
}
//----------------------------------------------------    
void StBemcTrigger::MakeTrigger()
{ 
    
  StDetectorId id=kBarrelEmcTowerId;
  StEmcDetector* det=EMC()->detector(id);
  if(!det) return;
  if(det->numberOfHits()==0) return;
  
  Float_t e8bits[4800],adc8bits[4800];
  for(Int_t i=0;i<4800;i++) {e8bits[i]=0; adc8bits[i]=0;}
    
  for(Int_t i=1;i<121;i++)
  {
    StEmcModule* module=det->module(i);
    if(module)
    { 
      StSPtrVecEmcRawHit& hits=module->hits();
      for(Int_t j=0;j<(Int_t)hits.size();j++)
      {
        UInt_t module=hits[j]->module();
        UInt_t eta=hits[j]->eta();
        UInt_t sub=abs(hits[j]->sub());
        Int_t id;
        geo->getId(module,eta,sub,id);
        Int_t adc8=(Int_t)(hits[j]->adc()/16); // convert 12 bits to 8 bits adc
        Float_t scalefactor=16.*hits[j]->energy()/(Float_t)hits[j]->adc();
        e8bits[id-1]=(Float_t)adc8*scalefactor; //convert to energy
        adc8bits[id-1]=(Float_t)adc8;
        //cout <<hits[j]->adc()<<"   "<<e8bits[id-1]<<"   "<<scalefactor<<endl;
      }
    }
  }
  
// making high tower trigger, 0.2x0.2 patch trigger and jet trigger
  
  Float_t Patch[300],HT[300],Jet[14];
  Int_t HTId[300];
  
  for(Int_t i=0;i<300;i++) {Patch[i]=0;HT[i]=0;HTId[i]=0;}
  for(Int_t i=0;i<14;i++)  {Jet[i]=0;}
  
  Float_t totalEt=0;
  
  Int_t sizeHT= m_HighTowerTh.GetSize();
  Int_t sizePatch=m_TrigPatchTh.GetSize();
  Int_t sizeJet=m_JetPatchTh.GetSize();
  Int_t sizeEt=m_EtTh.GetSize();
  Int_t sizeRa=m_RatioTh.GetSize();
    
  emcTrigger_st* triggerRows=EmcTrigger->GetTable();
  emcPatchTrigger_st* patchRows=PatchTrigger->GetTable();
  emcJetTrigger_st* jetRows=JetTrigger->GetTable();

/////////////////////////////////////////////////////////////////////
// filling patch information and generatin et and jet trigger

  for(Int_t patch=1;patch<301;patch++)
  {   
    Float_t etah=GetEtaPatch(patch);        
    Float_t theta = atan(exp(-etah))*2.;
    Int_t crate,subpatch;    
    GetCrateEtaPatch(patch,&crate,&subpatch);
    
    Int_t ti=0;
    patchRows[patch-1].PatchAdcSum6bits=0;
    
    Float_t eta=0,phi=0,eta1=0,phi1=0,patchtemp=0;
    Int_t HTTemp=0;
    for(Int_t k=0;k<16;k++)
    {
       Int_t positionInCrate=k+subpatch;
       Int_t id;
       GetTowerIdFromCrate(crate,positionInCrate,id); 
       //cout <<"  crate = "<<crate<<"  position = "<<positionInCrate<<"  id = "<<id<<endl;       
       patchRows[patch-1].TowerId[ti]=id;
       ti++;
       //if(e8bits[id-1]>=HT[patch-1])   
       if((Int_t)adc8bits[id-1]>=HTTemp) 
       {
         HT[patch-1]=e8bits[id-1]; 
         HTId[patch-1]=id;
         HTTemp=(Int_t)adc8bits[id-1];
       }
       Patch[patch-1]+=e8bits[id-1];
       patchtemp+=(Int_t)adc8bits[id-1];
       geo->getEtaPhi(id,eta1,phi1);
       eta+=eta1;
       phi+=phi1;
    }
    Int_t patchtemp1=(Int_t)patchtemp/64;  // linear transformation. should replace for lookup table
    patchRows[patch-1].PatchAdcSum6bits+=patchtemp1;
    patchRows[patch-1].Eta=eta/16;
    patchRows[patch-1].Phi=phi/16;
    
    patchRows[patch-1].HighTowerAdc6bits=(Int_t)(adc8bits[HTId[patch-1]-1]/4);
    
    HT[patch-1]*=sin(theta);   // et
    Patch[patch-1]*=sin(theta);// et
    
//    cout <<" before conv "<<HT[patch-1]<<"  "<<Patch[patch-1]<<endl;
    
// convert 8 bits HT to 6 bits HT
    if(HT[patch-1]>DigEnergyHT) HT[patch-1]=DigEnergyHT;
    Int_t adc6bits=(Int_t)(HT[patch-1]*64./DigEnergyHT);
    HT[patch-1]=(Float_t)adc6bits*DigEnergyHT/64.;
  
// convert 12 bits patch to 6 bits energy 
    if(Patch[patch-1]>DigEnergyPatch) Patch[patch-1]=DigEnergyPatch;
    adc6bits=(Int_t)(Patch[patch-1]*64./DigEnergyPatch);
    Patch[patch-1]=(Float_t)adc6bits*DigEnergyPatch/64.;  

//    cout <<" after  conv "<<HT[patch-1]<<"  "<<Patch[patch-1]<<endl<<endl;

    Int_t jet=GetJetId(crate);
    if(jet>0) Jet[jet-1]+=Patch[patch-1];
    totalEt+=Patch[patch-1];
    
// filling HighTower trigger
    for(Int_t i=0;i<sizeHT;i++) 
      if(HT[patch-1]>= m_HighTowerTh[i])
      {
        patchRows[patch-1].HighTowerBits[i]=1;
        triggerRows[0].HighTowerBits[i]=1;
      }
      else patchRows[patch-1].HighTowerBits[i]=0;
      
// filling Patch trigger
    for(Int_t i=0;i<sizePatch;i++) 
      if(Patch[patch-1]>= m_TrigPatchTh[i])
      {
        patchRows[patch-1].PatchBits[i]=1;
        triggerRows[0].PatchBits[i]=1;
      }
      else patchRows[patch-1].PatchBits[i]=0;

// filling Ratio trigger
    Float_t ratio=0;
    if(Patch[patch-1]>0) ratio=HT[patch-1]/Patch[patch-1];    

    for(Int_t i=0;i<sizeRa;i++) 
      if(ratio>=m_RatioTh[i])
      {
        patchRows[patch-1].RatioBits[i]=1;
        triggerRows[0].RatioBits[i]=1;
      }
      else patchRows[patch-1].RatioBits[i]=0;
            
// filling Patch information   
    patchRows[patch-1].PatchNumber=patch;
    patchRows[patch-1].PatchEt6bits=Patch[patch-1];
    patchRows[patch-1].HighTowerEt6bits=HT[patch-1];
    patchRows[patch-1].HighTowerId=HTId[patch-1];
    patchRows[patch-1].Ratio=ratio;
      
  }
  
// filling Patch trigger
  triggerRows[0].NPatchThresholds=sizePatch;
  for(Int_t i=0;i<sizePatch;i++) 
    triggerRows[0].PatchThresholds[i]=m_TrigPatchTh[i];

// filling HighTower trigger
  triggerRows[0].NHighTowerThresholds=sizeHT;
  for(Int_t i=0;i<sizeHT;i++) 
    triggerRows[0].HighTowerThresholds[i]=m_HighTowerTh[i];

// filling Ratio trigger
  triggerRows[0].NRatioThresholds=sizeRa;
  for(Int_t i=0;i<sizeRa;i++) 
    triggerRows[0].RatioThresholds[i]=m_RatioTh[i];

/////////////////////////////////////////////////////////////////////
// filling Et trigger
  triggerRows[0].NEtThresholds=sizeEt;
  triggerRows[0].Et=totalEt;
  for(Int_t i=0;i<sizeEt;i++)
  {
    triggerRows[0].EtThresholds[i]=m_EtTh[i];
    if(totalEt>=m_EtTh[i]) triggerRows[0].EtBits[i]=1;
    else triggerRows[0].EtBits[i]=0;
  }
  
/////////////////////////////////////////////////////////////////////
// filling Jet trigger  
  for(Int_t jet=1;jet<15;jet++)
  {
    TArrayI Jetbits(sizeJet);
    for(Int_t i=0;i<sizeJet;i++) 
      if(Jet[jet-1]>=m_JetPatchTh[i])
      {
        jetRows[jet-1].JetBits[i]=1;
        triggerRows[0].JetBits[i]=1;
      }
      else jetRows[jet-1].JetBits[i]=0;
    
    jetRows[jet-1].JetPatchNumber=jet;
    jetRows[jet-1].JetEt6bits=Jet[jet-1];
  }
  
  triggerRows[0].NJetThresholds=sizeJet;
  for(Int_t i=0;i<sizeJet;i++) 
    triggerRows[0].JetThresholds[i]=m_JetPatchTh[i];

 
  return;
}
//----------------------------------------------------    
void StBemcTrigger::GetCrateEtaPatch(Int_t patch,Int_t* mi,Int_t* ei)
{

  Int_t crate=(patch-1)/10+1; // crate number
  Int_t subpatch=(patch-1)%10*16;
  //cout <<"patch = "<<patch<<"  crate = "<<crate <<"  subpatch = "<<subpatch<<endl;
    
  *mi=crate;
  *ei=subpatch;
}
//----------------------------------------------------    
Float_t StBemcTrigger::GetEtaPatch(Int_t patch)
{
  Int_t crate,subpatch;
  GetCrateEtaPatch(patch,&crate,&subpatch);
  Float_t eta=0,x=0;
  for(Int_t k=0;k<16;k++)
  {
     Int_t positionInCrate=k+subpatch;
     Int_t id;
     GetTowerIdFromCrate(crate,positionInCrate,id);        
     Int_t m,e,s;
     geo->getBin(id,m,e,s);
     Float_t temp;
     geo->getEta(m,e,temp);
     eta+=temp;
     x++;
  }
  return eta/x;
}
//----------------------------------------------------    
Int_t StBemcTrigger::GetJetId(Int_t crate)
{
  if(crate!=15 && crate!=30) return (crate-1)/2+1;
  return 0;
}
