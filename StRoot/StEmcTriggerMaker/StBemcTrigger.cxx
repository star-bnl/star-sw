//
// $Id: StBemcTrigger.cxx,v 1.1 2001/05/24 14:42:10 suaide Exp $
//
//    

#include "StBemcTrigger.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "StEmcUtil/StEmcGeom.h"
  
ClassImp(StBemcTrigger);

StEmcGeom* geo;

//-------------------------------------------------------------------
StBemcTrigger::StBemcTrigger():StEmcTrigger()
{   
  EmcTrigger=new St_emcTrigger("BemcTrigger",1);
  EmcTrigger->SetNRows(1);
  
  PatchTrigger=new St_emcPatchTrigger("BemcPatch",300);
  PatchTrigger->SetNRows(300);
  
  JetTrigger=new St_emcJetTrigger("BemcJet",14);
  JetTrigger->SetNRows(14);
      
  geo=new StEmcGeom("bemc");

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
  
  Float_t e8bits[4800];
  for(Int_t i=0;i<4800;i++) {e8bits[i]=0;}
    
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
        Int_t adc8bits=(Int_t)(hits[j]->adc()/16); // convert 12 bits to 8 bits adc
        Float_t scalefactor=16.*hits[j]->energy()/(Float_t)hits[j]->adc();
        e8bits[id-1]=(Float_t)adc8bits*scalefactor; //convert to energy
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
    Int_t mi,ei;    
    GetModuleEtaPatch(patch,&mi,&ei);
    
    for(Int_t m=mi;m<mi+2;m++)
      for(Int_t e=ei;e<ei+4;e++)
        for(Int_t s=1;s<3;s++)
        {
          Int_t id;
          geo->getId(m,e,s,id);        
          if(e8bits[id-1]>HT[patch-1]) 
          {
            HT[patch-1]=e8bits[id-1]; 
            HTId[patch-1]=id;
          }
          Patch[patch-1]+=e8bits[id-1];
        }
        
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

    Int_t jet=GetJetId(mi);
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
    patchRows[patch-1].FirstModule=mi;
    patchRows[patch-1].FirstEta=ei;
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
    jetRows[jet-1].FirstModule=GetModuleId(jet);
    jetRows[jet-1].JetEt6bits=Jet[jet-1];
  }
  
  triggerRows[0].NJetThresholds=sizeJet;
  for(Int_t i=0;i<sizeJet;i++) 
    triggerRows[0].JetThresholds[i]=m_JetPatchTh[i];

 
  return;
}
//----------------------------------------------------    
void StBemcTrigger::GetModuleEtaPatch(Int_t patch,Int_t* mi,Int_t* ei)
{
  Int_t module=2*((patch-1)/5)+1;
  Int_t eta=17-4*((patch-1)%5);
  
  *mi=module;
  *ei=eta;
}
//----------------------------------------------------    
Int_t StBemcTrigger::GetPatchId(Int_t m,Int_t e)
{
  Int_t id;
  geo->getId(m,e,1,id);
  Int_t emcrow=(id-1)/20;
  Int_t emccol=(id-1)%20;
  Int_t patchrow=emcrow/4;
  Int_t patchcol=emccol/4;
  Int_t patch=patchrow*5+patchcol+1;
  return patch; 
}
//----------------------------------------------------    
Float_t StBemcTrigger::GetEtaPatch(Int_t patch)
{
  Int_t mi,ei;
  GetModuleEtaPatch(patch,&mi,&ei);
  Float_t eta=0,x=0;
  for(Int_t m=mi;m<mi+2;m++)
    for(Int_t e=ei;e<ei+4;e++)
    {
      Float_t temp;
      geo->getEta(m,e,temp);
      eta+=temp;
      x++;
    }
  return eta/x;
}
//----------------------------------------------------    
Int_t StBemcTrigger::GetJetId(Int_t m)
{
  for(Int_t i=0;i<14;i++) 
  {
    Int_t mi=i*8+1;
    if(i>=7) mi=(i-7)*8+61;
    Int_t mf=mi+8;
    if(m>=mi && m<mf) return i+1;
  }
  return 0;
}
//----------------------------------------------------    
Int_t StBemcTrigger::GetModuleId(Int_t jet)
{
  Int_t i=jet-1;
  Int_t mi=i*8+1;
  if(i>=7) mi=(i-7)*8+61;
  return mi;
}
