//
// $Id: StEmcTrigger.cxx,v 1.2 2002/01/03 21:41:15 suaide Exp $
//
//    

#include "StEmcTrigger.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"    
 
#include "StDaqLib/EMC/StEmcDecoder.h"
 
ClassImp(StEmcTrigger);

StEmcCollection* emc;
  
//-------------------------------------------------------------------
StEmcTrigger::StEmcTrigger(Int_t date, Int_t time):TObject()
{ 
  emcdec = new StEmcDecoder(date,time);
  Float_t et[3]={5.0,10.0,20.0};
  Float_t ra[3]={0,0.4,0.8};
  Float_t ht[3]={1.0,4.0,7.0};
  Float_t tt[3]={1.0,4.0,7.0};
  Float_t jt[3]={1.0,4.0,7.0};
  TArrayF ET(3,et);
  TArrayF RA(3,ra);
  TArrayF HT(3,ht);
  TArrayF TT(3,tt);
  TArrayF JT(3,jt);
  SetThreshold(ET,RA,HT,TT,JT);
  SetDigEnergy(32,32); // parameters used in simulation
}
//-------------------------------------------------------------------
StEmcTrigger::~StEmcTrigger()
{ 
  delete emcdec;
}
//-------------------------------------------------------------------
void StEmcTrigger::SetEmcCollection(StEmcCollection* e) 
{
  emc=e;
}
//-------------------------------------------------------------------
void StEmcTrigger::SetThreshold(TArrayF et,TArrayF ra,
                                TArrayF ht,TArrayF tt,TArrayF jt)
{
  m_EtTh=et;
  m_RatioTh=ra;
  m_HighTowerTh=ht;
  m_TrigPatchTh=tt;
  m_JetPatchTh=jt;
}
//-------------------------------------------------------------------
void StEmcTrigger::SetThreshold(TArrayF ht)
{
  SetThreshold(ht,ht,ht,ht,ht);
}
//-------------------------------------------------------------------
StEmcCollection* StEmcTrigger::EMC()
{
  return emc;
}
//-------------------------------------------------------------------
void StEmcTrigger::Browse(TBrowser *b)
{
}
