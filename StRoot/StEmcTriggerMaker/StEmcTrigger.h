//
// $Id: StEmcTrigger.h,v 1.2 2002/01/03 21:41:15 suaide Exp $
//
//    

#ifndef STAR_StEmcTrigger
#define STAR_StEmcTrigger
#include "TObject.h"
#include "TArrayF.h"
#include "TArrayI.h"
#include "TMatrix.h"
#include "tables/St_emcTrigger_Table.h"
#include "tables/St_emcPatchTrigger_Table.h"
#include "tables/St_emcJetTrigger_Table.h"

#include "StDaqLib/EMC/StEmcDecoder.h"

class StEmcCollection; 
class StEmcGeom;
class StEmcDecoder;

class StEmcTrigger : public TObject
{
  protected:    
    TArrayF m_EtTh;          // total Et
    TArrayF m_RatioTh;       // HT/Patch
    TArrayF m_HighTowerTh;   // 0.05 x 0.05
    TArrayF m_TrigPatchTh;   // 0.20 x 0.20
    TArrayF m_JetPatchTh;    // 0.80 x 1.00
       
    Float_t DigEnergyHT;
    Float_t DigEnergyPatch;
    StEmcDecoder *emcdec; //! 
    StEmcGeom    *geo; //!
    

  public: 
                 StEmcTrigger(Int_t date=20330101,Int_t time=120000);
    virtual     ~StEmcTrigger();

    void         SetThreshold(TArrayF,TArrayF,TArrayF,TArrayF,TArrayF);
    void         SetThreshold(TArrayF); 
    void         SetEmcCollection(StEmcCollection*); 
    void         SetDigEnergy(Float_t,Float_t);
    void         Browse(TBrowser *b);
    StEmcCollection* EMC();    
    
    St_emcTrigger*      EmcTrigger;
    St_emcPatchTrigger* PatchTrigger;
    St_emcJetTrigger*   JetTrigger;
    
  ClassDef(StEmcTrigger, 1) 
};

// Different threshold for different trigger

inline void StEmcTrigger::SetDigEnergy(Float_t x,Float_t y) {DigEnergyHT=x;DigEnergyPatch=y;}
     
#endif
