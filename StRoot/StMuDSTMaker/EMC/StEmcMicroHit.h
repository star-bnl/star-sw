/*!\class StEmcMicroHit
\author Alexandre A. P. Suaide

This is the EMC micro HIT object
*/
#ifndef StEmcMicroHit__h
#define StEmcMicroHit__h
 
#include "TObject.h"

class StEmcMicroHit: public TObject
{
  public:
                    StEmcMicroHit();
                    StEmcMicroHit(StEmcMicroHit*);
    virtual         ~StEmcMicroHit();
    
    Int_t           getModule()        { return (Int_t)mModule; }      ///< Return Module number
    Int_t           getEta()           { return (Int_t)mEta; }         ///< Return Eta division number
    Int_t           getSub()           { return (Int_t)mSub; }         ///< Return Sub division number
    Int_t           getAdc()           { return (Int_t)mAdc; }         ///< Return ADC value
    Float_t         getEnergy()        { return mEnergy; }             ///< Return Hit energy
    
    void            setModule(Int_t m)  { mModule = (UChar_t)m;}
    void            setEta(Int_t e)     { mEta = (UChar_t)e;}
    void            setSub(Int_t s)     { mSub = (UChar_t)s;}
    void            setAdc(Int_t a)     { mAdc = (Short_t)a;}
    void            setEnergy(Float_t e){ mEnergy = e;}    
    
  private:
    UChar_t         mModule;
    UChar_t         mEta;
    UChar_t         mSub;
    Short_t         mAdc;
    Float_t         mEnergy;
        
    ClassDef(StEmcMicroHit,1)
};
#endif  
