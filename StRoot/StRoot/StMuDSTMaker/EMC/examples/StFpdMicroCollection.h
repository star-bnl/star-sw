/*!\class StFpdMicroCollection
\author Alexandre A. P. Suaide

This is the EMC micro Fpd structure.
*/
#ifndef StFpdMicroCollection__h
#define StFpdMicroCollection__h
 
#include "TObject.h"

class StFpdMicroCollection: public TObject
{
  public:
                      StFpdMicroCollection();
    virtual           ~StFpdMicroCollection();
    
    Int_t      getToken()            const { return mToken; }        ///< Return FPD token
    Int_t      getSumAdcNorth()      const { return mAdcNorth; }     ///< Return FPD Sum ADC North
    Int_t      getSumAdcSouth()      const { return mAdcSouth; }     ///< Return FPD Sum ADC South
    Int_t      getSumAdcTop()        const { return mAdcTop; }       ///< Return FPD Sum ADC Top
    Int_t      getSumAdcBottom()     const { return mAdcBottom; }    ///< Return FPD Sum ADC Bottom
    Int_t      getSumAdcPreShower1() const { return mAdcPS1; }       ///< Return FPD Sum ADC Pre Shower 1
    Int_t      getSumAdcPreShower2() const { return mAdcPS2; }       ///< Return FPD Sum ADC Pre Shower 2
    Int_t      getSumAdcSmdX()       const { return mAdcSmdX; }      ///< Return FPD Sum ADC Shower Max X
    Int_t      getSumAdcSmdY()       const { return mAdcSmdY; }      ///< Return FPD Sum ADC Shower Max Y
    
    void       setToken(Int_t a)             { mToken = a; }
    void       setSumAdcNorth(Int_t a)       { mAdcNorth = a; }
    void       setSumAdcSouth(Int_t a)       { mAdcSouth = a; }
    void       setSumAdcTop(Int_t a)         { mAdcTop = a; }
    void       setSumAdcBottom(Int_t a)      { mAdcBottom = a; }
    void       setSumAdcPreShower1(Int_t a)  { mAdcPS1 = a; }
    void       setSumAdcPreShower2(Int_t a)  { mAdcPS2 = a; }
    void       setSumAdcSmdX(Int_t a)        { mAdcSmdX = a; }
    void       setSumAdcSmdY(Int_t a)        { mAdcSmdY = a; }
    
  private:
    Int_t      mToken; 
    Int_t      mAdcNorth; 
    Int_t      mAdcSouth; 
    Int_t      mAdcTop; 
    Int_t      mAdcBottom; 
    Int_t      mAdcPS1; 
    Int_t      mAdcPS2; 
    Int_t      mAdcSmdX; 
    Int_t      mAdcSmdY; 
    
    ClassDef(StFpdMicroCollection,1)
};
#endif  
    
