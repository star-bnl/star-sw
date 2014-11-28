/*!\class StMuEmcHit
\author Alexandre A. P. Suaide

This is the EMC micro HIT object
*/
#ifndef StMuEmcHit__h
#define StMuEmcHit__h
 
#include "TObject.h"

class StMuEmcHit: public TObject
{
  public:
                    StMuEmcHit();
                    StMuEmcHit(const StMuEmcHit&);
    virtual         ~StMuEmcHit();
    
    int             getId() const      { return (int)mId; }      ///< Return Module number
    int             getAdc() const     { return (int)mAdc; }         ///< Return ADC value
    int             getCalType() const { return (int)mCalType; }
    float           getEnergy() const  { return mEnergy; }             ///< Return Hit energy
    
    void            setId(int id)     { mId = (short)id;}
    void            setAdc(int a)     { mAdc = (short)a;}
    void            setCalType(int a) { mCalType = (char)a;}
    void            setEnergy(float e){ mEnergy = e;}    
    
  protected:
    float           mEnergy;
    short           mId;
    short           mAdc;
    char            mCalType;
        
    ClassDef(StMuEmcHit,2)
};
#endif  
