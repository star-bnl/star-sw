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
                    StMuEmcHit(StMuEmcHit*);
    virtual         ~StMuEmcHit();
    
    int             getId()           { return (int)mId; }      ///< Return Module number
    int             getAdc()          { return (int)mAdc; }         ///< Return ADC value
    int             getCalType()      { return (int)mCalType; }
    float           getEnergy()       { return mEnergy; }             ///< Return Hit energy
    
    void            setId(int id)     { mId = (short)id;}
    void            setAdc(int a)     { mAdc = (short)a;}
    void            setCalType(int a) { mCalType = (char)a;}
    void            setEnergy(float e){ mEnergy = e;}    
    
  private:
    short           mId;
    short           mAdc;
    char            mCalType;
    float           mEnergy;
        
    ClassDef(StMuEmcHit,1)
};
#endif  
