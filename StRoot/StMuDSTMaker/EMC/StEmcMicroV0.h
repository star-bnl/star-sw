/*!\class StEmcMicroV0
\author Alexandre A. P. Suaide

This is the EMC micro HIT object
*/
#ifndef StEmcMicroV0__h
#define StEmcMicroV0__h
 
#include "TObject.h"
#include "StEmcMicroTrack.h"

class StEmcMicroV0: public TObject
{
  public:
                     StEmcMicroV0();
                     StEmcMicroV0(StEmcMicroV0*);
    virtual          ~StEmcMicroV0();
    
    StEmcMicroTrack* getDaughter(Int_t i)   const { if(i>=0 && i<2) return mTracks[i]; else return NULL; } ///< Return daughter
    Float_t          getVertexX()           const { return mX; } ///< Return vertex X
    Float_t          getVertexY()           const { return mY; } ///< Return vertex Y
    Float_t          getVertexZ()           const { return mZ; } ///< Return vertex Z
        
    void             setVertex(Float_t x,Float_t y,Float_t z) { mX=x; mY=y; mZ=z; }
    void             setDaughter(Int_t i,StEmcMicroTrack* t)  {if(i>=0 && i<2) mTracks[i] = t; }
    
  private:
    StEmcMicroTrack *mTracks[2];
    Float_t mX;
    Float_t mY;
    Float_t mZ;
    
    ClassDef(StEmcMicroV0,1)
};
#endif  
