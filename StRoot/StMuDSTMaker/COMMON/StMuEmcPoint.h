/*!\class StMuEmcPoint
\author Alexandre A. P. Suaide

This is the EMC micro Point. <br><br>
EmcDet is defined as follow:
  - 1 = bemc
  - 2 = bprs
  - 3 = bsmde
  - 4 = bsmdp
*/
#ifndef StMuEmcPoint__h
#define StMuEmcPoint__h
 
#include "TObject.h"
#include "StMuEmcCluster.h"

class StMuEmcPoint: public TObject
{
  public:
                      StMuEmcPoint();
                      StMuEmcPoint(StMuEmcPoint*);
    virtual           ~StMuEmcPoint();
    
    float             getEta()                { return mEta;}            ///< Return Eta of the point
    float             getPhi()                { return mPhi;}            ///< Return Phi of the point
    float             getRadius()             { return mRadius;}         ///< return radius of the point
    float             getDeltaEta()           { return mDeltaEta;}       ///< Return DeltaEta of the point
    float             getDeltaPhi()           { return mDeltaPhi;}       ///< Return DeltaPhi of the point
    float             getEnergy()             { return mEnergy;}         ///< Return Energy of the point
    float             getChiSquare()          { return mChiSquare;}      ///< Return ChiSquare of the point
    
    StMuEmcCluster*   getCluster(Int_t EmcDet){ return mEmc[EmcDet-1]; } ///< Return one cluster of the point
    
    void              setCluster(StMuEmcCluster* cl, Int_t EmcDet) { mEmc[EmcDet-1] = cl;}    
    void              setEta(float e)         { mEta = e;}
    void              setPhi(float p)         { mPhi = p;}
    void              setRadius(float r)      { mRadius = r;}
    void              setDeltaEta(float s)    { mDeltaEta = s;}
    void              setDeltaPhi(float s)    { mDeltaPhi = s;}
    void              setEnergy(float e)      { mEnergy = e;}
    void              setChiSquare(float e)   { mChiSquare = e;}
   
  private:
    float         mEta;
    float         mPhi;
    float         mRadius;
    float         mDeltaEta;
    float         mDeltaPhi;
    float         mEnergy;
    float         mChiSquare;    
    StMuEmcCluster* mEmc[8];
        
  ClassDef(StMuEmcPoint,2)
};
#endif
