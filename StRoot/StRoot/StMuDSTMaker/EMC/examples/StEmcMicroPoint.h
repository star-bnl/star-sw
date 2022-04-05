/*!\class StEmcMicroPoint
\author Alexandre A. P. Suaide

This is the EMC micro Point. <br><br>
EmcDet is defined as follow:
  - 1 = bemc
  - 2 = bprs
  - 3 = bsmde
  - 4 = bsmdp
*/
#ifndef StEmcMicroPoint__h
#define StEmcMicroPoint__h
 
#include "TObject.h"
#include "TObjArray.h"
#include "StEmcMicroCluster.h"

class StEmcMicroPoint: public TObject
{
  public:
                        StEmcMicroPoint();
                        StEmcMicroPoint(StEmcMicroPoint*);
    virtual             ~StEmcMicroPoint();
    
    Float_t             getEta()                { return mEta;}            ///< Return Eta of the point
    Float_t             getPhi()                { return mPhi;}            ///< Return Phi of the point
    Float_t             getDeltaEta()           { return mDeltaEta;}       ///< Return DeltaEta of the point
    Float_t             getDeltaPhi()           { return mDeltaPhi;}       ///< Return DeltaPhi of the point
    Float_t             getEnergy()             { return mEnergy;}         ///< Return Energy of the point
    Float_t             getChiSquare()          { return mChiSquare;}      ///< Return ChiSquare of the point
    
    StEmcMicroCluster*  getCluster(Int_t EmcDet,Int_t ClId) { return (StEmcMicroCluster*) mEmc[EmcDet-1]->At(ClId); } ///< Return one cluster of the point
    Int_t               getNClusters(Int_t EmcDet)          { return mEmc[EmcDet-1]->GetEntries();}                   ///< Return number of cluster of the point for one sub detector
    
    void                addCluster(Int_t EmcDet,StEmcMicroCluster* cl) { mEmc[EmcDet-1]->AddLast(cl);}
    
    void                setEta(Float_t e)         { mEta = e;}
    void                setPhi(Float_t p)         { mPhi = p;}
    void                setDeltaEta(Float_t s)    { mDeltaEta = s;}
    void                setDeltaPhi(Float_t s)    { mDeltaPhi = s;}
    void                setEnergy(Float_t e)      { mEnergy = e;}
    void                setChiSquare(Float_t e)   { mChiSquare = e;}
   
  private:
    Float_t         mEta;
    Float_t         mPhi;
    Float_t         mDeltaEta;
    Float_t         mDeltaPhi;
    Float_t         mEnergy;
    Float_t         mChiSquare;
    
    TObjArray*      mEmc[4];
        
  ClassDef(StEmcMicroPoint,1)
};
#endif
