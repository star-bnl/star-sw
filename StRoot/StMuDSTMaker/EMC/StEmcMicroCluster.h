/*!\class StEmcMicroCluster
\author Alexandre A. P. Suaide

This is the EMC micro cluster object.
*/
#ifndef StEmcMicroCluster__h
#define StEmcMicroCluster__h
 
#include "TObject.h"
#include "TObjArray.h"
#include "StEmcMicroHit.h"

class StEmcMicroCluster: public TObject
{
  public:
                    StEmcMicroCluster();
                    StEmcMicroCluster(StEmcMicroCluster*);
    virtual         ~StEmcMicroCluster();
    
    Float_t         getEta()                     { return mEta;}                             ///< Return Eta of the cluster
    Float_t         getPhi()                     { return mPhi;}                             ///< Return Phi of the cluster
    Float_t         getSigmaEta()                { return mSigmaEta;}                        ///< Return SigmaEta of the cluster
    Float_t         getSigmaPhi()                { return mSigmaPhi;}                        ///< Return SigmaPhi of the cluster
    Float_t         getEnergy()                  { return mEnergy;}
    
    Int_t           getNHits()                   { return mHits->GetEntries();}              ///< Return Number of hits of the cluster
    StEmcMicroHit*  getHit(Int_t hiId)        { return (StEmcMicroHit*)mHits->At(hiId);}  ///< Return one hit of the cluster
    
    void            addHit(StEmcMicroHit* hit){ mHits->AddLast(hit);}
    
    void            setEta(Float_t e)         { mEta = e;}
    void            setPhi(Float_t p)         { mPhi = p;}
    void            setSigmaEta(Float_t s)    { mSigmaEta = s;}
    void            setSigmaPhi(Float_t s)    { mSigmaPhi = s;}
    void            setEnergy(Float_t e)      { mEnergy = e;}
  
  private:
    Float_t         mEta;
    Float_t         mPhi;
    Float_t         mSigmaEta;
    Float_t         mSigmaPhi;
    Float_t         mEnergy;
    TObjArray*      mHits;    
    
  ClassDef(StEmcMicroCluster,1)
};
#endif
