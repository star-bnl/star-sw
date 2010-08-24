/*!\class StMuEmcCluster
\author Alexandre A. P. Suaide

This is the EMC micro cluster object.
*/
#ifndef StMuEmcCluster__h
#define StMuEmcCluster__h
 
#include "TObject.h"
#include "TArrayS.h"

class StMuEmcCluster: public TObject
{
  public:
                    StMuEmcCluster();
                    StMuEmcCluster(const StMuEmcCluster*);
    virtual         ~StMuEmcCluster();
    virtual         void Clear(Option_t* opt="");
   
    float           getEta() const               { return mEta;}                             ///< Return Eta of the cluster
    float           getPhi() const               { return mPhi;}                             ///< Return Phi of the cluster
    float           getSigmaEta() const          { return mSigmaEta;}                        ///< Return SigmaEta of the cluster
    float           getSigmaPhi() const          { return mSigmaPhi;}                        ///< Return SigmaPhi of the cluster
    float           getEnergy() const            { return mEnergy;}
    
    int             getNHits() const             { return mNHits;}              ///< Return Number of hits of the cluster
    int             getHitId(int hitNumber) const { return mHits[hitNumber];}    ///< Return one hit of the cluster
        
    void            setEta(float e)              { mEta = e;}
    void            setPhi(float p)              { mPhi = p;}
    void            setSigmaEta(float s)         { mSigmaEta = s;}
    void            setSigmaPhi(float s)         { mSigmaPhi = s;}
    void            setEnergy(float e)           { mEnergy = e;}
    void            setNHits(int h)              { mNHits = (short)h; mHits.Set(h); mHits.Reset();}
    void            setHitId(int h,int id)       { mHits[h] = (short)id;}
  
  protected:
    float           mEta;
    float           mPhi;
    float           mSigmaEta;
    float           mSigmaPhi;
    float           mEnergy;
    short           mNHits;
    TArrayS         mHits;    
    
  ClassDef(StMuEmcCluster,1)
};
#endif
