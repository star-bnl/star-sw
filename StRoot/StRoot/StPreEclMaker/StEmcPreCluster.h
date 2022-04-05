/*!\class StEmcPreCluster
\author Alexandre A. P. Suaide
 
This is the definition of a pre cluster object for the 
BEMC detector. It has more functionality than the regular
StEmcCluster (StEvent) such as splitting, merging, etc which
is very usefull for cluster finding algorithms.
 
*/
#include "TObject.h"
#include "TList.h"
#include <math.h>
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/StEmcMath.h"
#include "StEmcRawMaker/defines.h"

#ifndef StEmcPreCluster_HH
#define StEmcPreCluster_HH

class StEmcRawHit;
class StEmcCluster;
class StEmcPreCluster: public TObject
{
private:
    StEmcGeom*  mGeom;
    TList       mHits;

    Int_t       mDetector;
    Int_t       mMatchingId;
    Float_t     mEta;
    Float_t     mPhi;
    Float_t     mSigmaEta;
    Float_t     mSigmaPhi;
    Float_t     mEnergy;

protected:
public:
    StEmcPreCluster(Int_t);
    StEmcPreCluster(StEmcPreCluster&);
    StEmcPreCluster(StEmcCluster*);
    virtual     ~StEmcPreCluster();

    Float_t     eta() const; ///< returns the eta position of the cluster
    Float_t     phi() const; ///< returns the phi position of the cluster
    Float_t     sigmaEta() const; ///< returns the width in eta
    Float_t     sigmaPhi() const; ///< returns the width in phi
    Float_t     energy() const; ///< returns the energy of the cluster
    Int_t       nHits() const; ///< returns the number of hits in the cluster
    Int_t       detector() const; ///< returns the detector number
    Int_t       matchingId() const; ///< returns the matching id with other detectors. 0 means no matching

    void        addHit(StEmcRawHit*); ///< add a hit to the cluster
    void        removeHit(StEmcRawHit*); ///< removes a hit from the cluster
    void        removeHit(Int_t); ///< removes a ht from the cluster
    StEmcRawHit* getHit(Int_t i)
    {
        return (StEmcRawHit*)mHits.At(i);
    } ///< gets a pointer to a hit in the cluster

    void        addCluster(StEmcPreCluster*); ///< add another cluster to this one. Does not delete the added cluster
    void        addCluster(StEmcCluster*);///< add another cluster to this one. Does not delete the added cluster
    void        subtractCluster(StEmcPreCluster*);///< subtract another cluster to this one.
    void        subtractCluster(StEmcCluster*);///< subtract another cluster to this one.
    StEmcPreCluster*   splitInEta(Float_t); ///< split the cluster in the eta coordinate. Returns a pointer of the splitted cluster. The spllited cluster is not added to any collection
    StEmcPreCluster*   splitInPhi(Float_t); ///< split the cluster in the phi coordinate. Returns a pointer of the splitted cluster. The spllited cluster is not added to any collection

    void        setMatchingId(Int_t a)
    {
        mMatchingId = a;
    } ///< sets the matching id

    void        update(); ///< updates cluster information. Calculates eta,phi, ernergy, etc  from the hits added to the cluster
    StEmcCluster* makeStCluster(); ///< creates an StEmcCluster from the information  in this pre cluster

    ClassDef(StEmcPreCluster,1)
};
inline   Float_t    StEmcPreCluster::eta() const
{
    return mEta;
}
inline   Float_t    StEmcPreCluster::phi() const
{
    return mPhi;
}
inline   Float_t    StEmcPreCluster::sigmaEta() const
{
    return mSigmaEta;
}
inline   Float_t    StEmcPreCluster::sigmaPhi() const
{
    return mSigmaPhi;
}
inline   Float_t    StEmcPreCluster::energy() const
{
    return mEnergy;
}
inline   Int_t      StEmcPreCluster::nHits() const
{
    return mHits.GetSize();
}
inline   Int_t      StEmcPreCluster::detector() const
{
    return mDetector;
}
inline   Int_t      StEmcPreCluster::matchingId() const
{
    return mMatchingId;
}
#endif
