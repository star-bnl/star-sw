/***************************************************************************
 *
 * $Id: StEmcCluster.h,v 2.1 2000/02/23 17:55:43 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description: Base class for electromagnetic calorimeter cluster
 *
 ***************************************************************************
 *
 * $Log: StEmcCluster.h,v $
 * Revision 2.1  2000/02/23 17:55:43  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcCluster_hh
#define StEmcCluster_hh

#include <iostream.h>
#include "StObject.h"
#include "StContainers.h"

class StEmcCluster : public StObject {
public: 
    StEmcCluster();
    ~StEmcCluster();
    // StEmcCluster(const StEmcCluster&);            use default
    // StEmcCluster& operator=(const StEmcCluster&); use default
    
    Float_t eta() const;
    Float_t phi() const;
    Float_t sigmaEta() const;
    Float_t sigmaPhi() const;
    Float_t energy() const;
    Int_t   nHits() const; 
    Int_t   nNeighbors() const;
    
    StPtrVecEmcRawHit& hit();
    const StPtrVecEmcRawHit& hit() const;
    StPtrVecEmcCluster& neighbor();
    const StPtrVecEmcCluster& neighbor() const;
    
    void setEta(Float_t);
    void setPhi(Float_t);
    void setSigmaEta(Float_t);
    void setSigmaPhi(Float_t);
    void setEnergy(Float_t);
    
    void addHit(StEmcRawHit*);
    void addNeighbor(StEmcCluster*);
    
    void print(ostream& os = cout) const;
    
private:
    Float_t mEta;
    Float_t mPhi;
    Float_t mSigmaEta;
    Float_t mSigmaPhi;
    Float_t mEnergy;
    StPtrVecEmcRawHit  mHits;
    StPtrVecEmcCluster mNeighbors;
    
    StObject* clone();
    ClassDef(StEmcCluster,1)
};

ostream& operator<<(ostream&, const StEmcCluster&); // Printing operator
#endif






