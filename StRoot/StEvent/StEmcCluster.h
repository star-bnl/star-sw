/*!
 * \class StEmcCluster 
 * \author Akio Ogawa, Jan 2000
 */
/***************************************************************************
 *
 * $Id: StEmcCluster.h,v 2.7 2003/09/02 17:58:05 perev Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description: Base class for electromagnetic calorimeter cluster
 *
 ***************************************************************************
 *
 * $Log: StEmcCluster.h,v $
 * Revision 2.7  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.6  2002/02/22 22:56:47  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.5  2001/04/05 04:00:34  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2001/03/24 03:34:44  perev
 * clone() -> clone() const
 *
 * Revision 2.3  2000/07/31 22:12:23  akio
 * eliminate print() for L3(?)
 *
 * Revision 2.2  2000/03/23 22:24:06  akio
 * Initial version of Emc Point, and Inclusion of track pointers
 *
 * Revision 2.1  2000/02/23 17:55:43  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcCluster_hh
#define StEmcCluster_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StContainers.h"

class StEmcCluster : public StObject {
public:
    StEmcCluster();
    ~StEmcCluster();
    // StEmcCluster(const StEmcCluster&);            use default
    // StEmcCluster& operator=(const StEmcCluster&); use default
    
    float eta() const;
    float phi() const;
    float sigmaEta() const;
    float sigmaPhi() const;
    float energy() const;
    int   nHits() const;
    int   nNeighbors() const;
    int   nTracks() const;
    
    StPtrVecEmcRawHit&        hit();
    const StPtrVecEmcRawHit&  hit() const;
    StPtrVecEmcCluster&       neighbor();
    const StPtrVecEmcCluster& neighbor() const;
    StPtrVecTrack&            track();
    const StPtrVecTrack&      track() const;
    
    void setEta(float);
    void setPhi(float);
    void setSigmaEta(float);
    void setSigmaPhi(float);
    void setEnergy(float);
    
    void addHit(StEmcRawHit*);
    void addNeighbor(StEmcCluster*);
    void addTrack(StTrack*);
    
private:
    Float_t mEta;
    Float_t mPhi;
    Float_t mSigmaEta;
    Float_t mSigmaPhi;
    Float_t mEnergy;
    StPtrVecEmcRawHit  mHits;
    StPtrVecEmcCluster mNeighbors;
    StPtrVecTrack      mTracks;
    
    StObject* clone() const;
    ClassDef(StEmcCluster,1)
};

ostream& operator<<(ostream&, const StEmcCluster&); // Printing operator
#endif






