/***************************************************************************
 *
 * $Id: StMuFgtStripAssociation.h,v 1.1 2012/11/15 22:27:24 sangalin Exp $
 * Author: S. Gliske, Jul 2012
 *
 ***************************************************************************
 *
 * Description: data used for keeping track of which strips are
 * associated with which clusters, as well as the membership weight of
 * each strip.  The MuDst contains a TClonesArray of this class,
 * ordered such that elements with the same clusIdx are contiguous.
 * Each cluster then keeps track of the first position in this array
 * associated with itself, as well as the number of elements in this
 * array.  Note: this class strictly inlined (no .cxx file).
 *
 ***************************************************************************
 *
 * $Log: StMuFgtStripAssociation.h,v $
 * Revision 1.1  2012/11/15 22:27:24  sangalin
 * Copied over from StFgtDevel.
 *
 * Revision 1.1  2012/07/20 16:11:24  sgliske
 * Added StFgtStripAssociation, and removed all dynamically
 * allocated memory from StMuFgt* containers.
 * Also removed StMuFgtInfo
 *
 *
 **************************************************************************/

#ifndef _ST_MU_FGT_STRIP_ASSOCIATION_H_
#define _ST_MU_FGT_STRIP_ASSOCIATION_H_

#include <TObject.h>

class StFgtStrip;

class StMuFgtStripAssociation : public TObject {
 public:
   // constructors
   StMuFgtStripAssociation( Int_t clusIdx = -1, Int_t stripIdx = -1, Float_t weight = 0 );

   // defaults
   // StMuFgtStripAssociation(const StMuFgtStripAssociation&);  use default
   // StMuFgtStripAssociation& operator=(const StMuFgtStripAssociation&); use default
   // ~StMuFgtStripAssociation(); use default

   // accessors
   Int_t   getClusIdx() const;
   Int_t   getStripIdx() const;
   Float_t getWeight() const;

   // modifiers
   void setClusIdx  ( Int_t clusIdx );
   void setStripIdx ( Int_t stripIdx );
   void setWeight   ( Float_t weight );
    
 protected:
   // data members
   Int_t   mClusIdx;   // index of the cluster in the muFgtClusters TClonesArray
   Int_t   mStripIdx;  // index of the strip in the muFgtStrips TClonesArray
   Float_t mWeight;    // membership weight of the strip in the given cluster.

 private:   
   ClassDef(StMuFgtStripAssociation,1);
}; 

// inline functions

// constructor
inline StMuFgtStripAssociation::StMuFgtStripAssociation( Int_t clusIdx, Int_t stripIdx, Float_t weight ) :
   mClusIdx( clusIdx ), mStripIdx( stripIdx ), mWeight( weight ){ /* */ };

// accessors
inline Int_t   StMuFgtStripAssociation::getClusIdx()  const { return mClusIdx; };
inline Int_t   StMuFgtStripAssociation::getStripIdx() const { return mStripIdx; };
inline Float_t StMuFgtStripAssociation::getWeight()   const { return mWeight; };

// modifiers
inline void    StMuFgtStripAssociation::setClusIdx  ( Int_t clusIdx  ){ mClusIdx = clusIdx; };
inline void    StMuFgtStripAssociation::setStripIdx ( Int_t stripIdx ){ mStripIdx = stripIdx; };
inline void    StMuFgtStripAssociation::setWeight   ( Float_t Weight ){ mWeight = Weight; };

#endif

