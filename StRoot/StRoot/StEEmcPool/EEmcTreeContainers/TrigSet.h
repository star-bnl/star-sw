/*
 * Created by S. Gliske, Aug 2012
 *
 * Description: Container to hold the set of triggers.
 *
 */

#ifndef TrigSet_H_
#define TrigSet_H_

#include <Rtypes.h>
#include <TObject.h>

#include <set>
#include <vector>

class TrigSet : public TObject {
 public:
   TrigSet();
   virtual ~TrigSet();
   void Clear( const Option_t* opt = "" );

   // modifier
   void insert( const std::vector< UInt_t >& trigVec );

   // accessors
   Bool_t isTrig( UInt_t trig ) const;
   Int_t numOfTrigs() const;
   void copyToVector( std::vector< UInt_t >& vec ) const;

   // needs to be called after being read from a TTree or TFile
   void resync() const;

 protected:
   Int_t nTrigs;                 // number of triggers
   UInt_t *mTrigArray;           //[nTrigs] array of triggers

   mutable std::set< UInt_t > mTrigSet;  //! do not save

 private:
   ClassDef( TrigSet, 1 );
};

// inline functions
inline Int_t TrigSet::numOfTrigs() const { return nTrigs; };
inline Bool_t TrigSet::isTrig( UInt_t trig ) const { return mTrigSet.count( trig ); };

#endif

/*
 * $Id: TrigSet.h,v 1.1 2012/11/26 19:04:31 sgliske Exp $
 * $Log: TrigSet.h,v $
 * Revision 1.1  2012/11/26 19:04:31  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/EEmcTreeContainers to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
