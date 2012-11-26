/*
 * Created by S. Gliske, May 2012
 *
 * Description: Container used in the EEmcAnalysisTree.  Note: this
 * class does not explicitly depend on the STAR framework, and so the
 * trees can be read outside of the STAR framework.  Note: data
 * members are most all public, to allow a lighter weight
 * implementation.
 *
 */

#ifndef EEmcHit_H_
#define EEmcHit_H_

#include <Rtypes.h>
#include <TObject.h>
#include <TArrayS.h>
#include <TArrayF.h>

class EEmcHit_t : public TObject{
 public:
   EEmcHit_t();
   virtual ~EEmcHit_t(){ /* */ };

   void Clear( const Option_t* );

   Int_t uClusIdx, vClusIdx;

   Float_t x, y;      // position on the EEMC
   Float_t eta, phi;  // x,y in other coord system

   Float_t eTow, ePost;   // Total energy in the towers and post, summed over all used towers
   Float_t  ePre1, ePre2; // Energy in each preshower layer, for just the central tower

   // the following is to store the indices and weights of the "used"
   // towers

   enum { kMaxNumTowers = 9 };

   Short_t centralTowerIdx;
   Short_t numUsedTowers;

   Short_t usedTowerIdx[ kMaxNumTowers ];
   Float_t usedTowerWeight[ kMaxNumTowers ];

 private:
   ClassDef( EEmcHit_t, 3 );
};

#endif

/*
 * $Id: EEmcHit.h,v 1.1 2012/11/26 19:04:30 sgliske Exp $
 * $Log: EEmcHit.h,v $
 * Revision 1.1  2012/11/26 19:04:30  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/EEmcTreeContainers to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
