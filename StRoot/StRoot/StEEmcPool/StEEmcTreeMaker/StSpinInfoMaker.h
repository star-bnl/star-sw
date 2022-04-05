/*!
 *
 * \class StSpinInfoMaker_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Make (fills) the StSpinInfo_t class, which can then be saved in a
 * branch of TTree.  The StSpinInfo_t class stores the basic spin
 * information needed for spin-dependent analyses.
 *
 * Based on the 'StGammaMaker/StGammaSpinMaker' class of Jason Webb.
 *
 */

#ifndef _ST_SPIN_INFO_MAKER_H_
#define _ST_SPIN_INFO_MAKER_H_

#include "StRoot/StEEmcPool/./EEmcTreeContainers/StSpinInfo.h"

/// Include StRoot headers
#include "StMaker.h"

class StSpinInfoMaker_t : public StMaker {
 public:
   /// Construtor
   StSpinInfoMaker_t( const Char_t *name ) : StMaker( name ) { /* */ };

   /// Deconstructor
   virtual ~StSpinInfoMaker_t(){ /* */ };

   /// Initialize
   Int_t Init();

   /// Fill the spin info class
   Int_t Make();

   /// Clear for next event
   void  Clear(Option_t *opts="");

   /// Accessors
   StSpinInfo_t* getSpinInfoPtr(){ return &mSpinInfo; };

 protected:
   StSpinInfo_t mSpinInfo;

 private:
   ClassDef( StSpinInfoMaker_t, 1 );
};

#endif

/*
 * $Id: StSpinInfoMaker.h,v 1.1 2012/11/26 19:06:11 sgliske Exp $
 * $Log: StSpinInfoMaker.h,v $
 * Revision 1.1  2012/11/26 19:06:11  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcTreeMaker to StRoot/StEEmcPool/StEEmcTreeMaker
 *
 * 
 */
