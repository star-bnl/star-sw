/*
 * Created by S. Gliske, May 2012
 *
 * Description: maker to fill the EEmcEnergy_t structure.
 *
 */

#ifndef StEEmcEnergyMaker_H_
#define StEEmcEnergyMaker_H_

#include <Rtypes.h>
#include "StMaker.h"
#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcEnergy.h"

class StEEmcA2EMaker;

class StEEmcEnergyMaker_t : public StMaker {
 public:
   /// constructor
   StEEmcEnergyMaker_t( const Char_t *myName, const Char_t *a2EMakerName );

   /// deconstructor
   virtual ~StEEmcEnergyMaker_t();

   /// Initialize
   Int_t Init();

   /// Build an event
   Int_t Make();

   /// Clear for next event
   void Clear(Option_t *opts="");

   // accessor/modifier for energy
   EEmcEnergy_t*       getEEmcEnergyPtr();
   const EEmcEnergy_t* getEEmcEnergyPtr() const;

   // modifiers for parameters
   void setTowerThres( Float_t thres );
   void setStripThres( Float_t thres );

   /// TODO: write copy constructor and equals operator.  Should not
   /// ever be used anyhow, but for completeness should eventually
   /// write them.

 protected:
   // data
   EEmcEnergy_t mEEmcEnergy;

   // for input
   std::string mA2EMkrName;
   StEEmcA2EMaker *mA2EMkr;

   // parameters
   Float_t mTowerThres, mStripThres;

 private:
   // for ROOT
   ClassDef( StEEmcEnergyMaker_t, 1 );

};

// inline functions

inline EEmcEnergy_t*       StEEmcEnergyMaker_t::getEEmcEnergyPtr()       { return &mEEmcEnergy; };
inline const EEmcEnergy_t* StEEmcEnergyMaker_t::getEEmcEnergyPtr() const { return &mEEmcEnergy; };

inline void StEEmcEnergyMaker_t::setTowerThres( Float_t thres ){ mTowerThres = thres; };
inline void StEEmcEnergyMaker_t::setStripThres( Float_t thres ){ mStripThres = thres; };

#endif

/*
 * $Id: StEEmcEnergyMaker.h,v 1.1 2012/11/26 19:06:10 sgliske Exp $
 * $Log: StEEmcEnergyMaker.h,v $
 * Revision 1.1  2012/11/26 19:06:10  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcTreeMaker to StRoot/StEEmcPool/StEEmcTreeMaker
 *
 * 
 */
