/*
 * Created by S. Gliske, May 2012
 *
 * Description: Containers used in the EEmcEnergyTree.  Note: they do
 * not explicitly depend on the STAR framework, and so the trees can
 * be read outside of the STAR framework.  Note: data members are all
 * public, to allow a lighter weight implementation.
 *
 */

#ifndef EEmcEnergy_H_
#define EEmcEnergy_H_

#include <Rtypes.h>
#include <TObject.h>

//
// TOWERS
//

class EEmcElement_t {
 public:
   EEmcElement_t();
   virtual ~EEmcElement_t(){ /* */ };
   void Clear();

   Int_t fail;
   Float_t energy;

 private:
   ClassDef( EEmcElement_t, 1 );
};

inline void EEmcElement_t::Clear(){ fail = 0; energy = 0; };


class ETowEnergy_t {
 public:
   ETowEnergy_t(){ /* */ };
   virtual ~ETowEnergy_t(){ /* */ };
   void Clear();

   // methods to return elements.
   // WARNING: no bounds checking!
   EEmcElement_t& getByIdx( Int_t idx );
   EEmcElement_t& getByBin( Int_t sec, Int_t sub, Int_t etabin );
   EEmcElement_t& getByBin( Int_t phibin, Int_t etabin );
   const EEmcElement_t& getByIdx( Int_t idx ) const;
   const EEmcElement_t& getByBin( Int_t sec, Int_t sub, Int_t etabin ) const;
   const EEmcElement_t& getByBin( Int_t phibin, Int_t etabin ) const;

 protected:
   EEmcElement_t element[720];

 private:
   ClassDef( ETowEnergy_t, 1 );
};

inline EEmcElement_t& ETowEnergy_t::getByIdx( Int_t idx ){
   return element[ idx ];
};

inline const EEmcElement_t& ETowEnergy_t::getByIdx( Int_t idx ) const{
   return element[ idx ];
};

inline EEmcElement_t& ETowEnergy_t::getByBin( Int_t sec, Int_t sub, Int_t etabin ){
   return element[ 12*( 5*sec + sub ) + etabin ];
};

inline const EEmcElement_t& ETowEnergy_t::getByBin( Int_t sec, Int_t sub, Int_t etabin ) const {
   return element[ 12*( 5*sec + sub ) + etabin ];
};

inline EEmcElement_t& ETowEnergy_t::getByBin( Int_t phibin, Int_t etabin ){
   return element[ 12*phibin + etabin ];
};

inline const EEmcElement_t& ETowEnergy_t::getByBin( Int_t phibin, Int_t etabin ) const {
   return element[ 12*phibin + etabin ];
};


//
// STRIPS
//

class ESmdLayer_t {
 public:
   ESmdLayer_t(){ /* */ };
   virtual ~ESmdLayer_t(){ /* */ };
   void Clear();

   Int_t nStrips;
   EEmcElement_t strip[288];

 private:
   ClassDef( ESmdLayer_t, 1);
};

class ESmdSector_t {
 public:
   ESmdSector_t(){ /* */ };
   virtual ~ESmdSector_t(){ /* */ };
   void Clear();

   ESmdLayer_t layer[2];

 private:
   ClassDef( ESmdSector_t, 1 );
};

class ESmdEnergy_t {
 public:
   ESmdEnergy_t(){ /* */ };
   virtual ~ESmdEnergy_t(){ /* */ };
   void Clear();

   ESmdSector_t sec[12];

 private:
   ClassDef( ESmdEnergy_t, 1 );
};

//
// TOTAL EEMC 
//

class EEmcEnergy_t : public TObject {
 public:
   EEmcEnergy_t() : nTowers(0), nStrips(0) { /* */ };
   virtual ~EEmcEnergy_t(){ /* */ };
   void Clear( const Option_t* opt = "" );

   ETowEnergy_t eTow;
   ETowEnergy_t ePre1;
   ETowEnergy_t ePre2;
   ETowEnergy_t ePost;
   ESmdEnergy_t eSmd;

   UInt_t nTowers, nStrips;

 private:
   ClassDef( EEmcEnergy_t, 1 );
};

#endif

/*
 * $Id: EEmcEnergy.h,v 1.1 2012/11/26 19:04:30 sgliske Exp $
 * $Log: EEmcEnergy.h,v $
 * Revision 1.1  2012/11/26 19:04:30  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/EEmcTreeContainers to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
