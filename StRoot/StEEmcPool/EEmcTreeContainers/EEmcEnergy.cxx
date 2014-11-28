/*
 * Created by S. Gliske, May 2012
 *
 * Description: see header.
 *
 */

#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcEnergy.h"

//
// TOWERS
//

EEmcElement_t::EEmcElement_t() : fail( 0 ), energy( 0 ) { /* */ };

ClassImp( EEmcElement_t );

void ETowEnergy_t::Clear(){
   for( EEmcElement_t *p=element; p != &element[720]; ++p )
      p->Clear();
};

ClassImp( ETowEnergy_t );



//
// STRIPS
//

ClassImp( ESmdLayer_t);
ClassImp( ESmdSector_t );
ClassImp( ESmdEnergy_t );

void ESmdLayer_t::Clear(){
   for( EEmcElement_t *p=strip; p != &strip[288]; ++p )
      p->Clear();
};

void ESmdSector_t::Clear(){
   layer[0].Clear();
   layer[1].Clear();
};

void ESmdEnergy_t::Clear(){
   for( ESmdSector_t *p=sec; p != &sec[12]; ++p )
      p->Clear();
};

//
// TOTAL EEMC 
//

ClassImp( EEmcEnergy_t );

void EEmcEnergy_t::Clear(const Option_t* ){
   eTow.Clear();
   ePre1.Clear();
   ePre2.Clear();
   ePost.Clear();
   eSmd.Clear();

   nTowers = nStrips = 0;
};

/*
 * $Id: EEmcEnergy.cxx,v 1.1 2012/11/26 19:04:30 sgliske Exp $
 * $Log: EEmcEnergy.cxx,v $
 * Revision 1.1  2012/11/26 19:04:30  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/EEmcTreeContainers to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
