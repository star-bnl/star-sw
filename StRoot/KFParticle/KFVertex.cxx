/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
 *
 * KFParticle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * KFParticle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


#include "KFVertex.h"

#ifndef KFParticleStandalone
ClassImp(KFVertex);
#endif

KFVertex::KFVertex( const KFPVertex &vertex ): fIsConstrained(0)
{
  /** Constructor from KFPVertex. **/

  vertex.GetXYZ( fP );
  vertex.GetCovarianceMatrix( fC );  
  fChi2 = vertex.GetChi2();  
  fNDF = 2*vertex.GetNContributors() - 3;
  fQ = 0;
  fAtProductionVertex = 0;
  fSFromDecay = 0;
}

void KFVertex::SetBeamConstraint( float x, float y, float z, 
                                  float errX, float errY, float errZ )
{
  /** Sets a soft beam constraint on the vertex position.
   ** \param[in] x, y, z - coordinates of the constraint
   ** \param[in] errX, errY, errZ - corresponding errors
   **/
  fP[0] = x;
  fP[1] = y;
  fP[2] = z;
  fC[0] = errX*errX;
  fC[1] = 0;
  fC[2] = errY*errY;
  fC[3] = 0;
  fC[4] = 0;
  fC[5] = errZ*errZ;
  fIsConstrained = 1;
}

void KFVertex::SetBeamConstraintOff()
{
  /** Switches off the constraint. Should be called before KFVertex::ConstructPrimaryVertex() **/
  fIsConstrained = 0;
}

void KFVertex::ConstructPrimaryVertex( const KFParticle *vDaughters[], 
                                       int nDaughters, Bool_t vtxFlag[],
                                       float ChiCut  )
{
  /** Reconstructs the primary vertex from a set of particles. Reconstruction is 
   ** parformed in three steps:\n
   ** 1) vertex seed is constructed from all particles; \n
   ** 2) if particle deviates more then on the "ChiCut" it is rejected; \n
   ** 3) the final vertex is constructed from the set of remaining particles.\n
   ** Rejected particles are marked with "false" in the output array of flags.
   ** \param[in] vDaughters - input array of pointers to the particles
   ** \param[in] nDaughters - number of particles in the input array
   ** \param[out] vtxFlag - array of flags showing if particle was used in the 
   ** vertex fit, if yes - set to "true"
   ** \param[in] ChiCut - cut on the chi2-deviation of the particle from the created
   ** seed, by default the cut is set to 3.5
   **/

  if( nDaughters<2 ) return;
  float constrP[3]={fP[0], fP[1], fP[2]};
  float constrC[6]={fC[0], fC[1], fC[2], fC[3], fC[4], fC[5]};

  Construct( vDaughters, nDaughters, 0, -1 );

//   SetVtxGuess( fVtxGuess[0], fVtxGuess[1], fVtxGuess[2] );

  for( int i=0; i<nDaughters; i++ ) vtxFlag[i] = 1;

  Int_t nRest = nDaughters;
//   while( nRest>2 )
//   {    
//     float worstChi = 0.;
//     Int_t worstDaughter = 0;
//     for( Int_t it=0; it<nDaughters; it++ ){
//       if( !vtxFlag[it] ) continue;        
//       const KFParticle &p = *(vDaughters[it]);
//       //KFVertex tmp = *this - p;
//       //float chi = p.GetDeviationFromVertex( tmp );      
//       float chi = p.GetDeviationFromVertex( *this );      
//       if( worstChi < chi ){
//         worstChi = chi;
//         worstDaughter = it;
//       }
//     }
//     if( worstChi < ChiCut ) break;
//       std::cout <<"worst 1 " <<  worstDaughter << " " << worstChi << std::endl;
//     vtxFlag[worstDaughter] = 0;    
//     //*this -= *(vDaughters[worstDaughter]);
//     nRest--;
//   } 

  for( Int_t it=0; it<nDaughters; it++ ){
    const KFParticle &p = *(vDaughters[it]);
    float chi = p.GetDeviationFromVertex( *this );      
    if( chi >= ChiCut ){
      vtxFlag[it] = 0;    
      nRest--;
    }
  }

  if( nRest>=2 ) {// final refit     
//     SetVtxGuess( fP[0], fP[1], fP[2] );
    if( fIsConstrained ){
      fP[0] = constrP[0];
      fP[1] = constrP[1];
      fP[2] = constrP[2];
      for( int i=0; i<6; i++ ) fC[i] = constrC[i];
    }
    int nDaughtersNew=0;
    const KFParticle **vDaughtersNew=new const KFParticle *[nDaughters];
    for( int i=0; i<nDaughters; i++ ){
      if( vtxFlag[i] )  vDaughtersNew[nDaughtersNew++] = vDaughters[i];
    }
    Construct( vDaughtersNew, nDaughtersNew, 0, -1 );
    if (vDaughtersNew) delete[] vDaughtersNew;
  }

  if( nRest<=2 && GetChi2() > ChiCut*ChiCut*GetNDF() ) {
    for( int i=0; i<nDaughters; i++ ) vtxFlag[i] = 0;
    fNDF = -3;
    fChi2 = 0;
  }
}
