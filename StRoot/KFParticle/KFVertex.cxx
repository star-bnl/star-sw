//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  S.Gorbunov, I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________


#include "KFVertex.h"
#include "iostream"

#ifndef KFParticleStandalone
ClassImp(KFVertex);
#endif

KFVertex::KFVertex( const KFPVertex &vertex ): fIsConstrained(0)
{
  // Constructor from ALICE VVertex

  vertex.GetXYZ( fP );
  vertex.GetCovarianceMatrix( fC );  
  fChi2 = vertex.GetChi2();  
  fNDF = 2*vertex.GetNContributors() - 3;
  fQ = 0;
  fAtProductionVertex = 0;
  fIsLinearized = 0;
  fSFromDecay = 0;
}

/*
void     KFVertex::Print(Option_t* ) const
{  
  cout<<"KFVertex position:    "<<GetX()<<" "<<GetY()<<" "<<GetZ()<<endl;
  cout<<"KFVertex cov. matrix: "<<GetCovariance(0)<<endl;
  cout<<"                         "<<GetCovariance(1)<<" "<<GetCovariance(2)<<endl;
  cout<<"                         "<<GetCovariance(3)<<" "<<GetCovariance(4)<<" "<<GetCovariance(5)<<endl;
}
  */

void KFVertex::SetBeamConstraint( float x, float y, float z, 
				     float errX, float errY, float errZ )
{
  // Set beam constraint to the vertex
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
  fIsConstrained = 0;
}

void KFVertex::ConstructPrimaryVertex( const KFParticle *vDaughters[], 
                                       int nDaughters, Bool_t vtxFlag[],
                                       float ChiCut  )
{
  //* Primary vertex finder with simple rejection of outliers

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
