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


#ifndef KFVERTEX_H
#define KFVERTEX_H

#include "KFParticle.h"
#include "KFPVertex.h"

class KFVertex : public KFParticle
{
  
 public:

  //*
  //*  INITIALIZATION
  //*

  //* Constructor (empty)

  KFVertex():KFParticle(),fIsConstrained(0){ } 
  KFVertex( const KFParticle &particle ): KFParticle(particle), fIsConstrained(0) {}
  //* Destructor (empty)

  ~KFVertex(){}

  //* Initialisation from VVertex 

  KFVertex( const KFPVertex &vertex );


  //*
  //*  ACCESSORS
  //*

  //* Number of tracks composing the vertex

  Int_t GetNContributors() const { return fIsConstrained ?fNDF/2:(fNDF+3)/2; }

  //* 
  //* CONSTRUCTION OF THE VERTEX BY ITS DAUGHTERS 
  //* USING THE KALMAN FILTER METHOD
  //*


  //* Simple way to construct vertices ex. D0 = Pion + Kaon;   

  void operator +=( const KFParticle &Daughter );  

  //* Subtract particle from vertex

  KFVertex operator -( const KFParticle &Daughter ) const;

  void operator -=( const KFParticle &Daughter );  

  //* Set beam constraint to the primary vertex

  void SetBeamConstraint( float X, float Y, float Z, 
			  float ErrX, float ErrY, float ErrZ );

  //* Set beam constraint off

  void SetBeamConstraintOff();

  //* Construct vertex with selection of tracks (primary vertex)

  void ConstructPrimaryVertex( const KFParticle *vDaughters[], int nDaughters,
			       Bool_t vtxFlag[], float ChiCut=3.5  );

 protected:

  Bool_t fIsConstrained; // Is the beam constraint set
  
#ifndef KFParticleStandalone
  ClassDef( KFVertex, 2 )
#endif
};


//---------------------------------------------------------------------
//
//     Inline implementation of the KFVertex methods
//
//---------------------------------------------------------------------


inline void KFVertex::operator+=( const KFParticle &Daughter )
{
  KFParticle::operator+=( Daughter );
}
  

inline void KFVertex::operator-=( const KFParticle &Daughter )
{
  Daughter.SubtractFromVertex( *this );
}
  
inline KFVertex KFVertex::operator-( const KFParticle &Daughter ) const 
{
  KFVertex tmp = *this;
  Daughter.SubtractFromVertex( tmp );
  return tmp;
}


#endif 
