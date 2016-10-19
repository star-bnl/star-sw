// $Id: AliHLTTPCCAMCTrack.cxx,v 1.3 2011/01/31 17:18:28 fisyak Exp $
// **************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
//                                                                          *
// Developed by:   Igor Kulakov <I.Kulakov@gsi.de>                          *
//                 Maksym Zyzak <M.Zyzak@gsi.de>                            *
//                                                                          *
// Permission to use, copy, modify and distribute this software and its     *
// documentation strictly for non-commercial purposes is hereby granted     *
// without fee, provided that the above copyright notice appears in all     *
// copies and that both the copyright notice and this permission notice     *
// appear in the supporting documentation. The authors make no claims       *
// about the suitability of this software for any purpose. It is            *
// provided "as is" without express or implied warranty.                    *
//                                                                          *
//***************************************************************************


#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMath.h"

#ifndef HLTCA_STANDALONE
#include "TParticle.h"
#include "TDatabasePDG.h"
#endif

AliHLTTPCCAMCTrack::AliHLTTPCCAMCTrack()
    : fPDG( 0 ), fP( 0 ), fPt( 0 ), fNHits( 0 ), fNMCPoints( 0 ), fFirstMCPointID( 0 ), fNReconstructed( 0 ), fSet( 0 ), fNTurns( 0 )
{
  //* Default constructor
}

#ifndef HLTCA_STANDALONE
AliHLTTPCCAMCTrack::AliHLTTPCCAMCTrack( const TParticle *part )
    : fPDG( 0 ), fP( 0 ), fPt( 0 ), fNHits( 0 ), fNMCPoints( 0 ), fFirstMCPointID( 0 ), fNReconstructed( 0 ), fSet( 0 ), fNTurns( 0 )
{
  //* Constructor from TParticle

  for ( int i = 0; i < 7; i++ ) fPar[i] = 0;
  for ( int i = 0; i < 7; i++ ) fTPCPar[i] = 0;
  fP = 0;
  fPt = 0;

  if ( !part ) return;
  TLorentzVector mom, vtx;
  part->ProductionVertex( vtx );
  part->Momentum( mom );
  fPar[0] = part->Vx();
  fPar[1] = part->Vy();
  fPar[2] = part->Vz();
  fP = part->P();
  fPt = part->Pt();
  float pi = ( fP > 1.e-4 ) ? 1. / fP : 0;
  fPar[3] = part->Px() * pi;
  fPar[4] = part->Py() * pi;
  fPar[5] = part->Pz() * pi;
  fPar[6] = 0;
  fPDG  = part->GetPdgCode();
  if ( CAMath::Abs( fPDG ) < 100000 ) {
    TParticlePDG *pPDG = TDatabasePDG::Instance()->GetParticle( fPDG );
    if ( pPDG ) fPar[6] = pPDG->Charge() / 3.0 * pi;
  }
}
#endif

// void AliHLTTPCCAMCTrack::SetTPCPar( float X, float Y, float Z,
//                                     float Px, float Py, float Pz )
// {
//   //* Set parameters at TPC entrance

//   for ( int i = 0; i < 7; i++ ) fTPCPar[i] = 0;

//   fTPCPar[0] = X;
//   fTPCPar[1] = Y;
//   fTPCPar[2] = Z;
//   float p = CAMath::Sqrt( Px * Px + Py * Py + Pz * Pz );
//   float pi = ( p > 1.e-4 ) ? 1. / p : 0;
//   fTPCPar[3] = Px * pi;
//   fTPCPar[4] = Py * pi;
//   fTPCPar[5] = Pz * pi;
//   fTPCPar[6] = 0;
//   if ( CAMath::Abs( fPDG ) < 100000 ) {
//     TParticlePDG *pPDG = TDatabasePDG::Instance()->GetParticle( fPDG );
//     if ( pPDG ) fTPCPar[6] = pPDG->Charge() / 3.0 * pi;
//   }
// }

ostream& operator<<(ostream& out, const AliHLTTPCCAMCTrack &a)
{
  out << a.fMotherId << " " << a.fPDG << std::endl;
  for (int i = 0; i < 7; i++) out << a.fPar[i] << std::endl;
  for (int i = 0; i < 7; i++) out << a.fTPCPar[i] << std::endl;
  out << a.fP << " " << a.fPt << std::endl;
  out << a.fNHits << " " << a.fNMCPoints << " " << a.fFirstMCPointID << std::endl;
  return out << a.fNReconstructed << " " << a.fSet << " " << a.fNTurns << std::endl;
}


istream& operator>>(istream& in, AliHLTTPCCAMCTrack &a)
{
  in >> a.fMotherId >> a.fPDG;
  for (int i = 0; i < 7; i++) in >> a.fPar[i];
  for (int i = 0; i < 7; i++) in >> a.fTPCPar[i];
  in >> a.fP >> a.fPt;
  in >> a.fNHits >> a.fNMCPoints >> a.fFirstMCPointID;
  return in >> a.fNReconstructed >> a.fSet >> a.fNTurns;
}

