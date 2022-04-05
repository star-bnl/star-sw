// @(#) $Id: AliHLTTPCCAParam.cxx,v 1.2 2016/06/21 03:39:45 smirnovd Exp $
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


#include "AliHLTTPCCAParam.h"
#include "AliHLTTPCCAMath.h"

#include <iostream>
#include "debug.h"

AliHLTTPCCAParam::AliHLTTPCCAParam()
  : fISlice( 0 ), fNRows( 0 ), fNInnerRows(0), fAlpha( 0.174533 ), fDAlpha( 0.349066 ),
    fCosAlpha( 0 ), fSinAlpha( 0 ), fAngleMin( 0 ), fAngleMax( 0 ), fRMin( 83.65 ), fRMax( 133.3 ),
    fZMin( 0.0529937 ), fZMax( 249.778 ), fErrX( 0 ), fErrY( 0 ), fErrZ( 0.228808 ), fPadPitch( 0.4 ), fBz( -5. ),
    fHitPickUpFactor( 1. ),
    fMaxTrackMatchDRow( 4 ), fTrackConnectionFactor( 3.5 ), fTrackChiCut( 3.5 ), fTrackChi2Cut( 10 ) // are rewrited from file. See operator>>()
    ,fRowX(0), fRecoType(0) //Default is Sti
{
  // constructor
///mvz start
/*  fParamS0Par[0][0][0] = 0.00047013f;
  fParamS0Par[0][0][1] = 2.00135e-05f;
  fParamS0Par[0][0][2] = 0.0106533f;
  fParamS0Par[0][0][3] = 5.27104e-08f;
  fParamS0Par[0][0][4] = 0.012829f;
  fParamS0Par[0][0][5] = 0.000147125f;
  fParamS0Par[0][0][6] = 4.99432f;
  fParamS0Par[0][1][0] = 0.000883342f;
  fParamS0Par[0][1][1] = 1.07011e-05f;
  fParamS0Par[0][1][2] = 0.0103187f;
  fParamS0Par[0][1][3] = 4.25141e-08f;
  fParamS0Par[0][1][4] = 0.0224292f;
  fParamS0Par[0][1][5] = 8.27274e-05f;
  fParamS0Par[0][1][6] = 4.17233f;
  fParamS0Par[0][2][0] = 0.000745399f;
  fParamS0Par[0][2][1] = 5.62408e-06f;
  fParamS0Par[0][2][2] = 0.0151562f;
  fParamS0Par[0][2][3] = 5.08757e-08f;
  fParamS0Par[0][2][4] = 0.0601004f;
  fParamS0Par[0][2][5] = 7.97129e-05f;
  fParamS0Par[0][2][6] = 4.84913f;
  fParamS0Par[1][0][0] = 0.00215126f;
  fParamS0Par[1][0][1] = 6.82233e-05f;
  fParamS0Par[1][0][2] = 0.0221867f;
  fParamS0Par[1][0][3] = -6.27825e-09f;
  fParamS0Par[1][0][4] = -0.00745378f;
  fParamS0Par[1][0][5] = 0.000172629f;
  fParamS0Par[1][0][6] = 6.24987f;
  fParamS0Par[1][1][0] = 0.00181667f;
  fParamS0Par[1][1][1] = -4.17772e-06f;
  fParamS0Par[1][1][2] = 0.0253429f;
  fParamS0Par[1][1][3] = 1.3011e-07f;
  fParamS0Par[1][1][4] = -0.00362827f;
  fParamS0Par[1][1][5] = 0.00030406f;
  fParamS0Par[1][1][6] = 17.7775f;
  fParamS0Par[1][2][0] = 0.00158251f;
  fParamS0Par[1][2][1] = -3.55911e-06f;
  fParamS0Par[1][2][2] = 0.0247899f;
  fParamS0Par[1][2][3] = 7.20604e-08f;
  fParamS0Par[1][2][4] = 0.0179946f;
  fParamS0Par[1][2][5] = 0.000425504f;
  fParamS0Par[1][2][6] = 20.9294f;*/

  fParamS0Par[0][0][0] = 0.0004f;
  fParamS0Par[0][0][1] = 0.001720216f;
  fParamS0Par[0][0][2] = 0.0236289f;
  fParamS0Par[0][0][3] = 0.001096839f;
  fParamS0Par[0][0][4] = 0.007570851f;
  fParamS0Par[0][0][5] = 0.01857614f;
  fParamS0Par[0][0][6] = 0.0f;
  fParamS0Par[0][1][0] = 0.001534805f;
  fParamS0Par[0][1][1] = 0.001422267f;
  fParamS0Par[0][1][2] = 0.07089186f;
  fParamS0Par[0][1][3] = 0.002919157f;
  fParamS0Par[0][1][4] = 0.006362385f;
  fParamS0Par[0][1][5] = 0.06090711f;
  fParamS0Par[0][1][6] = 0.0f;
  fParamS0Par[0][2][0] = 0.0f;
  fParamS0Par[0][2][1] = 0.0f;
  fParamS0Par[0][2][2] = 0.0f;
  fParamS0Par[0][2][3] = 0.0f;
  fParamS0Par[0][2][4] = 0.0f;
  fParamS0Par[0][2][5] = 0.0f;
  fParamS0Par[0][2][6] = 0.0f;
  fParamS0Par[1][0][0] = 0.0f;
  fParamS0Par[1][0][1] = 0.0f;
  fParamS0Par[1][0][2] = 0.0f;
  fParamS0Par[1][0][3] = 0.0f;
  fParamS0Par[1][0][4] = 0.0f;
  fParamS0Par[1][0][5] = 0.0f;
  fParamS0Par[1][0][6] = 0.0f;
  fParamS0Par[1][1][0] = 0.0f;
  fParamS0Par[1][1][1] = 0.0f;
  fParamS0Par[1][1][2] = 0.0f;
  fParamS0Par[1][1][3] = 0.0f;
  fParamS0Par[1][1][4] = 0.0f;
  fParamS0Par[1][1][5] = 0.0f;
  fParamS0Par[1][1][6] = 0.0f;
  fParamS0Par[1][2][0] = 0.0f;
  fParamS0Par[1][2][1] = 0.0f;
  fParamS0Par[1][2][2] = 0.0f;
  fParamS0Par[1][2][3] = 0.0f;
  fParamS0Par[1][2][4] = 0.0f;
  fParamS0Par[1][2][5] = 0.0f;
  fParamS0Par[1][2][6] = 0.0f;
///mvz end
  const double kCLight = 0.000299792458;

  fPolinomialFieldBz[0] = kCLight * 4.99643;
  fPolinomialFieldBz[1] = kCLight * -2.27193e-06;
  fPolinomialFieldBz[2] = kCLight * 0.000116475;
  fPolinomialFieldBz[3] = kCLight * -1.49956e-06;
  fPolinomialFieldBz[4] = kCLight * -1.01721e-07;
  fPolinomialFieldBz[5] = kCLight * 4.85701e-07;

  Update();
}

void AliHLTTPCCAParam::Initialize( int iSlice,
                                   int nRows, float rowX[],
                                   float alpha, float dAlpha,
                                   float rMin, float rMax,
                                   float zMin, float zMax,
                                   float padPitch, float zSigma,
                                   float bz
                                 )
{
  // initialization
  fISlice = iSlice;
  fAlpha = alpha;
  fDAlpha = dAlpha;
  fRMin = rMin;
  fRMax = rMax;
  fZMin = zMin;
  fZMax = zMax;
  fPadPitch = padPitch;
  fErrY = 1.; // not in use
  fErrZ = zSigma;
  fBz = bz;
  fNRows = nRows;
  fRowX.resize(fNRows);
  for ( int irow = 0; irow < nRows; irow++ ) {
    fRowX[irow] = rowX[irow];
  }

  Update();
}

void AliHLTTPCCAParam::Update()
{
  // update of calculated values
  fCosAlpha = CAMath::Cos( fAlpha );
  fSinAlpha = CAMath::Sin( fAlpha );
  fAngleMin = fAlpha - fDAlpha / 2.f;
  fAngleMax = fAlpha + fDAlpha / 2.f;
  fErrX = fPadPitch / CAMath::Sqrt( 12.f );
  fTrackChi2Cut = fTrackChiCut * fTrackChiCut;
}

void AliHLTTPCCAParam::Slice2Global( float x, float y,  float z,
                                     float *X, float *Y,  float *Z ) const
{
  // conversion of coorinates sector->global
  *X = x * fCosAlpha - y * fSinAlpha;
  *Y = y * fCosAlpha + x * fSinAlpha;
  *Z = z;
}

void AliHLTTPCCAParam::Global2Slice( float X, float Y,  float Z,
                                     float *x, float *y,  float *z ) const
{
  // conversion of coorinates global->sector
  *x = X * fCosAlpha + Y * fSinAlpha;
  *y = Y * fCosAlpha - X * fSinAlpha;
  *z = Z;
}

float AliHLTTPCCAParam::GetClusterError2( int yz, int type, float z, float angle ) const
{
  //* recalculate the cluster error wih respect to the track slope
  const float angle2 = angle * angle;
  const float *c = fParamS0Par[yz][type];
  const float v = c[0] + z * ( c[1] + c[3] * z ) + angle2 * ( c[2] + angle2 * c[4] + c[5] * z );
//std::cout << v << std::endl;
  return CAMath::Abs( v );
}
///mvz start 20.01.2010
/*
void AliHLTTPCCAParam::GetClusterErrors2( int iRow, float z, float sinPhi, float cosPhi, float DzDs, float &Err2Y, float &Err2Z ) const
{
  //
  // Use calibrated cluster error from OCDB
  //

  z = CAMath::Abs( ( 250. - 0.275 ) - CAMath::Abs( z ) );
  const int type = errorType( iRow );
  float cosPhiInv = CAMath::Abs( cosPhi ) > 1.e-2 ? 1. / cosPhi : 0;
  float angleY = sinPhi * cosPhiInv ;
  float angleZ = DzDs * cosPhiInv ; // SG was bug???
  Err2Y = GetClusterError2( 0, type, z, angleY );
  Err2Z = GetClusterError2( 1, type, z, angleZ );
}*/
void AliHLTTPCCAParam::GetClusterErrors2( int iRow, const AliHLTTPCCATrackParam &t, float &Err2Y, float &Err2Z ) const
{
enum {kYErr=0,kZErr=1,kWidTrk=2,kThkDet=3,kYDiff=4,kZDiff=5};
  float z = t.Z();
  const int type = errorType( iRow );
  z = (200. - CAMath::Abs(z)) * 0.01;
  if(z<0.) z=0;

  float sin2Phi = t.GetSinPhi()*t.GetSinPhi();
  float cos2Phi = (1.f - sin2Phi);
  if (cos2Phi<0.0001) cos2Phi=0.0001;
  float tg2Phi = sin2Phi/cos2Phi;

  float tg2Lambda = t.DzDs()*t.DzDs();
 
  const float *c = fParamS0Par[0][type];
  switch (fRecoType) {
  case 0: {/*Sti*/
    Err2Y = c[0] + c[1]*z/cos2Phi + c[2]*tg2Phi;
    Err2Z = c[3] + c[4]*z*(1.f+tg2Lambda) + c[5]*tg2Lambda;
    break;}

  case 1: {/*Sti*/ 
    float cos2lambda = 1/(tg2Lambda+1);
    float sin2lambda = tg2Lambda*cos2lambda;
    z = (210. - CAMath::Abs(z)) * 0.01;

    Err2Y = (c[kThkDet]*sin2Phi   + c[kWidTrk])
             / (cos2Phi)             + c[kYErr] + c[kYDiff]*z;
    Err2Z = (c[kThkDet]*(sin2lambda) + c[kWidTrk]*((sin2Phi)*(sin2lambda)+cos2Phi))
             / (cos2Phi*cos2lambda)  + c[kZErr] + c[kZDiff]*z;
   break; }  
   default: assert(0);
  }  
    
    
  if(Err2Y<1e-6) Err2Y = 1e-6;
  if(Err2Z<1e-6) Err2Z = 1e-6;

  if(Err2Y>1.f) Err2Y = 1.f;
  if(Err2Z>1.f) Err2Z = 1.f;

//  std::cout <<Err2Y<<"  "<<Err2Z<< std::endl;
}
///mvz end 20.01.2010

std::ostream &operator<<( std::ostream &out, const AliHLTTPCCAParam &p )
{
  // write settings to the file
  out << p.fISlice << std::endl;
  out << p.fNRows << std::endl;
  out << p.fAlpha << std::endl;
  out << p.fDAlpha << std::endl;
  out << p.fCosAlpha << std::endl;
  out << p.fSinAlpha << std::endl;
  out << p.fAngleMin << std::endl;
  out << p.fAngleMax << std::endl;
  out << p.fRMin << std::endl;
  out << p.fRMax << std::endl;
  out << p.fZMin << std::endl;
  out << p.fZMax << std::endl;
  out << p.fErrX << std::endl;
  out << p.fErrY << std::endl;
  out << p.fErrZ << std::endl;
  out << p.fPadPitch << std::endl;
  out << p.fBz << std::endl;
  out << p.fHitPickUpFactor << std::endl;
  out << p.fMaxTrackMatchDRow << std::endl;
  out << p.fTrackConnectionFactor << std::endl;
  out << p.fTrackChiCut << std::endl;
  out << p.fTrackChi2Cut << std::endl;
  for ( int iRow = 0; iRow < p.fNRows; iRow++ ) {
    out << p.fRowX[iRow] << std::endl;
  }
  out << std::endl;
  for ( int i = 0; i < 2; i++ )
    for ( int j = 0; j < 3; j++ )
      for ( int k = 0; k < 7; k++ )
        out << p.fParamS0Par[i][j][k] << std::endl;
  out << std::endl;

  return out;
}

std::istream &operator>>( std::istream &in, AliHLTTPCCAParam &p )
{
  // Read settings from the file

  in >> p.fISlice;
  in >> p.fNRows;
  p.SetNInnerRows( 13 ); // TODO move to input file
  in >> p.fAlpha;
  in >> p.fDAlpha;
  in >> p.fCosAlpha;
  in >> p.fSinAlpha;
  in >> p.fAngleMin;
  in >> p.fAngleMax;
  in >> p.fRMin;
  in >> p.fRMax;
  in >> p.fZMin;
  in >> p.fZMax;
  in >> p.fErrX;
  in >> p.fErrY;
  in >> p.fErrZ;
  in >> p.fPadPitch;
  in >> p.fBz;
  in >> p.fHitPickUpFactor;
  in >> p.fMaxTrackMatchDRow;
  in >> p.fTrackConnectionFactor;
  in >> p.fTrackChiCut;
  in >> p.fTrackChi2Cut;
  p.fRowX.resize(p.fNRows);
  for ( int iRow = 0; iRow < p.fNRows; iRow++ ) {
    in >> p.fRowX[iRow];
  }

  for ( int i = 0; i < 2; i++ )
    for ( int j = 0; j < 3; j++ )
      for ( int k = 0; k < 7; k++ )
        in >> p.fParamS0Par[i][j][k];

  return in;
}

#include "BinaryStoreHelper.h"

void AliHLTTPCCAParam::StoreToFile( FILE *f ) const
{
  BinaryStoreWrite( *this, f );
}

void AliHLTTPCCAParam::RestoreFromFile( FILE *f )
{
  BinaryStoreRead( *this, f );
}

