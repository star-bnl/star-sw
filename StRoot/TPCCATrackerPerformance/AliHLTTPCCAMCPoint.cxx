// $Id: AliHLTTPCCAMCPoint.cxx,v 1.3 2012/08/13 19:35:05 fisyak Exp $
//***************************************************************************
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
//***************************************************************************

#include "AliHLTTPCCAMCPoint.h"
#include "AliHLTTPCCAMath.h"

AliHLTTPCCAMCPoint::AliHLTTPCCAMCPoint()
    : fX( 0 ), fY( 0 ), fZ( 0 ), fSx( 0 ), fSy( 0 ), fSz( 0 ), fTime( 0 ), fISlice( 0 ), fTrackID( 0 )
{
  //* Default constructor
}

ostream& operator<<(ostream& out, const AliHLTTPCCALocalMCPoint &a)
{
  out << a.fX << " " << a.fY << " " << a.fZ << std::endl;
  out << a.fPx << " " << a.fPy << " " << a.fPz << " " << a.fQP << std::endl;
  return out << a.fISlice << " " << a.fIRow << " " << a.fTrackI << " " << a.fTrackID << std::endl;
}


istream& operator>>(istream& in, AliHLTTPCCALocalMCPoint &a)
{
  in >> a.fX >> a.fY >> a.fZ;
  in >> a.fPx >> a.fPy >> a.fPz >> a.fQP;
  return in >> a.fISlice >> a.fIRow >> a.fTrackI >> a.fTrackID;
}


void AliHLTTPCCALocalMCPoint::RotateXY( float alpha) {
  const float cA = CAMath::Cos( alpha );
  const float sA = CAMath::Sin( alpha );
  {
    const float x = X(), y = Y();
    SetX(  x*cA +  y*sA );
    SetY( -x*sA +  y*cA );
  }
  {
    const float x = Px(), y = Py();
    SetPx(  x*cA +  y*sA );
    SetPy( -x*sA +  y*cA );
  }
}
