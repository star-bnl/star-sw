// $Id: AliHLTTPCCAMCPoint.cxx,v 1.2 2010/09/01 10:38:27 ikulakov Exp $
//***************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
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


AliHLTTPCCAMCPoint::AliHLTTPCCAMCPoint()
    : fX( 0 ), fY( 0 ), fZ( 0 ), fSx( 0 ), fSy( 0 ), fSz( 0 ), fTime( 0 ), fISlice( 0 ), fTrackID( 0 )
{
  //* Default constructor
}

ostream& operator<<(ostream& out, const AliHLTTPCCALocalMCPoint &a)
{
  out << a.fX << " " << a.fY << " " << a.fZ << std::endl;
  out << a.fPx << " " << a.fPy << " " << a.fPz << " " << a.fQP << std::endl;
  out << a.fISlice << " " << a.fIRow << " " << a.fTrackI << " " << a.fTrackID << std::endl;
}


istream& operator>>(istream& in, AliHLTTPCCALocalMCPoint &a)
{
  in >> a.fX >> a.fY >> a.fZ;
  in >> a.fPx >> a.fPy >> a.fPz >> a.fQP;
  in >> a.fISlice >> a.fIRow >> a.fTrackI >> a.fTrackID;
}
