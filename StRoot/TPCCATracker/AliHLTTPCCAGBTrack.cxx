// $Id: AliHLTTPCCAGBTrack.cxx,v 1.1 2016/02/05 23:27:27 fisyak Exp $
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

#include "AliHLTTPCCAGBTrack.h"
#include <iostream>

std::ostream &operator<<( std::ostream &out, const AliHLTTPCCAGBTrack &t )
{
  out << t.fNHits;
  out << t.fFirstHitRef;
  out << t.fAlpha;
  out << t.fDeDx;
  out << t.fInnerParam;
  out << t.fOuterParam;

  return out;
}

std::istream &operator>>( std::istream &in, AliHLTTPCCAGBTrack &t )
{
  in >> t.fNHits;
  in >> t.fFirstHitRef;
  in >> t.fAlpha;
  in >> t.fDeDx;
  in >> t.fInnerParam;
  in >> t.fOuterParam;

  return in;
}
