// @(#) $Id: AliHLTTPCCAOutTrack.cxx,v 1.1.1.1 2010/07/26 20:55:38 ikulakov Exp $
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

#include "AliHLTTPCCAOutTrack.h"

#include <istream>

std::istream &operator>>( std::istream &in, AliHLTTPCCAOutTrack &t )
{
  in >> t.fNHits;
  in >> t.fFirstHitRef;
  in >> t.fOrigTrackID;

  in >> t.fStartPoint;
  in >> t.fEndPoint;

  return in;
}

std::ostream &operator<<( std::ostream &out, const AliHLTTPCCAOutTrack &t )
{
  out << t.NHits() << " "
  << t.FirstHitRef() << " "
  << t.OrigTrackID() << " "
  << std::endl;
  out << t.fStartPoint;
  out << t.fEndPoint;

  return out;
}
