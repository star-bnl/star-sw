// $Id: AliHLTTPCCAGBHit.cxx,v 1.2 2010/09/01 10:38:27 ikulakov Exp $
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

#include "AliHLTTPCCAGBHit.h"

ostream& operator<<(ostream& out, const AliHLTTPCCAGBHit &a)
{
  out << a.fX << " " << a.fY << " " << a.fZ << std::endl;
  out << a.fErrX << " " << a.fErrY << " " << a.fErrZ << std::endl;
  out << a.fAmp << " " << a.fISlice << " " << a.fIRow << " " << a.fID << " " << a.fIsUsed << std::endl;

}

istream& operator>>(istream& in, AliHLTTPCCAGBHit &a)
{
  in >> a.fX >> a.fY >> a.fZ;
  in >> a.fErrX >> a.fErrY >> a.fErrZ;
  in >> a.fAmp >> a.fISlice >> a.fIRow >> a.fID >> a.fIsUsed;
}


//ClassImp(AliHLTTPCCAGBHit)
