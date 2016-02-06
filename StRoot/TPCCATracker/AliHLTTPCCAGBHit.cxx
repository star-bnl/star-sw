// $Id: AliHLTTPCCAGBHit.cxx,v 1.1 2016/02/05 23:27:27 fisyak Exp $
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

#include "AliHLTTPCCAGBHit.h"

ostream& operator<<(ostream& out, const AliHLTTPCCAGBHit &a)
{
  return out << a.fX << " " << a.fY << " " << a.fZ << std::endl
   << a.fErrX << " " << a.fErrY << " " << a.fErrZ << std::endl
   << a.fAmp << " " << a.fISlice << " " << a.fIRow << " " << a.fID << " " << a.fIsUsed << std::endl;

}

istream& operator>>(istream& in, AliHLTTPCCAGBHit &a)
{
  return in >> a.fX >> a.fY >> a.fZ
   >> a.fErrX >> a.fErrY >> a.fErrZ
   >> a.fAmp >> a.fISlice >> a.fIRow >> a.fID >> a.fIsUsed;
}


//ClassImp(AliHLTTPCCAGBHit)
