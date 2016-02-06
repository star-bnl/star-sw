//-*- Mode: C++ -*-
// @(#) $Id: AliHLTTPCCAHit.h,v 1.1 2016/02/05 23:27:27 fisyak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAHIT_H
#define ALIHLTTPCCAHIT_H

#include "AliHLTTPCCADef.h"

/**
 * @class AliHLTTPCCAHit
 *
 * The AliHLTTPCCAHit class is the internal representation
 * of the TPC clusters for the AliHLTTPCCATracker algorithm.
 *
 */
class AliHLTTPCCAHit
{
  public:

    float Y() const   { return fY;    }
    float Z() const  { return fZ;    }

    void SetY( float v ) { fY = v;    }
    void SetZ( float v ) { fZ = v;    }

    void SetCoordinates( float y, float z ) { fY = y; fZ = z; }

  protected:
    float fY, fZ;       // Y and Z position of the TPC cluster
};


#endif
