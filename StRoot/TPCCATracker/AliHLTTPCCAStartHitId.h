/*
    Copyright (C) 2009 Matthias Kretz <kretz@kde.org>

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) version 3.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; see the file COPYING.LIB.  If not, write to
    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301, USA.

*/

#ifndef ALIHLTTPCCASTARTHITID_H
#define ALIHLTTPCCASTARTHITID_H

#include "AliHLTTPCCAHitId.h"
#include "AliHLTTPCCADef.h"

class AliHLTTPCCAStartHitId: public AliHLTTPCCAHitId
{
 public:
  inline void Set( unsigned short row, unsigned short hit, unsigned short length ) { fRow = row; fHit = hit; fLength = length; }
  inline short RowIndex() const { return fRow; }
  inline unsigned short HitIndex() const { return fHit; }
  inline unsigned short Length() const { return fLength; }
  
  inline bool operator<( const AliHLTTPCCAStartHitId &rhs ) const {
    const int rowStep = AliHLTTPCCAParameters::RowStep;
    assert( rowStep <= 2 );
    const bool lL = fLength < rhs.fLength;
    // const bool eL = fLength == rhs.fLength;
    bool lR, eR;
    if (rowStep == 2){
      lR = ( fRow & 1 ) < ( rhs.fRow & 1 )
        || ( ( fRow & 1 ) == ( rhs.fRow & 1 ) && fRow < rhs.fRow );
      eR = fRow == rhs.fRow;
    }
    else{
      lR = fRow <  rhs.fRow;
      eR = fRow == rhs.fRow;
    }
    // return lL || (eL && lR); // todo: why lose efficiency with diff sort
    return lR || (eR && lL); 
  }
  
  unsigned short fLength; // length of chain
};

#endif // ALIHLTTPCCASTARTHITID_H
