/*
 * This file is part of TPCCATracker package
 * Copyright (C) 2009 Matthias Kretz <kretz@kde.org>
 *
 * TPCCATracker is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * TPCCATracker is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef ALIHLTTPCCASTARTHITID_H
#define ALIHLTTPCCASTARTHITID_H

#include "AliHLTTPCCAHitId.h"
#include "AliHLTTPCCADef.h"

class AliHLTTPCCAStartHitId: public AliHLTTPCCAHitId
{
 public:
  inline void Set( unsigned int row, unsigned int hit, unsigned int length ) { fRow = row; fHit = hit; fLength = length; }
  inline int RowIndex() const { return fRow; }
  inline unsigned int HitIndex() const { return fHit; }
  inline unsigned int Length() const { return fLength; }
  
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
  
  unsigned int fLength; // length of chain
};

#endif // ALIHLTTPCCASTARTHITID_H
