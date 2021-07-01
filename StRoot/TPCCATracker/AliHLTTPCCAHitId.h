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

#ifndef ALIHLTTPCCAHITID_H
#define ALIHLTTPCCAHITID_H

class AliHLTTPCCAHitId
{
  public:
    inline void Set( unsigned int row, unsigned int hit ) { fRow = row; fHit = hit; }
    inline int RowIndex() const { return fRow; }
    inline unsigned int HitIndex() const { return fHit; }

    inline bool operator<( const AliHLTTPCCAHitId &rhs ) const {
      const int rowStep = AliHLTTPCCAParameters::RowStep;
      assert( rowStep <= 2 );
      if (rowStep == 2){
        return ( fRow & 1 ) < ( rhs.fRow & 1 )
            || ( ( fRow & 1 ) == ( rhs.fRow & 1 ) && fRow < rhs.fRow );
      }
      else{
        return ( fRow < rhs.fRow );
      }
    }

    int fRow;
    unsigned int fHit; // index of hit in row array. Use data.ClusterDataIndex( row, iHit ) in order to obtain index in the slice array.
};

typedef AliHLTTPCCAHitId HitId;

#endif // ALIHLTTPCCAHITID_H
