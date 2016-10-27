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

#ifndef ALIHLTTPCCAHITID_H
#define ALIHLTTPCCAHITID_H

class AliHLTTPCCAHitId
{
  public:
    inline void Set( unsigned short row, unsigned short hit ) { fRow = row; fHit = hit; }
    inline short RowIndex() const { return fRow; }
    inline unsigned short HitIndex() const { return fHit; }

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

    short fRow;
    unsigned short fHit; // index of hit in row array. Use data.ClusterDataIndex( row, iHit ) in order to obtain index in the slice array.
};

typedef AliHLTTPCCAHitId HitId;

#endif // ALIHLTTPCCAHITID_H
