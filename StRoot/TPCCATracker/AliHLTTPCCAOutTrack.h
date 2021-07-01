/*
 * This file is part of TPCCATracker package
 * Copyright (C) 2007-2020 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2020 Goethe University of Frankfurt
 *               2007-2020 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Sergey Gorbunov
 *               2007-2019 Maksym Zyzak
 *               2007-2014 Igor Kulakov
 *               2014-2020 Grigory Kozlov
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

#ifndef ALIHLTTPCCAOUTTRACK_H
#define ALIHLTTPCCAOUTTRACK_H

#include "AliHLTTPCCATrackParam.h"

namespace std
{
  template<typename T> struct char_traits;
  template<typename _CharT, typename _Traits> class basic_istream;
  typedef basic_istream<char, char_traits<char> > istream;
  template<typename _CharT, typename _Traits> class basic_ostream;
  typedef basic_ostream<char, char_traits<char> > ostream;
} // namespace std

/**
 * @class AliHLTTPCCAOutTrack
 * AliHLTTPCCAOutTrack class is used to store the final
 * reconstructed tracks which will be then readed
 * by the AliHLTTPCCATrackerComponent
 *
 * The class contains no temporary variables, etc.
 *
 */
class AliHLTTPCCAOutTrack
{
    friend std::istream &operator>>( std::istream &, AliHLTTPCCAOutTrack & );
    friend std::ostream &operator<<( std::ostream &, const AliHLTTPCCAOutTrack & );
  public:

    AliHLTTPCCAOutTrack(): fFirstHitRef( 0 ), fNHits( 0 ), fStartPoint(), fEndPoint(), fOrigTrackID( 0 ) {}
    virtual ~AliHLTTPCCAOutTrack() {}

    int NHits()               const { return fNHits; }
    int FirstHitRef()         const { return fFirstHitRef; }

    const AliHLTTPCCATrackParam &StartPoint() const { return fStartPoint; }
    const AliHLTTPCCATrackParam &EndPoint()   const { return fEndPoint; }
    int OrigTrackID()                const { return fOrigTrackID; }

    void SetNHits( int v )               { fNHits = v; }
    void SetFirstHitRef( int v )         { fFirstHitRef = v; }

    void SetStartPoint( const AliHLTTPCCATrackParam &v ) { fStartPoint = v; }
    void SetEndPoint( const AliHLTTPCCATrackParam &v )   { fEndPoint = v; }
    void SetOrigTrackID( int v )                { fOrigTrackID = v; }

  protected:
    int fFirstHitRef;   //* index of the first hit reference in track->hit reference array
    int fNHits;         //* number of track hits
    AliHLTTPCCATrackParam fStartPoint; //* fitted track parameters at the start point
    AliHLTTPCCATrackParam fEndPoint;   //* fitted track parameters at the start point
    int fOrigTrackID;                //* index of the original slice track
};

std::istream &operator>>( std::istream &in, AliHLTTPCCAOutTrack &ot );
std::ostream &operator<<( std::ostream &out, const AliHLTTPCCAOutTrack &ot );

#endif
