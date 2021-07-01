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

#ifndef DEBUG_H
#define DEBUG_H

#include <iostream>

static class AliHLTTPCCANoDebugStream {
  public:
      inline AliHLTTPCCANoDebugStream &operator<<( std::ostream &(*)(std::ostream &) ) { return *this; }

      template<typename T>
      inline AliHLTTPCCANoDebugStream &operator<<( const T & ) { return *this; }
} AliHLTTPCCANoDebugStreamInst;

#ifdef DEBUG_MESSAGES
template<typename T> inline void prepareForDebugOutput( const typename T::Mask k, T *x0,
    T *x1 = 0, T *x2 = 0, T *x3 = 0, T *x4 = 0, T *x5 = 0, T *x6 = 0, T *x7 = 0, T *x8 = 0, T *x9 = 0) {
  if ( x0 ) ( *x0 )( k ) = T( 0 );
  if ( x1 ) ( *x1 )( k ) = T( 0 );
  if ( x2 ) ( *x2 )( k ) = T( 0 );
  if ( x3 ) ( *x3 )( k ) = T( 0 );
  if ( x4 ) ( *x4 )( k ) = T( 0 );
  if ( x5 ) ( *x5 )( k ) = T( 0 );
  if ( x6 ) ( *x6 )( k ) = T( 0 );
  if ( x7 ) ( *x7 )( k ) = T( 0 );
  if ( x8 ) ( *x8 )( k ) = T( 0 );
  if ( x9 ) ( *x9 )( k ) = T( 0 );
}
#if DEBUG_MESSAGES & 1
inline std::ostream &debugS() { return std::cerr; }
#else
inline AliHLTTPCCANoDebugStream &debugS() { return AliHLTTPCCANoDebugStreamInst; }
#endif
#if DEBUG_MESSAGES & 2
inline std::ostream &debugF() { return std::cerr; }
#else
inline AliHLTTPCCANoDebugStream &debugF() { return AliHLTTPCCANoDebugStreamInst; }
#endif
#if DEBUG_MESSAGES & 4
inline std::ostream &debugKF() { return std::cerr; }
#else
inline AliHLTTPCCANoDebugStream &debugKF() { return AliHLTTPCCANoDebugStreamInst; }
#endif
#if DEBUG_MESSAGES & 8
inline std::ostream &debugTS() { return std::cerr; }
#else
inline AliHLTTPCCANoDebugStream &debugTS() { return AliHLTTPCCANoDebugStreamInst; }
#endif
#if DEBUG_MESSAGES & 16
inline std::ostream &debugWO() { return std::cerr; }
#else
inline AliHLTTPCCANoDebugStream &debugWO() { return AliHLTTPCCANoDebugStreamInst; }
#endif
#else
template<typename T> inline void prepareForDebugOutput( const typename T::Mask, T *,
    T * = 0, T * = 0, T * = 0, T * = 0, T * = 0, T * = 0, T * = 0, T * = 0, T * = 0) {}
inline AliHLTTPCCANoDebugStream &debugS() { return AliHLTTPCCANoDebugStreamInst; }
inline AliHLTTPCCANoDebugStream &debugF() { return AliHLTTPCCANoDebugStreamInst; }
inline AliHLTTPCCANoDebugStream &debugKF() { return AliHLTTPCCANoDebugStreamInst; }
inline AliHLTTPCCANoDebugStream &debugTS() { return AliHLTTPCCANoDebugStreamInst; }
inline AliHLTTPCCANoDebugStream &debugWO() { return AliHLTTPCCANoDebugStreamInst; }
#endif

#endif // DEBUG_H
