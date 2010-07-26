/**************************************************************************
 * This file is property of and copyright by the ALICE HLT Project        *
 * All rights reserved.                                                   *
 *                                                                        *
 * Primary Authors:                                                       *
 *     Copyright 2009       Matthias Kretz <kretz@kde.org>                *
 *                                                                        *
 * Permission to use, copy, modify and distribute this software and its   *
 * documentation strictly for non-commercial purposes is hereby granted   *
 * without fee, provided that the above copyright notice appears in all   *
 * copies and that both the copyright notice and this permission notice   *
 * appear in the supporting documentation. The authors make no claims     *
 * about the suitability of this software for any purpose. It is          *
 * provided "as is" without express or implied warranty.                  *
 **************************************************************************/

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
