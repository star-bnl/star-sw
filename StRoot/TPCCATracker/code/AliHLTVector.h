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

#ifndef ALIHLTVECTOR_H
#define ALIHLTVECTOR_H

#ifdef ENABLE_VECTORIZATION
#include <Vc/Vc>
#include <Vc/limits>
#include "AliHLTTPCCAMath.h"

using ::Vc::double_v;
using ::Vc::float_v;
using ::Vc::sfloat_v;
using ::Vc::int_v;
using ::Vc::uint_v;
using ::Vc::short_v;
using ::Vc::ushort_v;
using ::Vc::VectorAlignment;
using ::Vc::double_m;
using ::Vc::float_m;
using ::Vc::sfloat_m;
using ::Vc::int_m;
using ::Vc::uint_m;
using ::Vc::short_m;
using ::Vc::ushort_m;

static inline ushort_m validHitIndexes( const ushort_v &v )
{
  return static_cast<short_v>( v ) >= short_v( Vc::Zero );
}

namespace CAMath
{
  template<> inline Vc::int_v Abs  <Vc::int_v >( const Vc::int_v &x ) { return Vc::abs( x ); }
#define SPECIALIZATION( T ) \
  template<> inline Vc::T Min  <Vc::T>( const Vc::T &x, const Vc::T &y ) { return Vc::min( x, y ); } \
  template<> inline Vc::T Max  <Vc::T>( const Vc::T &x, const Vc::T &y ) { return Vc::max( x, y ); }
  SPECIALIZATION( int_v )
  SPECIALIZATION( uint_v )
#ifndef ENABLE_LARRABEE
  template<> inline Vc::short_v Abs  <Vc::short_v >( const Vc::short_v &x ) { return Vc::abs( x ); }
  SPECIALIZATION( short_v )
  SPECIALIZATION( ushort_v )
#endif
#undef SPECIALIZATION
#define SPECIALIZATION( T ) \
  template<> inline T Min  <T>( const T &x, const T &y ) { return Vc::min( x, y ); } \
  template<> inline T Max  <T>( const T &x, const T &y ) { return Vc::max( x, y ); } \
  template<> inline T Sqrt <T>( const T &x ) { return Vc::sqrt( x ); } \
  template<> inline T Abs  <T>( const T &x ) { return Vc::abs( x ); } \
  template<> inline T Log  <T>( const T &x ) { return Vc::log( x ); } \
  template<> inline T Reciprocal<T>( const T &x ) { return Vc::reciprocal( x ); } \
  template<> inline T Round<T>( const T &x ) { return Vc::round( x ); } \
  template<> inline T RSqrt<T>( const T &x ) { return Vc::rsqrt( x ); } \
  template<> struct FiniteReturnTypeHelper<T> { typedef T::Mask R; }; \
  template<> inline FiniteReturnTypeHelper<T>::R Finite<T>( const T &x ) { return Vc::isfinite( x ); } \
  template<> inline T ATan2<T>( const T &x, const T &y ) { return Vc::atan2( x, y ); } \
  template<> inline T ASin<T> ( const T &x ) { return Vc::asin( x ); } \
  template<> inline T Sin  <T>( const T &x ) { return Vc::sin( x ); } \
  template<> inline T Cos  <T>( const T &x ) { return Vc::cos( x ); }

  SPECIALIZATION( float_v )
#if VC_IMPL_SSE
  SPECIALIZATION( sfloat_v )
#endif
  SPECIALIZATION( double_v )
#undef SPECIALIZATION

  static void AtomicMax( unsigned int volatile *addr, uint_v val ) {
    for ( int i = 0; i < uint_v::Size; ++i ) {
      AtomicMax( &addr[i], val[i] );
    }
  }
} // namespace CAMath

#else

#error foo
typedef int int_v;
typedef unsigned int uint_v;
typedef short short_v;
typedef unsigned short ushort_v;
typedef float float_v;
typedef double double_v;
typedef bool Mask;

#endif

#endif // ALIHLTVECTOR_H
