//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAMATH_H
#define ALIHLTTPCCAMATH_H

#include "AliHLTTPCCADef.h"

#include <cstdlib>
#include <cmath>
#include <algorithm>

#if defined(HLTCA_STANDALONE)
#include <limits>
#else
#include "TMath.h"
#endif

#ifdef __SSE__
#include <xmmintrin.h>
#endif

/**
 * @class ALIHLTTPCCAMath
 *
 *
 */
namespace CAMath
{
  template<typename T> static inline T Min ( const T &x, const T &y ) { return std::min( x, y ); }
  template<typename T> static inline T Max ( const T &x, const T &y ) { return std::max( x, y ); }
  template<typename T> static inline T Sqrt( const T &x ) { return std::sqrt( x ); }
  template<typename T> static inline T RSqrt( const T &x ) { const T one = 1.; return one / std::sqrt( x ); }
  template<typename T> typename Vc::Vector<T> RSqrt(const Vc::Vector<T> &x) { return Vc::rsqrt( x ); }
  template<typename T> static inline T Abs ( const T &x ) { return std::abs( x ); }
  template<typename T> static inline T Log ( const T &x ) { return std::log( x ); }
  template<typename T> static inline T Log10( const T &x ) { return std::log10( x ); }
  template<typename T> static inline T Sin  ( const T &x ) { return std::sin( x ); }
  template<typename T> static inline T Cos  ( const T &x ) { return std::cos( x ); }
  template<typename T> static T Reciprocal( const T &x );
  template<typename T> typename Vc::Vector<T> Reciprocal(const Vc::Vector<T> &x) { return Vc::reciprocal( x ); }
  template<typename T> static T ApproxSqrt( const T &x );
#ifdef USE_TBB
  template<typename T> static T AtomicMax( T volatile *addr, T val );
#endif //USE_TBB

  template<typename T> struct FiniteReturnTypeHelper { typedef bool R; };
  template<typename T> static typename FiniteReturnTypeHelper<T>::R Finite( const T &x );
  template<typename T> typename Vc::Vector<T>::Mask Finite(const Vc::Vector<T> &x) { return Vc::isfinite( x ); }
  
  template<typename T> static T Round( const T &x ) { return round( x ); }

  template<typename T> static inline T Recip( const T &x ) { return T( 1 ) / x; }
  template<typename T> static T ATan2( const T &y, const T &x ) { return atan2( y, x ); }
  template<typename T> static T ASin( const T &x ) { return asin( x ); }

  float Tan( float x );
  float Copysign( float x, float y );
  static inline float TwoPi() { return 6.283185307179586f; }
  static inline float Pi() { return 3.1415926535897f; }
  int Nint( float x );

  template<typename T> static T ACos( const T &x ) { return (Pi()/2.f - asin( x )); }
  
#ifdef USE_TBB
  int AtomicExch( int volatile *addr, int val );
  int AtomicAdd ( int volatile *addr, int val );
  int AtomicMin ( int volatile *addr, int val );
#endif //USE_TBB
}

#if defined( HLTCA_STANDALONE )
#define choice(c1,c2,c3) c2
#else
#define choice(c1,c2,c3) c3
#endif

namespace CAMath
{

  template<> inline float Reciprocal<float>( const float &x )
  {
    return 1.f / x;
  }

  template<> inline double Reciprocal<double>( const double &x )
  {
    return 1. / x;
  }

#ifdef __SSE__
  template<> inline float RSqrt<float>( const float &x )
  {
    float r = x;
    __m128 tmp;
    asm(
        "rsqrtss %0,%1\n\t"
        "movss %1,%0\n\t"
        : "+m"( r ), "=x"( tmp )
       );
    return r;
  }
#endif // __SSE__

  template<> inline float ApproxSqrt<float>( const float &x )
  {
    float r = x;
    asm(
        "shr %0\n\t"
        "add $0x1fc00000,%0\n\t"
        : "+r"( r )
       );
    return r;
  }
}

inline int CAMath::Nint( float x )
{
  int i;
  if ( x >= 0 ) {
    i = int( x + 0.5f );
    if ( x + 0.5f == float( i ) && i & 1 ) i--;
  } else {
    i = int( x - 0.5f );
    if ( x - 0.5f == float( i ) && i & 1 ) i++;
  }
  return i;
}

namespace CAMath
{
template<> inline bool Finite<float>( const float &x )
{
  return choice( 1,
                 x < std::numeric_limits<float>::infinity() && -x < std::numeric_limits<float>::infinity(),
                 finite( x ) );
}

template<> inline bool Finite<double>( const double &x )
{
  return choice( 1,
                 x < std::numeric_limits<double>::infinity() && -x < std::numeric_limits<double>::infinity(),
                 finite( x ) );
}

template<> inline float Round<float>( const float &x ) { return static_cast<float>( Nint( x ) ); }

template<> inline float ATan2<float>( const float &y, const float &x )
{
  return choice( atan2f( y, x ), atan2( y, x ), TMath::ATan2( y, x ) );
}

template<> inline float ASin( const float &x )
{
  return choice( asinf( x ), asin( x ), TMath::ASin( x ) );
}


} // namespace CAMath

inline float CAMath::Copysign( float x, float y )
{
  x = CAMath::Abs( x );
  return ( y >= 0 ) ? x : -x;
}


inline float CAMath::Tan( float x )
{
  return choice( tanf( x ), tan( x ), TMath::Tan( x ) );
}

#ifdef USE_TBB

#include <tbb/atomic.h>

inline int CAMath::AtomicExch( int volatile *addr, int val )
{
  tbb::atomic<int> &a = *reinterpret_cast<tbb::atomic<int> *>( const_cast<int *>( addr ) );
  return a.fetch_and_store( val );
}

inline int CAMath::AtomicAdd ( int volatile *addr, int val )
{
  tbb::atomic<int> &a = *reinterpret_cast<tbb::atomic<int> *>( const_cast<int *>( addr ) );
  return a.fetch_and_add( val );
}

namespace CAMath
{
template<> inline int AtomicMax<int>( int volatile *addr, int val )
{
  tbb::atomic<int> &a = *reinterpret_cast<tbb::atomic<int> *>( const_cast<int *>( addr ) );
  int old = a;
  if ( old < val ) {
    while ( old != a.compare_and_swap( val, old ) ) {
      old = a;
      if ( old >= val ) {
        break;
      }
    }
  }
  return old;
}

template<> inline unsigned int AtomicMax<unsigned int>( unsigned int volatile *addr, unsigned int val )
{
  tbb::atomic<unsigned int> &a = *reinterpret_cast<tbb::atomic<unsigned int> *>( const_cast<unsigned int *>( addr ) );
  unsigned int old = a;
  if ( old < val ) {
    while ( old != a.compare_and_swap( val, old ) ) {
      old = a;
      if ( old >= val ) {
        break;
      }
    }
  }
  return old;
}
} // namespace CAMath

inline int CAMath::AtomicMin ( int volatile *addr, int val )
{
  tbb::atomic<int> &a = *reinterpret_cast<tbb::atomic<int> *>( const_cast<int *>( addr ) );
  int old = a;
  if ( old > val ) {
    while ( old != a.compare_and_swap( val, old ) ) {
      old = a;
      if ( old <= val ) {
        break;
      }
    }
  }
  return old;
}
#endif //USE_TBB

#undef choice

#endif
