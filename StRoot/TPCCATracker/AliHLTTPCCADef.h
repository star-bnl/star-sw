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

#ifndef ALIHLTTPCCADEF_H
#define ALIHLTTPCCADEF_H


/**
 * Definitions needed for AliHLTTPCCATracker
 *
 */

/**
 * Obsolete and testing-only definitions
 */
//#define AVX1V
//#define TETA
//#define NO_NTRACKLET_FIX
//#define FOURHITSEGMENTS
#define LOOPER_TEST
//#define VC_GATHER_SCATTER

/**
 * Technical options
 */
#define CALC_DCA_ON

/**
 * Basic options
 */
#define ITPC_TCUT		// Chi2cut on triplets (first iteration only) x3.5 for iTPC(72)
#define TRACKLET_EXT		// Use wider cuts on track fittinig and track extension in TrackletConstructor
#define EXTEND_ALL_TRAKCS	// Extend all tracks in TrackletConstructor
#define MERGE_LOOPERS		// Switch on LooperMerger


/**
 * Tracking algorithm versions
 * V6 - basic variant
 * If none of the versions is selected, standard CA tracking with two iterations will be used.
 */
#define V6	// [2 iterations CA] + [saved 3-hit seeds]
//#define V6_1	// Only with V6. Faster, more clones and ghosts. A bit more efficient(?). [1 iteration CA] + [saved 3-hit seeds]
//#define V7	// Without V6(V6_1) options. [1 iteration CA] + [new 3-hit seeds (exV5)]



#define NDEBUG
#define USE_TIMERS

// ----- Vc -----

#include <Vc/Vc>
#include <Vc/limits>
#include <Vc/array>
#include <Vc/vector>

using ::Vc::double_v;
using ::Vc::float_v;
using ::Vc::short_v;
using ::Vc::ushort_v;
using ::Vc::VectorAlignment;
using ::Vc::double_m;
using ::Vc::float_m;
using ::Vc::short_m;
using ::Vc::ushort_m;
using ::Vc::atan2;
using ::Vc::asin;
using ::Vc::round;
using ::Vc::isfinite;
#ifndef AVX1V
using ::Vc::int_v;
using ::Vc::uint_v;
using ::Vc::int_m;
using ::Vc::uint_m;
#else
typedef float_v int_v;
typedef float_v uint_v;
typedef float_m int_m;
typedef float_m uint_m;
#endif

static inline uint_m validHitIndexes( const uint_v &v )
{
//  return (static_cast<int_v>( v ) >= int_v( Vc::Zero ) && v < std::numeric_limits<unsigned int>::max());
  return (static_cast<int_v>( v ) >= int_v( Vc::Zero ) && v < 100000000);

}


/*
 * Cut for track segments to count as looper parts if QPt is less
 */
const int looperQPtCut = 5;

#ifdef V6
struct hit_link {
  hit_link( int r, int h, int l )
    : row(r)
    , hit(h)
    , link(l)
  {}
  int row;
  int hit;
  int link;
};
#endif
	
#ifdef USE_TBB
// static void AtomicMax( unsigned int volatile *addr, uint_v val ) {
//   for ( int i = 0; i < uint_v::Size; ++i ) {
//     AtomicMax( &addr[i], val[i] );
//   }
// }
#endif //USE_TBB

#ifdef CALC_DCA_ON
  struct point_3d{
    float x, y, z;
  };
#endif

// ---------

#ifdef NVALGRIND
#define VALGRIND_CHECK_VALUE_IS_DEFINED( mask )
#define VALGRIND_CHECK_MASKED_VECTOR_IS_DEFINED( v, k )
#define VALGRIND_CHECK_MEM_IS_DEFINED( v, k );
#define VALGRIND_CHECK_MEM_IS_ADDRESSABLE( v, k );
#else // NVALGRIND
#include <valgrind/memcheck.h>
#define VALGRIND_CHECK_MASKED_VECTOR_IS_DEFINED( v, k ) \
{ \
  __typeof__( v + v ) tmp( v ); \
  tmp.setZero( !k ); \
  VALGRIND_CHECK_VALUE_IS_DEFINED( tmp ); \
}
#endif // NVALGRIND

// --------


#if defined(HLTCA_STANDALONE)
typedef unsigned char UChar_t;
typedef UChar_t Byte_t;
typedef int Int_t;
typedef double Double_t;
#else
#include "Rtypes.h"
#endif


#ifdef NDEBUG
#define ASSERT(v, msg)
#else
#define ASSERT(v, msg) \
if (v) {} else { \
  std::cerr << __FILE__ << ":" << __LINE__ << " assertion failed: " \
            << #v << " = " << (v) << "\n" << msg << std::endl; \
  abort(); \
}
#endif


struct float2 { float x; float y; };
/*
 * Helper for compile-time verification of correct API usage
 */
namespace
{
  template<bool> struct HLTTPCCA_STATIC_ASSERT_FAILURE;
  template<> struct HLTTPCCA_STATIC_ASSERT_FAILURE<true> {};
}

#define HLTTPCCA_STATIC_ASSERT_CONCAT_HELPER(a, b) a##b
#define HLTTPCCA_STATIC_ASSERT_CONCAT(a, b) HLTTPCCA_STATIC_ASSERT_CONCAT_HELPER(a, b)
#define STATIC_ASSERT(cond, msg) \
  typedef HLTTPCCA_STATIC_ASSERT_FAILURE<cond> HLTTPCCA_STATIC_ASSERT_CONCAT(_STATIC_ASSERTION_FAILED_##msg, __LINE__); \
  HLTTPCCA_STATIC_ASSERT_CONCAT(_STATIC_ASSERTION_FAILED_##msg, __LINE__) Error_##msg; \
  (void) Error_##msg

namespace
{
  template<typename T1>
  void UNUSED_PARAM1( const T1 & ) {}
  template<typename T1, typename T2>
  void UNUSED_PARAM2( const T1 &, const T2 & ) {}
  template<typename T1, typename T2, typename T3>
  void UNUSED_PARAM3( const T1 &, const T2 &, const T3 & ) {}
  template<typename T1, typename T2, typename T3, typename T4>
  void UNUSED_PARAM4( const T1 &, const T2 &, const T3 &, const T4 & ) {}
  template<typename T1, typename T2, typename T3, typename T4, typename T5>
  void UNUSED_PARAM5( const T1 &, const T2 &, const T3 &, const T4 &, const T5 & ) {}
  template<typename T1, typename T2, typename T3, typename T4, typename T5, typename T6>
  void UNUSED_PARAM6( const T1 &, const T2 &, const T3 &, const T4 &, const T5 &, const T6 & ) {}
  template<typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7>
  void UNUSED_PARAM7( const T1 &, const T2 &, const T3 &, const T4 &, const T5 &, const T6 &, const T7 & ) {}
  template<typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8>
  void UNUSED_PARAM8( const T1 &, const T2 &, const T3 &, const T4 &, const T5 &, const T6 &, const T7 &, const T8 & ) {}
  template<typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8, typename T9>
  void UNUSED_PARAM9( const T1 &, const T2 &, const T3 &, const T4 &, const T5 &, const T6 &, const T7 &, const T8 &, const T9 & ) {}
}

#define unrolled_loop4( _type_, _it_, _start_, _end_, _code_ ) \
if (_start_ +  0 < _end_) { enum { _it_ = (_start_ +  0) < _end_ ? (_start_ +  0) : _start_ }; _code_ } \
if (_start_ +  1 < _end_) { enum { _it_ = (_start_ +  1) < _end_ ? (_start_ +  1) : _start_ }; _code_ } \
if (_start_ +  2 < _end_) { enum { _it_ = (_start_ +  2) < _end_ ? (_start_ +  2) : _start_ }; _code_ } \
if (_start_ +  3 < _end_) { enum { _it_ = (_start_ +  3) < _end_ ? (_start_ +  3) : _start_ }; _code_ } \
do {} while ( false )

#ifdef __GNUC__
#define MAY_ALIAS __attribute__((__may_alias__))
#else
#define MAY_ALIAS
#endif

#if defined( __GNUC__ ) && __GNUC__ - 0 >= 3
# define ISLIKELY(  x )    __builtin_expect( !!( x ),1 )
# define ISUNLIKELY(  x )  __builtin_expect( !!( x ),0 )
#else
# define ISLIKELY(  x )   (  x )
# define ISUNLIKELY(  x )  (  x )
#endif

#endif
