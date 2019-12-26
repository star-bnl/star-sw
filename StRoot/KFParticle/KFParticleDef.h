/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
 *
 * KFParticle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * KFParticle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


#ifndef KFParticleDef_H
#define KFParticleDef_H

#ifdef __ROOT__ //for the STAR experiment
#define HomogeneousField
#endif

#ifdef HLTCA_STANDALONE
#include "RootTypesDef.h"
#else
#include "TObject.h"
#endif

#define NInputSets 8

#include <Vc/Vc>
#include <Vc/version.h>
#include <Vc/limits>
using ::Vc::float_v;
using ::Vc::double_v;
using ::Vc::float_v;
using ::Vc::int_v;
using ::Vc::uint_v;
using ::Vc::VectorAlignment;
using ::Vc::double_m;
using ::Vc::float_m;
using ::Vc::int_m;
using ::Vc::uint_m;
using ::Vc::atan2;
using ::Vc::asin;
using ::Vc::round;
using ::Vc::isfinite;

#ifdef VC_VERSION_NUMBER
#if VC_VERSION_NUMBER < VC_VERSION_CHECK(1,0,0)
template <typename To, typename From> To simd_cast(From &&x) { return static_cast<To>(x); }
#endif
#elif defined(Vc_VERSION_NUMBER)
#if Vc_VERSION_NUMBER < Vc_VERSION_CHECK(1,0,0)
template <typename To, typename From> To simd_cast(From &&x) { return static_cast<To>(x); }
#endif
#endif

const int float_vLen = float_v::Size;

#if defined(HLTCA_STANDALONE)
typedef unsigned char UChar_t;
typedef UChar_t Byte_t;
typedef int Int_t;
typedef double Double_t;
#else
#include "Rtypes.h"
#endif

#include "KFPSimdAllocator.h"    
typedef std::vector<float_v, KFPSimdAllocator<float_v> > kfvector_floatv;

typedef std::vector<float, KFPSimdAllocator<float> > kfvector_float;
typedef std::vector<int, KFPSimdAllocator<int> > kfvector_int;
typedef std::vector<unsigned int, KFPSimdAllocator<unsigned int> > kfvector_uint;

namespace KFPMath
{
  static inline __attribute__((always_inline)) float_v Sin  ( const float_v &phi ) 
  {
    const float_v pi(3.1415926535897932f);
    const float_v nTurnsF = (phi + pi) / (float_v(2.f)*pi);
    int_v nTurns = simd_cast<int_v>( nTurnsF );
    nTurns( (nTurns<=int_v(Vc::Zero)) && simd_cast<int_m>(phi<-pi)) -= 1;
    
    const float_v& x = phi - simd_cast<float_v>(nTurns)*(float_v(2.f)*pi);
    
    const float_v& B = 4.f/pi;
    const float_v& C = -B/pi;

    float_v y = (B + C * Vc::abs(x)) * x;

    const float_v& P = 0.218f;
    y = P * (y * Vc::abs(y) - y) + y;
    
    return y;    
  }
  static inline __attribute__((always_inline)) float_v Cos  ( const float_v &phi )
  {     
    return Sin( phi + 1.570796326795f ); //x + pi/2
  }
  static inline __attribute__((always_inline)) float_v ATan2( const float_v &y, const float_v &x )
  { 
    const float_v pi(3.1415926535897932f);
    const float_v zero(Vc::Zero);

    const float_m &xZero = (x == zero);
    const float_m &yZero = (y == zero);
    const float_m &xNeg  = (x < zero);
    const float_m &yNeg  = (y < zero);

    const float_v &absX = Vc::abs(x);
    const float_v &absY = Vc::abs(y);

    float_v a = absY / absX;
    const float_m &gt_tan_3pi_8 = (a > float_v(2.414213562373095f));
    const float_m &gt_tan_pi_8  = (a > float_v(0.4142135623730950f)) && (!gt_tan_3pi_8);
    float_v b(Vc::Zero);
    b(gt_tan_3pi_8) = pi/2.f;
    b(gt_tan_pi_8) = pi/4.f;
    a(gt_tan_3pi_8) =  (-1.f / a);
    a(gt_tan_pi_8) =  ((absY - absX) / (absY + absX)) ;
    const float_v &a2 = a * a;
    b += (((8.05374449538e-2f * a2
          - 1.38776856032E-1f) * a2
          + 1.99777106478E-1f) * a2
          - 3.33329491539E-1f) * a2 * a
          + a;
    b(xNeg ^ yNeg) = -b;
    b(xNeg && !yNeg) = (b+pi);
    b(xNeg &&  yNeg)  = (b-pi);
    b(xZero && yZero) = zero;
    b(xZero &&  yNeg) = (-pi/2.f);
    return b;
  }
  template<typename T> static inline __attribute__((always_inline)) 
    typename Vc::Vector<T>::Mask Finite(const Vc::Vector<T> &x) { return Vc::isfinite( x ); }
  template<typename T> static inline __attribute__((always_inline)) T Log ( const T &x ) { return std::log( x ); }
  template<typename T> static inline __attribute__((always_inline)) T ACos( const T &x ) { return (3.1415926535897f/2.f - asin( x )); }
}

#endif 
