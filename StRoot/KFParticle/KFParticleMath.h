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


#ifndef KFParticleMath_H
#define KFParticleMath_H

#include "KFParticleDef.h"

namespace KFPMath
{
  static inline __attribute__((always_inline)) float_v multiplySign(const int_v& sign, const float_v& x) {
    const uint_v& signU = reinterpret_cast<const uint_v&>(sign);
    const uint_v& xU = reinterpret_cast<const uint_v&>(x);
    const uint_v signMask(0x80000000U);
    const uint_v& outU = (signU & signMask) ^ xU;
    const float_v& out = reinterpret_cast<const float_v&>(outU);
    return out;
  }

  static inline __attribute__((always_inline)) float_v sinSeries(const float_v x) {
    const float_v x2 = x*x;
    float_v y = -1.984126984e-4f;
    y = y * x2 + 8.333333333e-3f;
    y = y * x2 - 1.666666667e-1f;
    return y * (x2 * x) + x;
  }

  static inline __attribute__((always_inline)) float_v cosSeries(const float_v x) {
    const float_v x2 = x*x;
    float_v y = 2.48015873e-5f;
    y = y * x2 - 1.388888889e-03f;
    y = y * x2 + 4.166666667e-2f;
    return y * (x2 * x2) - .5f * x2 + 1.f;
  }

  static inline __attribute__((always_inline)) void sincos(float_v x, float_v& sinX, float_v& cosX) {
#if 1
//     const float_v sin0 = sin(x);
//     const float_v cos0 = cos(x);

    const float_v pi2i = 6.36619772e-1f;
    const int_v nPi2 = simd_cast<int_v>( Vc::round(x*pi2i) );
    const int_v q = nPi2 & 3;
    const int_v sinSign = q << 30;
    const int_v cosSign = (q+1) << 30;
    
    const float_v nPi2f = simd_cast<float_v>(nPi2);
    x -= 1.5707969666f * nPi2f;
    x += 6.3975784e-7f * nPi2f;
    
    const float_v sinS = sinSeries(x);
    const float_v cosS = cosSeries(x);
    
    const float_m mask = simd_cast<float_m>(q == 0 || q == 2);
    sinX = multiplySign( sinSign, iif(mask, sinS, cosS) );
    cosX = multiplySign( cosSign, iif(mask, cosS, sinS) );

//     for(int i=0; i<4; i++) {
//       if(fabs(sinX[i] - sin0[i]) > 1.e-7f * fabs(sin0[i]) || fabs(cosX[i] - cos0[i]) > 1.e-7f * fabs(cos0[i]) ) {
//         std::cout << "x " << x << "   npi2 " << nPi2 << std::endl;
//         std::cout << "sin " << sinX << "     " << sin0 << " " << sin(x) << std::endl;
//         std::cout << "cos " << cosX << "     " << cos0 << " " << cos(x) << std::endl;
//         std::cin.get();
//       }
//     }
#else
    const float_v sin0 = sin(x);
    const float_v cos0 = cos(x);
    
    constexpr double pi2 = 1.5707963267948966192313216916398;
    constexpr double pi2i = 0.63661977236758134307553505349006;
    
    typedef Vc::SimdArray<double, 4> TD;
    TD xd = simd_cast<TD>(x);
    TD nPi2 = round(xd * pi2i);
    x = simd_cast<float_v>(xd - nPi2 * pi2);
    const int_v q = simd_cast<int_v>(nPi2) & 3;
    
    const int_v sinSign = q << 30;
    const int_v cosSign = (q+1) << 30;

    const float_v sinS = sinSeries(x);
    const float_v cosS = cosSeries(x);
    
    const float_m mask = simd_cast<float_m>(q == 0 || q == 2);
    sinX = multiplySign( sinSign, iif(mask, sinS, cosS) );
    cosX = multiplySign( cosSign, iif(mask, cosS, sinS) );
    
    for(int i=0; i<4; i++) {
      if(fabs(sinX[i] - sin0[i]) > 1.e-7f * fabs(sin0[i]) || fabs(cosX[i] - cos0[i]) > 1.e-7f * fabs(cos0[i]) ) {
        std::cout << "x " << x << "   npi2 " << nPi2 << std::endl;
        std::cout << "sin " << sinX << "     " << sin0 << " " << sin(x) << std::endl;
        std::cout << "cos " << cosX << "     " << cos0 << " " << cos(x) << std::endl;
        std::cin.get();
      }
    }
#endif
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

#endif // KFParticleMath_H
