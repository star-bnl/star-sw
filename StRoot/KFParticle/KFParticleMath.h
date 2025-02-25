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
  static inline __attribute__((always_inline)) float32_v multiplySign(const int32_v& sign, const float32_v& value)
  {
    const __m128 s = _mm_and_ps(_mm_castsi128_ps(_mm_set1_epi32(0x80000000)), _mm_castsi128_ps(sign.simd()));
    return float32_v(_mm_xor_ps(value.simd(), s));
  }

  static inline __attribute__((always_inline)) float32_v sinSeries(const float32_v x) {
    const float32_v x2 = x*x;
    float32_v y = -1.984126984e-4f;
    y = y * x2 + 8.333333333e-3f;
    y = y * x2 - 1.666666667e-1f;
    return y * (x2 * x) + x;
  }

  static inline __attribute__((always_inline)) float32_v cosSeries(const float32_v x) {
    const float32_v x2 = x*x;
    float32_v y = 2.48015873e-5f;
    y = y * x2 - 1.388888889e-03f;
    y = y * x2 + 4.166666667e-2f;
    return y * (x2 * x2) - .5f * x2 + 1.f;
  }

  static inline __attribute__((always_inline)) void sincos(float32_v x, float32_v& sinX, float32_v& cosX) {
//     const float32_v sin0 = sin(x);
//     const float32_v cos0 = cos(x);

    const float32_v pi2i = 6.36619772e-1f;
    const int32_v nPi2 = KFP::SIMD::toInt( round(x*pi2i) );
    const int32_v q = nPi2 & 3;
    const int32_v sinSign = q << 30;
    const int32_v cosSign = (q+1) << 30;
    
    const float32_v nPi2f = KFP::SIMD::toFloat(nPi2);
    x -= 1.5707969666f * nPi2f;
    x += 6.3975784e-7f * nPi2f;
    
    const float32_v sinS = sinSeries(x);
    const float32_v cosS = cosSeries(x);
    
    const mask32_v mask = (q == 0) || (q == 2);
    sinX = multiplySign(sinSign, select(mask, sinS, cosS));
    cosX = multiplySign(cosSign, select(mask, cosS, sinS));

//     for(int i=0; i<4; i++) {
//       if(fabs(sinX[i] - sin0[i]) > 1.e-7f * fabs(sin0[i]) || fabs(cosX[i] - cos0[i]) > 1.e-7f * fabs(cos0[i]) ) {
//         std::cout << "x " << x << "   npi2 " << nPi2 << std::endl;
//         std::cout << "sin " << sinX << "     " << sin0 << " " << sin(x) << std::endl;
//         std::cout << "cos " << cosX << "     " << cos0 << " " << cos(x) << std::endl;
//         std::cin.get();
//       }
//     }
  }
  
  static inline __attribute__((always_inline)) float32_v ATan2( const float32_v &y, const float32_v &x )
  { 
    const float32_v pi(3.1415926535897932f);
    const float32_v zero(0.f);

    const mask32_v &xZero = (x == zero);
    const mask32_v &yZero = (y == zero);
    const mask32_v &xNeg  = (x < zero);
    const mask32_v &yNeg  = (y < zero);

    const float32_v &absX = abs(x);
    const float32_v &absY = abs(y);

    float32_v a = absY / absX;
    const mask32_v &gt_tan_3pi_8 = (a > float32_v(2.414213562373095f));
    const mask32_v &gt_tan_pi_8  = (a > float32_v(0.4142135623730950f)) && (!gt_tan_3pi_8);
    float32_v b = select(gt_tan_3pi_8, pi/2.f, 0.f);
    b = select(gt_tan_pi_8, pi/4.f, b);
    a = select(gt_tan_3pi_8, (-1.f / a), a);
    a = select(gt_tan_pi_8, ((absY - absX) / (absY + absX)), a) ;
    const float32_v &a2 = a * a;
    b += (((8.05374449538e-2f * a2
          - 1.38776856032E-1f) * a2
          + 1.99777106478E-1f) * a2
          - 3.33329491539E-1f) * a2 * a
          + a;
    b = select(xNeg ^ yNeg, -b, b);
    b = select(xNeg && !yNeg, (b+pi), b);
    b = select(xNeg &&  yNeg, (b-pi), b);
    b = select(xZero && yZero, zero, b);
    b = select(xZero &&  yNeg, (-pi/2.f), b);
    return b;
  }

  static inline __attribute__((always_inline)) float32_v log(const float32_v x)
  {
    return x;
  }

  static inline __attribute__((always_inline)) float32_v acos(const float32_v x)
  {
    return x;
  }

  // template<typename T> static inline __attribute__((always_inline)) T Log ( const T &x ) { return std::log( x ); }
  // template<typename T> static inline __attribute__((always_inline)) T ACos( const T &x ) { return (3.1415926535897f/2.f - asin( x )); }


}

#endif // KFParticleMath_H
