//---------------------------------------------------------------------------------
// The KFParticleBaseSIMD class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   13.05.07
// 
// Class to reconstruct and store the decayed particle parameters.
// The method is described in CBM-SOFT note 2007-003, 
// ``Reconstruction of decayed particles based on the Kalman filter'', 
// http://www.gsi.de/documents/DOC-2007-May-14-1.pdf
//
// This class describes general mathematics which is used by KFParticle class
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//_________________________________________________________________________________


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
// #include "KFPVector.h"
// typedef KFPVector<float> kfvector_float;
// typedef KFPVector<int> kfvector_int;
// typedef KFPVector<unsigned int> kfvector_uint;

namespace KFPMath
{
  static inline __attribute__((always_inline)) float_v Sin  ( const float_v &phi ) 
  {
    const float_v pi(3.1415926535897932f);
    const float_v nTurnsF = (phi + pi) / (float_v(2.f)*pi);
    int_v nTurns = int_v( nTurnsF );
    nTurns( (nTurns<=int_v(Vc::Zero)) && int_m(phi<-pi)) -= 1;
    
    const float_v& x = phi - float_v(nTurns)*(float_v(2.f)*pi);
    
    const float_v& B = 4.f/pi;
    const float_v& C = -B/pi;

    float_v y = (B + C * abs(x)) * x;

    const float_v& P = 0.218f;
    y = P * (y * abs(y) - y) + y;
    
    return y;
    
//     return sin(phi);
  }
  static inline __attribute__((always_inline)) float_v Cos  ( const float_v &phi )
  {     
    return Sin( phi + 1.570796326795f ); //x + pi/2
//     return cos(phi);
  }
  static inline __attribute__((always_inline)) float_v ATan2( const float_v &y, const float_v &x )
  { 
    const float_v pi(3.1415926535897932f);
    const float_v zero(Vc::Zero);

    const float_m &xZero = (x == zero);
    const float_m &yZero = (y == zero);
    const float_m &xNeg  = (x < zero);
    const float_m &yNeg  = (y < zero);

    const float_v &absX = abs(x);
    const float_v &absY = abs(y);

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
