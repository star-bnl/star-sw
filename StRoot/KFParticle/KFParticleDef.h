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

#include "KFPSimd/simd.h"

using float32_v = KFP::SIMD::simd_float;
using int32_v = KFP::SIMD::simd_int;
using mask32_v = KFP::SIMD::simd_mask;
using KFP::SIMD::SimdLen;
using KFP::SIMD::SimdSize;

// using ::Vc::float32_v;
// using ::Vc::double_v;
// using ::Vc::float32_v;
// using ::Vc::int32_v;
// using ::Vc::VectorAlignment;
// using ::Vc::double_m;
// using ::Vc::float_m;
// using ::Vc::int_m;
// using ::Vc::uint_m;
// using ::Vc::atan2;
// using ::Vc::asin;
// using ::Vc::round;
// using ::Vc::isfinite;

#ifdef VC_VERSION_NUMBER
#if VC_VERSION_NUMBER < VC_VERSION_CHECK(1,0,0)
template <typename To, typename From> To simd_cast(From &&x) { return static_cast<To>(x); }
#endif
#elif defined(Vc_VERSION_NUMBER)
#if Vc_VERSION_NUMBER < Vc_VERSION_CHECK(1,0,0)
template <typename To, typename From> To simd_cast(From &&x) { return static_cast<To>(x); }
#endif
#endif

#if defined(HLTCA_STANDALONE)
typedef unsigned char UChar_t;
typedef UChar_t Byte_t;
typedef int Int_t;
typedef double Double_t;
#else
#include "Rtypes.h"
#endif

#include "KFPSimdAllocator.h"    
typedef std::vector<float32_v, KFPSimdAllocator<float32_v> > kfvector_floatv;

typedef std::vector<float, KFPSimdAllocator<float> > kfvector_float;
typedef std::vector<int, KFPSimdAllocator<int> > kfvector_int;

#endif 
