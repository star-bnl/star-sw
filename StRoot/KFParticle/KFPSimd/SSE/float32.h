// -*- C++ Header -*-
/*
==================================================
Authors: A.Mithran;
Emails: mithran@fias.uni-frankfurt.de
==================================================
*/

#ifndef SIMD_SSE_FLOAT32_H
#define SIMD_SSE_FLOAT32_H

#include "../Utils/macros.h"
#include "../Utils/tag.h"
#include "int32.h"
#include "mask32.h"
#include "constants.h"

#include <cstdint>
#include <string>
#include <cassert>

namespace KFP {
namespace SIMD {

class Float32_128
{
public:
    typedef float value_type;
    typedef __m128 simd_type;
    static constexpr Tag tag{ Tag::SSE };

    // ------------------------------------------------------
    // Constructors
    // ------------------------------------------------------
    // Default constructor:
    Float32_128() {
        m_data = _mm_setzero_ps();
    }
    Float32_128(UninitializeTag) {}
    // Constructor to broadcast the same value into all elements:
    Float32_128(float val)
    {
        m_data = _mm_set1_ps(val);
    }
    Float32_128(const __m128& val_simd)
    {
        m_data = val_simd;
    }
    Float32_128(const float* val_ptr)
    {
        m_data = _mm_loadu_ps(val_ptr);
    }
    Float32_128(const Float32_128& class_simd) = default;

    // Assignment constructors:
    Float32_128& operator=(float val)
    {
        m_data = _mm_set1_ps(val);
        return *this;
    }
    Float32_128& operator=(const __m128& val_simd)
    {
        m_data = val_simd;
        return *this;
    }
    Float32_128& operator=(const Float32_128& class_simd) = default;

    // ------------------------------------------------------
    // Load and Store
    // ------------------------------------------------------
    // Member function to load from array (unaligned)
    KFP_SIMD_INLINE Float32_128& loadUnaligned(const float* val_ptr)
    {
        m_data = _mm_loadu_ps(val_ptr);
        return *this;
    }
    // Member function to load from array (aligned)
    KFP_SIMD_INLINE Float32_128& load(const float* val_ptr)
    {
        m_data = _mm_load_ps(val_ptr);
        return *this;
    }
    // Member function to store into array (unaligned)
    KFP_SIMD_INLINE void storeUnaligned(float* val_ptr) const
    {
        _mm_storeu_ps(val_ptr, m_data);
    }
    // Member function storing into array (aligned)
    KFP_SIMD_INLINE void store(float* val_ptr) const
    {
        _mm_store_ps(val_ptr, m_data);
    }

    // ------------------------------------------------------
    // Gather and Scatter
    // ------------------------------------------------------
    KFP_SIMD_INLINE Float32_128& gather(const float* val_ptr, const Int32_128& index)
    {
        alignas(SimdSize) std::int32_t
        indices[SimdLen]{}; // Helper indices array
        index.store(indices);
        m_data = _mm_setr_ps(
            val_ptr[indices[0]], val_ptr[indices[1]],
            val_ptr[indices[2]], val_ptr[indices[3]]
        );
        return *this;
    }
    KFP_SIMD_INLINE void scatter(float* val_ptr, const Int32_128& index) const
    {
        alignas(SimdSize) float
        data[SimdLen]{}; // Helper data array
        store(data);

        alignas(SimdSize) std::int32_t
        indices[SimdLen]{}; // Helper indices array
        index.store(indices);

        val_ptr[indices[0]] = data[0];
        val_ptr[indices[1]] = data[1];
        val_ptr[indices[2]] = data[2];
        val_ptr[indices[3]] = data[3];
    }

    // ------------------------------------------------------
    // Data member accessors
    // ------------------------------------------------------
    KFP_SIMD_INLINE __m128& simd()
    {
        return m_data;
    }
    KFP_SIMD_INLINE const __m128& simd() const
    {
        return m_data;
    }
    template <int N>
    KFP_SIMD_INLINE float get() const {
        static_assert(N >= 0,
        "[Error] (Float32_128::get): Invalid value of index N. Negative");
        static_assert(N < SimdLen,
        "[Error] (Float32_128::get): Invalid value of index N. Out of range.");
        return _mm_extract_ps(m_data, N);
    }
    KFP_SIMD_INLINE float operator[](int index) const
    {
        assert((index >= 0) && (index < int(SimdLen)));
        alignas(SimdSize) float
        data[SimdLen]{}; // Helper array
        store(data);
        return data[index];
    }

    // ------------------------------------------------------
    // Data lanes manipulation
    // ------------------------------------------------------
    KFP_SIMD_INLINE friend Float32_128 select(const Mask32_128& mask, const Float32_128& a,
                                       const Float32_128& b) {
        return _mm_blendv_ps(b.m_data, a.m_data, mask.simdf());
    }

    KFP_SIMD_INLINE Float32_128 multiplySign(const Float32_128& a) const
    {
        const __m128 sign(
            _mm_and_ps(_mm_castsi128_ps(_mm_set1_epi32(0x80000000)), a.m_data)
        );
        return Float32_128{ _mm_xor_ps(m_data, sign) };
    }
    template<int N>
    KFP_SIMD_INLINE Float32_128 rotate() const
    {
        if (N < 0) {
            constexpr int num_shift = (-N) % SimdLen;
            constexpr int left_shift_bytes = num_shift*4;
            constexpr int right_shift_bytes = (SimdLen - num_shift)*4;
            const __m128 left = _mm_castsi128_ps(
                _mm_bsrli_si128(_mm_castps_si128(m_data), left_shift_bytes)
            );
            const __m128 right = _mm_castsi128_ps(
                _mm_bslli_si128(_mm_castps_si128(m_data), right_shift_bytes)
            );
            return Float32_128{_mm_or_ps(left, right)};
        } else {
            constexpr int num_shift = N % SimdLen;
            constexpr int left_shift_bytes = (SimdLen - num_shift)*4;
            constexpr int right_shift_bytes = num_shift*4;
            const __m128 left = _mm_castsi128_ps(
                _mm_bsrli_si128(_mm_castps_si128(m_data), left_shift_bytes)
            );
            const __m128 right = _mm_castsi128_ps(
                _mm_bslli_si128(_mm_castps_si128(m_data), right_shift_bytes)
            );
            return Float32_128{_mm_or_ps(left, right)};
        }
    }

    // ------------------------------------------------------
    // Rounding
    // ------------------------------------------------------

    KFP_SIMD_INLINE friend Float32_128 round(const Float32_128& a)
    { 
        return _mm_round_ps(a.m_data, (_MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC));
    }
    
    KFP_SIMD_INLINE friend Float32_128 trunc(const Float32_128& a)
    {
        return _mm_round_ps(a.m_data, (_MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC));
    }


    // ------------------------------------------------------
    // Basic Arithmetic
    // ------------------------------------------------------
    friend Float32_128 operator-(const Float32_128& a)
    {
        return Float32_128{ _mm_sub_ps(_mm_setzero_ps(), a.m_data) };
    }
    friend Float32_128 operator+(const Float32_128& a,
                                   const Float32_128& b)
    {
        return Float32_128{
            _mm_add_ps(a.m_data, b.m_data)
        };
    }
    Float32_128& operator+=(const Float32_128& a)
    {
        *this = *this + a;
        return *this;
    }
    friend Float32_128 operator-(const Float32_128& a,
                                   const Float32_128& b)
    {
        return Float32_128{
            _mm_sub_ps(a.m_data, b.m_data)
        };
    }
    Float32_128& operator-=(const Float32_128& a)
    {
        *this = *this - a;
        return *this;
    }
    friend Float32_128 operator*(const Float32_128& a,
                                   const Float32_128& b)
    {
        return Float32_128{
            _mm_mul_ps(a.m_data, b.m_data)
        };
    }
    Float32_128& operator*=(const Float32_128& a)
    {
        *this = *this * a;
        return *this;
    }
    friend Float32_128 operator/(const Float32_128& a,
                                   const Float32_128& b)
    {
        return Float32_128{
            _mm_div_ps(a.m_data, b.m_data)
        };
    }
    Float32_128& operator/=(const Float32_128& a)
    {
        *this = *this / a;
        return *this;
    }

    // Comparison (mask returned)
    friend Mask32_128 operator<(const Float32_128& a,
                                   const Float32_128& b)
    {
        Mask32_128 result{UninitializeTag{}};
        result.m_data = _mm_castps_si128(
            _mm_cmplt_ps(a.m_data, b.m_data)
        );
        return result;
    }
    friend Mask32_128 operator<=(const Float32_128& a,
                                    const Float32_128& b)
    {
        Mask32_128 result{UninitializeTag{}};
        result.m_data = _mm_castps_si128(
            _mm_cmple_ps(a.m_data, b.m_data)
        );
        return result;
    }
    friend Mask32_128 operator>(const Float32_128& a,
                                   const Float32_128& b)
    {
        Mask32_128 result{UninitializeTag{}};
        result.m_data = _mm_castps_si128(
            _mm_cmpgt_ps(a.m_data, b.m_data)
        );
        return result;
    }
    friend Mask32_128 operator>=(const Float32_128& a,
                                    const Float32_128& b)
    {
        Mask32_128 result{UninitializeTag{}};
        result.m_data = _mm_castps_si128(
            _mm_cmpge_ps(a.m_data, b.m_data)
        );
        return result;
    }
    friend Mask32_128 operator==(const Float32_128& a,
                                    const Float32_128& b)
    {
        Mask32_128 result{UninitializeTag{}};
        result.m_data = _mm_castps_si128(
            _mm_cmpeq_ps(a.m_data, b.m_data)
        );
        return result;
    }
    friend Mask32_128 operator!=(const Float32_128& a,
                                    const Float32_128& b)
    {
        Mask32_128 result{UninitializeTag{}};
        result.m_data = _mm_castps_si128(
            _mm_cmpneq_ps(a.m_data, b.m_data)
        );
        return result;
    }

    KFP_SIMD_INLINE friend Float32_128 min(const Float32_128& a, const Float32_128& b)
    {
        return _mm_min_ps(a.m_data, b.m_data);
    }

    KFP_SIMD_INLINE friend Float32_128 max(const Float32_128& a, const Float32_128& b)
    {
        return _mm_max_ps(a.m_data, b.m_data);
    }

    KFP_SIMD_INLINE friend Float32_128 abs(const Float32_128& a)
    {
        return _mm_and_ps(a.m_data, _mm_castsi128_ps(_mm_set1_epi32(0x7FFFFFFF)));
    }

    KFP_SIMD_INLINE friend Float32_128 sqrt(const Float32_128& a)
    {
        return _mm_sqrt_ps(a.m_data);
    }

    // ------------------------------------------------------
    // Float checks
    // ------------------------------------------------------

    KFP_SIMD_INLINE friend Mask32_128 isNan(const Float32_128& a)
    {
        Mask32_128 result(UninitializeTag{});
        result.m_data = _mm_castps_si128(_mm_cmpunord_ps(a.m_data, a.m_data));
        return result;
    }

    KFP_SIMD_INLINE friend Mask32_128 isFinite(const Float32_128& a)
    {
        Mask32_128 result(UninitializeTag{});
        result.m_data = _mm_castps_si128(_mm_cmpord_ps(a.m_data, _mm_mul_ps(_mm_setzero_ps(), a.m_data)));
        return result;
    }


private:
    alignas(SimdSize) __m128 m_data;
};

} // namespace SIMD
} // namespace KFP

#endif // !SIMD_SSE_FLOAT32_H
