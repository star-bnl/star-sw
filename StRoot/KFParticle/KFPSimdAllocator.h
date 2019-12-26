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

#ifndef KFPSimdAllocator_H
#define KFPSimdAllocator_H

#include <Vc/Vc>

/** @class KFPSimdAllocator
 ** @brief Allocator which is needed to allocate memory in std::vector aligned by the size of SIMD vectors.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **/

template <class T>
class KFPSimdAllocator {
 public:
 // type definitions
  typedef T        value_type;
  typedef T*       pointer;
  typedef const T* const_pointer;
  typedef T&       reference;
  typedef const T& const_reference;
  typedef std::size_t    size_type;
  typedef std::ptrdiff_t difference_type;

  /** @class rebind
   ** @brief Rebind allocator to type U of the SIMD allocator.
   ** @author  M.Zyzak, I.Kisel
   ** @date 05.02.2019
   ** @version 1.0
   **/
  template <class U>
  struct rebind {
    typedef KFPSimdAllocator<U> other;
  };

  /** Return address of "value". */
  pointer address (reference value) const {
    return &value;
  }
  /** Return address of "value". */
  const_pointer address (const_reference value) const {
    return &value;
  }

  /* constructors and destructor
        * - nothing to do because the allocator has no state
  */
  KFPSimdAllocator() throw() { }
  KFPSimdAllocator(const KFPSimdAllocator&) throw() {  }
  template <class U>
  KFPSimdAllocator (const KFPSimdAllocator<U>&) throw() {  }
  virtual ~KFPSimdAllocator() throw() {  }

  /** Return maximum number of elements that can be allocated. */
  size_type max_size () const throw() {
    return std::numeric_limits<std::size_t>::max() / sizeof(T);
  }

  /** Allocate but don't initialize num elements of type T. */
  pointer allocate (size_type num, const void* = 0) {
//               print message and allocate memory with global new
    pointer ret = reinterpret_cast<pointer>( /*T::*/operator new(num*sizeof(T)) );
    return ret;
  }

  /** Initialize elements of allocated storage "p" with an empty element. */
  void construct (pointer p) {
  // initialize memory with placement new
    new(p) T();
  }
  
  /** Initialize elements of allocated storage "p" with value "value". */
  void construct (pointer p, const T& value) {
    new(p) T(value);
  }

  /** Destroy elements of initialized storage "p". */
  void destroy (pointer p) {
  // destroy objects by calling their destructor
    p->~T();
  }

  /** Deallocate storage p of deleted elements. */
  void deallocate (pointer p, size_type num) {
  // print message and deallocate memory with global delete
    /*T::*/operator delete(static_cast<void*>(p), num*sizeof(T));

  }

  void *operator new(size_t size, void *ptr) { return ::operator new(size, ptr);}      ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new[](size_t size, void *ptr) { return ::operator new(size, ptr);}    ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new(size_t size) { return _mm_malloc(size, sizeof(Vc::float_v)); }    ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new[](size_t size) { return _mm_malloc(size, sizeof(Vc::float_v)); }  ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void operator delete(void *ptr, size_t) { _mm_free(ptr); }                           ///< delete operator for the SIMD-alligned dynamic memory release
  void operator delete[](void *ptr, size_t) { _mm_free(ptr); }                         ///< delete operator for the SIMD-alligned dynamic memory release
}; // KFPSimdAllocator
      
#endif //KFPSimdAllocator
