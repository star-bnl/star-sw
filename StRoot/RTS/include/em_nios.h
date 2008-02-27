#ifndef _EN_NIOS_HH_
#define _EN_NIOS_HH_
// NIOS device support from linux
//



#include <stdlib.h>

typedef unsigned int UINT32;
typedef unsigned short UINT16;
typedef unsigned char UINT8;

// Prototypes the fiber read/write calls.
template<class T> void nios_write(UINT32 address, T val);
template<class T> T nios_read(UINT32 address);
// each of the following has identical results, however the chunksize of the copy
// is sizeof(T).   Returns number of bytes copied.
template<class T> UINT32 nios_copyto(UINT32 address, UINT32 src, UINT32 bytes);
template<class T> UINT32 nios_copyfrom(UINT32 address, UINT32 src, UINT32 bytes);


// Test functions for use in linux testing.
#ifndef REAL_NIOS_WRITE_FUNCTIONS
template<class T> void nios_write(UINT32 address, T val)
{
  T *addr = (T *)address;
  *addr = val;  
}


template<class T> T nios_read(UINT32 address)
{
  T *addr = (T *)address;
  T val = *addr;
  return val;
}

template<class T> UINT32 nios_copyto(UINT32 dest, UINT32 src, UINT32 sz)
{
  memcpy((UINT32 *)dest, (UINT32 *)src, sz);
  return sz;
}

template<class T> UINT32 nios_copyfrom(UINT32 dest, UINT32 src, UINT32 sz)
{
  memcpy((UINT32 *)dest, (UINT32 *)src, sz);
  return sz;
}
#endif



//#define LOCAL_NIOS

// LOCAL_NIOS is designed to be used from the NIOS itself.
// In this case, the nios_dev class is only needed to transparently 
// wrap the nios_read/write calls.

// From LINUX, LOCAL_NIOS is not set.  Here, I use some memory equal to the size 
// of the NIOS device being mapped, although this should be transparent to the user
//


#ifndef LOCAL_NIOS

template<class T> class nios_int {
 public:
  T ptr;

  T operator=(T x)
    {
      nios_write<T>(ptr, x);
      return x;
    }
  
  operator T() const 
    { 
      T x = nios_read<T>(ptr);
      return x;
    } 
  
};

template<class T> class nios_device {
 public:
  UINT32 base_addr;
  UINT32 size;
  nios_int<T> *shadow;

  nios_device<T>(UINT32 base, UINT32 size) { 
    
    base_addr = base; 
    shadow = NULL;
    shadow = (nios_int<T> *)malloc(size*sizeof(nios_int<T>));
    
    for(int i=0;i<size;i++) {
      shadow[i].ptr = base + sizeof(T) * i;
    }
  };
  
  ~nios_device<T>() {
    if(shadow) free(shadow);
  }
  
  nios_int<T> &operator[](int i) { 
    return shadow[i];
  }

  UINT32 copyto(nios_int<T> *dest, T *src, UINT32 bytes) {
    return nios_copyto<T>(dest->ptr, (UINT32)src, bytes);
  }

  UINT32 copyto(UINT32 dest, T *src, UINT32 bytes) {
    return nios_copyto<T>(dest+base_addr,(UINT32)src,bytes);
  }

  UINT32 copyfrom(T *dest, nios_int<T> *src, UINT32 bytes) {
    return nios_copyfrom<T>((UINT32)dest, src->ptr, bytes);
  }

  UINT32 copyfrom(T * dest, UINT32 src, UINT32 bytes) {
    return nios_copyfrom<T>(dest, src+base_addr, bytes);
  }
};      // Over fiber


#else   // LOCAL_NIOS

template<class T> class nios_int {
 public:
  T val;

  T operator=(T x)
    {
      char *t = (char *)this;        // don't want to use the int cast!
      nios_write<T>((UINT32)t, x);
      return x;
    }
  
  operator T() const 
    { 
      T x;
      char *t = (char *)this;
      x = nios_read<T>((UINT32)t);
      return x;
    } 
};

template<class T> class nios_device {
 public:
  UINT32 base_addr;
  nios_int<T> *shadow;

  nios_device(UINT32 base, UINT32) { 
    base_addr = base; 
    shadow = (nios_int<T> *)base;
  };
  
  
  nios_int<T> &operator[](int i) { 
    return shadow[i];
  }

  UINT32 copyto(nios_int<T> *dest, T *src, UINT32 bytes) {
    return nios_copyto<T>((UINT32)dest, (UINT32)src, bytes);
  }

  UINT32 copyto(UINT32 dest, T *src, UINT32 bytes) {
    return nios_copyto<T>(dest+base_addr,(UINT32)src,bytes);
  }

  UINT32 copyfrom(T *dest, nios_int<T> *src, UINT32 bytes) {
    return nios_copyfrom<T>((UINT32)dest, (UINT32)src, bytes);
  }

  UINT32 copyfrom(T * dest, UINT32 src, UINT32 bytes) {
    return nios_copyfrom<T>(dest, src+base_addr, bytes);
  }
};

#endif  // over fiber

#endif
