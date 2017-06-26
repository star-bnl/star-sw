#ifndef __Mortran_h__
#define __Mortran_h__

#include <vector>
#include <assert.h>
#include <iostream>
#include "TString.h"

#if 1
// =================================================================================
// An array type which can accept both Fortran and c++ indexing.
template <typename T> class Array_t
{

 public:
  Array_t( int n=1, int m=1 );     /// 2D constructor
  Array_t( const Array_t &other ); /// copy constructor

  virtual ~Array_t(){ /* nada */ };

  int n() const { return _n; }
  int m() const { return _m; }

  const T &operator[]( int i ) const { return at(i); }
  const T &operator()( int i ) const { return at(i-_min); }
  const T &operator()( int i, int j ) const { return at(i-1,j-1); }  /// 2D Fortran-like array

  T &operator[]( int i ){ return at(i); }
  T &operator()( int i ){ return at(i-_min); }      /// 1D Fortran-like array
  T &operator()( int i, int j ) { return at(i-1,j-1); }  /// 2D Fortran-like array

  const T &at( int i, int j ) const { return at(i + _n*j);    }             /// 2D c-style accessor
  const T &at( int i )        const { 
    if ( i>=size() or i<0 )
      {
	std::cout << Form("Array_t out of bounds error for array at address %p",this) << std::endl;
	assert(0);
      } return mData[i]; }             /// 1D c-style accessor

  T &at( int i, int j )  { return at(i + _n*j);    }             /// 2D c-style accessor
  T &at( int i )         { 
    if ( i>=size() or i<0 )
      {
	std::cout << Form("Array_t out of bounds error for array at address %p",this) << std::endl;
	assert(0);
      } return mData[i]; }             /// 1D c-style accessor

  Int_t size() const { return _size; }

  void SetRange( Int_t mn, Int_t mx ){
    _min=mn;
    _max=mx;
    _size=mx-mn+1;
    mData.resize(size());
  } /// Sets the range of 1D fortran-like arrays

  std::ostream &Out( std::ostream &out );
  
 private:
 protected:

  std::vector<T> mData;
  Int_t _size;
  Int_t _n, _m;
  Int_t _min, _max;

   
};

template <typename T> std::ostream &operator<<( std::ostream &out, Array_t<T> &array ){ return array.Out(out); }

template <typename T> Array_t<T>::Array_t(int n, int m) : _size(n*m), _n(n), _m(m)
{
  mData.resize( size() );
  SetRange( 1, size()+1 ); // fortran 1D range
}

template <typename T> Array_t<T>::Array_t( const Array_t &other )
{
  mData = other.mData;
  _size = other._size;
  _n    = other._n;
  _m    = other._m;
  _min  = other._min;
  _max  = other._max;
}

template <typename T> std::ostream &Array_t<T>::Out( std::ostream &out )
{
  out << "["; 
  for ( int i=0;i<_m;i++ ) 
    {
      //out << (_m>1)?"[ ":" "; 
      if ( _m>1 ) out << "["; else out << " ";

      for ( int j=0;j<_n;j++ )
	{
	  out << at(j+1,i+1) << ", ";
	}
      //out << (_m>1)?"] ":" ";
      if ( _m>1 ) out << "]"; else out << " ";
    }
  out << "]" <<std::endl;
  return out;
}

#endif

// ---------------------------------------------------------------------------------
// USE operator to select a structure from a list of structures
#define USE(STRUCT,VAR,VAL)			\
  {						\
    UInt_t _i=0, _n = _fill_##STRUCT.size();	\
    while ( 1 ) {				\
      STRUCT = _fill_##STRUCT[_i++];		\
      if ( STRUCT.VAR == VAL ) break;		\
      assert( _i <= _n );			\
    }						\
  }						\
    
#endif


// ----------------------------------------------------------------------------------
// Swallow some "Ag" calls
#define AGSSTEP(x) { /* call AgsSTEP(x) */ }
