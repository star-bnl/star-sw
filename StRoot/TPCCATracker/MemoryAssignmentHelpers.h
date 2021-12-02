#ifndef MEMORYASSIGNMENTHELPERS_H
#define MEMORYASSIGNMENTHELPERS_H

#include <assert.h>

template<unsigned int X>
static inline void AlignTo( char *&mem )
{
  STATIC_ASSERT( ( X & ( X - 1 ) ) == 0, X_needs_to_be_a_multiple_of_2 );
  const int offset = reinterpret_cast<unsigned long>( mem ) & ( X - 1 );
  if ( offset > 0 ) {
    mem += ( X - offset );
  }
  //assert( ( reinterpret_cast<unsigned long>( mem ) & ( X - 1 ) ) == 0 );
}

template<unsigned int X>
static inline unsigned int NextMultipleOf( unsigned int value )
{
  STATIC_ASSERT( ( X & ( X - 1 ) ) == 0, X_needs_to_be_a_multiple_of_2 );
  const int offset = value & ( X - 1 );
  if ( offset > 0 ) {
    return value + X - offset;
  }
  return value;
}

template<typename T, unsigned int Alignment> static T *AssignMemory( char *&mem, unsigned int size )
{
  STATIC_ASSERT( ( Alignment & ( Alignment - 1 ) ) == 0, Alignment_needs_to_be_a_multiple_of_2 );
  AlignTo<Alignment> ( mem );
  T *r = new( mem ) T[size];
  mem += size * sizeof( T );
  return r;
}

template<typename T, unsigned int Alignment> static inline T *AssignMemory( char *&mem, unsigned int stride, unsigned int count )
{
  assert( 0 == ( stride & ( Alignment - 1 ) ) );
  return AssignMemory<T, Alignment>( mem, stride * count );
}

template<typename T, unsigned int Alignment> static T *_assignMemory( char *&mem, unsigned int size )
{
  STATIC_ASSERT( ( Alignment & ( Alignment - 1 ) ) == 0, Alignment_needs_to_be_a_multiple_of_2 );
  AlignTo<Alignment>( mem );
  T *r = new( mem ) T[size];
  mem += size * sizeof( T );
  return r;
}

template<typename T> static inline void AssignMemoryV( T *&dst, char *&mem, int count )
{
  dst = _assignMemory<T, ( sizeof( T ) & ( sizeof( T ) - 1 ) ) == 0 && sizeof( T ) <= 16 ? sizeof( T ) : sizeof( float_v )>( mem, count );
}

template<typename T> static inline void AssignMemory( T *&dst, char *&mem, int count )
{
  dst = _assignMemory<T, ( sizeof( T ) & ( sizeof( T ) - 1 ) ) == 0 && sizeof( T ) <= 16 ? sizeof( T ) : sizeof( void * )>( mem, count );
}

template<unsigned int Alignment, typename T> static inline void AssignMemoryAligned( T *&dst, char *&mem, int count )
{
  dst = _assignMemory<T, Alignment>( mem, count );
}

template<typename T> static inline int RequiredMemory( const T *const, int count )
{
  enum {
    Alignment = ( sizeof( T ) & ( sizeof( T ) - 1 ) ) == 0 && sizeof( T ) <= 16 ? sizeof( T ) : sizeof( void * )
  };
  return Alignment + count * sizeof( T );
}

#endif // MEMORYASSIGNMENTHELPERS_H
