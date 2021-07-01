/*
 * This file is part of TPCCATracker package
 * Copyright (C) 2009 Matthias Kretz <kretz@kde.org>
 *
 * TPCCATracker is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * TPCCATracker is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef BINARYSTOREHELPER_H
#define BINARYSTOREHELPER_H

#include <assert.h>
#include "AliHLTTPCCADef.h"
#include <cstdio>

// writes

template<typename T> static inline void BinaryStoreWrite( const T *mem, int count, FILE *f )
{
  const int written = std::fwrite( mem, sizeof( T ), count, f );
  assert( written == count ); UNUSED_PARAM1( written );
}

template<typename T> static inline void BinaryStoreWrite( const T &mem, FILE *f )
{
  const size_t written = std::fwrite( &mem, sizeof( T ), 1, f );
  assert( written == 1 ); UNUSED_PARAM1( written );
}

static inline void BinaryStoreWrite( const void *offsetPtr, const void *mem, FILE *f )
{
  const int offset = static_cast<const char *>( offsetPtr ) - static_cast<const char *>( mem );
  const size_t written = std::fwrite( &offset, sizeof( int ), 1, f );
  assert( written == 1 ); UNUSED_PARAM1( written );
}

// reads

template<typename T> static inline void BinaryStoreRead( T *mem, int count, FILE *f )
{
  const int read = std::fread( mem, sizeof( T ), count, f );
  assert( read == count ); UNUSED_PARAM1( read );
}

template<typename T> static inline void BinaryStoreRead( T &mem, FILE *f )
{
  const size_t read = std::fread( &mem, sizeof( T ), 1, f );
  assert( read == 1 ); UNUSED_PARAM1( read );
}

template<typename T>
static inline void BinaryStoreRead( T *&offsetPtr, char *mem, FILE *f )
{
  int offset;
  const size_t read = std::fread( &offset, sizeof( int ), 1, f );
  assert( read == 1 ); UNUSED_PARAM1( read );
  offsetPtr = reinterpret_cast<T *>( mem + offset );
}

#endif // BINARYSTOREHELPER_H
