#ifndef FINDMARK_H
#define FINDMARK_H
/*
The functions of this family are for finding wanted sequences of symbols
in strings or input streams. Such functions are often needed
at treating of any data for finding right
position from which some meaningful data begin at stream.

Copyright (c) 2000 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file for any purpose
is hereby granted without fee, provided that the above copyright notice,
this permission notice, and notices about any modifications of the original
text appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

#include <cstring>
#include "wcpplib/util/FunNameStack.h"

namespace Heed {

// The function findmark finds string s in stream file
// and returns 1 at current position at the next symbol.
// Finding no string it returns 0.
int findmark(std::istream& file, const char* s);

// Returns the same, also finds the length of string, the index of its
// beginning and the index of the next symbol after string.
// Indexes are count from current position in stream at which the function
// was called.
// Previously this function was findmark_uca from unsigned char alternative.
// Note that dispite of T the internally this class is working with char.
// The string ws is copied to string char before the work.
// T is any class with index operation.
template <class T>
int findmark_a(std::istream& file, T ws, long qws, long& nbeg, long& nnext);

// The same but returns also the previous symbol or '\0' for the string
// at the beginning of the stream.
// T is any class with index operation.
template <class T>
int findmark_b(std::istream& file, T ws, long qws, long& nbeg, long& nnext,
               char& prev);

template <class T>
int findmark_a(std::istream& file, T ws, long qws, long& nbeg, long& nnext) {
  mfunname("int findmark_a(...)");
  check_econd11(qws, < 1, mcerr);
  // check_econd12(qws , > ,  ws.get_qel() , mcerr);

  nbeg = 0;
  nnext = 0;

  char* s = new char[qws + 1];
  for (long n = 0; n < qws; n++) s[n] = ws[n];
  s[qws] = '\0';
  int ic;

  char* fs = new char[qws + 1];
  for (long n = 0; n < qws; n++) {
    if (file.eof() == 1) {
      delete[] fs;
      delete[] s;
      return 0;
    }
    ic = file.get();
    fs[n] = ic;
  }
  fs[qws] = '\0';
  nnext = qws;

  while (strcmp(fs, s) != 0) {
    for (long n = 1; n < qws; n++) fs[n - 1] = fs[n];
    if (file.eof() == 1) {
      delete[] fs;
      delete[] s;
      return 0;
    }
    ic = file.get();
    fs[qws - 1] = ic;
    nbeg++;
    nnext++;
  }
  delete[] fs;
  delete[] s;
  return 1;
}

template <class T>
int findmark_b(std::istream& file, T ws, long qws, long& nbeg, long& nnext,
               char& prev) {
  mfunname("int findmark_b(...)");
  check_econd11(qws, < 1, mcerr);

  nbeg = 0;
  nnext = 0;
  prev = '\0';

  char* s = new char[qws + 1];
  for (long n = 0; n < qws; n++) s[n] = ws[n];
  s[qws] = '\0';
  int ic;

  char* fs = new char[qws + 1];
  for (long n = 0; n < qws; n++) {
    if (file.eof() == 1) {
      delete[] fs;
      delete[] s;
      return 0;
    }
    ic = file.get();
    fs[n] = ic;
  }
  fs[qws] = '\0';
  nnext = qws;

  while (strcmp(fs, s) != 0) {
    prev = fs[0];
    for (long n = 1; n < qws; n++) fs[n - 1] = fs[n];
    if (file.eof() == 1) {
      delete[] fs;
      delete[] s;
      return 0;
    }
    ic = file.get();
    fs[qws - 1] = ic;
    nbeg++;
    nnext++;
  }
  delete[] fs;
  delete[] s;
  return 1;
}

// The following two functions find any of given strings, the first met.
// They return the number of string met ( in the interval 0 -- q-1).
// If none of strings found, they return -1.
int find1ofnmark(std::istream& file, int q,  // number of strings
                 char* s[]);                 // addresses
int find1ofnmark(std::istream& file, int q,  // number of strings
                 const std::string str[]);        // addresses

}

#endif
