/*!
 * \file Stsstream.h
 *
 * \author Victor Perev  8/22/2003
 *  Wrapper for old <strstream>
 *                                          
 *                                                                      
 */
#ifndef STSSTREAM_H
#define STSSTREAM_H
#include <ctype.h>
#include "Rstrstream.h"
#ifdef R__SSTREAM
using std::streamsize;
class ostrstream : public std::ostringstream {
std::string myString;
public:
const char *str()         
{
  std::string tmp = std::ostringstream::str();
  if (myString != tmp) myString=tmp;
  return myString.c_str();
}	
int        pcount()       {return int(tellp()) ;}
void       seekp(int pos) {if (int(tellp())>=0) std::ostringstream::seekp(pos);}
void freeze(bool) const{;}
};	


class istrstream : public std::istringstream {
public:
istrstream(): std::istringstream(){}
istrstream(const char *init):std::istringstream(std::string(init)){}
};
#else
#endif 

// Hide the function from `rootcint` as it cannot process variadic template parameters
#ifndef __CINT__

#include <iostream>
#include <cstdio>
#include <memory>
#include <string>

/**
 * This is an alternative to the ROOT's global function Form() defined in
 * TString.cxx. The implementation of this function is based on the answer to
 * https://stackoverflow.com/questions/2342162/stdstring-formatting-like-sprintf
 *
 * Returns an std::string
 */
template<typename ... Args>
std::string FormString(const std::string& format, Args ... args)
{
    size_t cstr_size = snprintf(nullptr, 0, format.c_str(), args ...) + 1; // Extra space for '\0'
    std::unique_ptr<char[]> buf(new char[cstr_size]);
    snprintf(buf.get(), cstr_size, format.c_str(), args ...);
    return std::string(buf.get(), buf.get() + cstr_size - 1); // We don't want the '\0' inside
}
#endif

#endif 
