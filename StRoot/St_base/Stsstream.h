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

#endif 
