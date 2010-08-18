#ifndef STRING_SWITCHING_H
#define STRING_SWITCHING_H
/*
This is the file for indication which String class should be used by
the wcpplib library. This library does not depend on very sophicticated
operations with strings. The strings are used for inclusion of notations 
or character names in various objects and also for keeping the file names. 
The typical string operations used in wcpplib are 
initialization from characters array, 
convertion to characters array, 
initialization from integer number, 
and addition of strings. 
Such simple operations are either supported by any library or some of them
can be easily added, as done below with STL. 
Since the wcpplib was started before the standard library was firmly 
established, and because there were many other libraries and it was not 
clear which of them will predominate, 
the source texts of wcpplib reference the string type by 
symbolic name "String" (which seem to be unexisting in all known libraries 
except GNU library),
and this type can and must be redefined right here, in this file, 
to actual name of string class of actually applied library (except GNU library,
for which it should not be redefined at all).
 
There are currently 3 options.
One is String class from gnu C++ library.
(This library seems to be not supported anymore).
Another is HepString from CLHEP library. 
(This collaboration have decided to eliminate strings in favour to std.)
The last is standard library string.
The standard library appears to be the most inconvenient (!) in supporting
the strings. It does not support conversion from integers and long integers
to strings, and it does not support the default conversion of strings to 
char*. Therefore when the support of other strings dissappeared,
I had to edit the programs to pass to this "standard".
The first type of conversions is performed by little function long_to_String()
defined as inline below. The second type of conversions is performed
in program with the use of macro USE_STLSTRING and c_str() member function
like this:

Example of program:
#ifdef USE_STLSTRING
  ifstream fh(hfile.c_str());    // use standard library
#else
  ifstream fh(hfile);     // most of other libraries
#endif

Not convenient! Hope there was real reasons to forbid default conversions in 
standard library.


Copyright (c) 2001 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice, 
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
It is provided "as is" without express or implied warranty.
*/ 

//#define USE_HEPSTRING          // use HepString
#define USE_STLSTRING      // use standard library
  // otherwise use gnu C++ String

#ifdef USE_HEPSTRING

#include "CLHEP/String/Strings.h"     // HepString
typedef HepString String;

#elif defined( USE_STLSTRING )

#include <string>
//typedef string String;
 typedef std::string String; 

#else  // use String from gnu C++ lib
#include <String.h>                   // gnu C++ String
#endif

// For any library:
#include <iostream>

//#define STRSTREAM_AVAILABLE
#ifdef STRSTREAM_AVAILABLE
#include <strstream.h>  // valid for old systems
#else
#include <sstream>      // good for new ones
#endif

// In <string> initialization from integer is absent
// In HepString it is present.
// So as to write code not dependent on the type of strings used,
// or to quickly modify the old code rich with String(n)
// I have defined this macro. 
// The old String(n) should be converted to long_to_String(n)
#ifdef USE_HEPSTRING
inline String long_to_String(int n) { return String(n); }
// call of overloaded `HepString(long int &)' is ambiguous !!
// perhaps there are HepString::HepString(char) and  HepString::HepString(int)
#else
inline String long_to_String(long n) 
     // This worked well for old standards.
     // In new ones strstream has dissapeared.     
     //{ std::ostrstream s; s << n <<'\0'; return String(s.str()); }
{ std::ostringstream s; s << n; return String(s.str()); }
//{ std::ostringstream s; s << n <<'\0'; return String(s.str()); }

inline String double_to_String(double d) 
     // This worked well for old standards.
     // In new ones strstream has dissapeared.     
     //{ std::ostrstream s; s << n <<'\0'; return String(s.str()); }
{ std::ostringstream s; s << d; return String(s.str()); }
//{ std::ostringstream s; s << n <<'\0'; return String(s.str()); }
#endif

// puts one \n at the end of string stream:

inline void put_one_n(std::ostringstream& ost)
{ 
  long qost = ost.str().length();
  if(qost > 0)
  {
    if(ost.str()[qost-1] == '\n')
    {  // nothing
    }
    else
    {
      ost<<'\n';
    }
  }
  else
    ost<<'\n';
}


#endif



