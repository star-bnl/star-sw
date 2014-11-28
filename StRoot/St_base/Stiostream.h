/*!
  \file Stiostream.h
 
  \author Victor Perev 
  \date 8/22/2003
 
  Wrapper for "iostream.h" using ROOT definitions (compiler 
  dependence, OS/platform issues). There are at least two
  version of iostream (one ANSI and one not). Old gcc 
  (pre 3.2) did not use the ANSI way.
  
  
  Working Paper for Draft Proposed INternational Standard for Information
  Systems (ANSI) -- Programming Language C++, Section 27.6.1.4.1

  Description is as follow
 
  <UL>
  <LI><U>pre-ANSI iostream class</U><BR>
  This class combines the istream and ostream classes. You use it to carry
  out bidirectional operations (inserting into and extracting from a single
  sequence of characters).
 
  <LI><U>ANSI iostream</U><BR>
  The class basic_iostream inherits a number of functions from classes
  <TT>basic_ostream&lt;charT, traits&gt;</TT> and <TT>basic_istream&lt;charT, traits&gt;</TT>. They assist
  in formatting and interpreting sequences of characters controlled by a
  stream buffer. Two groups of functions share common properties, the formatted
  functions and the unformatted functions.
 
  </UL>
                                                                       
*/
#ifndef STIOSTREAM_H
#define STIOSTREAM_H
#include "Riostream.h"
/* using namespace std; */
using std::istream;
using std::ostream;
using std::fstream;
using std::ifstream;
using std::ofstream;
using std::cout;
using std::endl;
#endif 
