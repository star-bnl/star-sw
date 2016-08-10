#ifndef FUNNAMESTACK_H
#define FUNNAMESTACK_H
/*
Copyright (c) 1999 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
It is provided "as is" without express or implied warranty.
*/

#include <stdlib.h>
#include <string.h>
#include <iostream>
#include "wcpplib/stream/prstream.h"

//#define USE_BOOST_MULTITHREADING
//#define PRINT_MESSAGE_ABOUT_THREAD_INITIALIZATION
// Prints a long message at initialization of FunNameStack.
// The message is quite long and nusty and it is unlikely that
// it can be visually missed even if it is interrupted by play of threads.
// This nusty message is not necessary at the routine work.

#ifdef USE_BOOST_MULTITHREADING
#include "boost/thread/mutex.hpp"
#endif

/*
// Here there is a good place to switch off the
// initialization of the function names in all the programs
#ifdef FUNNAMESTACK
#undef FUNNAMESTACK
#endif
*/
// Here there is a good place to switch off the
// assertion checks in all the programs
//#ifdef DO_CHECKS
//#undef DO_CHECKS
//#endif

#define stackprt(stream) stream << FunNameStack::instance()
// Definitions which can be switched off
#ifdef FUNNAMESTACK
#define mfunname(string)             \
  static char* FunNameIIII = string; \
  FunNameWatch funnw(FunNameIIII)
#define mfunname1(string)             \
  static char* FunNameIIII1 = string; \
  FunNameWatch funnw1(FunNameIIII1)
#define mfunname2(string)             \
  static char* FunNameIIII2 = string; \
  FunNameWatch funnw2(FunNameIIII2)
#define mfunname3(string)             \
  static char* FunNameIIII3 = string; \
  FunNameWatch funnw3(FunNameIIII3)
#define mfunname4(string)             \
  static char* FunNameIIII4 = string; \
  FunNameWatch funnw4(FunNameIIII4)
#define mfunname5(string)             \
  static char* FunNameIIII5 = string; \
  FunNameWatch funnw5(FunNameIIII5)
//#define stackprt(stream)  stream<<funnamestack
#else
#define mfunname(string)
#define mfunname1(string)
#define mfunname2(string)
#define mfunname3(string)
#define mfunname4(string)
#define mfunname5(string)
//#define stackprt(stream)
#endif

// Permanent definitions
#define mfunnamep(string)                  \
  static const char* FunNameIIII = string; \
  FunNameWatch funnw(FunNameIIII)
#define mfunnamep1(string)                  \
  static const char* FunNameIIII1 = string; \
  FunNameWatch funnw1(FunNameIIII1)
#define mfunnamep2(string)                  \
  static const char* FunNameIIII2 = string; \
  FunNameWatch funnw2(FunNameIIII2)
#define mfunnamep3(string)                  \
  static const char* FunNameIIII3 = string; \
  FunNameWatch funnw3(FunNameIIII3)
#define mfunnamep4(string)                  \
  static const char* FunNameIIII4 = string; \
  FunNameWatch funnw4(FunNameIIII4)
#define mfunnamep5(string)                  \
  static const char* FunNameIIII5 = string; \
  FunNameWatch funnw5(FunNameIIII5)

// Checks
#define DO_CHECKS
#ifdef DO_CHECKS

#ifdef FUNNAMESTACK

#define check_econd(condition, add, stream) \
  if (condition) {                          \
    funnw.ehdr(stream);                     \
    stream << '\n' << #condition << '\n';   \
    stream << add;                          \
    spexit(stream);                         \
  }
#define check_wcond(condition, add, stream) \
  if (condition) {                          \
    funnw.whdr(stream);                     \
    stream << '\n' << #condition << '\n';   \
    stream << add;                          \
  }

#define check_econd1(condition, a1, stream) \
  if (condition) {                          \
    funnw.ehdr(stream);                     \
    stream << '\n' << #condition << '\n';   \
    stream << #a1 << '=' << (a1) << '\n';   \
    spexit(stream);                         \
  }
#define check_wcond1(condition, a1, stream) \
  if (condition) {                          \
    funnw.whdr(stream);                     \
    stream << '\n' << #condition << '\n';   \
    stream << #a1 << '=' << (a1) << '\n';   \
  }

#define check_econd2(condition, a1, a2, stream)                        \
  if (condition) {                                                     \
    funnw.ehdr(stream);                                                \
    stream << '\n' << #condition << '\n';                              \
    stream << #a1 << '=' << (a1) << ' ' << #a2 << '=' << (a2) << '\n'; \
    spexit(stream);                                                    \
  }
#define check_wcond2(condition, a1, a2, stream)                        \
  if (condition) {                                                     \
    funnw.whdr(stream);                                                \
    stream << '\n' << #condition << '\n';                              \
    stream << #a1 << '=' << (a1) << ' ' << #a2 << '=' << (a2) << '\n'; \
  }

#define check_econd3(condition, a1, a2, a3, stream)                         \
  if (condition) {                                                          \
    funnw.ehdr(stream);                                                     \
    stream << '\n' << #condition << '\n';                                   \
    stream << #a1 << '=' << (a1) << ' ' << #a2 << '=' << (a2) << ' ' << #a3 \
           << '=' << (a3) << '\n';                                          \
    spexit(stream);                                                         \
  }
#define check_wcond3(condition, a1, a2, a3, stream)                         \
  if (condition) {                                                          \
    funnw.whdr(stream);                                                     \
    stream << '\n' << #condition << '\n';                                   \
    stream << #a1 << '=' << (a1) << ' ' << #a2 << '=' << (a2) << ' ' << #a3 \
           << '=' << (a3) << '\n';                                          \
  }

#define check_econd4(condition, a1, a2, a3, a4, stream)                     \
  if (condition) {                                                          \
    funnw.ehdr(stream);                                                     \
    stream << '\n' << #condition << '\n';                                   \
    stream << #a1 << '=' << (a1) << ' ' << #a2 << '=' << (a2) << ' ' << #a3 \
           << '=' << (a3) << ' ' << #a4 << '=' << (a4) << '\n';             \
    spexit(stream);                                                         \
  }
#define check_wcond4(condition, a1, a2, a3, a4, stream)                     \
  if (condition) {                                                          \
    funnw.whdr(stream);                                                     \
    stream << '\n' << #condition << '\n';                                   \
    stream << #a1 << '=' << (a1) << ' ' << #a2 << '=' << (a2) << ' ' << #a3 \
           << '=' << (a3) << ' ' << #a4 << '=' << (a4) << '\n';             \
  }

#define check_econd11(a, signb, stream)     \
  if (a signb) {                            \
    funnw.ehdr(stream);                     \
    stream << '\n' << #a << #signb << '\n'; \
    stream << #a << '=' << (a) << '\n';     \
    spexit(stream);                         \
  }
#define check_wcond11(a, signb, stream)     \
  if (a signb) {                            \
    funnw.whdr(stream);                     \
    stream << '\n' << #a << #signb << '\n'; \
    stream << #a << '=' << (a) << '\n';     \
  }

#define check_econd12(a, sign, b, stream)                          \
  if (a sign b) {                                                  \
    funnw.ehdr(stream);                                            \
    stream << '\n' << #a << #sign << #b << '\n';                   \
    stream << #a << '=' << (a) << ' ' << #b << '=' << (b) << '\n'; \
    spexit(stream);                                                \
  }
#define check_wcond12(a, sign, b, stream)                          \
  if (a sign b) {                                                  \
    funnw.whdr(stream);                                            \
    stream << '\n' << #a << #sign << #b << '\n';                   \
    stream << #a << '=' << (a) << ' ' << #b << '=' << (b) << '\n'; \
  }

//condition + additional any commands
#define check_econd11a(a, signb, add, stream) \
  if (a signb) {                              \
    funnw.ehdr(stream);                       \
    stream << '\n' << #a << #signb << '\n';   \
    stream << #a << '=' << (a) << '\n';       \
    stream << add;                            \
    spexit(stream);                           \
  }
#define check_wcond11a(a, signb, add, stream) \
  if (a signb) {                              \
    funnw.whdr(stream);                       \
    stream << '\n' << #a << #signb << '\n';   \
    stream << #a << '=' << (a) << '\n';       \
    stream << add;                            \
  }

#define check_econd12a(a, sign, b, add, stream)                    \
  if (a sign b) {                                                  \
    funnw.ehdr(stream);                                            \
    stream << '\n' << #a << #sign << #b << '\n';                   \
    stream << #a << '=' << (a) << ' ' << #b << '=' << (b) << '\n'; \
    stream << add;                                                 \
    spexit(stream);                                                \
  }
#define check_wcond12a(a, sign, b, add, stream)                    \
  if (a sign b) {                                                  \
    funnw.whdr(stream);                                            \
    stream << '\n' << #a << #sign << #b << '\n';                   \
    stream << #a << '=' << (a) << ' ' << #b << '=' << (b) << '\n'; \
    stream << add;                                                 \
  }

// and of two conditions for one variable
#define check_econd21(a, sign1_b1_sign0, sign2_b2, stream)              \
  if(a sign1_b1_sign0 a sign2_b2) {                                     \
    funnw.ehdr(stream);                                                 \
    stream << '\n' << #a << #sign1_b1_sign0 << #a << #sign2_b2 << '\n'; \
    stream << #a << '=' << (a) << '\n';                                 \
    spexit(stream);                                                     \
  }
#define check_wcond21(a, sign1_b1_sign0, sign2_b2, stream)              \
  if(a sign1_b1_sign0 a sign2_b2) {                                     \
    funnw.whdr(stream);                                                 \
    stream << '\n' << #a << #sign1_b1_sign0 << #a << #sign2_b2 << '\n'; \
    stream << #a << '=' << (a) << '\n';                                 \
  }

// and of two conditions for one variable
#define check_econd23(a, sign1, b1, sign0, sign2, b2, stream)              \
  if(a sign1 b1 sign0 a sign2 b2) {                                        \
    funnw.ehdr(stream);                                                    \
    stream << '\n' << #a << #sign1 << #b1 << #sign0 << #a << #sign2 << #b2 \
           << '\n';                                                        \
    stream << #a << '=' << (a) << ' ' << #b1 << '=' << (b1) << ' ' << #b2  \
           << '=' << (b2) << '\n';                                         \
    spexit(stream);                                                        \
  }
#define check_wcond23(a, sign1, b1, sign0, sign2, b2, stream)              \
  if(a sign1 b1 sign0 a sign2 b2) {                                        \
    funnw.whdr(stream);                                                    \
    stream << '\n' << #a << #sign1 << #b1 << #sign0 << #a << #sign2 << #b2 \
           << '\n';                                                        \
    stream << #a << '=' << (a) << ' ' << #b1 << '=' << (b1) << ' ' << #b2  \
           << '=' << (b2) << '\n';                                         \
  }

// two conditions for four variables
#define check_econd24(a1, sign1, b1, sign0, a2, sign2, b2, stream)           \
  if(a1 sign1 b1 sign0 a2 sign2 b2) {                                        \
    funnw.ehdr(stream);                                                      \
    stream << '\n' << #a1 << #sign1 << #b1 << #sign0 << #a2 << #sign2 << #b2 \
           << '\n';                                                          \
    stream << #a1 << '=' << (a1) << ' ' << #b1 << '=' << (b1) << '\n';       \
    stream << #a2 << '=' << (a2) << ' ' << #b2 << '=' << (b2) << '\n';       \
    spexit(stream);                                                          \
  }
#define check_wcond24(a1, sign1, b1, sign0, a2, sign2, b2, stream)           \
  if(a1 sign1 b1 sign0 a2 sign2 b2) {                                        \
    funnw.whdr(stream);                                                      \
    stream << '\n' << #a1 << #sign1 << #b1 << #sign0 << #a2 << #sign2 << #b2 \
           << '\n';                                                          \
    stream << #a1 << '=' << (a1) << ' ' << #b1 << '=' << (b1) << '\n';       \
    stream << #a2 << '=' << (a2) << ' ' << #b2 << '=' << (b2) << '\n';       \
  }

#else  // without FUNNAMESTACK, print only condition

#define check_econd(condition, add, stream) \
  if (condition) {                          \
    stream << "ERROR:\n";                   \
    stream << '\n' << #condition << '\n';   \
    stream << add;                          \
    spexit(stream);                         \
  }
#define check_wcond(condition, add, stream) \
  if (condition) {                          \
    stream << "WARNING:\n";                 \
    stream << '\n' << #condition << '\n';   \
    stream << add;                          \
  }

#define check_econd1(condition, a1, stream) \
  if (condition) {                          \
    stream << "ERROR:\n";                   \
    stream << '\n' << #condition << '\n';   \
    stream << #a1 << '=' << (a1) << '\n';   \
    spexit(stream);                         \
  }
#define check_wcond1(condition, a1, stream) \
  if (condition) {                          \
    stream << "WARNING:\n";                 \
    stream << '\n' << #condition << '\n';   \
    stream << #a1 << '=' << (a1) << '\n';   \
  }

#define check_econd2(condition, a1, a2, stream)                        \
  if (condition) {                                                     \
    stream << "ERROR:\n";                                              \
    stream << '\n' << #condition << '\n';                              \
    stream << #a1 << '=' << (a1) << ' ' << #a2 << '=' << (a2) << '\n'; \
    spexit(stream);                                                    \
  }
#define check_wcond2(condition, a1, a2, stream)                        \
  if (condition) {                                                     \
    stream << "WARNING:\n";                                            \
    stream << '\n' << #condition << '\n';                              \
    stream << #a1 << '=' << (a1) << ' ' << #a2 << '=' << (a2) << '\n'; \
  }

#define check_econd3(condition, a1, a2, a3, stream)                         \
  if (condition) {                                                          \
    stream << "ERROR:\n";                                                   \
    stream << '\n' << #condition << '\n';                                   \
    stream << #a1 << '=' << (a1) << ' ' << #a2 << '=' << (a2) << ' ' << #a3 \
           << '=' << (a3) << '\n';                                          \
    spexit(stream);                                                         \
  }
#define check_wcond3(condition, a1, a2, a3, stream)                         \
  if (condition) {                                                          \
    stream << "WARNING:\n";                                                 \
    stream << '\n' << #condition << '\n';                                   \
    stream << #a1 << '=' << (a1) << ' ' << #a2 << '=' << (a2) << ' ' << #a3 \
           << '=' << (a3) << '\n';                                          \
  }

#define check_econd4(condition, a1, a2, a3, a4, stream)                     \
  if (condition) {                                                          \
    stream << "ERROR:\n";                                                   \
    stream << '\n' << #condition << '\n';                                   \
    stream << #a1 << '=' << (a1) << ' ' << #a2 << '=' << (a2) << ' ' << #a3 \
           << '=' << (a3) << ' ' << #a4 << '=' << (a4) << '\n';             \
    spexit(stream);                                                         \
  }
#define check_wcond4(condition, a1, a2, a3, a4, stream)                     \
  if (condition) {                                                          \
    stream << "WARNING:\n";                                                 \
    stream << '\n' << #condition << '\n';                                   \
    stream << #a1 << '=' << (a1) << ' ' << #a2 << '=' << (a2) << ' ' << #a3 \
           << '=' << (a3) << ' ' << #a4 << '=' << (a4) << '\n';             \
  }

#define check_econd11(a, signb, stream)     \
  if (a signb) {                            \
    stream << "ERROR:\n";                   \
    stream << '\n' << #a << #signb << '\n'; \
    stream << #a << '=' << (a) << '\n';     \
    spexit(stream);                         \
  }
#define check_wcond11(a, signb, stream)     \
  if (a signb) {                            \
    stream << "WARNING:\n";                 \
    stream << '\n' << #a << #signb << '\n'; \
    stream << #a << '=' << (a) << '\n';     \
  }

#define check_econd12(a, sign, b, stream)                          \
  if (a sign b) {                                                  \
    stream << "ERROR:\n";                                          \
    stream << '\n' << #a << #sign << #b << '\n';                   \
    stream << #a << '=' << (a) << ' ' << #b << '=' << (b) << '\n'; \
    spexit(stream);                                                \
  }
#define check_wcond12(a, sign, b, stream)                          \
  if (a sign b) {                                                  \
    stream << "WARNING:\n";                                        \
    stream << '\n' << #a << #sign << #b << '\n';                   \
    stream << #a << '=' << (a) << ' ' << #b << '=' << (b) << '\n'; \
  }

//condition + additional any commands
#define check_econd11a(a, signb, add, stream) \
  if (a signb) {                              \
    stream << "ERROR:\n";                     \
    stream << '\n' << #a << #signb << '\n';   \
    stream << #a << '=' << (a) << '\n';       \
    stream << add;                            \
    spexit(stream);                           \
  }
#define check_wcond11a(a, signb, add, stream) \
  if (a signb) {                              \
    stream << "WARNING:\n";                   \
    stream << '\n' << #a << #signb << '\n';   \
    stream << #a << '=' << (a) << '\n';       \
    stream << add;                            \
  }

#define check_econd12a(a, sign, b, add, stream)                    \
  if (a sign b) {                                                  \
    stream << "ERROR:\n";                                          \
    stream << '\n' << #a << #sign << #b << '\n';                   \
    stream << #a << '=' << (a) << ' ' << #b << '=' << (b) << '\n'; \
    stream << add;                                                 \
    spexit(stream);                                                \
  }
#define check_wcond12a(a, sign, b, add, stream)                    \
  if (a sign b) {                                                  \
    stream << "WARNING:\n";                                        \
    stream << '\n' << #a << #sign << #b << '\n';                   \
    stream << #a << '=' << (a) << ' ' << #b << '=' << (b) << '\n'; \
    stream << add;                                                 \
  }

// and of two conditions for one variable
#define check_econd21(a, sign1_b1_sign0, sign2_b2, stream)              \
  if(a sign1_b1_sign0 a sign2_b2) {                                     \
    stream << "ERROR:\n";                                               \
    stream << '\n' << #a << #sign1_b1_sign0 << #a << #sign2_b2 << '\n'; \
    stream << #a << '=' << (a) << '\n';                                 \
    spexit(stream);                                                     \
  }
#define check_wcond21(a, sign1_b1_sign0, sign2_b2, stream)              \
  if(a sign1_b1_sign0 a sign2_b2) {                                     \
    stream << "WARNING:\n";                                             \
    stream << '\n' << #a << #sign1_b1_sign0 << #a << #sign2_b2 << '\n'; \
    stream << #a << '=' << (a) << '\n';                                 \
  }

// and of two conditions for one variable
#define check_econd23(a, sign1, b1, sign0, sign2, b2, stream)              \
  if(a sign1 b1 sign0 a sign2 b2) {                                        \
    stream << "ERROR:\n";                                                  \
    stream << '\n' << #a << #sign1 << #b1 << #sign0 << #a << #sign2 << #b2 \
           << '\n';                                                        \
    stream << #a << '=' << (a) << ' ' << #b1 << '=' << (b1) << ' ' << #b2  \
           << '=' << (b2) << '\n';                                         \
    spexit(stream);                                                        \
  }
#define check_wcond23(a, sign1, b1, sign0, sign2, b2, stream)              \
  if(a sign1 b1 sign0 a sign2 b2) {                                        \
    stream << "WARNING:\n";                                                \
    stream << '\n' << #a << #sign1 << #b1 << #sign0 << #a << #sign2 << #b2 \
           << '\n';                                                        \
    stream << #a << '=' << (a) << ' ' << #b1 << '=' << (b1) << ' ' << #b2  \
           << '=' << (b2) << '\n';                                         \
  }

// two conditions for four variables
#define check_econd24(a1, sign1, b1, sign0, a2, sign2, b2, stream)           \
  if(a1 sign1 b1 sign0 a2 sign2 b2) {                                        \
    stream << "ERROR:\n";                                                    \
    stream << '\n' << #a1 << #sign1 << #b1 << #sign0 << #a2 << #sign2 << #b2 \
           << '\n';                                                          \
    stream << #a1 << '=' << (a1) << ' ' << #b1 << '=' << (b1) << '\n';       \
    stream << #a2 << '=' << (a2) << ' ' << #b2 << '=' << (b2) << '\n';       \
    spexit(stream);                                                          \
  }
#define check_wcond24(a1, sign1, b1, sign0, a2, sign2, b2, stream)           \
  if(a1 sign1 b1 sign0 a2 sign2 b2) {                                        \
    stream << "WARNING:\n";                                                  \
    stream << '\n' << #a1 << #sign1 << #b1 << #sign0 << #a2 << #sign2 << #b2 \
           << '\n';                                                          \
    stream << #a1 << '=' << (a1) << ' ' << #b1 << '=' << (b1) << '\n';       \
    stream << #a2 << '=' << (a2) << ' ' << #b2 << '=' << (b2) << '\n';       \
  }

#endif

#else  // without checks

#define check_econd(condition, add, stream)
#define check_wcond(condition, add, stream)

#define check_econd1(condition, a1, stream)
#define check_wcond1(condition, a1, stream)

#define check_econd2(condition, a1, a2, stream)
#define check_wcond2(condition, a1, a2, stream)

#define check_econd3(condition, a1, a2, a3, stream)
#define check_wcond3(condition, a1, a2, a3, stream)

#define check_econd4(condition, a1, a2, a3, a4, stream)
#define check_wcond4(condition, a1, a2, a3, a4, stream)

#define check_econd11(a, signb, stream)
#define check_wcond11(a, signb, stream)

#define check_econd12(a, sign, b, stream)
#define check_wcond12(a, sign, b, stream)

#define check_econd11a(a, signb, add, stream)
#define check_wcond11a(a, signb, add, stream)

#define check_econd12a(a, sign, b, add, stream)
#define check_wcond12a(a, sign, b, add, stream)

// and of two conditions for one variable
#define check_econd21(a, sign1_b1_sign0, sign2_b2, stream)
#define check_wcond21(a, sign1_b1_sign0, sign2_b2, stream)

// and of two conditions for one variable
#define check_econd23(a, sign1, b1, sign0, sign2, b2, stream)
#define check_wcond23(a, sign1, b1, sign0, sign2, b2, stream)

// two conditions for four variables
#define check_econd24(a1, sign1, b1, sign0, a2, sign2, b2, stream)
#define check_wcond24(a1, sign1, b1, sign0, a2, sign2, b2, stream)

#endif

class ExcFromSpexit {
 public:
  ExcFromSpexit(void) { ; }
};

void spexit_action(std::ostream& file);
extern int s_throw_exception_in_spexit;  // if == 1, does exit(1) of abort()
                                         // depending on the key below
extern int s_exit_without_core;          // the key above have larger priority

// Normal exit:
#define spexit(stream)                                                   \
  {                                                                      \
    stackprt(stream);                                                    \
    stream << "File is " << __FILE__ << " , line number is " << __LINE__ \
           << '\n';                                                      \
    spexit_action(stream);                                               \
  }

const int pqname = 1000;  // this depth of stack is completely OK
                          // for all correct programs.
// If you have overflow, it is likely the infinite loop with recursion
// in your program!

// Converting to a quasi-singleton class:
// The program operates only with main hidden exsemplar.
// Nothing could be copied to, but it itself can be copied,
// for example for passing with exception classes

#ifdef USE_BOOST_MULTITHREADING
/* For multithreading we will keep own stack for each thread.
This complicates things a lot, but still managable.
In one thread case the stack data qname and name[] are kept directly
in a singleton object of FunNameStack class.
For multithreading mode we need to introduce special intermediate class
NameStack and initialize one object of this class per each found thread.
*/
//#include "wcpplib/safetl/AbsList.h"  // this does not work, because it
// refers through some sequence to this file.
template <class T> class AbsList;  // This is enough.

class NameStack {
 public:
  int qname;
  char* name[pqname];
  pthread_t id;
  int nmode;  // 0 - name points to a string in outside world
              //     used for global object funnamestack
              // 1 - name is inited by new and deleted by delete
              //     may be used for sending as parameter of exception
              //     It is not convenient for normal work due
              //     to time expense
  NameStack(void) : qname(0), id(0), nmode(0) {
    for (int n = 0; n < pqname; n++)
      name[n] = NULL;
  }
  NameStack(const NameStack& f) : qname(0), id(0), nmode(0) { *this = f; }

  NameStack& operator=(const NameStack& f);

  ~NameStack(void) {
    int n;
    if (nmode == 1) for (n = 0; n < qname; n++)
        delete name[n];
  }
};
#endif

class FunNameStack {
 public:
  static FunNameStack& instance();

  // make instances of the class uncopyable to hidden exsemplar,
  // but copyable away.
  //

  FunNameStack(const FunNameStack& f);
  FunNameStack& operator=(const FunNameStack& f);

  FunNameStack(void);  // usually called inly from instance()
 private:
#ifdef USE_BOOST_MULTITHREADING
  // Two next functions return NameStack corresponding to
  // current thread.
  NameStack* get_thread_stack(void) const;
  NameStack* get_thread_stack_q(long& nthread, long& qthread) const;
  // retrieve not only NameStack, but also nthread and qthread.

  void remove_thread_stack(void);
#endif
//const static int pqname;   // doesn't work on Sun
#ifdef USE_BOOST_MULTITHREADING
  AbsList<NameStack>* namestack;
// Cannot put ActivePtr here because it uses  FunNameStack itself
// and wants it to be completed.
#else
  int qname;
  char* name[pqname];
#endif
  int s_init;  // 1 - sign that it is inited,
               // any other value - not inited
               // Now this variable is used only for debug printing.

  int s_act;  // 1 - active, 0 - not active
              //( to switch without recompilation)
 public:
  int s_print;  // works only is s_act=1;
                // 0 - no print
                // 1 - at each entry print the name of current function
                // 2 - at each entry and each exit
                //    print the name of current function
                // 3 - at each entry print all stack
                // 4 - at each entry and exit print all stack
 private:
  int nmode;  // 0 - name points to a string in outside world
              //     used for global object funnamestack
              // 1 - name is inited by new and deleted by delete
              //     may be used for sending as parameter of exception
              //     It is not convenient for normal work due
              //     to time expense

#ifdef USE_BOOST_MULTITHREADING
  std::ostream& printname(std::ostream& file, NameStack* ns, int n);  //
#else
  std::ostream& printname(std::ostream& file, int n);  //
#endif
 public:
  void set_parameters(int fs_act = 1, int fs_print = 0);
  ~FunNameStack();

 private:
  void printput(
      std::ostream& file);  // called at insertion of new name in stack
  void printdel(std::ostream& file);  // called at deletion of name from stack
 public:
  //#ifdef USE_BOOST_MULTITHREADING
  //inline void put(char* fname);
  //#else
  inline int put(const char* fname);
  //#endif
  inline void del(int nname);
  inline void replace(const char* fname);
  friend std::ostream& operator<<(std::ostream& file, const FunNameStack& f);
};
std::ostream& operator<<(std::ostream& file, const FunNameStack& f);

// The following class is used solerly
// to change parameters of main FunNameStack, where it needs to do
// "globally", before main() is started.
// see the file FunNameStack_ts.c for example.
class FunNameStack_Assist {
 public:
  FunNameStack_Assist(int fs_act = 1, int fs_print = 0) {
    FunNameStack::instance().set_parameters(fs_act = 1, fs_print);
  }
};

//extern FunNameStack funnamestack;

// Object of this class FunNameWatch is created at the beginning of function.
// At creation it writes function name in stack.
// At deletion it deletes the name from stack,
// checking that it is last.

class FunNameWatch {
  int nname;         // index of this name in array of FunNameStack
  const char* name;  // it is memorized independenlty on s_act.
                     // Used for printing of headers.
 public:
  inline FunNameWatch(const char* fname);
  /*
  FunNameWatch(char* fname):name(fname)
    {
#ifdef FUNNAMESTACK
      nname=funnamestack.put(fname);
#else
      nname=0;
#endif
    }
  */
  //FunNameWatch(void):name(NULL), nname(-1) { ; }  // temporary

  inline ~FunNameWatch();
  /*
    {
#ifdef FUNNAMESTACK
      if( nname>=0)
	funnamestack.del(nname);
#endif
    }
  */

  // print header
  std::ostream& hdr(std::ostream& file) const {
    file << name << ": ";
    return file;
  }
  // print header with word WARNING
  std::ostream& whdr(std::ostream& file) const {
    file << name << ": WARNING:\n";
    return file;
  }
  // print header with word ERROR
  std::ostream& ehdr(std::ostream& file) const {
    file << name << ": ERROR:\n";
    return file;
  }
};
std::ostream& operator<<(std::ostream& file, const FunNameWatch& f);

/* It is assumed to be general basical class for catching of errors.
Any error thrown with FunNameStack can be catched with pointer to this class.
*/
class GenError : public FunNameStack {
 public:
  GenError(const FunNameStack& f) : FunNameStack(f) { ; }
  GenError(const GenError& f) : FunNameStack(f) { ; }
  virtual void print(std::ostream& file);
  virtual void finish(std::ostream& file);
  virtual ~GenError() {}
};
//std::ostream& operator<<(std::ostream& file, const GenError& f);
#include "wcpplib/util/FunNameStack.ic"

#endif
