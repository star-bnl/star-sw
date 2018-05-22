#ifndef ABSCONT_H
#define ABSCONT_H
/*
Copyright (c) 1999-2004 I. B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

#include <iostream>
#include <cstring>
#include <limits.h>
#include <typeinfo>
#include "wcpplib/util/FunNameStack.h"

//#define IMPLICIT_X_STAR

#define USE_DOUBLE_PTR_IN_PASSIVEPTR 
// New option. Makes passive ptr keeping address of counter and address
// of object. This allow to avoid dynamic casts while still allowing
// the class RegPassivePtr be virtual.

//#define USE_CHECK_DOUBLE_PTR_IN_PASSIVEPTR
// for debug of the option above. At each access it checks
// that the object address in ptr is equal to the address
// restored from the counter.

#define USE_DELETE_AT_ZERO_COUNT  
// to switch on option for emulation of shared pointer.
// But it only gives possibility to emulate it by the passive pointer,
// but does not make passive pointer equival to shared one.
// You should assign 1 to s_allow_del_at_zero_count of each particular object,
// which should be deleted.

//#define SKIP_CHECKS_NULL  // skip checks of smart pointers
// (that they are not NULL) at
// dereferencing via getver(), *, and ->.
// In all cases at the normal execution the object should not be NULL
// for these operators.
// Skiping checks, we assume that they are not NULL
// (and risk to have bad undetected errors at the execution time).
// This option is incerted due to curiosity about for how much
// the checks delay the program.
// For Monte Carlo simulation the result was a few percent.
// Therefore the use (uncommenting) of this macro for the purposes
// other than similar curiosity does NOT have any sense.

// To be addressed by active pointer an object of a class
// should have a function copy() which clones the object and returns 
// the new address, and may have have a function print() 
// which prints it to a stream.
// The copy() is declared pure just to ensure that the user does not
// forget to define it in derivative.

#define COPY_TYPE_CHECK  // make execution longer, but allows
// to detect missings of copy functions in classes referred by
// active pointers.

#define USE_GETSETTERS_IN_PASSIVEPTR  // new option allowing to use
// getters and setters in the passive pointer
// instead or in addition to direct access to auxiliary parameters.
// It allows to control correctness of input values and also to use
// fields if the next option is switched on. But note that using fields
// can increase the size of the code.
// Switching this option on does not forbid to use direct access to
// these parameters.

#define USE_PRIVATE_PARAM_IN_PASSIVEPTR  // allows to change parameters
                                         // only through getters and setters.
// Obviously, switching this option on requires switching on the previous
// option, which is controlled by the following:
#if defined(USE_PRIVATE_PARAM_IN_PASSIVEPTR) && \
    !defined(USE_GETSETTERS_IN_PASSIVEPTR)
"options incompatible\n";  // any line to trigger compiler diagnostic
#endif

#define USE_CHAR_CONTROL_VARIABLES  // instead of int control variables
// enables to use char control variables (except two static ones),
// which could be optimal
// both for memory and speed, but that it is really optimal
// is not checked so far.

//#define USE_BIT_FIELDS  // instead of int control variables
// allowes to pack control auxiliary
// variables presented in passive pointer into bit fields.
// It is assumed that the passive pointer objects and the whole
// programs using it could be smaller in size with this option switched on.
// But be informed that according to Stroustrup this is not always the fact:
// this reduces the data but increases the executable code size.
// Also the performance could be
// decreased due to necessity to unpack these thinigs each time at their use.
// This option does not compatible with the previous one.
// So only one of them can be switched on, which is checked by the following:

//#define USE_BIT_OPERA  // Instead of the two previous options
// allowes to pack control auxiliary
// variables presented in passive pointer into bit fields
// and to use bit operations.
// Currently under debug.
#if defined(USE_CHAR_CONTROL_VARIABLES) && defined(USE_BIT_FIELDS)
"options incompatible\n";  // any line to trigger compiler diagnostic
#endif
#if defined(USE_CHAR_CONTROL_VARIABLES) && defined(USE_BIT_OPERA)
"options incompatible\n";  // any line to trigger compiler diagnostic
#endif
#if defined(USE_BIT_FIELDS) && defined(USE_BIT_OPERA)
"options incompatible\n";  // any line to trigger compiler diagnostic
#endif

#define USE_CHAR_GETSETTERS_PARAMETERS  // related to type of parameters
                                        // in gatters and setters.
// If this macro is enabled, the parameters are "char".
// Otherwise the parameters and int.
// What is faster I do not know at the moment
// They are not bool since in many instanses they can have the values
// of 0, 1, or 2 (and the author does not like bool type at all).
#if defined(USE_CHAR_CONTROL_VARIABLES) && \
    !defined(USE_CHAR_GETSETTERS_PARAMETERS)
"options incompatible\n";  // any line to trigger compiler diagnostic
#endif

namespace Heed {

template <class X>
class StandardCopyDefinition {
 public:
  static X* copy(const X* f) {
#ifdef DEBUG_ACTIVEPTR
    mcout << "static X* StandardCopyDefinition::copy(const X* f)\n";
#endif
  // If to allow the type of copy function be any (void* for example,
  // it needs to convert it. This seems there is no reason to allow this.
#ifndef COPY_TYPE_CHECK
    return f->copy();
#else
    X* p = f->copy();
#ifdef DEBUG_ACTIVEPTR
    mcout << "X* StandardCopyDefinition::copy(const X* f): f->copy() returned "
          << p << '\n';
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    mcerr << "Type of *p is (in internal notations) " << typeid(*p).name()
          << '\n';
#endif
    if (typeid(*p) != typeid(*f)) {
      mcerr << "Error in X* StandardCopyDefinition::copy(const X* f): "
            << "typeid(*p) != typeid(*f) \n";
      mcerr << "Type of X is (in internal notations) " << typeid(X).name()
            << '\n';
      mcerr << "Type of *p is (in internal notations) " << typeid(*p).name()
            << '\n';
      mcerr << "Type of *f is (in internal notations) " << typeid(*f).name()
            << '\n';
      mcerr << "Possible reason is omiting of copy function in one of the "
               "derivative classes\n";
      spexit(mcerr);
    }
    return p;
#endif
  }
};

/* The second parameter determines the class which has to have only one
function copy() which "knows" the actual name of cloning function.
Normally the cloning function is copy() as well.
But can be any.
Previously at this place there were some terrible comment
which told that there might be some problems. But it was outdated.
At contemporary systems there is no problems.
So any complicated tree of derived classes even with multiple inheritance
can each have the same-named function
returning different types with accordance with
the type of the class.
Everything is compiled and work.
But do not forget to add virtual destructors in pointee classes!
In particular, when playing with multiple inheritance.
*/

enum Pilfer {
  steal
};
enum Clone {
  do_clone
};
enum Pass {
  dont_clone
};

template <class X>
class PassivePtr;

class RegPassivePtr;

namespace CountPP_ns {
/// Counter of protected pointers.
class CountPassivePtr {
 public:
  CountPassivePtr(const RegPassivePtr* frpp) : rpp(frpp), number_of_booked(0) {}

  // In the following I will use the word book instead of register,
  // because register seems to be interfered with some computer definition
  inline void book(void);
  inline void unbook(void);
  inline long get_number_of_booked(void) { return number_of_booked; }
  inline const RegPassivePtr* get_rpp(void) { return rpp; }
  inline void change_rpp(const RegPassivePtr* frpp) { rpp = frpp; }
  inline ~CountPassivePtr();  // here to delete reference from RegPassivePtr
 private:
  const RegPassivePtr* rpp;
  // const is necessary here to make possible booking of RegPassivePtr
  // by a function which is declared as const.

  long number_of_booked;  // the counter of pointers
};

inline void CountPassivePtr::book(void) {
  if (number_of_booked > LONG_MAX - 1) {
    mcerr << "Error in CountPassivePtr::book(void):\n"
          << " too much booked counters, number_of_booked > LONG_MAX-1, "
             "number_of_booked=" << number_of_booked << '\n';
    spexit(mcerr);
  }
  number_of_booked++;
}

inline void CountPassivePtr::unbook(void) {
  if (number_of_booked < 1) {
    mcerr << "Error in CountPassivePtr::unbook(void):\n"
          << " number_of_booked < 1, number_of_booked=" << number_of_booked
          << '\n';
    spexit(mcerr);
  }
  number_of_booked--;
}
}

#ifdef USE_BIT_OPERA

const unsigned char eb_s_ban_del = 1;
const unsigned char eb_s_ban_sub1 = 2;
const unsigned char eb_s_ban_sub2 = 4;
const unsigned char eb_s_ban_cop1 = 8;
const unsigned char eb_s_ban_cop2 = 16;
#ifdef USE_DELETE_AT_ZERO_COUNT
const unsigned char eb_s_allow_del_at_zero_count = 32;
#endif
#endif

class RegPassivePtr {
 public:
  friend class CountPP_ns::CountPassivePtr;
// used for making cpp NULL when CountPassivePtr
// is destructed

#ifdef USE_BIT_OPERA
 private:
  inline static void clear_bit(unsigned char& cw, unsigned char b) {
    if ((cw & b) != 0) {
      cw ^= b;
    }
  }
  inline static void rise_bit(unsigned char& cw, unsigned char b) { cw |= b; }

 public:
// To define auxiliary class, used only in the following case:
#elif defined(USE_BIT_FIELDS)
  class ControlParam {
   public:
    unsigned int s_ban_del : 1;  // see below for comments
    unsigned int s_ban_sub : 2;
    unsigned int s_ban_cop : 2;
#ifdef USE_DELETE_AT_ZERO_COUNT
    unsigned int s_allow_del_at_zero_count : 1;
#endif
    ControlParam(void)
        : s_ban_del(0),
          s_ban_sub(0),
          s_ban_cop(0)
#ifdef USE_DELETE_AT_ZERO_COUNT
          ,
          s_allow_del_at_zero_count(0)
#endif
    {
    }
#ifdef USE_CHAR_GETSETTERS_PARAMETERS
    ControlParam(char fs_ban_del, char fs_ban_sub, char fs_ban_cop = 0
#ifdef USE_DELETE_AT_ZERO_COUNT
                 ,
                 char fs_allow_del_at_zero_count = 0
#endif
                 )
        :
#else
    ControlParam(int fs_ban_del, int fs_ban_sub, int fs_ban_cop = 0
#ifdef USE_DELETE_AT_ZERO_COUNT
                 ,
                 int fs_allow_del_at_zero_count = 0
#endif
                 )
        :
#endif
          s_ban_del(fs_ban_del),
          s_ban_sub(fs_ban_sub),
          s_ban_cop(fs_ban_cop)
#ifdef USE_DELETE_AT_ZERO_COUNT
          ,
          s_allow_del_at_zero_count(fs_allow_del_at_zero_count)
#endif
    {
      if (!(fs_ban_del == 0 || fs_ban_del == 1)) {
        mcerr << "ERROR in ControlParam::ControlParam(...)\n";
        mcerr << "s_ban_del is outside limits, s_ban_del=" << fs_ban_del
              << '\n';
        spexit(mcerr);
      }
      if (fs_ban_sub < 0 || fs_ban_sub > 2) {
        mcerr << "ERROR in ControlParam::ControlParam(...):\n";
        mcerr << "s_ban_sub is outside limits, s_ban_sub=" << fs_ban_sub
              << '\n';
        spexit(mcerr);
      }
      if (fs_ban_cop < 0 || fs_ban_cop > 2) {
        mcerr << "ERROR in ControlParam::ControlParam(...):\n";
        mcerr << "s_ban_cop is outside limits, s_ban_cop=" << fs_ban_cop
              << '\n';
        spexit(mcerr);
      }
#ifdef USE_DELETE_AT_ZERO_COUNT
      if (!(s_allow_del_at_zero_count == 0 || s_allow_del_at_zero_count == 1)) {
        mcerr << "ERROR in ControlParam::ControlParam(...)\n";
        mcerr << "s_allow_del_at_zero_count is outside limits, "
                 "s_allow_del_at_zero_count=" << fs_allow_del_at_zero_count
              << '\n';
        spexit(mcerr);
      }
#endif
    }
  };  // the total 6 bits
#endif  // ifdef USE_BIT_FIELDS

  inline RegPassivePtr(void)
      :
#ifdef USE_BIT_OPERA
        control_word(0),
#elif defined USE_BIT_FIELDS
        conparam(),
#else
        s_ban_del(0),
        s_ban_sub(0),
        s_ban_cop(0),
#ifdef USE_DELETE_AT_ZERO_COUNT
        s_allow_del_at_zero_count(0),
#endif
#endif
        cpp(NULL) {
  }
#ifdef USE_CHAR_GETSETTERS_PARAMETERS
  inline RegPassivePtr(char fs_ban_del, char fs_ban_sub, char fs_ban_cop = 0)
      :
#else
  inline RegPassivePtr(int fs_ban_del, int fs_ban_sub, int fs_ban_cop = 0)
      :
#endif
#ifdef USE_BIT_OPERA
        control_word(0),
#elif defined(USE_BIT_FIELDS)
        conparam(fs_ban_del, fs_ban_sub, fs_ban_cop),
#else
  s_ban_del(fs_ban_del), s_ban_sub(fs_ban_sub), s_ban_cop(fs_ban_cop),
#ifdef USE_DELETE_AT_ZERO_COUNT
      s_allow_del_at_zero_count(0),
#endif
#endif
        cpp(NULL) {
#ifdef USE_BIT_OPERA
    set_s_ban_del(fs_ban_del);
    set_s_ban_sub(fs_ban_sub);
    set_s_ban_cop(fs_ban_cop);
#endif
  }

  RegPassivePtr(const RegPassivePtr& f);

  RegPassivePtr& operator=(const RegPassivePtr& f);
  // Copies s_ban_del and s_ban_sub
  // Also calls clear_pointers if s_ban_sub==1 or
  // spexit() if s_ban_sub==2 and the alist_of_prot_pointers is not empty.

  inline CountPP_ns::CountPassivePtr* book(void) const {
    if (!cpp) cpp = new CountPP_ns::CountPassivePtr(this);
    cpp->book();
    return cpp;
  }

  inline void clear_pointers(void) const { 
    // all pointers addressing this are cleared;
    if (cpp) cpp->change_rpp(NULL);
  }

  virtual RegPassivePtr* copy() const { return new RegPassivePtr(*this); }

  virtual ~RegPassivePtr();
  // If there are pointers addressed this object and s_ban_del = 1,
  // the program is terminated with output message to mcerr.
  // Otherwise (normal behaviour) the references are cleared.

  virtual void print(std::ostream& file, int l = 1) const;
  friend std::ostream& operator<<(std::ostream& file, const RegPassivePtr& f);

// Three parameters controlling behaviour of pointee object at
// its deletion and substitution.
// They are included for the sake of flexibility and
// they are necessary  very rarely. Normally they are not used.
// If needed, you can set them though constructors,
// or change responsibly directly.
// Obviously, this is a temporary design.
// It needs then to pack all these small numbers in one computer word.

#ifdef USE_BIT_OPERA
 private:
  unsigned char control_word;

 public:
#ifdef USE_PRIVATE_PARAM_IN_PASSIVEPTR
 private:
#endif
  static int s_ban_del_ignore;
  static int s_print_adr_cpp;
#ifdef USE_PRIVATE_PARAM_IN_PASSIVEPTR
 public:
#endif

#elif defined(USE_BIT_FIELDS)
 private:
  ControlParam conparam;

 public:
#ifdef USE_PRIVATE_PARAM_IN_PASSIVEPTR
 private:
#endif
  static int s_ban_del_ignore;
  static int s_print_adr_cpp;
#ifdef USE_PRIVATE_PARAM_IN_PASSIVEPTR
 public:
#endif

#else  // for elif defined( USE_BIT_FIELDS )

#ifdef USE_PRIVATE_PARAM_IN_PASSIVEPTR
 private:
#endif
#ifdef USE_CHAR_CONTROL_VARIABLES
  char s_ban_del;
#else
  int s_ban_del;
#endif
  // sign whether this object can (0) or cannot (1)
  // be deleted if it is addressed.
  // Normally it can. When it happens, the pointers addressing this object
  // are cleared and set to NULL.
  // If user needs to find where he deletes referenced object,
  // he may assign this variable to 1

  static int s_ban_del_ignore;
// auxiliary variable with default value 0,
// meaning that s_ban_del is not ignored.
// In the case of value 1 s_ban_del is ignored, whatever it is.
// It is used in RegPassivePtr::~RegPassivePtr() to avoid loops at exiting
// after the fist deletion of referenced object at s_ban_del = 1 is found.

#ifdef USE_CHAR_CONTROL_VARIABLES
  char s_ban_sub;
#else
  int s_ban_sub;
#endif
// sign controlling substitution of addressed object:
// 0 - it can be substituted and the pointers addressing "this" preserve
//     their values (normal default case).
// 1 - it can be substituted, but the pointers will be assigned NULL.
//     In the case of self-assignment and valid pointers to "this"
//     the run-time error is triggered, because otherwise either
//     the pointers to copied object would not be preserved or
//     the pointers to "this" would not be cleared - principal contradiction.
// 2 - it cannot be substituted, if there are pointers to "this",
//     attempt to substitute in this case triggers error.
//     In the case of absence of pointers to this it is substituted.
// Note: if the substitution is allowed,
// s_ban_del, s_ban_sub, and  s_ban_cop are themselves substituted -
// arbitrary decision, perhaps correct.

#ifdef USE_CHAR_CONTROL_VARIABLES
  char s_ban_cop;
#else
  int s_ban_cop;
#endif
// sign controlling copying addressed object
// 0 - it can always be copied
// 1 - it can be copied only if there are no references,
//     otherwise it will be runtime error.
// 2 - it can never be copied, but it is anyway runtime error,
//     the compiler cannot detect it.

#ifdef USE_DELETE_AT_ZERO_COUNT
#ifdef USE_CHAR_CONTROL_VARIABLES
  char s_allow_del_at_zero_count;
#else
  int s_allow_del_at_zero_count;
#endif
// concession to people who likes shared pointers.
// Instructs PassivePtr::~PassivePtr to delete the object if that
// pointer is last. Think twice before use.
// It basically violates the ideoma of reference to independent object.
// Also in the case of circular references the program can fall into loop
// of destructors, which is not the case for regular passive pointers.
#endif

  static int s_print_adr_cpp;
// signature to print the address of cpp in operator<<
// by default it is 0 and address is not printed,
// but only the remark that it is NULL or not NULL is printed.
// It is good for making such output listings which do not depend
// on computer and can be automatically compared.
// But in the case of some deep debug the user can make them printed
// if he assigns s_print_adr_cpp = 1.
#ifdef USE_PRIVATE_PARAM_IN_PASSIVEPTR
 public:
#endif

#endif  // for elif defined( USE_BIT_FIELDS )

#ifdef USE_GETSETTERS_IN_PASSIVEPTR
 public:
#ifdef USE_CHAR_GETSETTERS_PARAMETERS
  inline void set_s_ban_del(char fs_ban_del)
#else
  inline void set_s_ban_del(int fs_ban_del)
#endif
  {
#ifdef USE_BIT_OPERA
    if (fs_ban_del == 0)
      clear_bit(control_word, eb_s_ban_del);
    else if (fs_ban_del == 1)
      rise_bit(control_word, eb_s_ban_del);
#else
    if (fs_ban_del == 0 || fs_ban_del == 1) {
#ifdef USE_BIT_FIELDS
      conparam.s_ban_del = fs_ban_del;
#else
      s_ban_del = fs_ban_del;
#endif
    }
#endif
    else {
      mcerr << "ERROR in inline void set_s_ban_del(int fs_ban_del):\n";
      mcerr << "s_ban_del is outside limits, s_ban_del=" << int(fs_ban_del)
            << '\n';
      spexit(mcerr);
    }
  }

#ifdef USE_CHAR_GETSETTERS_PARAMETERS
  inline char get_s_ban_del(void) const
#else
  inline int get_s_ban_del(void) const
#endif
#ifdef USE_BIT_OPERA
  {
    if ((control_word & eb_s_ban_del) != 0)
      return 1;
    else
      return 0;
  }
#elif defined(USE_BIT_FIELDS)
  {
    return conparam.s_ban_del;
  }
#else
  { return s_ban_del; }
#endif

#ifdef USE_CHAR_GETSETTERS_PARAMETERS
  inline static void set_s_ban_del_ignore(char fs_ban_del_ignore)
#else
  inline static void set_s_ban_del_ignore(int fs_ban_del_ignore)
#endif
  {
    if (fs_ban_del_ignore == 0 || fs_ban_del_ignore == 1) {
      s_ban_del_ignore = fs_ban_del_ignore;
    } else {
      mcerr << "ERROR in inline void set_s_ban_del_ignore(int "
               "fs_ban_del_ignore ):\n";
      mcerr << "s_ban_del_ignore is outside limits, s_ban_del_ignore="
            << int(fs_ban_del_ignore) << '\n';
      spexit(mcerr);
    }
  }

#ifdef USE_CHAR_GETSETTERS_PARAMETERS
  inline static char get_s_ban_del_ignore(void)
#else
  inline static int get_s_ban_del_ignore(void)
#endif
  {
    return s_ban_del_ignore;
  }

#ifdef USE_CHAR_GETSETTERS_PARAMETERS
  inline void set_s_ban_sub(char fs_ban_sub)
#else
  inline void set_s_ban_sub(int fs_ban_sub)
#endif
  {
#ifdef USE_BIT_OPERA
    if (fs_ban_sub == 0) {
      clear_bit(control_word, eb_s_ban_sub1);
      clear_bit(control_word, eb_s_ban_sub2);
    } else if (fs_ban_sub == 1) {
      rise_bit(control_word, eb_s_ban_sub1);
      clear_bit(control_word, eb_s_ban_sub2);
    } else if (fs_ban_sub == 2) {
      clear_bit(control_word, eb_s_ban_sub1);
      rise_bit(control_word, eb_s_ban_sub2);
    }
#else
    if (fs_ban_sub >= 0 && fs_ban_sub <= 2) {
#ifdef USE_BIT_FIELDS
      conparam.s_ban_sub = fs_ban_sub;
#else
      s_ban_sub = fs_ban_sub;
#endif
    }
#endif
    else {
      mcerr << "ERROR in inline void set_s_ban_sub(int fs_ban_sub):\n";
      mcerr << "s_ban_sub is outside limits, s_ban_sub=" << int(fs_ban_sub)
            << '\n';
      spexit(mcerr);
    }
  }

#ifdef USE_CHAR_GETSETTERS_PARAMETERS
  inline char get_s_ban_sub(void) const
#else
  inline int get_s_ban_sub(void) const
#endif
#ifdef USE_BIT_OPERA
  {
    if ((control_word & eb_s_ban_sub2) == 0) {
      if ((control_word & eb_s_ban_sub1) == 0)
        return 0;
      else
        return 1;
    } else {
      // Iprintn(mcout, (control_word & eb_s_ban_sub1) );
      // Iprintn(mcout, (control_word & eb_s_ban_sub2) );
      return 2;
    }
  }
#elif defined(USE_BIT_FIELDS)
  {
    return conparam.s_ban_sub;
  }
#else
  { return s_ban_sub; }
#endif

#ifdef USE_CHAR_GETSETTERS_PARAMETERS
  inline void set_s_ban_cop(char fs_ban_cop)
#else
  inline void set_s_ban_cop(int fs_ban_cop)
#endif
  {
#ifdef USE_BIT_OPERA
    if (fs_ban_cop == 0) {
      clear_bit(control_word, eb_s_ban_cop1);
      clear_bit(control_word, eb_s_ban_cop2);
    } else if (fs_ban_cop == 1) {
      rise_bit(control_word, eb_s_ban_cop1);
      clear_bit(control_word, eb_s_ban_cop2);
    } else if (fs_ban_cop == 2) {
      clear_bit(control_word, eb_s_ban_cop1);
      rise_bit(control_word, eb_s_ban_cop2);
    }
#else
    if (fs_ban_cop >= 0 && fs_ban_cop <= 2) {
#ifdef USE_BIT_FIELDS
      conparam.s_ban_cop = fs_ban_cop;
#else
      s_ban_cop = fs_ban_cop;
#endif
    }
#endif
    else {
      mcerr << "ERROR in inline void set_s_ban_cop(int fs_ban_cop):\n";
      mcerr << "s_ban_cop is outside limits, s_ban_cop=" << int(fs_ban_cop)
            << '\n';
      spexit(mcerr);
    }
  }

#ifdef USE_CHAR_GETSETTERS_PARAMETERS
  inline char get_s_ban_cop(void) const
#else
  inline int get_s_ban_cop(void) const
#endif
#ifdef USE_BIT_OPERA
  {
    if ((control_word & eb_s_ban_cop2) == 0) {
      if ((control_word & eb_s_ban_cop1) == 0)
        return 0;
      else
        return 1;
    } else {
      return 2;
    }
  }
#elif defined USE_BIT_FIELDS
  {
    return conparam.s_ban_cop;
  }
#else
  { return s_ban_cop; }
#endif

#ifdef USE_DELETE_AT_ZERO_COUNT

#ifdef USE_CHAR_GETSETTERS_PARAMETERS
  inline void set_s_allow_del_at_zero_count(char fs_allow_del_at_zero_count)
#else
  inline void set_s_allow_del_at_zero_count(int fs_allow_del_at_zero_count)
#endif
  {
#ifdef USE_BIT_OPERA
    if (fs_allow_del_at_zero_count == 0)
      clear_bit(control_word, eb_s_allow_del_at_zero_count);
    else if (fs_allow_del_at_zero_count == 1)
      rise_bit(control_word, eb_s_allow_del_at_zero_count);
#else
    if (fs_allow_del_at_zero_count == 0 || fs_allow_del_at_zero_count == 1) {
#ifdef USE_BIT_FIELDS
      conparam.s_allow_del_at_zero_count = fs_allow_del_at_zero_count;
#else
      s_allow_del_at_zero_count = fs_allow_del_at_zero_count;
#endif
    }
#endif
    else {
      mcerr << "ERROR in inline void set_s_allow_del_at_zero_count(int "
               "fs_allow_del_at_zero_count):\n";
      mcerr << "s_allow_del_at_zero_count is outside limits, "
               "s_allow_del_at_zero_count=" << int(fs_allow_del_at_zero_count)
            << '\n';
      spexit(mcerr);
    }
  }

#ifdef USE_CHAR_GETSETTERS_PARAMETERS
  inline char get_s_allow_del_at_zero_count(void) const
#else
  inline int get_s_allow_del_at_zero_count(void) const
#endif
#ifdef USE_BIT_OPERA
  {
    if ((control_word & eb_s_allow_del_at_zero_count) != 0)
      return 1;
    else
      return 0;
  }
#elif defined(USE_BIT_FIELDS)
  {
    return conparam.s_allow_del_at_zero_count;
  }
#else
  { return s_allow_del_at_zero_count; }
#endif

#endif  // for ifdef USE_DELETE_AT_ZERO_COUNT

#ifdef USE_CHAR_GETSETTERS_PARAMETERS
  inline static void set_s_print_adr_cpp(char fs_print_adr_cpp)
#else
  inline static void set_s_print_adr_cpp(int fs_print_adr_cpp)
#endif
  {
    if (fs_print_adr_cpp == 0 || fs_print_adr_cpp == 1) {
      s_print_adr_cpp = fs_print_adr_cpp;
    } else {
      mcerr << "ERROR in inline void set_s_print_adr_cpp(int fs_print_adr_cpp "
               "):\n";
      mcerr << "s_print_adr_cpp is outside limits, s_print_adr_cpp="
            << int(fs_print_adr_cpp) << '\n';
      spexit(mcerr);
    }
  }

#ifdef USE_CHAR_GETSETTERS_PARAMETERS
  inline static char get_s_print_adr_cpp(void)
#else
  inline static int get_s_print_adr_cpp(void)
#endif
  {
    return s_print_adr_cpp;
  }

#endif  // for ifdef USE_GETSETTERS_IN_PASSIVEPTR

  long get_total_number_of_references(void) const;

 private:
  mutable CountPP_ns::CountPassivePtr* cpp;  // reference to counter class
};

CountPP_ns::CountPassivePtr::~CountPassivePtr() {
  if (number_of_booked != 0) {
    mcerr << "Error in CountPassivePtr::~CountPassivePtr():\n"
          << " number_of_booked != 0, number_of_booked=" << number_of_booked
          << '\n';
    if (rpp != NULL)
      mcerr << (*rpp);
    else
      mcerr << "rpp = NULL\n";
    spexit(mcerr);
  }
  if (rpp != NULL) rpp->cpp = NULL;
}

template <class X>
class PassivePtr {
 public:
  friend class RegPassivePtr;

  inline X* get(void) const {
    if (cpp == NULL) return NULL;
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR

    if (cpp->get_rpp() == NULL) return NULL;
#ifdef USE_CHECK_DOUBLE_PTR_IN_PASSIVEPTR
    X* temp_ptr = dynamic_cast<X*>(const_cast<RegPassivePtr*>(cpp->get_rpp()));
    if (ptr != temp_ptr) {
      mcerr << "Error in inline X* PassivePtr::get(void):\n";
      mcerr << "ptr != temp_ptr\n";
      spexit(mcerr);
    }
#endif
    return ptr;
#else  // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
    else
      return (X*)(cpp->get_rpp());
#endif  // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
  }

  // Since the pointer is passive, there is no cloning and pass and put
  // make the same job.
  // 03.06.2006: commenting off pass in order to forbid
  // correct for active pointer but
  // erroneous for passive pointer statements like ptr.pass(new any_class).
  // inline void pass(const X* fptr) {put( fptr );}

  inline void put(X* fptr);  // unregirster old registered object and
                             // pass fptr to  this.

  // void print(std::ostream& file, int l) const
  // { if(ptr!=NULL) ptr->print(file, l); }
  inline PassivePtr(void)
      : cpp(NULL)
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
        ,
        ptr(NULL)
#endif
  {
  }
  inline PassivePtr(X* fptr) {
    if (fptr != NULL)
      cpp = fptr->book();
    else
      cpp = NULL;
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
    ptr = fptr;
#endif
  }
  inline PassivePtr(X& fptr) {
    cpp = fptr.book();
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
    ptr = &fptr;
#endif
  }

  inline PassivePtr(const PassivePtr<X>& f);

  inline PassivePtr<X>& operator=(const PassivePtr<X>& f);
  inline PassivePtr<X>& operator=(X* f);

  // Y may be any derived class from X
  template <class Y>
  PassivePtr<X>(const PassivePtr<Y>& f);
  inline void move_pointer(PassivePtr<X>& f);
  // get the pointer from f, clear f,
  // put the pointer to this->ptr
  // and register it. Thus, the pointer is passed from f to this.

  inline X* operator->(void) const;
  inline X& operator*(void) const;
  inline X* getver(void) const;
#ifdef IMPLICIT_X_STAR
  inline operator X*(void) const;
#endif
  long get_total_number_of_references(void) const {
    if (cpp == NULL)
      return 0;
    else
      return cpp->get_number_of_booked();
  }
  void print(std::ostream& file, int l = 1) const;
  virtual PassivePtr* copy() const { return new PassivePtr(*this); }
  virtual ~PassivePtr();

 private:
  CountPP_ns::CountPassivePtr* cpp;
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
  X* ptr;
#endif
};

template <class X>
template <class Y>
PassivePtr<X>::PassivePtr(const PassivePtr<Y>& f)
    : cpp(NULL) {
  put((f.get()));
}

template <class X>
void PassivePtr<X>::print(std::ostream& file, int l) const {
  Ifile << "PassivePtr<X>:";
  if (get() == NULL)
    file << " pointer is NULL, no object, number of ref's is "
         << get_total_number_of_references() << "\n";
  else {
    file << noindent;
    get()->print(file, l);
    file << yesindent;
    indn.n += 2;
    Ifile << "number of ref's is " << get_total_number_of_references() << '\n';
    indn.n -= 2;
  }
}
template <class X>
std::ostream& operator<<(std::ostream& file, const PassivePtr<X>& f) {
  Ifile << "PassivePtr<X>:";
  if (f.get() == NULL)
    file << " pointer is NULL, no object, number of ref's is "
         << f.get_total_number_of_references() << "\n";
  else {
    file << noindent;
    file << (*f.get());
    file << yesindent;
    indn.n += 2;
    Ifile << "number of ref's is " << f.get_total_number_of_references()
          << '\n';
    indn.n -= 2;
  }
  return file;
}

template <class X>
inline void PassivePtr<X>::put(X* fptr) {
  // unregister old registered object and pass fptr to this.
  if (cpp != NULL) {
#ifdef USE_DELETE_AT_ZERO_COUNT
    const RegPassivePtr* arptr;
    if ((arptr = cpp->get_rpp()) != NULL &&
#ifdef USE_PRIVATE_PARAM_IN_PASSIVEPTR
        arptr->get_s_allow_del_at_zero_count() == 1 &&
#else
        arptr->s_allow_del_at_zero_count == 1 &&
#endif
        cpp->get_number_of_booked() == 1)
      delete arptr;
// fptr should not be part of old object!
#endif
    cpp->unbook();
    if (cpp->get_rpp() == NULL && cpp->get_number_of_booked() == 0)
      // there is no referred object and no other references
      delete cpp;
  }
  if (fptr != NULL)
    cpp = fptr->book();
  else
    cpp = NULL;
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
  ptr = fptr;
#endif
}

template <class X>
inline PassivePtr<X>::PassivePtr(const PassivePtr<X>& f) {
  if (f.cpp != NULL) {
    f.cpp->book();
    cpp = f.cpp;
  } else
    cpp = NULL;
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
  ptr = f.ptr;
#endif
}
template <class X>
inline PassivePtr<X>& PassivePtr<X>::operator=(const PassivePtr<X>& f) {
  if (this != &f) put(f.get());
  return *this;
}

template <class X>
inline PassivePtr<X>& PassivePtr<X>::operator=(X* f) {
  put(f);
  return *this;
}

template <class X>
inline void PassivePtr<X>::move_pointer(PassivePtr<X>& f) {
  if (cpp != NULL) {
#ifdef USE_DELETE_AT_ZERO_COUNT
    const RegPassivePtr* arptr;
    if ((arptr = cpp->get_rpp()) != NULL &&
#ifdef USE_PRIVATE_PARAM_IN_PASSIVEPTR
        arptr->get_s_allow_del_at_zero_count() == 1 &&
#else
        arptr->s_allow_del_at_zero_count == 1 &&
#endif
        cpp->get_number_of_booked() == 1)
      delete arptr;
// fptr should not be part of old object!
#endif
    cpp->unbook();
    if (cpp->get_rpp() == NULL && cpp->get_number_of_booked() == 0) {
      delete cpp;
      cpp = NULL;
    }
  }
  cpp = f.cpp;
  if (cpp != NULL) cpp->book();
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
  ptr = f.ptr;
#endif
  f.put(NULL);
}

template <class X>
inline X* PassivePtr<X>::operator->(void) const {
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
#ifdef SKIP_CHECKS_NULL
  return ptr;
#else
  if (cpp == NULL) {
    mcerr << "Error in X* PassivePtr<X>::operator->(void) const: cpp == NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
  if (cpp->get_rpp() == NULL) {
    mcerr << "Error in X* PassivePtr<X>::operator->(void) const: "
             "cpp->get_rpp() == NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
#ifdef USE_CHECK_DOUBLE_PTR_IN_PASSIVEPTR
  X* temp_ptr = dynamic_cast<X*>(const_cast<RegPassivePtr*>(cpp->get_rpp()));
  if (ptr != temp_ptr) {
    mcerr << "Error in inline X* PassivePtr::operator->(void):\n";
    mcerr << "ptr != temp_ptr\n";
    spexit(mcerr);
  }
#endif
  return ptr;
#endif  // for ifdef SKIP_CHECKS_NULL
#else   // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
#ifdef SKIP_CHECKS_NULL
  const RegPassivePtr* rpp = cpp->get_rpp();
#else
  if (cpp == NULL) {
    mcerr << "Error in X* PassivePtr<X>::operator->(void) const: cpp == NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
  const RegPassivePtr* rpp;
  if ((rpp = cpp->get_rpp()) == NULL) {
    mcerr << "Error in X* PassivePtr<X>::operator->(void) const: "
             "cpp->get_rpp() == NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
#endif
  return (X*)(rpp);
#endif  // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
}

template <class X>
inline X& PassivePtr<X>::operator*(void) const {
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
#ifdef SKIP_CHECKS_NULL
  return *ptr;
#else
  if (cpp == NULL) {
    mcerr << "Error in X& PassivePtr<X>::operator*(void) const: cpp == NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
  if (cpp->get_rpp() == NULL) {
    mcerr << "Error in X& PassivePtr<X>::operator*(void) const: cpp->get_rpp() "
             "== NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
#ifdef USE_CHECK_DOUBLE_PTR_IN_PASSIVEPTR
  X* temp_ptr = dynamic_cast<X*>(const_cast<RegPassivePtr*>(cpp->get_rpp()));
  if (ptr != temp_ptr) {
    mcerr << "Error in inline X& PassivePtr::operator*(void):\n";
    mcerr << "ptr != temp_ptr\n";
    spexit(mcerr);
  }
#endif
  return *ptr;
#endif  // for ifdef SKIP_CHECKS_NULL
#else   // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
#ifdef SKIP_CHECKS_NULL
  static const RegPassivePtr* rpp = cpp->get_rpp();
#else
  if (cpp == NULL) {
    mcerr << "Error in X& PassivePtr<X>::operator*(void) const: cpp == NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
  static const RegPassivePtr* rpp;
  // I have put static here only to avoid
  // perhaps erroneous warning which appear
  // in compiler : gcc version egcs-2.91.66 19990314/Linux (egcs-1.1.2 release)
  // This is old compiler. At newer ones there is no problems.
  // The message is
  // AbsPtr.h:997: warning: reference to local variable `rpp' returned
  // This is in fact wrong statement, but so as not to make users thinking
  // about it, I made this variable static.

  if ((rpp = cpp->get_rpp()) == NULL) {
    mcerr << "Error in X& PassivePtr<X>::operator*(void) const: cpp->get_rpp() "
             "== NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
#endif
  return *((X*)(rpp));
#endif  // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
}

template <class X>
inline X* PassivePtr<X>::getver(void) const {
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
#ifdef SKIP_CHECKS_NULL
  return ptr;
#else
  if (cpp == NULL) {
    mcerr << "Error in X* PassivePtr<X>::getver(void) const: cpp == NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
  if (cpp->get_rpp() == NULL) {
    mcerr << "Error in X* PassivePtr<X>::getver(void) const: cpp->get_rpp() == "
             "NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
#ifdef USE_CHECK_DOUBLE_PTR_IN_PASSIVEPTR
  X* temp_ptr = dynamic_cast<X*>(const_cast<RegPassivePtr*>(cpp->get_rpp()));
  if (ptr != temp_ptr) {
    mcerr << "Error in inline X* PassivePtr::getver(void):\n";
    mcerr << "ptr != temp_ptr\n";
    spexit(mcerr);
  }
#endif
  return ptr;
#endif  // for ifdef SKIP_CHECKS_NULL
#else   // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
#ifdef SKIP_CHECKS_NULL
  const RegPassivePtr* rpp = cpp->get_rpp();
#else
  if (cpp == NULL) {
    mcerr << "Error in X* PassivePtr<X>::getver(void) const: cpp == NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
  const RegPassivePtr* rpp;
  if ((rpp = cpp->get_rpp()) == NULL) {
    mcerr << "Error in X* PassivePtr<X>::getver(void) const: cpp->get_rpp() == "
             "NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
#endif
  return (X*)(rpp);
#endif  // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
}

#ifdef IMPLICIT_X_STAR
template <class X>
inline PassivePtr<X>::operator X*(void) const {
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
#ifdef SKIP_CHECKS_NULL
  return ptr;
#else
  if (cpp == NULL) {
    mcerr << "Error in PassivePtr<X>::operator X*(void) const: cpp == NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
  if (cpp->get_rpp() == NULL) {
    mcerr << "Error in X* PassivePtr<X>::operator X*(void) const: "
             "cpp->get_rpp() == NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
#ifdef USE_CHECK_DOUBLE_PTR_IN_PASSIVEPTR
  X* temp_ptr = dynamic_cast<X*>(const_cast<RegPassivePtr*>(cpp->get_rpp()));
  if (ptr != temp_ptr) {
    mcerr << "Error in inline X* PassivePtr::operator X*(void):\n";
    mcerr << "ptr != temp_ptr\n";
    spexit(mcerr);
  }
#endif
  return ptr;
#endif  // for ifdef SKIP_CHECKS_NULL
#else   // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
#ifdef SKIP_CHECKS_NULL
  const RegPassivePtr* rpp = cpp->get_rpp();
#else
  if (cpp == NULL) {
    mcerr
        << "Error in X* PassivePtr<X>::operator X*(void) const: cpp == NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
  const RegPassivePtr* rpp;
  if ((rpp = cpp->get_rpp()) == NULL) {
    mcerr << "Error in X* PassivePtr<X>::operator X*(void) const: "
             "cpp->get_rpp() == NULL\n";
    mcerr << "This means that the pointer is emtpy, "
          << "there is no addressed object.\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
#endif
  return (X*)(rpp);
#endif  // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
}
#endif
template <class X>
inline int operator==(const PassivePtr<X>& f1, const PassivePtr<X>& f2) {
  // comparison of addresses, so it mimics regular pointers
  return f1.get() == f2.get();
}

template <class X>
PassivePtr<X>::~PassivePtr() {
  if (cpp) {
#ifdef USE_DELETE_AT_ZERO_COUNT
    const RegPassivePtr* arptr;
    if ((arptr = cpp->get_rpp()) != NULL &&
#ifdef USE_PRIVATE_PARAM_IN_PASSIVEPTR
        arptr->get_s_allow_del_at_zero_count() == 1 &&
#else
        arptr->s_allow_del_at_zero_count == 1 &&
#endif
        cpp->get_number_of_booked() == 1)
      delete arptr;
#endif
    cpp->unbook();
    // mcout<<"PassivePtr<X>::~PassivePtr(): &ptr="<<&ptr<<" *ptr="<<*ptr<<'\n';
    if (cpp->get_rpp() == NULL && cpp->get_number_of_booked() == 0) {
      delete cpp;
      cpp = NULL;
    }
  }
}

template <class X>
bool operator<(PassivePtr<X> f1, PassivePtr<X> f2) {
  // necessary for std::set
  return f1.get() < f2.get();
}

}

#endif
