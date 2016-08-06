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
#include "wcpplib/stream/prstream.h"
#include "wcpplib/util/FunNameStack.h"

//#define USE_REPLACE_ALLOC

#ifdef USE_REPLACE_ALLOC
#include "wcpplib/safetl/ReplaceAlloc.h"
#endif

//#define IMPLICIT_X_STAR
//#define INCLUDE_ActivePtrWI

//#define USE_DYNCAST_IN_PASSIVEPTR  // (possibly obsolete, see below) default
// option used to provide possibility of making base class RegPassivePtr
// virtual.
// In order to cast from base RegPassivePtr to the real pointer type
// we need to use dynamic cast, which makes the referencing
// little bit slower.

#define USE_DOUBLE_PTR_IN_PASSIVEPTR  //
// New option. Makes passive ptr keeping address of counter and address
// of object. This allow to avoid dynamic casts while still allowing
// the class RegPassivePtr be virtual.
//#define USE_CHECK_DOUBLE_PTR_IN_PASSIVEPTR
// for debug of the option above. At each access it checks
// that the object address in ptr is equal to the address
// restored from the counter.

#define USE_DELETE_AT_ZERO_COUNT  // to switch on option
                                  // for emulation of shared pointer.
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
// Therefore the use (uncommenting ) of this macro for the purposes
// other than similar curiosity does NOT have any sence.

// To be addressed by active pointer an object of a class
// should have functions
// copy() which clones the object and returns new address, and may have
// print() which prints it to a stream.
// The others (the class name, its bases etc.) is not matter.
// However sometimes it is convenient to derive such a class from AbsCont -
// an abstract class, in which the function copy is declared pure
// and the function print does nothing.
// The copy() is declared pure just to insure that the user does not
// forget to define it in derivative.
// Its body is approximately the same and consists of two lines.

#define COPY_TYPE_CHECK  // make execution longer, but allows
// to detect missings of copy functions in classes referred by
// active pointers.

//#define DEBUG_ACTIVEPTR  // print some messages at running ActivePtr
//  components
// The pilfer constructor and the pilfer function is sometimes not resolved
// (i.e. at Scientific Linux 4.1,
// on the contrary at Windows Vis. Studio it is resolved always)
// if they are declared with non-const argument.
// The solution is declaration them with const and class fields as mutable
// as follows:
#define PILF_CONST const
#define PILF_MUTABLE mutable
// Otherwise leave these macros empty
//#define PILF_CONST
//#define PILF_MUTABLE
// The mutable fields do not seem to be harmful, except if
// a user wants to add a new member functions. In the latter case
// it might be useful to compile a program which does not use pilfers
// with non-const arguments and without muables in order to verify
// whether new functions erroneously change their constant arguments.
// Then, mutables should be again switched on.
// Note: these desclarations are repeated in AbsArr.h and AbsList,
// see it for details.

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

#ifdef COPY_RETURNS_COMMON_BASE
class CommonBase {
 public:
  //virtual void printCommonBase(void) { cout<<"CommonBase::print: Hello\n";}
  // This function is just to make this "polymorphic", that is
  // with at least one virtual function
  virtual ~CommonBase() {}
};
// For macros which are designed to disappear completely without
// editing of source when covariant types are
// included and COPY_RETURNS_COMMON_BASE gets not defined anymore:
#define virt_common_base \
 public                  \
  virtual CommonBase
#define virt_common_base_col :public virtual CommonBase  // for single case
#define virt_common_base_comma \
 public                        \
  virtual CommonBase,  // one of many
#define virt_common_base_col_comma :public virtual CommonBase, // first of many
#define virt_common_base_pcomma , public virtual CommonBase    // last of many
#else
// define them just empty
#define virt_common_base
#define virt_common_base_col        // for single case
#define virt_common_base_comma      // one of many
#define virt_common_base_col_comma  // first of many
#define virt_common_base_pcomma     // last of many

#endif

/*
// Original source:
//#define AbsCont_copy(type)  virtual AbsCont* copy(void) const \
//{return new type(*this);}
//#define AnyType_copy(derived_type, base_type)  \
//virtual base_type* copy(void) const \
//{return new derived_type(*this);}
*/
// These are obliterate macros. Actually parameter base_type is not necessary
#ifndef COPY_RETURNS_COMMON_BASE
// return type
#define AbsCont_copy(type) \
  virtual type* copy(void) const { return new type(*this); }
#define AnyType_copy(derived_type, base_type) \
  virtual derived_type* copy(void) const { return new derived_type(*this); }
#else
// return CommonBase
#define AbsCont_copy(type) \
  virtual CommonBase* copy(void) const { return new type(*this); }
#define AnyType_copy(derived_type, base_type) \
  virtual CommonBase* copy(void) const { return new derived_type(*this); }
#endif

// New definitions
#ifndef COPY_RETURNS_COMMON_BASE
// return type

class AbsCont  // Abstract Container
    {
 public:
  virtual AbsCont* copy(void) const = 0;
  // printing function
  // parameter l is useful so as to control length of listing.
  virtual void print(std::ostream& file, int l) const {
    Ifile << "AbsCont::print is called, l=" << l << "\n";
  }
  virtual ~AbsCont() {}
};

// Interesting that the following macros do not seem to be working
// with templates with more than one argument, since preprocessor
// perhaps interprate the template arguments as macro arguments
// and produce compiler error.
// Direct use of these functions is recommended in this case
#define macro_copy_total(type) \
  virtual type* copy(void) const { return new type(*this); }
#define macro_copy_total_zero(type) virtual type* copy(void) const = 0
#define macro_copy_header(type) virtual type* copy(void) const
#define macro_copy_body(type) \
  type* type::copy(void) const { return new type(*this); }
#define macro_copy_body_not_defined(type)                                      \
  type* type::copy(void) const {                                               \
    mcerr << "macro_copy_body_not_defined(" << #type << "): forbidden call\n"; \
    spexit(mcerr);                                                             \
    return NULL;                                                               \
  }
// return NULL to calm compiler of Solaris which wants to see return value

#else

// return void

class AbsCont virt_common_base_col  // Abstract Container
    {
 public:
  virtual CommonBase* copy(void) const = 0;
  virtual void print(std::ostream& file, int l) const {
    Ifile << "AbsCont::print is called, l=" << l << "\n";
  }  // printing function
     // parameter l is useful so as to control length of listing.
  virtual ~AbsCont() {}
};

#define macro_copy_total(type) \
  virtual CommonBase* copy(void) const { return new type(*this); }
#define macro_copy_total_zero(type) virtual CommonBase* copy(void) const = 0
#define macro_copy_header(type) virtual CommonBase* copy(void) const;
#define macro_copy_body(type) \
  CommonBase* type::copy(void) const { return new type(*this); }
#define macro_copy_body_not_defined(type)                                      \
  CommonBase* type::copy(void) const {                                         \
    mcerr << "macro_copy_body_not_defined(" << #type << "): forbidden call\n"; \
    spexit(mcerr);                                                             \
    return NULL;                                                               \
  }
// return NULL to calm compiler of Solaris which wants to see return value
#endif

inline std::ostream& operator<<(std::ostream& file, const AbsCont& f) {
  Ifile << "operator<<AbsCont& is called.\n";
  f.print(file, 0);  // this line is needed only to avoid warnings
                     // at some severe compiler options that f is unused.
  return file;
}

// The auxiliary class determining the name of clone function
// which is member of class X and will be used for cloning
//#ifdef COPY_TYPE_CHECK
//class FunNameStack;
//static FunNameStack& FunNameStack::instance(void);
//#endif

template <class X> class StandardCopyDefinition {
 public:
  static X* copy(const X* f) {
#ifdef DEBUG_ACTIVEPTR
    mcout << "static X* StandardCopyDefinition::copy(const X* f)\n";
#endif
// If to allow the type of copy function be any (void* for example,
// it needs to convert it. This seems there is no reason to allow this.
#ifndef COPY_TYPE_CHECK
#ifndef COPY_RETURNS_COMMON_BASE
    return f->copy();
#else
    X* ptr = dynamic_cast<X*>(f->copy());
    if (ptr == NULL) {
      mcerr << "Error in X* StandardCopyDefinition::copy(const X* f): "
            << "cannot convert pointer returned from f->copy()"
            << "to type " << typeid(X).name() << '\n';
    }
    return ptr;
#endif
#else
    //return (X*)(f->copy());
    //X* p =  static_cast< X* >(f->copy());
    //#ifndef COPY_RETURNS_VOID // this macro should NOT be used, leads to
    //errors
    X* p = f->copy();
//#else
//X* p =  dynamic_cast< X* >(f->copy());
//if(p == NULL)
//{
//	mcerr<<"Error in X* StandardCopyDefinition::copy(const X* f): "
//	     <<"cannot convert pointer returned from f->copy()"
//	     <<"to type "<<typeid(X).name()<<'\n';
//}
//#endif
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

/*
template<class X>class CopyDefinition_1
{public:
  static X* copy(const X* f)
    {
      //mcout<<"static X* CopyDefinition_1::copy(const X* f)\n";
      //return (X*)(f->copy1());
#ifndef COPY_TYPE_CHECK
#ifndef COPY_RETURNS_VOID
      return f->copy1();
#else
      return static_cast< X* >(f->copy1());
#endif
#else
#ifndef COPY_RETURNS_VOID
      X* p = f->copy1();
#else
      X* p =  static_cast< X* >(f->copy1());
#endif
      if(typeid(*p) != typeid(*f))
      {
	mcerr<<"Error in X* CopyDefinition_1::copy(const X* f): "
	     <<"typeid(*p) != typeid(*f) \n";
	mcerr<<"Type of X is (in internal notations) "<<typeid(X).name()<<'\n';
	mcerr<<"Type of *p is (in internal notations) "<<typeid(*p).name()<<'\n';
	mcerr<<"Type of *f is (in internal notations) "<<typeid(*f).name()<<'\n';
	mcerr<<"Possible reasone is omiting of copy function in one of the derivative
classes\n";
	spexit(mcerr);
      }
      return p;
#endif
    }
};
*/

/*
It looks like that this thing is not necessary
template<class X>class VoidCopyDefinition
{public:
  static X* copy(const X* f)
    {
      //mcout<<"static X* VoidCopyDefinition::copy(const X* f)\n";
      //mcout<<"Type of X is (in internal notations) "<<typeid(X).name()<<'\n';
      //mcout<<"Type of f is (in internal notations) "<<typeid(f).name()<<'\n';
      //mcout<<"Type of *f is (in internal notations)
"<<typeid(*f).name()<<'\n';
      //return (X*)(f->copy()); // return of f->copy();
      // assumed to be void, and we convert it directly
      X* p =  static_cast< X* >(f->copy());
      if(typeid(*p) != typeid(*f))
      {
	mcerr<<"Error in X* VoidCopyDefinition::copy(const X* f): "
	     <<"typeid(*p) != typeid(*f) \n";
	mcerr<<"Type of X is (in internal notations) "<<typeid(X).name()<<'\n';
	mcerr<<"Type of *p is (in internal notations) "<<typeid(*p).name()<<'\n';
	mcerr<<"Type of *f is (in internal notations) "<<typeid(*f).name()<<'\n';
	mcerr<<"Possible reasone is omiting of copy function in one of the derivative
classes\n";
	spexit(mcerr);
      }
      return p;
    }
};
*/
// This does not require from pointee object to provide
// copy function, but don't preserve inheritance as well

template <class X> class CopyDefinitionWithoutInheritance {
 public:
  static X* copy(const X* f) {
#ifdef DEBUG_ACTIVEPTR
    mcout << "static X* CopyDefinitionWithoutInheritance::copy(const X* f)\n";
#endif
    return new X(*f);
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

#define USE_OLD_POINTER_NAMES
#ifdef USE_OLD_POINTER_NAMES
// To use old name:
#define AutoCont ActivePtr
#endif

enum Pilfer {
  steal
};
enum Clone {
  do_clone
};
enum Pass {
  dont_clone
};

template <class X, class C = StandardCopyDefinition<X> >
class ActivePtr virt_common_base_col
    // Active pointer or automatic container or controlling pointer
    {
 public:
  // First of all, two pointer's operations.
  // NULL address is interpreted as error.
  inline X* operator->(void) const;
  inline X& operator*(void) const;

  // get with verification, that is the same as two operators above,
  // but with sintax of function:
  inline X* getver(void) const;

#ifdef IMPLICIT_X_STAR
  inline operator X*(void) const;
#endif

  // Then, gentle exctraction of pointer without check:
  inline X* get(void) const { return ptr; }

  // Two variants of putting the object under control of the pointer:
  // 1. put():  with cloning
  //            (the ActivePtr will keep copy of what is found at fptr).
  // 2. pass(): with incerting given address under control
  //            (the ActivePtr will keep fptr, which should be address of
  //             block of dynamic memory).
  // The old kept object in deleted in both cases.
  //
  inline void put(const X* fptr)  // delete old owned object and
      // copy object fptr to ownership of this.
      // At fptr==NULL old object is deleted and new one is not copyed.
      {                           //
#ifdef DEBUG_ACTIVEPTR
    mcout << "inline void put(const X* fptr)\n";
    Iprintn(mcout, ptr);
    mcout << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    mcout << "Type of *fptr is (in internal notations) " << typeid(*fptr).name()
          << '\n';
#endif
    // Initialization of copy should go ahead, since
    // object *fptr could be dependant or part of *this,
    // and could be deleted prior to copying  at the reverse order.
    X* ptr_temp = (fptr != NULL ? C::copy(fptr) : (X*)(NULL));
    if (ptr != NULL) delete ptr;
    ptr = ptr_temp;
#ifdef DEBUG_ACTIVEPTR
    mcout << "finishing inline void put(const X* fptr):\n";
    Iprintn(mcout, ptr);
#endif
  }
  inline void pass(X* fptr)  // delete old owned object and
      // pass address of object fptr to ownership of this
      // without copying.
      // At fptr==NULL old object is deleted and new one is not copyed.
      // In general this is dengerous operation.
      // In particular if object pointed by fptr is direct or indirect part
      // of object pointed by ptr, it will be destroyed, which would probably
      // cause error.
      // But it is more efficient since does not require copying.
      {                      //
                             //if(ptr!=NULL) delete ptr;

    if (ptr != NULL) {
      if (fptr != NULL) {
        mcerr << "ERROR in ActivePtr::pass(X* fptr):\n";
        mcerr << "Both the destination and source pointers are not empty\n";
        // if f belongs to *ptr, deletion of *ptr would lead to
        // deletion of f. In order to avoid this unexpected behaviour,
        // we interprete non-zero ptr as error.
        // The user should clear ptr first and then use this function.
        spexit(mcerr);
      } else {
        delete ptr;
      }
    }
    ptr = fptr;
  }

  // Removing the object from the responsibility of smart pointer
  // without copying
  inline X* extract(void) {
    X* ret = ptr;
    ptr = NULL;
    return ret;
  }
  inline void clear(void) {
#ifdef DEBUG_ACTIVEPTR
    mcout << "ActivePtr::clear is called, ptr =" << ptr << '\n';
#endif
    if (ptr != NULL) {
      delete ptr;
      ptr = NULL;
    }
  }

  inline void pilfer(PILF_CONST ActivePtr<X, C>& f)
      //!Attention: actually not const
      {
#ifdef DEBUG_ACTIVEPTR
    mcout << "ActivePtr::pilfer is called\n";
#endif
    if (this != &f) {
      if (ptr != NULL) {
        if (f.ptr != NULL) {
          mcerr << "ERROR in ActivePtr::pilfer(...):\n";
          mcerr << "Both the destination and source pointers are not empty\n";
          // if f belongs to *ptr, deletion of *ptr would lead to
          // deletion of f. In order to avoid this unexpected behaviour,
          // we interprete non-zero ptr as error.
          // The user should clear ptr first and then use this function.
          spexit(mcerr);
        } else {
          delete ptr;
        }
      }
      ptr = f.ptr;
      f.ptr = NULL;
    }
    return;
  }

  void print(std::ostream& file, int l = 1) const;
  inline ActivePtr(void) : ptr(NULL) {}

  // The constructors and the assignment are interpreted as put(),
  // that is they assume cloning the object.
  /*
  inline ActivePtr(const X* fptr):ptr(fptr != NULL ? C::copy(fptr) : ((X*)NULL))
//inline ActivePtr(const X* fptr):ptr(fptr != NULL ? fptr->copy() : ((X*)NULL))
#ifndef DEBUG_ACTIVEPTR
    {}  //normal case
#else
    {mcout<<"ActivePtr(const X* fptr) is finished\n";} // debug
#endif
  */
  inline ActivePtr(const X* fptr, Clone)
      : ptr(fptr != NULL ? C::copy(fptr) : ((X*)NULL))
#ifndef DEBUG_ACTIVEPTR
        {
  }  //normal case
#else
  {
    mcout << "ActivePtr(const X* fptr, Clone) is finished, ptr =" << ptr
          << '\n';
    delete ptr;
    cout << "deleted, exit\n";
    exit(0);
  }
// debug
#endif
  //inline ActivePtr(      ActivePtr<X,C>& f):   // was required at some old
  //systems, don't remember why.
  //  ptr(f.ptr != NULL ? C::copy(f.ptr) : f.ptr) {}
  inline ActivePtr(const ActivePtr<X, C>& f)
      : ptr(f.ptr != NULL ? C::copy(f.ptr) : f.ptr)
#ifndef DEBUG_ACTIVEPTR
        {
  }
#else
  {
    mcout << "ActivePtr(const ActivePtr<X,C>& f) is finished\n";
  }  // debug
#endif

  // Pilfer constructor
  inline ActivePtr(PILF_CONST ActivePtr<X, C>& f, Pilfer)
      : ptr(f.ptr) {  //!Attention: actually not const
#ifdef DEBUG_ACTIVEPTR
    mcout << "ActivePtr(const ActivePtr<X,C>& f, Pilfer) is run\n";
#endif
    f.ptr = NULL;
  }  // debug

  // Pass constructor
  inline ActivePtr(X* fptr, Pass) : ptr(fptr) {}

  inline ActivePtr& operator=(const ActivePtr<X, C>& f) {
#ifdef DEBUG_ACTIVEPTR
    mcout << "inline ActivePtr& operator=(const ActivePtr<X,C>& f)\n";
#endif
    if (this != &f) put(f.ptr);
    return *this;
  }

  macro_copy_total(ActivePtr);
  virtual ~ActivePtr()  // unfortunately it should be virtual
      // in order to avoid some rare (not met in practice but
      // in principle possible) problems with ActivePtrReg.
      // Therefore (if there is a virtual member)
      // we can also include virtual copy (see above),
      // although there is no obvious application for it
      // (multiple indirection depth, but for what?).
      {
    if (ptr != NULL) {
      delete ptr;
      ptr = NULL;
    }
  }
  // the last assignment is for debug to assure the fast detection of error
  // in the case of continuing access to this ptr.
 private:
  PILF_MUTABLE X* ptr;  // mutable is only to allow pilfer function and pilfer
// constructor to have const descriptor of its argument and still to steal
// the object.
// Somewhy if without const, this function and this constructor
// is sometimes not recognized by Scientific Linux 4.1 compiler.
// (when argument is temporary, actual at receiving return value).
#ifdef USE_REPLACE_ALLOC
  macro_alloc
#endif
};

// Trying to change name
//typedef ActivePtr<class X, class C > ActivePtr;
// does not work

template <class X, class C>
void ActivePtr<X, C>::print(std::ostream& file, int l) const {
  //if(ptr!=NULL) ptr->print(file, l);
  Ifile << "ActivePtr<X,C>: ";
  if (ptr == NULL)
    file << " ptr==NULL. \n";  // optimized to provide automatic reading
  else {
    file << " ptr!=NULL: ";
    file << noindent;
    ptr->print(file, l);
    file << yesindent;
  }
  /*
  Ifile<<"ActivePtr<X,C>: ";
  if(ptr == NULL)
    file<<" pointer is NULL, no object.\n";
  else
  {
    file<<noindent;
    ptr->print(file, l);
    file<<yesindent;
  }
  */
}

template <class X, class C>
    std::ostream& operator<<(std::ostream& file, const ActivePtr<X, C>& f) {
  Ifile << "ActivePtr<X,C>: ";
  if (f.get() == NULL)
    file << " ptr==NULL. \n";  // optimized to provide automatic reading
  else {
    file << " ptr!=NULL: ";
    file << noindent;
    file << (*f.get());
    file << yesindent;
  }

  /*
  if(f.get() == NULL)
    file<<" pointer in NULL, no object.\n";
  else
  {
    file<<noindent;
    file<<(*f.get());
    //f.get()->print(file,1); alternatively,
    //                         but logically it is better as above
    file<<yesindent;
  }
  */
  return file;
}

template <class X, class C>
    std::istream& operator>>(std::istream& file, ActivePtr<X, C>& f) {
  long q = 13;
  long n;
  char keyline[13];
  for (n = 0; n < q - 1; n++) {
    keyline[n] = file.get();
  }
  keyline[n] = '\0';
  if (!strcmp(&(keyline[0]), " ptr==NULL. ")) {
    f.clear();
  } else {
    X x;
    file >> x;
    f.put(x);
  }
  return file;
}
/*
template<class X>
std::ostream& operator<<(std::ostream& file, const ActivePtr<X>& f)
{
  Ifile<<"ActivePtr<X>:";
  if(f.get() == NULL)
    file<<" pointer in NULL, no object\n";
  else
  {
    file<<noindent;
    f.get()->print(file,1);
    file<<yesindent;
  }
  return file;
}
*/

template <class X, class C> inline X* ActivePtr<X, C>::operator->(void) const {
#ifdef SKIP_CHECKS_NULL
  return ptr;
#else
  if (ptr != NULL) return ptr;
  mcerr << "Error in X* ActivePtr<X,C>::operator->(void) const: "
        << " ptr == NULL\n";
  mcerr << "Type of X is (in internal notations) " << typeid(X).name() << '\n';
  //mcerr<<"Type of ptr is "<<typeid(ptr).name()<<'\n';
  spexit(mcerr);
  return ptr;  // just to quiet compiler which wants to see a return value
#endif
}
template <class X, class C> inline X& ActivePtr<X, C>::operator*(void) const {
#ifdef SKIP_CHECKS_NULL
  return *ptr;
#else
  if (ptr != NULL) return *ptr;
  mcerr << "Error in X& ActivePtr<X,C>::operator*(void) const: "
        << " ptr == NULL\n";
  mcerr << "Type of X is (in internal notations) " << typeid(X).name() << '\n';
  spexit(mcerr);
  return *ptr;  // just to quiet compiler which wants to see a return value
#endif
}
template <class X, class C> inline X* ActivePtr<X, C>::getver(void) const {
#ifdef SKIP_CHECKS_NULL
  return ptr;
#else
  if (ptr != NULL) return ptr;
  mcerr << "Error in X* ActivePtr<X,C>::getver(void) const: "
        << " ptr == NULL\n";
  mcerr << "Type of X is (in internal notations) " << typeid(X).name() << '\n';
  spexit(mcerr);
  return ptr;  // just to quiet compiler which wants to see a return value
#endif
}
#ifdef IMPLICIT_X_STAR
template <class X, class C> inline ActivePtr<X, C>::operator X*(void) const {
#ifdef SKIP_CHECKS_NULL
  return ptr;
#else
  if (ptr != NULL) return ptr;
  mcerr << "Error in ActivePtr<X,C>::operator X*(void) const: "
        << " ptr == NULL\n";
  mcerr << "Type of X is (in internal notations) " << typeid(X).name() << '\n';
  spexit(mcerr);
  return ptr;  // just to quiet compiler which wants to see a return value
#endif
}
#endif
template <class X, class C>
inline void exchange(ActivePtr<X, C>& f1, ActivePtr<X, C>& f2) {
  X* ptr = f1.extract();
  f1.pass(f2.extract());
  f2.pass(ptr);
}

#ifdef INCLUDE_ActivePtrWI

// The following of obliterate class.
// Use ActivePtr <X, CopyDefinitionWithoutInheritance >

// The inheritance is not preserved in the following

#define USE_OLD_POINTER_NAMES
#ifdef USE_OLD_POINTER_NAMES
// To use old name:
#define AutoPtr ActivePtrWI
#endif

// WI means without inheritance

template <class X> class ActivePtrWI {
 public:
  inline X* get(void) const { return ptr; }
  inline void put(const X* fptr)  // delete old owned object and
      // copy object fptr to ownership of this.
      // At fptr==NULL old object is deleted and new one is not copyed.
      {                           //
    if (ptr != NULL) {
      delete ptr;
      ptr = NULL;
    }
    if (fptr != NULL) ptr = new X(*fptr);
  }
  inline void pass(X* fptr)  // delete old owned object and
      // pass address of object fptr to ownership of this
      // without copying.
      // At fptr==NULL old object is deleted and new one is not copyed.
      {                      //
    if (ptr != NULL) delete ptr;
    ptr = fptr;
  }
  void print(std::ostream& file, int l = 1) const;
  inline ActivePtrWI(void) : ptr(NULL) {}
  inline ActivePtrWI(const X* fptr)
      : ptr(fptr != NULL ? new X(*fptr) : ((X*)NULL)) {}
  //inline ActivePtrWI(ActivePtrWI<X>& f): ptr(NULL)
  //  { if(f.ptr != NULL) ptr= new X(*(f.ptr)); }
  inline ActivePtrWI(const ActivePtrWI<X>& f) : ptr(NULL) {
    if (f.ptr != NULL) ptr = new X(*(f.ptr));
  }
  inline ActivePtrWI& operator=(const ActivePtrWI<X>& f) {
    if (this != &f) put(f.ptr);
    return *this;
  }
  inline X* operator->(void) const;
  inline X& operator*(void) const;
#ifdef IMPLICIT_X_STAR
  operator X*(void) const;
#endif
  virtual ~ActivePtrWI() {
    if (ptr != NULL) delete ptr;
  }

 private:
  X* ptr;
#ifdef USE_REPLACE_ALLOC
  macro_alloc
#endif
};

template <class X> inline X* ActivePtrWI<X>::operator->(void) const {
#ifdef SKIP_CHECKS_NULL
  return ptr;
#else
  if (ptr != NULL) return ptr;
  mcerr << "Error in X* ActivePtrWI<X>::operator->(void) const: "
        << " ptr == NULL\n";
  mcerr << "Type of X is (in internal notations) " << typeid(X).name() << '\n';
  spexit(mcerr);
  return ptr;  // just to quiet compiler which wants to see a return value
#endif
}
template <class X> inline X& ActivePtrWI<X>::operator*(void) const {
#ifdef SKIP_CHECKS_NULL
  return *ptr;
#else
  if (ptr != NULL) return *ptr;
  mcerr << "Error in X& ActivePtrWI<X>::operator*(void) const: "
        << " ptr == NULL\n";
  mcerr << "Type of X is (in internal notations) " << typeid(X).name() << '\n';
  spexit(mcerr);
  return *ptr;  // just to quiet compiler which wants to see a return value
#endif
}

#ifdef IMPLICIT_X_STAR
template <class X> ActivePtrWI<X>::operator X*(void) const {
#ifdef SKIP_CHECKS_NULL
  return ptr;
#else
  if (ptr != NULL) return ptr;
  mcerr << "Error in ActivePtrWI<X>::operator X*(void) const: "
        << " ptr == NULL\n";
  mcerr << "Type of X is (in internal notations) " << typeid(X).name() << '\n';
  spexit(mcerr);
  return ptr;  // just to quiet compiler which wants to see a return value
#endif
}
#endif

template <class X> void ActivePtrWI<X>::print(std::ostream& file, int l) const {
  //if(ptr!=NULL) ptr->print(file, l);
  Ifile << "ActivePtrWI<X>:";
  if (ptr == NULL)
    file << " pointer in NULL, no object.\n";
  else {
    file << noindent;
    ptr->print(file, l);
    file << yesindent;
  }
}
template <class X>
    std::ostream& operator<<(std::ostream& file, const ActivePtrWI<X>& f) {
  Ifile << "ActivePtrWI<X>:";
  if (f.get() == NULL)
    file << " pointer in NULL, no object\n";
  else {
    file << noindent;
    file << (*f.get());
    //f.get()->print(file,1);
    file << yesindent;
  }
  return file;
}
#endif

#define USE_OLD_POINTER_NAMES
#ifdef USE_OLD_POINTER_NAMES
// To use old name:
#define ProtPtr PassivePtr
#define RegProtPtr RegPassivePtr
#define CountProtPtr CountPassivePtr
#endif

template <class X> class PassivePtr;

class RegPassivePtr;

namespace CountPP_ns {
class CountPassivePtr  // counter of protected pointers
    {
 public:
  //CountPassivePtr();
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
#ifdef USE_REPLACE_ALLOC
  macro_alloc
#endif
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
//template<class X>class PassivePtr;

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

class RegPassivePtr virt_common_base_col
    // virt_common_base_col is added just to avoid repetitions in application
    // classes
    {
 public:
  friend class CountPP_ns::CountPassivePtr;
// used for making cpp NULL when CountPassivePtr
// is destructed

#ifdef USE_BIT_OPERA
 private:
  /*
  enum E_cwb{eb_s_ban_del=01, eb_s_ban_sub1 = 010, eb_s_ban_sub2 = 0100,
	     eb_s_ban_cop1=01000, eb_s_ban_cop2=010000
	     #ifdef USE_DELETE_AT_ZERO_COUNT
	     , eb_s_allow_del_at_zero_count = 0100000
	     #endif
	     };*/
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
  };    // the total 6 bits
#endif  // ifdef USE_BIT_FIELDS

  //friend template<class X>class PassivePtr;
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
          s_ban_del(fs_ban_del),
          s_ban_sub(fs_ban_sub),
          s_ban_cop(fs_ban_cop),
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
  //{mcout<<"RegPassivePtr::RegPassivePtr(void) is run\n";}

  RegPassivePtr(const RegPassivePtr& f);

  RegPassivePtr& operator=(const RegPassivePtr& f);
  // Copies s_ban_del and s_ban_sub
  // Also calls clear_pointers if s_ban_sub==1 or
  // spexit() if s_ban_sub==2 and the alist_of_prot_pointers is not empty.

  inline CountPP_ns::CountPassivePtr* book(void) const {
    if (cpp == NULL) {
      cpp = new CountPP_ns::CountPassivePtr(this);
    }
    cpp->book();
    return cpp;
  }

  inline void clear_pointers(void) const  // all pointers addressing this
      // are cleared;
      {
    if (cpp != NULL) cpp->change_rpp(NULL);
  }

  macro_copy_total(RegPassivePtr);

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
  { return conparam.s_ban_del; }
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
      //Iprintn(mcout, (control_word & eb_s_ban_sub1) );
      //Iprintn(mcout, (control_word & eb_s_ban_sub2) );
      return 2;
    }
  }
#elif defined(USE_BIT_FIELDS)
  { return conparam.s_ban_sub; }
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
  { return conparam.s_ban_cop; }
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
  { return conparam.s_allow_del_at_zero_count; }
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
#ifdef USE_REPLACE_ALLOC
  macro_alloc
#endif
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

template <class X> class PassivePtr virt_common_base_col {
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

#ifdef USE_DYNCAST_IN_PASSIVEPTR
    //else return dynamic_cast< X* >
    //	     (const_cast< RegPassivePtr* >( cpp->get_rpp() ) );
    const RegPassivePtr* rpp = cpp->get_rpp();
    if (rpp == NULL) return NULL;
    X* temp_ptr = dynamic_cast<X*>(const_cast<RegPassivePtr*>(rpp));
    if (temp_ptr == NULL) {
      mcerr << "Error in inline X* PassivePtr::get(void):\n"
            << " dynamic cast from RegPassivePtr to X* is not successful."
            << " You have probably tried to address with passive pointer "
            << " an object which is not derived from RegPassivePtr\n";
      spexit(mcerr);
    }
    return temp_ptr;
#else  // for ifdef USE_DYNCAST_IN_PASSIVEPTR
    else return (X*)(cpp->get_rpp());
#endif  // for ifdef USE_DYNCAST_IN_PASSIVEPTR

#endif  // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
  }

  // Since the pointer is passive, there is no cloning and pass and put
  // make the same job.
  // 03.06.2006: commenting off pass in order to forbid
  // correct for active pointer but
  // erroneous for passive pointer statements like ptr.pass(new any_class).
  //inline void pass(const X* fptr) {put( fptr );}

  inline void put(X* fptr);  // unregirster old registered object and
                             // pass fptr to  this.

  //void print(std::ostream& file, int l) const
  // { if(ptr!=NULL) ptr->print(file, l); }
  inline PassivePtr(void)
      : cpp(NULL)
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
        ,
        ptr(NULL)
#endif
        {
  }
  //inline PassivePtr(X* fptr):ptr(fptr)
  //  { if(ptr!=NULL) ptr->loc_book((void**)&ptr); }
  inline PassivePtr(X* fptr) {
    if (fptr != NULL)
      cpp = fptr->book();
    else
      cpp = NULL;
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
    ptr = fptr;
#endif
  }
  //inline PassivePtr(Y* fptr):ptr(fptr)
  //  { if(ptr!=NULL) ptr->loc_book((void**)&ptr); }
  inline PassivePtr(X& fptr) {
    cpp = fptr.book();
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
    ptr = &fptr;
#endif
  }
  //inline PassivePtr(PassivePtr<X>& f);

  // The following contractor allows to avoid copying of pointer at s = 0.
  // Perhaps it is not necessary
  //inline PassivePtr(PassivePtr<X>& f, int s); // s==0 do not copy, s==1 copy
  //to new

  inline PassivePtr(const PassivePtr<X>& f);
  //inline PassivePtr(const PassivePtr<X>& f, int s);
  // s==0 do not copy, s==1 copy to new

  inline PassivePtr<X>& operator=(const PassivePtr<X>& f);
  inline PassivePtr<X>& operator=(X* f);
  //template< class Y > explicit operator PassivePtr< Y >() const ; does not
  //work,
  // produces ambiguity with operators X* and constructors

  // Y may be any derived class from X
  template <class Y> PassivePtr<X>(const PassivePtr<Y>& f);
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
  macro_copy_total(PassivePtr);
  virtual ~PassivePtr();

 private:
  CountPP_ns::CountPassivePtr* cpp;
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
  X* ptr;
#endif
#ifdef USE_REPLACE_ALLOC
  macro_alloc
#endif
};

/*
template<class X >
template<class Y >
PassivePtr<X>::operator PassivePtr<Y>() const { return PassivePtr<Y>(get());}
*/

template <class X>
template <class Y>
PassivePtr<X>::PassivePtr(const PassivePtr<Y>& f)
    : cpp(NULL) {
  put((f.get()));
}

template <class X> void PassivePtr<X>::print(std::ostream& file, int l) const {
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
inline void PassivePtr<X>::put(X* fptr)
    // unregirster old registered object and
    // pass fptr to  this.
    {
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
/*
template<class X>
inline PassivePtr<X>::PassivePtr(PassivePtr<X>& f)
{
  if(f.cpp != NULL)
  {
    f.cpp->book();
    cpp = f.cpp;
  }
  else
    cpp = NULL;
}
*/

template <class X> inline PassivePtr<X>::PassivePtr(const PassivePtr<X>& f) {
  if (f.cpp != NULL) {
    f.cpp->book();
    cpp = f.cpp;
  } else
    cpp = NULL;
#ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
  ptr = f.ptr;
#endif
}
/*
template<class X>
inline PassivePtr<X>::PassivePtr(PassivePtr<X>& f, int s)
  // s==0 do not copy, s==1 copy to new
{
  if(f.cpp != NULL && s == 1)
  {
    f.cpp->book();
    cpp = f.cpp;
  }
  else
    cpp = NULL;
}

template<class X>
inline PassivePtr<X>::PassivePtr(const PassivePtr<X>& f, int s)
  // s==0 do not copy, s==1 copy to new
{
  if(f.cpp != NULL && s == 1)
  {
    f.cpp->book();
    cpp = f.cpp;
  }
  else
    cpp = NULL;
}
*/
template <class X>
inline PassivePtr<X>& PassivePtr<X>::operator=(const PassivePtr<X>& f) {
  if (this != &f) put(f.get());
  return *this;
}

template <class X> inline PassivePtr<X>& PassivePtr<X>::operator=(X* f) {
  put(f);
  return *this;
}

template <class X> inline void PassivePtr<X>::move_pointer(PassivePtr<X>& f) {
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

template <class X> inline X* PassivePtr<X>::operator->(void) const {
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
#ifdef USE_DYNCAST_IN_PASSIVEPTR
  X* temp_ptr = dynamic_cast<X*>(const_cast<RegPassivePtr*>(rpp));
  if (temp_ptr == NULL) {
    mcerr << "Error in X* PassivePtr<X>::operator->(void) const:\n"
          << " dynamic cast from RegPassivePtr to X* is not successful"
          << " (although the addresses of counter and abject are not zero)."
          << " You have probably initialized passive pointer "
          << " with address of deleted object\n";
    //<<" You have probably tried to address with passive pointer "
    //<<" an object which is not derived from RegPassivePtr\n";
    mcerr << "Type of X is (in internal notations) " << typeid(X).name()
          << '\n';
    spexit(mcerr);
  }
  return temp_ptr;
#else
  return (X*)(rpp);
#endif
#endif  // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
}

template <class X> inline X& PassivePtr<X>::operator*(void) const {
//mcout<<"PassivePtr<X>::operator* is called\n";
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
#ifdef USE_DYNCAST_IN_PASSIVEPTR
  X* temp_ptr = dynamic_cast<X*>(const_cast<RegPassivePtr*>(rpp));
  if (temp_ptr == NULL) {
    mcerr << "Error in X& PassivePtr<X>::operator*(void) const:\n"
          << " dynamic cast from RegPassivePtr to X* is not successful."
          << " You have probably tried to address with passive pointer "
          << " an object which is not derived from RegPassivePtr\n";
    spexit(mcerr);
  }
  return *temp_ptr;
#else
  return *((X*)(rpp));
#endif
#endif  // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
}

template <class X> inline X* PassivePtr<X>::getver(void) const {
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
#ifdef USE_DYNCAST_IN_PASSIVEPTR
  X* temp_ptr = dynamic_cast<X*>(const_cast<RegPassivePtr*>(rpp));
  if (temp_ptr == NULL) {
    mcerr << "Error in X* PassivePtr<X>::getver(void) const:\n"
          << " dynamic cast from RegPassivePtr to X* is not successful."
          << " You have probably tried to address with passive pointer "
          << " an object which is not derived from RegPassivePtr\n";
    spexit(mcerr);
  }
  return temp_ptr;
#else
  return (X*)(rpp);
#endif
#endif  // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
}

#ifdef IMPLICIT_X_STAR
template <class X> inline PassivePtr<X>::operator X*(void) const {
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
#ifdef USE_DYNCAST_IN_PASSIVEPTR
  X* temp_ptr = dynamic_cast<X*>(const_cast<RegPassivePtr*>(rpp));
  if (temp_ptr == NULL) {
    mcerr << "Error in X* PassivePtr<X>::operator X*(void) const:\n"
          << " dynamic cast from RegPassivePtr to X* is not successful."
          << " You have probably tried to address with passive pointer "
          << " an object which is not derived from RegPassivePtr\n";
    spexit(mcerr);
  }
  return temp_ptr;
#else
  return (X*)(rpp);
#endif
#endif  // for ifdef USE_DOUBLE_PTR_IN_PASSIVEPTR
}
#endif
template <class X>
    inline int operator==(const PassivePtr<X>& f1,
                          const PassivePtr<X>&
                              f2) {  // comparison of addresses, so it mimics
                                     // regular pointers
  return f1.get() == f2.get();
}

template <class X> PassivePtr<X>::~PassivePtr() {
  if (cpp != NULL) {
//mcout<<"PassivePtr<X>::~PassivePtr(): &ptr="<<&ptr<<" *ptr="<<*ptr<<'\n';
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
    //mcout<<"PassivePtr<X>::~PassivePtr(): &ptr="<<&ptr<<" *ptr="<<*ptr<<'\n';
    if (cpp->get_rpp() == NULL && cpp->get_number_of_booked() == 0) {
      delete cpp;
      cpp = NULL;
    }
  }
}

template <class X>
    bool operator<(PassivePtr<X> f1,
                   PassivePtr<X> f2)  // necessary for std::set
    {
  return f1.get() < f2.get();
}

#define USE_OLD_POINTER_NAMES
#ifdef USE_OLD_POINTER_NAMES
#define AutoContReg ActivePtrReg
#endif

template <class X, class C = StandardCopyDefinition<X> >
class ActivePtrReg : public RegPassivePtr,
                     public ActivePtr<X, C> {
 public:
  inline ActivePtrReg(void) : RegPassivePtr(), ActivePtr<X, C>() {}
  inline ActivePtrReg(const X* fptr, Clone)
      : RegPassivePtr(), ActivePtr<X, C>(fptr, do_clone) {}
  inline ActivePtrReg(const X* fptr, Pass)
      : RegPassivePtr(), ActivePtr<X, C>(fptr, dont_clone) {}

  inline ActivePtrReg(ActivePtrReg<X, C>& f)
      : RegPassivePtr(), ActivePtr<X, C>(f) {}
  inline ActivePtrReg(const ActivePtrReg<X, C>& f)
      : RegPassivePtr(), ActivePtr<X, C>(f) {}
  inline ActivePtrReg& operator=(const ActivePtrReg<X, C>& f) {
    if (this != &f) put(f.get());
    return *this;
  }
  virtual void print(std::ostream& file, int l = 1) const;
  macro_copy_total(ActivePtrReg);
  virtual ~ActivePtrReg() {}
#ifdef USE_REPLACE_ALLOC
  macro_alloc
#endif
};

template <class X, class C>
void ActivePtrReg<X, C>::print(std::ostream& file, int l) const {
  //if(ptr!=NULL) ptr->print(file, l);
  Ifile << "ActivePtrReg<X,C>:\n";
  indn.n += 2;
  RegPassivePtr::print(file, l);
  ActivePtr<X, C>::print(file, l);
  indn.n -= 2;
}

template <class X>
    std::ostream& operator<<(std::ostream& file, const ActivePtrReg<X>& f) {
  Ifile << "ActivePtrReg<X>:";
  file << noindent;
  file << (ActivePtr<X>&)f;
  file << yesindent;
  return file;
}

#ifdef INCLUDE_ActivePtrWI

#define USE_OLD_POINTER_NAMES
#ifdef USE_OLD_POINTER_NAMES
#define AutoPtrReg ActivePtrWIReg
#endif

template <class X>
class ActivePtrWIReg : public RegPassivePtr,
                       public ActivePtrWI<X> {
 public:
  inline ActivePtrWIReg(void) : ActivePtrWI<X>(), RegPassivePtr() {}
  inline ActivePtrWIReg(const X* fptr)
      : ActivePtrWI<X>(fptr), RegPassivePtr() {}
  inline ActivePtrWIReg(ActivePtrWIReg<X>& f)
      : ActivePtrWI<X>(fptr), RegPassivePtr() {}
  inline ActivePtrWIReg(const ActivePtrWIReg<X>& f)
      : ActivePtrWI<X>(fptr), RegPassivePtr() {}
  inline ActivePtrWIReg& operator=(const ActivePtrWIReg<X>& f) {
    if (this != &f) put(f.get());
    return *this;
  }
  virtual ~ActivePtrWIReg() {}
#ifdef USE_REPLACE_ALLOC
  macro_alloc
#endif
};
#endif

class DoubleReg : public RegPassivePtr {
 public:
  double val;
  inline DoubleReg(void) : val(0) {}
  inline DoubleReg(const DoubleReg& f) : RegPassivePtr(), val(f.val) {}
  inline DoubleReg(double f) : RegPassivePtr(), val(f) {}
  inline operator double(void) { return val; }
#ifdef USE_REPLACE_ALLOC
  macro_alloc
#endif
};

std::ostream& operator<<(std::ostream& file, const DoubleReg& f);
#endif
