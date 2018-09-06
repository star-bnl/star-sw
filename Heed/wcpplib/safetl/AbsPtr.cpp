#include "wcpplib/safetl/AbsPtr.h"
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

namespace Heed {

RegPassivePtr::RegPassivePtr(const RegPassivePtr& f)
    :
#ifdef USE_BIT_OPERA
      control_word(f.control_word),
#elif defined(USE_BIT_FIELDS)
      conparam(f.conparam),
#else
      s_ban_del(f.s_ban_del),
      s_ban_sub(f.s_ban_sub),
      s_ban_cop(f.s_ban_cop),
#ifdef USE_DELETE_AT_ZERO_COUNT
      s_allow_del_at_zero_count(f.s_allow_del_at_zero_count),
#endif
#endif
      cpp(NULL) {
// mcout<<"RegPassivePtr::RegPassivePtr(...) is started\n";
#ifdef USE_BIT_OPERA
  if (f.get_s_ban_cop() == 2) {
#elif defined(USE_BIT_FIELDS)
  if (f.conparam.s_ban_cop == 2) {
#else
  if (f.s_ban_cop == 2) {
#endif
    mcerr << "Error in "
          << "RegPassivePtr::RegPassivePtr(const RegPassivePtr& f):\n"
          << "attempt to copy object whose s_ban_cop == 2.\n";
    spexit(mcerr);
  }
#ifdef USE_BIT_OPERA
  else if (f.get_s_ban_cop() == 1 && f.cpp->get_number_of_booked() > 0) {
#elif defined(USE_BIT_FIELDS)
  else if (f.conparam.s_ban_cop == 1 && f.cpp->get_number_of_booked() > 0) {
#else
  else if (f.s_ban_cop == 1 && f.cpp->get_number_of_booked() > 0) {
#endif
    mcerr << "Error in "
          << "RegPassivePtr::RegPassivePtr(const RegPassivePtr& f):\n"
          << "attempt to copy referred object whose s_ban_cop == 1.\n"
          << "f.cpp->get_number_of_booked()=" << f.cpp->get_number_of_booked()
          << '\n';
    spexit(mcerr);
  }
}

RegPassivePtr& RegPassivePtr::operator=(const RegPassivePtr& f) {
// mcout<<"RegPassivePtr::operator= is started\n";
#ifdef USE_BIT_OPERA
  if (f.get_s_ban_cop() == 2) {
#elif defined(USE_BIT_FIELDS)
  if (f.conparam.s_ban_cop == 2) {
#else
  if (f.s_ban_cop == 2) {
#endif
    mcerr << "Error in "
          << "RegPassivePtr& RegPassivePtr::operator=(const RegPassivePtr& "
             "f):\n"
          << "attempt to copy object whose s_ban_cop == 2.\n";
    spexit(mcerr);
#ifdef USE_BIT_OPERA
  } else if (f.get_s_ban_cop() == 1 && f.cpp->get_number_of_booked() > 0) {
#elif defined(USE_BIT_FIELDS)
  } else if (f.conparam.s_ban_cop == 1 && f.cpp->get_number_of_booked() > 0) {
#else
  } else if (f.s_ban_cop == 1 && f.cpp->get_number_of_booked() > 0) {
#endif
    mcerr << "Error in "
          << "RegPassivePtr& RegPassivePtr::operator=(const "
             "RegPassivePtr& f):\n"
          << "attempt to copy referred object whose s_ban_cop == 1.\n"
          << "f.cpp->get_number_of_booked()=" << f.cpp->get_number_of_booked()
          << '\n';
    spexit(mcerr);
  }
#ifdef USE_BIT_OPERA
  set_s_ban_cop(f.get_s_ban_cop());
#ifdef USE_DELETE_AT_ZERO_COUNT
  set_s_allow_del_at_zero_count(f.get_s_allow_del_at_zero_count());
#endif
  set_s_ban_del(f.get_s_ban_del());
  if (get_s_ban_sub() == 1)
#elif defined(USE_BIT_FIELDS)
  conparam.s_ban_cop = f.conparam.s_ban_cop;
#ifdef USE_DELETE_AT_ZERO_COUNT
  conparam.s_allow_del_at_zero_count = f.conparam.s_allow_del_at_zero_count;
#endif
  conparam.s_ban_del = f.conparam.s_ban_del;
  if (conparam.s_ban_sub == 1)
#else
  s_ban_cop = f.s_ban_cop;
#ifdef USE_DELETE_AT_ZERO_COUNT
  s_allow_del_at_zero_count = f.s_allow_del_at_zero_count;
#endif
  s_ban_del = f.s_ban_del;
  if (s_ban_sub == 1)
#endif
  {
    if (this == &f &&  // self-assignment
        cpp != NULL && cpp->get_number_of_booked() > 0) {
      mcerr << "Error in "
            << "RegPassivePtr& RegPassivePtr::operator=(const RegPassivePtr& "
               "f):\n"
            << "self-assignment, s_ban_sub == 1, but the object is addressed.\n"
            << "This can lead to loss of pointers to copied object, not only "
               "to this one.\n";
      spexit(mcerr);
    }
    clear_pointers();
  }
#ifdef USE_BIT_OPERA
  else if (get_s_ban_sub() == 2 && cpp != NULL &&
           cpp->get_number_of_booked() > 0)
#elif defined(USE_BIT_FIELDS)
  else if (conparam.s_ban_sub == 2 && cpp != NULL &&
           cpp->get_number_of_booked() > 0)
#else
  else if (s_ban_sub == 2 && cpp != NULL && cpp->get_number_of_booked() > 0)
#endif
  {
    mcerr << "Error in "
          << "RegPassivePtr& RegPassivePtr::operator=(const RegPassivePtr& "
             "f):\n"
          << "s_ban_sub == 2, but the object is addressed.\n";
    spexit(mcerr);
  }
#ifdef USE_BIT_OPERA
  set_s_ban_sub(f.get_s_ban_sub());
#elif defined(USE_BIT_FIELDS)
  conparam.s_ban_sub = f.conparam.s_ban_sub;
#else
  s_ban_sub = f.s_ban_sub;
#endif
  return *this;
}

int RegPassivePtr::s_print_adr_cpp = 0;

void RegPassivePtr::print(std::ostream& file, int l) const {
  if (l > 0) file << (*this);
}

std::ostream& operator<<(std::ostream& file, const RegPassivePtr& f) {
#ifdef USE_BIT_OPERA
#ifdef USE_CHAR_GETSETTERS_PARAMETERS
  Ifile << "RegPassivePtr<X>: s_ban_del/sub/cop=" << int(f.get_s_ban_del())
        << "/" << int(f.get_s_ban_sub()) << "/" << int(f.get_s_ban_cop());
#else
  Ifile << "RegPassivePtr<X>: s_ban_del/sub/cop=" << f.get_s_ban_del() << "/"
        << f.get_s_ban_sub() << "/" << f.get_s_ban_cop();
#endif
#elif defined USE_BIT_FIELDS
  Ifile << "RegPassivePtr<X>: s_ban_del/sub/cop=" << f.conparam.s_ban_del << "/"
        << f.conparam.s_ban_sub << "/" << f.conparam.s_ban_cop;
#else
#ifdef USE_CHAR_CONTROL_VARIABLES
  Ifile << "RegPassivePtr<X>: s_ban_del/sub/cop=" << int(f.s_ban_del) << "/"
        << int(f.s_ban_sub) << "/" << int(f.s_ban_cop);
#else
  Ifile << "RegPassivePtr<X>: s_ban_del/sub/cop=" << f.s_ban_del << "/"
        << f.s_ban_sub << "/" << f.s_ban_cop;
#endif
#endif
  /*
    Ifile<<"RegPassivePtr<X>: s_ban_del="<<f.s_ban_del
         <<" s_ban_sub="<<f.s_ban_sub
         <<" s_ban_cop="<<f.s_ban_cop;
           */
  if (RegPassivePtr::s_print_adr_cpp == 0) {
    if (f.cpp == NULL) {
      file << " cpp=NULL\n";
    } else {
      file << " cpp!=NULL\n";
    }
  } else {
    file << " cpp=" << f.cpp << '\n';
  }
  if (f.cpp != NULL) {
    indn.n += 2;
    Ifile << "cpp->number_of_registered=" << f.cpp->get_number_of_booked()
          << '\n';
    indn.n -= 2;
  }
#ifdef USE_DELETE_AT_ZERO_COUNT
  indn.n += 2;
#ifdef USE_BIT_OPERA
  Ifile << "s_allow_del_at_zero_count="
#ifdef USE_CHAR_GETSETTERS_PARAMETERS
        << int(f.get_s_allow_del_at_zero_count()) << '\n';
#else
        << f.get_s_allow_del_at_zero_count() << '\n';
#endif
#elif defined(USE_BIT_FIELDS)
  Ifile << "s_allow_del_at_zero_count=" << f.conparam.s_allow_del_at_zero_count
        << '\n';
#else
#ifdef USE_CHAR_CONTROL_VARIABLES
  Ifile << "s_allow_del_at_zero_count=" << int(f.s_allow_del_at_zero_count)
        << '\n';
#else
  Ifile << "s_allow_del_at_zero_count=" << f.s_allow_del_at_zero_count << '\n';
#endif
#endif
  indn.n -= 2;
#endif
  return file;
}

long RegPassivePtr::get_total_number_of_references(void) const {
  if (cpp == NULL)
    return 0;
  else
    return cpp->get_number_of_booked();
}

int RegPassivePtr::s_ban_del_ignore = 0;

RegPassivePtr::~RegPassivePtr() {
  // mcout<<"~RegPassivePtr(): *this="<<(*this)<<'\n';
  if (cpp != NULL) {
    cpp->change_rpp(NULL);
    if (cpp->get_number_of_booked() == 0) {
      delete cpp;
      cpp = NULL;
    } else {
#ifdef USE_BIT_OPERA
      if (s_ban_del_ignore == 0 && get_s_ban_del() == 1)
#elif defined(USE_BIT_FIELDS)
      if (s_ban_del_ignore == 0 && conparam.s_ban_del == 1)
#else
      if (s_ban_del_ignore == 0 && s_ban_del == 1)
#endif
      {
        mcerr << "Error in RegPassivePtr::~RegPassivePtr() "
              << "s_ban_del == 1, but there are pointers to this class.\n";
        mcerr << "cpp->number_of_registered=" << cpp->get_number_of_booked()
              << '\n';
        s_ban_del_ignore = 1;
        spexit(mcerr);
      }
    }
  }
}

}
