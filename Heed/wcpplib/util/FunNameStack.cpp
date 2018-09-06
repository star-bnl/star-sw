/*
Copyright (c) 1999-2003 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file for any purpose
is hereby granted without fee, provided that the above copyright notice,
this permission notice, and notices about any modifications of the original
text appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

#include <iostream>
#include <iomanip>
#include <cstring>
#include "wcpplib/util/FunNameStack.h"


#ifdef USE_BOOST_MULTITHREADING
NameStack& NameStack::operator=(const NameStack& f) {
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
  MemoriseIgnore::instance().ignore();
#else
  s_ignore_keynumberlist = 1;  // to avoid report from delete at deletion
#endif
#endif
  if (this != &f) {
    int n;
    if (nmode == 1)
      for (n = 0; n < qname; n++) delete name[n];
    nmode = f.nmode;
    qname = f.qname;
    id = f.id;
    for (n = 0; n < f.qname; n++) {
      if (nmode == 0) {
        name[n] = f.name[n];
      } else {
        int l = strlen(f.name[n]) + 1;
        name[n] = new char[l];
        strcpy(name[n], f.name[n]);
      }
    }
  }
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
  MemoriseIgnore::instance().not_ignore();
#else
  s_ignore_keynumberlist = 0;
#endif
#endif
  return *this;
}
#endif

namespace Heed {

int s_throw_exception_in_spexit = 0;
int s_exit_without_core = 0;

FunNameStack& FunNameStack::instance() {
// static FunNameStack inst;  // According to some Internet site this is
// Meyer's approach,
// // time of descruction is not determined.
// // So it can potentially be destructed before it is still in use.
// return inst;
#ifdef USE_BOOST_MULTITHREADING
#ifdef PRINT_BEFORE_LOCK
  fprintf(stderr, "FunNameStack& FunNameStack::instance()\n");
#endif
  boost::mutex locked_object;
  boost::mutex::scoped_lock scopedLock_object(locked_object);
#endif

  static FunNameStack* inst = NULL;  // According to this site it is
                                     // "GoF" approach,
                                     // destruction is not performed at all.
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
  MemoriseIgnore::instance().ignore();
#else
  s_ignore_keynumberlist = 1;  // to avoid report from delete at deletion
#endif
#endif
  if (!inst) inst = new FunNameStack();
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
  MemoriseIgnore::instance().not_ignore();
#else
  s_ignore_keynumberlist = 0;
#endif
#endif
  return *inst;
}

FunNameStack::FunNameStack(void)
    :  // usually called inly from instance()
      s_init(1),
      s_act(1),
      s_print(0),
      nmode(0) {
#ifdef USE_BOOST_MULTITHREADING

#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
  MemoriseIgnore::instance().ignore();
#else
  s_ignore_keynumberlist = 1;  // to avoid report from delete at deletion
#endif
#endif
  namestack = new std::list<NameStack>;
  namestack->push_back(NameStack());
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
  MemoriseIgnore::instance().not_ignore();
#else
  s_ignore_keynumberlist = 0;
#endif
#endif  // for ifdef USE_TOGETHER_WITH_CLEAN_NEW

  pthread_t id = pthread_self();
  namestack->back().id = id;
#ifdef PRINT_MESSAGE_ABOUT_THREAD_INITIALIZATION
  mcerr
      << "-----------------------------------------------------------------\n";
  mcerr
      << "-----------------------------------------------------------------\n";
  mcerr
      << "-----------------------------------------------------------------\n";
  mcerr
      << "-----------------------------------------------------------------\n";
  mcerr
      << "-----------------------------------------------------------------\n";
  mcerr << "FunNameStack::FunNameStack(void) const:\n";
  mcerr << "thread is initialized:\n";
  Iprintn(mcerr, id);
  mcerr
      << "-----------------------------------------------------------------\n";
  mcerr
      << "-----------------------------------------------------------------\n";
  mcerr
      << "-----------------------------------------------------------------\n";
  mcerr
      << "-----------------------------------------------------------------\n";
  mcerr
      << "-----------------------------------------------------------------\n";
#endif
#else  // for ifdef USE_BOOST_MULTITHREADING
  qname = 0;
  for (int n = 0; n < pqname; n++) name[n] = NULL;
#endif
}

#ifdef USE_BOOST_MULTITHREADING
NameStack* FunNameStack::get_thread_stack(void) const {
  // long nret = 0;
  pthread_t id = pthread_self();
  int s_found = 0;
  std::list<NameStack>::const_iterator it;
  std::list<NameStack>::const_iterator end = namestack->end();
  for (it = namestack->begin(); it != end; ++it) {
    if (pthread_equal((*it).id, id)) {
      s_found = 1;
      break;
    }
  }
  if (s_found == 0) {
// new thread is detected
/*
mcerr<<"-----------------------------------------------------------------\n";
mcerr<<"-----------------------------------------------------------------\n";
mcerr<<"-----------------------------------------------------------------\n";
mcerr<<"-----------------------------------------------------------------\n";
mcerr<<"-----------------------------------------------------------------\n";
mcerr<<"NameStack*  FunNameStack::get_thread_stack(void) const:\n";
mcerr<<"new thread is detected\n";
Iprintn(mcerr, id);
 mcerr<<"-----------------------------------------------------------------\n";
*/
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
    MemoriseIgnore::instance().ignore();
#else
    s_ignore_keynumberlist = 1;  // to avoid report from delete at deletion
#endif
#endif
    namestack->push_back(NameStack());
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
    MemoriseIgnore::instance().not_ignore();
#else
    s_ignore_keynumberlist = 0;  // to avoid report from delete at deletion
#endif
#endif

    namestack->back().id = id;
  }
  return &(namestack - back());
}
NameStack* FunNameStack::get_thread_stack_q(long& nthread,
                                            long& qthread) const {
  nthread = 0;
  qthread = 0;
  pthread_t id = pthread_self();
  int s_found = 0;
  std::list<NameStack>::const_iterator it;
  std::list<NameStack>::const_iterator end = namestack->end();
  for (it = namestack->begin(); it != end; ++it) {
    if (s_found == 0 && pthread_equal((*it).id, id)) {
      s_found = 1;
      nthread = qthread;
    }
    qthread++;
  }
  if (s_found == 0) {
// new thread is detected
/*
mcerr<<"-----------------------------------------------------------------\n";
mcerr<<"-----------------------------------------------------------------\n";
mcerr<<"-----------------------------------------------------------------\n";
mcerr<<"-----------------------------------------------------------------\n";
mcerr<<"-----------------------------------------------------------------\n";
mcerr<<"NameStack*  FunNameStack::get_thread_stack(void) const:\n";
mcerr<<"new thread is detected\n";
Iprintn(mcerr, id);
mcerr<<"-----------------------------------------------------------------\n";
*/
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
    MemoriseIgnore::instance().ignore();
#else
    s_ignore_keynumberlist = 1;  // to avoid report from delete at deletion
#endif
#endif
    namestack->push_back(NameStack());
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
    MemoriseIgnore::instance().not_ignore();
#else
    s_ignore_keynumberlist = 0;  // to avoid report from delete at deletion
#endif
#endif
    namestack->back().id = id;
    nthread = qthread;
    qthread++;
  }
  return &(namestack->back());
}
void FunNameStack::remove_thread_stack(void) {
  pthread_t id = pthread_self();
  int s_found = 0;
  std::list<NameStack>::const_iterator it;
  std::list<NameStack>::const_iterator end = namestack->end();
  for (it = namestack->begin(); it != end; ++it) {
    if (pthread_equal((*it).id, id)) {
      s_found = 1;
      break;
    }
  }
  if (s_found == 0) {
    // new thread is detected
    mcerr << "ERROR in void FunNameStack::remove_thread_stack(void) const:\n";
    mcerr << "new thread is detected\n";
    Iprintn(mcerr, id);
    spexit(mcerr);
  }
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
  MemoriseIgnore::instance().ignore();
#else
  s_ignore_keynumberlist = 1;    // to avoid report from delete at deletion
#endif
#endif
  namestack->remove(an);
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
  MemoriseIgnore::instance().not_ignore();
#else
  s_ignore_keynumberlist = 0;    // to avoid report from delete at deletion
#endif
#endif
}
#endif

#ifdef USE_BOOST_MULTITHREADING
std::ostream& FunNameStack::printname(std::ostream& file, NameStack* ns,
                                      int n)  //
#else
std::ostream& FunNameStack::printname(std::ostream& file, int n)  //
#endif
{
#ifdef USE_BOOST_MULTITHREADING
  file << ns->name[n];
  return file;
#else
  file << name[n];
  return file;
#endif
}

void spexit_action(std::ostream& file) {
  file << "spexit_action: the streams will be now flushed\n";
  file.flush();
  mcout.flush();
  mcerr.flush();
  if (s_throw_exception_in_spexit != 1) {
    if (s_exit_without_core == 1) {
      file << "spexit_action: the exit(1) function is called\n";
      exit(1);
    } else {
      file << "spexit_action: the abort function is called\n";
      abort();
    }
  } else {
    file << "spexit_action: an exception is now called\n";
    throw ExcFromSpexit();
  }
}

FunNameStack::FunNameStack(const FunNameStack& f) { *this = f; }

FunNameStack& FunNameStack::operator=(const FunNameStack& f) {
  if (this == &(FunNameStack::instance())) {
    mcerr << "ERROR in FunNameStack& FunNameStack::operator=(const "
             "FunNameStack& f)\n";
    mcerr << "Attempt to copy to operative FunNameStack\n";
    mcerr << "You don't need and should not initialize main FunNameStack "
             "directly\n";
    mcerr << "If you want to change its parameters, use "
             "FunNameStack::instance()\n";
    Iprintn(mcout, this);
    Iprintn(mcout, &(FunNameStack::instance()));
    Iprintn(mcout, (*this));
    Iprintn(mcout, (FunNameStack::instance()));

    spexit(mcerr);
  }
#ifdef USE_BOOST_MULTITHREADING

#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
  MemoriseIgnore::instance().ignore();
#else
  s_ignore_keynumberlist = 1;
#endif
#endif
  if (namestack) delete namestack;
  namestack = new std::list<NameStack>(*(f.namestack));
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
  MemoriseIgnore::instance().not_ignore();
#else
  s_ignore_keynumberlist = 0;
#endif
#endif

#else
  qname = f.qname;
#endif
  s_init = f.s_init;
  s_act = f.s_act;
  s_print = f.s_print;
  nmode = f.nmode;
#ifdef USE_BOOST_MULTITHREADING
#else
  for (int n = 0; n < f.qname; n++) {
    if (nmode == 0) {
      name[n] = f.name[n];
    } else {
      int l = strlen(f.name[n]) + 1;
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
      s_ignore_keynumberlist = 1;
#endif
      name[n] = new char[l];
      strcpy(name[n], f.name[n]);
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
      s_ignore_keynumberlist = 0;
#endif
    }
  }
#endif
  return *this;
}

void FunNameStack::set_parameters(int fs_act, int fs_print) {
  s_act = fs_act;
  s_print = fs_print;  // only to correct this
}

FunNameStack::~FunNameStack() {
#ifdef USE_BOOST_MULTITHREADING
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
  MemoriseIgnore::instance().ignore();
#else
  s_ignore_keynumberlist = 1;
#endif
#endif
  if (namestack) delete namestack;
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
#if defined(MAINTAIN_KEYNUMBER_LIST) && defined(USE_BOOST_MULTITHREADING)
  MemoriseIgnore::instance().not_ignore();
#else
  s_ignore_keynumberlist = 0;
#endif
#endif
#else
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
  s_ignore_keynumberlist = 1;
#endif
  if (nmode == 1)
    for (int n = 0; n < qname; n++) delete name[n];
#ifdef USE_TOGETHER_WITH_CLEAN_NEW
  s_ignore_keynumberlist = 0;
#endif
#endif
}

void FunNameStack::printput(std::ostream& file) {
  if (s_print == 1 || s_print == 2) {
#ifdef USE_BOOST_MULTITHREADING
    NameStack* ns = get_thread_stack();
    file << "FunNameStack::put: id=" << ns->id << " qname=" << ns->qname
         << " last name= \"";
    printname(file, ns, ns->qname - 1);
    file << " \"\n";
#else
    file << "FunNameStack::put: qname =" << qname << " last name=";
    printname(file, qname - 1) << '\n';
#endif
  } else if (s_print >= 3) {
#ifdef USE_BOOST_MULTITHREADING
    NameStack* ns = get_thread_stack();
    file << "FunNameStack::put: id=" << ns->id << "\n" << (*this);
#else
    file << "FunNameStack::put:\n" << (*this);
#endif
  }
}
void FunNameStack::printdel(std::ostream& file) {
  if (s_print == 2) {
#ifdef USE_BOOST_MULTITHREADING
    NameStack* ns = get_thread_stack();
    file << "FunNameStack::del: id=" << ns->id << " qname =" << ns->qname
         << " last name= \"";
    printname(file, ns, ns->qname - 1);
    file << " \"\n";
#else
    file << "FunNameStack::del: qname =" << qname << " last name=";
    printname(file, qname - 1) << '\n';
#endif
  } else if (s_print == 4) {
    file << "FunNameStack::del:\n" << (*this);
  }
}

std::ostream& operator<<(std::ostream& file, const FunNameStack& f) {
  if (f.s_act == 1) {
#ifdef USE_BOOST_MULTITHREADING
    file << "FunNameStack: s_init=" << f.s_init << '\n';
    long nret, qret;
    NameStack* ns = f.get_thread_stack_q(nret, qret);
    file << " id=" << ns->id << " qname=" << ns->qname << '\n';
    file << "At the time of scanning there were " << qret << " threads \n"
         << "registered in FunNameStack system.\n";
    file << "The current one appeared nth: " << nret << '\n';
    for (int n = 0; n < ns->qname; n++) {
      file << std::setw(3) << n << "  " << ns->name[n] << " \n";
    }
#else
    file << "FunNameStack: s_init=" << f.s_init << " qname=" << f.qname << '\n';
    for (int n = 0; n < f.qname; n++) {
      file << std::setw(3) << n << "  " << f.name[n] << " \n";
    }
#endif
  }
  return file;
}

std::ostream& operator<<(std::ostream& file, const FunNameWatch& f) {
  f.hdr(file);
  return file;
}

}
