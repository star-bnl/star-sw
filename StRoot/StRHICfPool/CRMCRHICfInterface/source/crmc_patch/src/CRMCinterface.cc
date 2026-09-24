#include <CRMCinterface.h>
#include <CRMCconfig.h>
#include <iostream>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <stdexcept>
#if defined(CRMC_DPMJET06) and defined(CRMC_DPMJET17)
# error "DPMJet 6 and 17 both enabled - please select only one version"
#endif
#if defined(CRMC_DPMJET06) and defined(CRMC_DPMJET19)
# error "DPMJet 6 and 19 both enabled - please select only one version"
#endif
#if defined(CRMC_DPMJET17) and defined(CRMC_DPMJET19)
# error "DPMJet 17 and 19 both enabled - please select only one version"
#endif

using namespace std;

CRMCdata gCRMC_data;

CRMCinterface::CRMCinterface() :
  crmc_generate(NULL),
  crmc_set(NULL),
  crmc_init(NULL),
  crmc_xsection(NULL),
  fLibrary(NULL)
{
}

CRMCinterface::~CRMCinterface()
{
  if (fLibrary)
    {
      dlclose(fLibrary);
      fLibrary = NULL;
    }
}

void* CRMCinterface::find_symbol(const char* sym)
{
  if (!fLibrary) throw std::runtime_error("Library not open");

  void* ret =  dlsym(fLibrary, sym);
  if(ret == NULL)
    throw std::runtime_error(" dlsym error:\n \"" + std::string(dlerror()));

  return ret;
}

bool CRMCinterface::init(int HEmodel)
{
#ifdef CRMC_STATIC
  crmc_generate  = &crmc_f_;
  crmc_set       = &crmc_set_f_;
  crmc_init      = &crmc_init_f_;
  crmc_xsection  = &crmc_xsection_f_;
  crmc_defaults  = &aaset_;
  crmc_readparam = &eposinput_;
  crmc_pid       = &idtrafo_;
  crmc_ainit     = &ainit_;
#else
  ostringstream libname;
  if (!fLibrary)
    {
      //rpath is being checked for non absolute paths
      libname << "lib";
      switch (HEmodel)
        {
        case 0: libname << "Epos"; break;
        case 1: libname << "Epos"; break;
        case 2: libname << "Qgsjet01"; break;
        case 3: libname << "Gheisha"; break;
        case 4: libname << "Pythia"; break;
        case 5: libname << "Hijing"; break;
        case 6: libname << "Sibyll"; break;
#ifdef CRMC_QGSJETII04
        case 7: libname << "QgsjetII04"; break;
#endif
        case 8: libname << "Phojet"; break;
#ifdef CRMC_QGSJETII03
        case 11: libname << "QgsjetII03"; break;
#endif
#ifdef CRMC_DPMJET06
        case 12: libname << "Dpmjet06"; break;
#elif defined(CRMC_DPMJET17)
        case 12: libname << "Dpmjet17"; break;
#elif defined(CRMC_DPMJET19)
        case 12: libname << "Dpmjet19"; break;
#endif
#ifdef CRMC_QGSJETIII
        case 13: libname << "QgsjetIII"; break;
#endif
        default: libname << "UnknownModel"; break;
        }
      libname << ".so";
      fLibrary = dlopen(libname.str().c_str(), RTLD_NOW);
      cout << "Opening: " << libname.str() << endl;
      if (!fLibrary )
	  throw std::runtime_error("\n cannot open shared library "
				   + libname.str() + "\'\n\n"
				   + " Dynamic-link error:\n \""
				   + dlerror() + "\"\n");
    }

  crmc_generate  = (generate_t) find_symbol("crmc_f_");
  crmc_set       = (set_t)      find_symbol("crmc_set_f_");
  crmc_init      = (init_t)     find_symbol("crmc_init_f_");
  crmc_xsection  = (xsection_t) find_symbol("crmc_xsection_f_");
  crmc_defaults  = (defaults_t) find_symbol("aaset_");
  crmc_readparam = (readparam_t)find_symbol("eposinput_");
  crmc_ainit     = (ainit_t)    find_symbol("ainit_");
  crmc_pid       = (pid_t)      find_symbol("idtrafo_");
  
  //common blocks from library are not used. they come from DummyHepEvt library
  //grabbed in header with extern "C"
  // cevt_  = (cevt*)  dlsym(fLibrary, "cevt_");
  // if(cevt_ == NULL)
  //   {
  //     ostringstream errMsg;
  //     errMsg << " dlsym error:\n \"" << dlerror() << "\"\n";

  //     cerr << errMsg.str() << endl;
  //     exit(1);
  //   }
  // c2evt_ = (c2evt*) dlsym(fLibrary, "c2evt_");
  // if(c2evt_ == NULL)
  //   {
  //     ostringstream errMsg;
  //     errMsg << " dlsym error:\n \"" << dlerror() << "\"\n";

  //     cerr << errMsg.str() << endl;
  //     exit(1);
  //   }
  // hadr5_ = (hadr5*) dlsym(fLibrary, "hadr5_");
  // if(hadr5_ == NULL)
  //   {
  //     ostringstream errMsg;
  //     errMsg << " dlsym error:\n \"" << dlerror() << "\"\n";

  //     cerr << errMsg.str() << endl;
  //     exit(1);
  //   }

#endif
  return 1;
}
