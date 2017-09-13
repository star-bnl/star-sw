#ifndef __StarPythia5_h__
#define __StarPythia6_h__

/*!
  \class StarPythia6

  \brief Interface to pythia 6

  StarPythia6 provides the STAR user interface to the Pythia 6 event generator.
  Common blocks are exposed, to enable configuration of pythia.  The names of 
  elements in the structures are all in lower case, and correspond to the names
  of pythia 6 variables noted in the pythia 6 manual.  In the case of arrays 
  stored in common blocks, we have implemented access functions which account
  for the difference in memory layout between C++ and FORtran.  Users should 
  set array elements *exactly as described* in the pythia 6 manual. 

  Example:
  \code
  // Minbias process selection
  PySubs_t &pysubs = starpythia6->pysubs();
  pysubs.msel = 1; 
     
  // Set pi0 (id=102), pi+ (id=106) and eta (id=109) stable
  PyDat3_t &pydat3 = starpythia6->pydat3();
  pydat3.mdcy(102,1) = 0;
  pydat3.mdcy(106,1) = 0;
  pydat3.mdcy(109,1) = 0;
  \endcode

  For more information about pythia 6.4:
  http://arxiv.org/abs/hep-ph/0603175

  \author Jason C. Webb

 */

/**
   \example ../macros/starsim.pythia6.C
   Example how to run pythia6 events.

 */

#include "StarGenerator/BASE/StarGenerator.h"
#include "Pythia6.h"
#include <map>
using namespace std;

class StarPythia6 : public StarGenerator
{
 public:
  StarPythia6( const Char_t *name="Pythia6" );
  ~StarPythia6(){ /* nada */ };

  Int_t Init();
  Int_t Generate();
  StarGenStats Stats();

  /// Returns a reference to the /PYJETS/ common block
  PyJets_t &pyjets(){ return *address_of_pyjets(); }
  /// Returns a reference to the /PYSUBS/ common block
  PySubs_t &pysubs(){ return *address_of_pysubs(); }
  /// Returns a reference to the /PYDAT3/ common block
  PyDat3_t &pydat3(){ return *address_of_pydat3(); }
  /// Returns a reference to the /PYPARS/ common block
  PyPars_t &pypars(){ return *address_of_pypars(); }
  /// Returns a reference to the /PYINT5/ common block
  PyInt5_t &pyint5(){ return *address_of_pyint5(); }

  /// Calls the pytune function
  void PyTune( Int_t tune );
  /// Calls the pystat function
  void PyStat( Int_t stat );
  /// Calls the pylist function
  void PyList( Int_t list );


 private:
 protected:
  ClassDef(StarPythia6,1);

  void FillPP( StarGenEvent *event );
  void FillEP( StarGenEvent *event );

  map<Int_t,Int_t> mStatusCode;

};

#endif
