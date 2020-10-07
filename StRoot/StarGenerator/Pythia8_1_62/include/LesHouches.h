// LesHouches.h is a part of the PYTHIA event generator.
// Copyright (C) 2012 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for Les Houches Accord user process information.
// LHAProcess: stores a single process; used by the other classes.
// LHAParticle: stores a single particle; used by the other classes.
// LHAup: base class for initialization and event information.
// LHAupLHEF: derived class for reading from an Les Houches Event File.
// Code for interfacing with Fortran commonblocks is found in LHAFortran.h.

#ifndef Pythia8_LesHouches_H
#define Pythia8_LesHouches_H

#include "Event.h"
#include "Info.h"
#include "PythiaStdlib.h"
#include "Settings.h"

namespace Pythia8 {

//==========================================================================

// A class for the processes stored in LHAup.
  
class LHAProcess {

public:

  // Constructors.
  LHAProcess() : idProc(0), xSecProc(0.), xErrProc(0.), xMaxProc(0.) { }
  LHAProcess(int idProcIn, double xSecIn, double xErrIn, double xMaxIn) :
    idProc(idProcIn), xSecProc(xSecIn), xErrProc(xErrIn), 
    xMaxProc(xMaxIn) { }

  // Process properties.
  int idProc;
  double xSecProc, xErrProc, xMaxProc;

} ;

//==========================================================================

// A class for the particles stored in LHAup.

class LHAParticle {

public:

  // Constructors.   
  LHAParticle() : idPart(0), statusPart(0), mother1Part(0), 
    mother2Part(0), col1Part(0), col2Part(0), pxPart(0.), pyPart(0.), 
    pzPart(0.), ePart(0.), mPart(0.), tauPart(0.), spinPart(9.) { }
  LHAParticle(int idIn, int statusIn, int mother1In, int mother2In,
    int col1In, int col2In, double pxIn, double pyIn, double pzIn, 
    double eIn, double mIn, double tauIn, double spinIn) :
    idPart(idIn), statusPart(statusIn), mother1Part(mother1In), 
    mother2Part(mother2In), col1Part(col1In), col2Part(col2In), 
    pxPart(pxIn), pyPart(pyIn), pzPart(pzIn), ePart(eIn), mPart(mIn), 
    tauPart(tauIn), spinPart(spinIn) { }

  // Particle properties.    
  int idPart, statusPart, mother1Part, mother2Part, col1Part, col2Part;
  double pxPart, pyPart, pzPart, ePart, mPart, tauPart, spinPart;

} ;

//==========================================================================

// LHAup is base class for initialization and event information 
// from an external parton-level generator.

class LHAup {

public:

  // Destructor.
  virtual ~LHAup() {}

  // Set info pointer.
  void setPtr(Info* infoPtrIn) {infoPtr = infoPtrIn;}
 
  // Method to be used for LHAupLHEF derived class.
  virtual bool fileFound() {return true;} 
 
  // A pure virtual method setInit, wherein all initialization information 
  // is supposed to be set in the derived class. Can do this by reading a 
  // file or some other way, as desired. Returns false if it did not work. 
  virtual bool setInit() = 0; 

  // Give back info on beams.
  int    idBeamA()       const {return idBeamASave;}
  int    idBeamB()       const {return idBeamBSave;}
  double eBeamA()        const {return eBeamASave;}
  double eBeamB()        const {return eBeamBSave;}
  int    pdfGroupBeamA() const {return pdfGroupBeamASave;}
  int    pdfGroupBeamB() const {return pdfGroupBeamBSave;}
  int    pdfSetBeamA()   const {return pdfSetBeamASave;}
  int    pdfSetBeamB()   const {return pdfSetBeamBSave;}
    
  // Give back weight strategy.
  int    strategy()      const {return strategySave;}

  // Give back info on processes.
  int    sizeProc()      const {return processes.size();} 
  int    idProcess(int proc) const {return processes[proc].idProc;} 
  double xSec(int proc)  const {return processes[proc].xSecProc;}    
  double xErr(int proc)  const {return processes[proc].xErrProc;}    
  double xMax(int proc)  const {return processes[proc].xMaxProc;} 
   
  // Print the initialization info; useful to check that setting it worked.
  void   listInit(ostream& os = cout);  

  // A pure virtual method setEvent, wherein information on the next event
  // is supposed to be set in the derived class. 
  // Strategies +-1 and +-2: idProcIn is the process type, selected by PYTHIA.
  // Strategies +-3 and +-4: idProcIn is dummy; process choice is made locally.
  // The method can find the next event by a runtime interface to another
  // program, or by reading a file, as desired. 
  // The method should return false if it did not work.
  virtual bool setEvent(int idProcIn = 0) = 0; 

  // Give back process number, weight, scale, alpha_em, alpha_s.
  int    idProcess()       const {return idProc;} 
  double weight()          const {return weightProc;} 
  double scale()           const {return scaleProc;} 
  double alphaQED()        const {return alphaQEDProc;} 
  double alphaQCD()        const {return alphaQCDProc;} 

  // Give back info on separate particle.
  int    sizePart()        const {return particles.size();}
  int    id(int part)      const {return particles[part].idPart;}
  int    status(int part)  const {return particles[part].statusPart;}
  int    mother1(int part) const {return particles[part].mother1Part;}
  int    mother2(int part) const {return particles[part].mother2Part;}
  int    col1(int part)    const {return particles[part].col1Part;}
  int    col2(int part)    const {return particles[part].col2Part;}
  double px(int part)      const {return particles[part].pxPart;}
  double py(int part)      const {return particles[part].pyPart;}
  double pz(int part)      const {return particles[part].pzPart;}
  double e(int part)       const {return particles[part].ePart;}
  double m(int part)       const {return particles[part].mPart;}
  double tau(int part)     const {return particles[part].tauPart;}
  double spin(int part)    const {return particles[part].spinPart;}

  // Optional: give back info on parton density values of event.
  bool   pdfIsSet()        const {return pdfIsSetSave;}
  int    id1()             const {return id1Save;}
  int    id2()             const {return id2Save;}
  double x1()              const {return x1Save;}
  double x2()              const {return x2Save;}
  double scalePDF()        const {return scalePDFSave;}
  double xpdf1()           const {return xpdf1Save;}
  double xpdf2()           const {return xpdf2Save;}

  // Print the info; useful to check that reading an event worked.
  void   listEvent(ostream& os = cout);  

  // Skip ahead a number of events, which are not considered further.
  // Mainly intended for debug when using the LHAupLHEF class.
  virtual bool skipEvent(int nSkip) {
    for (int iSkip = 0; iSkip < nSkip; ++iSkip)
    if (!setEvent()) return false; return true;} 

  // Four routines to write a Les Houches Event file in steps.
  bool   openLHEF(string fileNameIn);
  bool   initLHEF();
  bool   eventLHEF();
  bool   closeLHEF(bool updateInit = false);

protected:

  // Constructor. Sets default to be that events come with unit weight.
  LHAup(int strategyIn = 3) : strategySave(strategyIn) 
    { processes.reserve(10); particles.reserve(20); } 

  // Allow conversion from mb to pb.
  static const double CONVERTMB2PB;

  // Pointer to various information on the generation.
  Info* infoPtr;

  // Input beam info.
  void setBeamA(int idIn, double eIn, int pdfGroupIn = 0, int pdfSetIn = 0) 
    { idBeamASave = idIn; eBeamASave = eIn; pdfGroupBeamASave = pdfGroupIn;  
    pdfSetBeamASave = pdfSetIn;} 
  void setBeamB(int idIn, double eIn, int pdfGroupIn = 0, int pdfSetIn = 0) 
    { idBeamBSave = idIn; eBeamBSave = eIn; pdfGroupBeamBSave = pdfGroupIn;  
    pdfSetBeamBSave = pdfSetIn;} 

  // Input process weight strategy.
  void setStrategy(int strategyIn) {strategySave = strategyIn;} 

  // Input process info.
  void addProcess(int idProcIn, double xSecIn = 1., double xErrIn = 0., 
    double xMaxIn = 1.) { processes.push_back( LHAProcess( idProcIn, 
    xSecIn, xErrIn, xMaxIn)); }

  // Possibility to update some cross section info at end of run.
  void setXSec(int iP, double xSecIn) {processes[iP].xSecProc = xSecIn;}
  void setXErr(int iP, double xErrIn) {processes[iP].xErrProc = xErrIn;}
  void setXMax(int iP, double xMaxIn) {processes[iP].xMaxProc = xMaxIn;}     
 
  // Input info on the selected process.
  void setProcess(int idProcIn = 0, double weightIn = 1., double 
    scaleIn = 0., double alphaQEDIn = 0.0073, double alphaQCDIn = 0.12) { 
    idProc = idProcIn; weightProc = weightIn; scaleProc = scaleIn; 
    alphaQEDProc = alphaQEDIn; alphaQCDProc = alphaQCDIn; 
    // Clear particle list. Add empty zeroth particle for correct indices.
    particles.clear(); addParticle(0); pdfIsSetSave = false;}

  // Input particle info, one particle at the time.
  void addParticle(LHAParticle particleIn) {
    particles.push_back(particleIn);}
  void addParticle(int idIn, int statusIn = 0, int mother1In = 0, 
    int mother2In = 0, int col1In = 0, int col2In = 0, double pxIn = 0., 
    double pyIn = 0., double pzIn = 0., double eIn = 0., double mIn = 0., 
    double tauIn = 0., double spinIn = 9.) { 
    particles.push_back( LHAParticle( idIn, statusIn, mother1In, mother2In, 
    col1In, col2In, pxIn, pyIn, pzIn, eIn, mIn, tauIn, spinIn)); }

  // Optionally input info on parton density values of event.
  void setPdf(int id1In, int id2In, double x1In, double x2In, 
    double scalePDFIn, double xpdf1In, double xpdf2In, bool pdfIsSetIn) 
    { id1Save = id1In; id2Save = id2In; x1Save = x1In; x2Save = x2In;
    scalePDFSave = scalePDFIn; xpdf1Save = xpdf1In; xpdf2Save = xpdf2In;
    pdfIsSetSave = pdfIsSetIn;}

  // Three routines for LHEF files, but put here for flexibility.
  bool setInitLHEF(istream& is);
  bool setNewEventLHEF(istream& is);
  bool setOldEventLHEF();

  // Event properties from LHEF files, for repeated use.
  int    nupSave, idprupSave;
  double xwgtupSave, scalupSave, aqedupSave, aqcdupSave;
  vector<LHAParticle> particlesSave;
  bool   getPDFSave;
  int    id1InSave, id2InSave;
  double x1InSave, x2InSave, scalePDFInSave, xpdf1InSave, xpdf2InSave;

private:

  // Event weighting and mixing strategy.
  int strategySave;

  // Beam particle properties.
  int idBeamASave, idBeamBSave;
  double eBeamASave, eBeamBSave;
  int pdfGroupBeamASave, pdfGroupBeamBSave, pdfSetBeamASave, pdfSetBeamBSave;

  // The process list, stored as a vector of processes.
  vector<LHAProcess> processes;

  // Store info on the selected process. 
  int idProc;
  double weightProc, scaleProc, alphaQEDProc, alphaQCDProc;

  // The particle list, stored as a vector of particles.
  vector<LHAParticle> particles;

  // Optional info on parton density values of event.
  bool   pdfIsSetSave;
  int    id1Save, id2Save;
  double x1Save, x2Save, scalePDFSave, xpdf1Save, xpdf2Save;
 
  // File to which to write Les Houches Event File information.
  string fileName;
  fstream osLHEF;
  char dateNow[12];
  char timeNow[9];

};

//==========================================================================

// A derived class with information read from a Les Houches Event File.

class LHAupLHEF : public LHAup {

public:

  // Constructor.
  LHAupLHEF(const char* fileIn);

  // Destructor.
  ~LHAupLHEF();

  // Confirm that file was found and opened as expected.
  bool fileFound() {return is->good();} 

  // Routine for doing the job of reading and setting initialization info.  
  bool setInit() {return setInitLHEF(*is);} 

  // Routine for doing the job of reading and setting info on next event.  
  bool setEvent(int = 0) {if (!setNewEventLHEF(*is)) return false;
    return setOldEventLHEF();} 

  // Skip ahead a number of events, which are not considered further.
  bool skipEvent(int nSkip) {for (int iSkip = 0; iSkip < nSkip; ++iSkip)
    if (!setNewEventLHEF(*is)) return false; return true;} 

private:

  // File from which to read (or a stringstream).
  // Note: for GZIP support, use a pointer to an istream to avoid #ifdef's 
  //   in the header file. Without gzip support, is = (istream *) &ifs .
  //   With gzip support, is = boost::iostreams::filtering_istream ,
  //   with ifs as a source.
  ifstream  ifs;
  istream  *is;

};

//==========================================================================

// A derived class with information read from PYTHIA 8 itself, for output.

class LHAupFromPYTHIA8 : public LHAup {

public:

  // Constructor.
  LHAupFromPYTHIA8(Event* processPtrIn, Info* infoPtrIn) {
    processPtr = processPtrIn; infoPtr = infoPtrIn;}

  // Destructor.
  ~LHAupFromPYTHIA8() {}

  // Routine for doing the job of reading and setting initialization info.  
  bool setInit(); 

  // Routine for doing the job of reading and setting info on next event.  
  bool setEvent(int = 0); 

  // Update cross-section information at the end of the run.
  bool updateSigma();

private:

  // Pointers to process event record and further information.
  Event* processPtr;
  Info*  infoPtr;

};

//==========================================================================

} // end namespace Pythia8

#endif // Pythia8_LesHouches_H
