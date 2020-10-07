// LesHouches.cc is a part of the PYTHIA event generator.
// Copyright (C) 2012 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the LHAup and
// LHAupLHEF classes.

#include "LesHouches.h"

// Access time information.
#include <ctime>

// GZIP support.
#ifdef GZIPSUPPORT

// For GCC versions >= 4.6, can switch off shadow warnings.
#if (defined GZIPSUPPORT && ((__GNUC__ * 100) + __GNUC_MINOR__) >= 406)
#pragma GCC diagnostic ignored "-Wshadow"
#endif

// Boost includes.
#include "boost/iostreams/filtering_stream.hpp"
#include "boost/iostreams/filter/gzip.hpp"

// Switch shadow warnings back on.
#if (defined GZIPSUPPORT && ((__GNUC__ * 100) + __GNUC_MINOR__) >= 406)
#pragma GCC diagnostic warning "-Wshadow"
#endif

#endif // GZIPSUPPORT

namespace Pythia8 {

//==========================================================================

// LHAup class.

//--------------------------------------------------------------------------

// Constants: could be changed here if desired, but normally should not.
// These are of technical nature, as described for each.

// LHA convention with cross section in pb may require conversion from mb.
const double LHAup::CONVERTMB2PB = 1e9;

//--------------------------------------------------------------------------

// Print the initialization info; to check it worked.

void LHAup::listInit(ostream& os) {

  // Header.
  os << "\n --------  LHA initialization information  ------------ \n"; 

  // Beam info.
  os << fixed << setprecision(3) 
     << "\n  beam    kind      energy  pdfgrp  pdfset \n" 
     << "     A  " << setw(6) << idBeamASave 
     <<  setw(12) << eBeamASave 
     << setw(8) << pdfGroupBeamASave 
     << setw(8) << pdfSetBeamASave << "\n" 
     << "     B  " << setw(6) << idBeamBSave 
     <<  setw(12) << eBeamBSave 
     << setw(8) << pdfGroupBeamBSave 
     << setw(8) << pdfSetBeamBSave << "\n"; 

  // Event weighting strategy.
  os << "\n  Event weighting strategy = " << setw(2) 
     << strategySave << "\n" ;

  // Process list.
  os << scientific << setprecision(4) 
     << "\n  Processes, with strategy-dependent cross section info \n" 
     << "  number      xsec (pb)      xerr (pb)      xmax (pb) \n" ;
  for (int ip = 0; ip < int(processes.size()); ++ip) {
    os << setw(8) << processes[ip].idProc 
       << setw(15) << processes[ip].xSecProc 
       << setw(15) << processes[ip].xErrProc 
       << setw(15) << processes[ip].xMaxProc << "\n";
  }

  // Finished.
  os << "\n --------  End LHA initialization information  -------- \n"; 

}

//--------------------------------------------------------------------------

// Print the event info; to check it worked.

void LHAup::listEvent(ostream& os) {

  // Header.
  os << "\n --------  LHA event information and listing  -------------"
     << "--------------------------------------------------------- \n"; 

  // Basic event info.
  os << scientific << setprecision(4) 
     << "\n    process = " << setw(8) << idProc 
     << "    weight = " << setw(12) << weightProc 
     << "     scale = " << setw(12) << scaleProc << " (GeV) \n"
     << "                   "
     << "     alpha_em = " << setw(12) << alphaQEDProc 
     << "    alpha_strong = " << setw(12) << alphaQCDProc << "\n";

  // Particle list
  os << fixed << setprecision(3) 
     << "\n    Participating Particles \n" 
     << "    no        id stat     mothers     colours      p_x        "
     << "p_y        p_z         e          m        tau    spin \n" ;
  for (int ip = 1; ip < int(particles.size()); ++ip) {
    os << setw(6) << ip 
       << setw(10) << particles[ip].idPart 
       << setw(5) << particles[ip].statusPart 
       << setw(6) << particles[ip].mother1Part
       << setw(6) << particles[ip].mother2Part 
       << setw(6) << particles[ip].col1Part
       << setw(6) << particles[ip].col2Part 
       << setw(11) << particles[ip].pxPart
       << setw(11) << particles[ip].pyPart
       << setw(11) << particles[ip].pzPart 
       << setw(11) << particles[ip].ePart 
       << setw(11) <<  particles[ip].mPart 
       << setw(8) <<  particles[ip].tauPart 
       << setw(8) <<  particles[ip].spinPart << "\n";
  }

  // PDF info - optional.
  if (pdfIsSetSave) os << "\n   pdf: id1 =" << setw(5) << id1Save  
    << " id2 =" << setw(5) << id2Save 
    << " x1 ="  << scientific << setw(10) << x1Save    
    << " x2 =" << setw(10) << x2Save 
    << " scalePDF =" << setw(10) << scalePDFSave 
    << " xpdf1 =" << setw(10) << xpdf1Save    
    << " xpdf2 =" << setw(10) << xpdf2Save << "\n";    

  // Finished.
  os << "\n --------  End LHA event information and listing  ---------"
     << "--------------------------------------------------------- \n"; 

}

//--------------------------------------------------------------------------

// Open and write header to a Les Houches Event File.

bool LHAup::openLHEF(string fileNameIn) {

  // Open file for writing. Reset it to be empty.
  fileName = fileNameIn;
  const char* cstring = fileName.c_str();
  osLHEF.open(cstring, ios::out | ios::trunc);  
  if (!osLHEF) {
    infoPtr->errorMsg("Error in LHAup::openLHEF:"
      " could not open file", fileName);
    return false;
  }

  // Read out current date and time.
  time_t t = time(0);
  strftime(dateNow,12,"%d %b %Y",localtime(&t));
  strftime(timeNow,9,"%H:%M:%S",localtime(&t));

  // Write header.
  osLHEF << "<LesHouchesEvents version=\"1.0\">\n" 
         << "<!--\n"
         << "  File written by Pythia8::LHAup on " 
         << dateNow << " at " << timeNow << "\n" 
         << "-->" << endl;

  // Done.
  return true;

}

//--------------------------------------------------------------------------

// Write initialization information to a Les Houches Event File.

bool LHAup::initLHEF() {

  // Write information on beams. 
  osLHEF << "<init>\n" << scientific << setprecision(6)
         << "  " << idBeamASave       << "  " << idBeamBSave 
         << "  " << eBeamASave        << "  " << eBeamBSave 
         << "  " << pdfGroupBeamASave << "  " << pdfGroupBeamBSave 
         << "  " << pdfSetBeamASave   << "  " << pdfSetBeamBSave 
         << "  " << strategySave      << "  " << processes.size() << "\n";

  // Write information on all the subprocesses.
  for (int ip = 0; ip < int(processes.size()); ++ip) 
    osLHEF << " " << setw(13) << processes[ip].xSecProc 
           << " " << setw(13) << processes[ip].xErrProc 
           << " " << setw(13) << processes[ip].xMaxProc 
           << " " << setw(6) << processes[ip].idProc << "\n";

  // Done.
  osLHEF << "</init>" << endl;
  return true;

}

//--------------------------------------------------------------------------

// Write event information to a Les Houches Event File.

bool LHAup::eventLHEF() {

  // Write information on process as such. 
  osLHEF << "<event>\n" << scientific << setprecision(6)
         << " " << setw(5) << particles.size() - 1 
         << " " << setw(5) << idProc
         << " " << setw(13) << weightProc 
         << " " << setw(13) << scaleProc
         << " " << setw(13) << alphaQEDProc
         << " " << setw(13) << alphaQCDProc << "\n";

  // Write information on the particles, excluding zeroth. 
  for (int ip = 1; ip < int(particles.size()); ++ip) {
    osLHEF << " " << setw(8) << particles[ip].idPart 
           << " " << setw(5) << particles[ip].statusPart 
           << " " << setw(5) << particles[ip].mother1Part
           << " " << setw(5) << particles[ip].mother2Part 
           << " " << setw(5) << particles[ip].col1Part
           << " " << setw(5) << particles[ip].col2Part << setprecision(10)
           << " " << setw(17) << particles[ip].pxPart
           << " " << setw(17) << particles[ip].pyPart
           << " " << setw(17) << particles[ip].pzPart 
           << " " << setw(17) << particles[ip].ePart 
           << " " << setw(17) <<  particles[ip].mPart << setprecision(6);
    if (particles[ip].tauPart == 0.) osLHEF << " 0.";
    else osLHEF << " " << setw(13) << particles[ip].tauPart;
    if (particles[ip].spinPart == 9.) osLHEF << " 9.";
    else osLHEF << " " << setw(13) << particles[ip].spinPart;
    osLHEF << "\n";
  }

  // Optionally write information on PDF values at hard interaction.
  if (pdfIsSetSave) osLHEF << "#pdf" 
           << " " << setw(4) << id1Save
           << " " << setw(4) << id2Save
           << " " << setw(13) << x1Save 
           << " " << setw(13) << x2Save 
           << " " << setw(13) << scalePDFSave 
           << " " << setw(13) << xpdf1Save 
           << " " << setw(13) << xpdf2Save << "\n"; 

  // Done.
  osLHEF << "</event>" << endl;
  return true;

}

//--------------------------------------------------------------------------

// Write end of a Les Houches Event File and close it.

bool LHAup::closeLHEF(bool updateInit) {

  // Write an end to the file.
  osLHEF << "</LesHouchesEvents>" << endl;
  osLHEF.close();

  // Optionally update the cross section information.
  if (updateInit) {
    const char* cstring = fileName.c_str();
    osLHEF.open(cstring, ios::in | ios::out); 
  
    // Rewrite header; identically with what openLHEF did.
    osLHEF << "<LesHouchesEvents version=\"1.0\">\n" 
           << "<!--\n"
           << "  File written by Pythia8::LHAup on " 
           << dateNow << " at " << timeNow << "\n" 
           << "-->" << endl;

    // Redo initialization information.
    initLHEF();
    osLHEF.close();
  }  

  // Done.
  return true;

}

//--------------------------------------------------------------------------

// Read in initialization information from a Les Houches Event File.

bool LHAup::setInitLHEF(istream& is) {

  // Check that first line is consistent with proper LHEF file.
  string line;
  if (!getline(is, line)) return false;
  if (line.find("<LesHouchesEvents") == string::npos) return false;  
  if (line.find("version=\"1.0\"" ) == string::npos ) return false;
 
  // Loop over lines until an <init tag is found first on a line.
  string tag = " ";
  do { 
    if (!getline(is, line)) return false;
    if (line.find_first_not_of(" \n\t\v\b\r\f\a") != string::npos) {
      istringstream getfirst(line);
      getfirst >> tag;
      if (!getfirst) return false;
    }
  } while (tag != "<init>" && tag != "<init"); 
  
  // Read in beam and strategy info, and store it. 
  int idbmupA, idbmupB;
  double ebmupA, ebmupB;
  int pdfgupA, pdfgupB, pdfsupA, pdfsupB, idwtup, nprup;
  if (!getline(is, line)) return false;
  istringstream getbms(line);
  getbms >> idbmupA >> idbmupB >> ebmupA >> ebmupB >> pdfgupA 
     >> pdfgupB >> pdfsupA >> pdfsupB >> idwtup >> nprup;
  if (!getbms) return false;
  setBeamA(idbmupA, ebmupA, pdfgupA, pdfsupA);
  setBeamB(idbmupB, ebmupB, pdfgupB, pdfsupB);
  setStrategy(idwtup);

  // Read in process info, one process at a time, and store it.
  double xsecup, xerrup, xmaxup;
  int lprup; 
  for (int ip = 0; ip < nprup; ++ip) { 
    if (!getline(is, line)) return false;
    istringstream getpro(line);
    getpro >> xsecup >> xerrup >> xmaxup >> lprup ;
    if (!getpro) return false;
    addProcess(lprup, xsecup, xerrup, xmaxup);
  }

  // Reading worked.
  return true;

}

//--------------------------------------------------------------------------

// Read in event information from a Les Houches Event File,
// into a staging area where it can be reused by setOldEventLHEF.

bool LHAup::setNewEventLHEF(istream& is) {
  
  // Loop over lines until an <event tag is found first on a line.
  string line, tag;
  do { 
    if (!getline(is, line)) return false;
    if (line.find_first_not_of(" \n\t\v\b\r\f\a") != string::npos) {
      istringstream getfirst(line);
      getfirst >> tag;
      if (!getfirst) return false;
    }
  } while (tag != "<event>" && tag != "<event"); 

  // Read in process info and store it.
  if (!getline(is, line)) return false;
  istringstream getpro(line);
  getpro >> nupSave >> idprupSave >> xwgtupSave >> scalupSave
    >> aqedupSave >> aqcdupSave;
  if (!getpro) return false;

  // Reset particlesSave vector, add slot-0 empty particle.
  particlesSave.clear(); 
  particlesSave.push_back( LHAParticle() );

  // Read in particle info one by one, and store it.
  // Note unusual C++ loop range, to better reflect LHA/Fortran standard.
  // (Recall that process(...) above added empty particle at index 0.) 
  int idup, istup, mothup1, mothup2, icolup1, icolup2; 
  double pup1, pup2, pup3, pup4, pup5, vtimup, spinup;
  for (int ip = 1; ip <= nupSave; ++ip) { 
    if (!getline(is, line)) return false;
    istringstream getall(line);
    getall >> idup >> istup >> mothup1 >> mothup2 >> icolup1 >> icolup2 
      >> pup1 >> pup2 >> pup3 >> pup4 >> pup5 >> vtimup >> spinup;
    if (!getall) return false;   
    particlesSave.push_back( LHAParticle( idup, istup, mothup1, mothup2, 
      icolup1, icolup2, pup1, pup2, pup3, pup4, pup5, vtimup, spinup) );
  }

  // Continue parsing till </event>. Extract pdf info if present.
  getPDFSave = false;
  do { 
    if (!getline(is, line)) return false;
    istringstream getpdf(line);
    getpdf >> tag;
    if (!getpdf) return false;
    if (tag == "#pdf") {
      getpdf >> id1InSave >> id2InSave >> x1InSave >> x2InSave 
             >> scalePDFInSave >> xpdf1InSave >> xpdf2InSave;
      if (!getpdf) return false;
      getPDFSave = true;
    }
  } while (tag != "</event>" && tag != "</event"); 

  // Need id and x values even when no PDF info. Rest empty.
  if (!getPDFSave) {
    id1InSave      = particlesSave[1].idPart;
    id2InSave      = particlesSave[2].idPart;
    x1InSave       = particlesSave[1].ePart / eBeamASave; 
    x2InSave       = particlesSave[2].ePart / eBeamBSave; 
    scalePDFInSave = 0.;
    xpdf1InSave    = 0.;
    xpdf2InSave    = 0.;
  }
  
  // Reading worked.
  return true;

}

//--------------------------------------------------------------------------

// Make current event information read in by setNewEventLHEF.

bool LHAup::setOldEventLHEF() {

  // Store saved event, optionally also parton density information.
  setProcess(idprupSave, xwgtupSave, scalupSave, aqedupSave, aqcdupSave);
  for (int ip = 1; ip <= nupSave; ++ip) addParticle( particlesSave[ip] );
  setPdf(id1InSave, id2InSave, x1InSave, x2InSave, scalePDFInSave, 
    xpdf1InSave, xpdf2InSave, getPDFSave);  

  // Done;
  return true;

}


//==========================================================================

// LHAupLHEF class.

//--------------------------------------------------------------------------

// Constructor.

LHAupLHEF::LHAupLHEF(const char* fileIn) : ifs(fileIn), is(NULL) {

// Construct istream without gzip support.
#ifndef GZIPSUPPORT
  is = (istream *) &ifs;

// Construct istream with gzip support.
#else
  boost::iostreams::filtering_istream *fis =
    new boost::iostreams::filtering_istream();

  // Pass along the 'good()' flag, so code elsewhere works unmodified.
  if (!ifs.good()) fis->setstate(ios_base::badbit);

  // Check filename ending to decide which filters to apply.
  else {
    const char *last = strrchr(fileIn, '.');
    if (last && strncmp(last, ".gz", 3) == 0)
      fis->push(boost::iostreams::gzip_decompressor());
    fis->push(ifs);
  }
  is = (istream *) fis;
#endif

}

//--------------------------------------------------------------------------

// Destructor.

LHAupLHEF::~LHAupLHEF() {

// Delete istream if constructed.
#ifdef GZIPSUPPORT
  if (is) delete is;
#endif

}

//==========================================================================

// LHAupFromPYTHIA8 class.

//--------------------------------------------------------------------------

// Read in initialization information from PYTHIA 8.

bool LHAupFromPYTHIA8::setInit() {
  
  // Read in beam from Info class. Parton density left empty. 
  int    idbmupA = infoPtr->idA();
  int    idbmupB = infoPtr->idB();
  double ebmupA  = infoPtr->eA();
  double ebmupB  = infoPtr->eB();
  int    pdfgupA = 0; 
  int    pdfgupB = 0; 
  int    pdfsupA = 0; 
  int    pdfsupB = 0; 
  setBeamA(idbmupA, ebmupA, pdfgupA, pdfsupA);
  setBeamB(idbmupB, ebmupB, pdfgupB, pdfsupB);

  // Currently only one allowed strategy.
  int    idwtup = 3;
  setStrategy(idwtup);

  // Only one process with dummy information. (Can overwrite at the end.)
  int    lprup  = 9999; 
  double xsecup = 1.;
  double xerrup = 0.;
  double xmaxup = 1.;
  addProcess(lprup, xsecup, xerrup, xmaxup);

  // Done.
  return true;

}

//--------------------------------------------------------------------------

// Read in event information from PYTHIA 8.

bool LHAupFromPYTHIA8::setEvent( int ) {

  // Read process information from Info class, and store it.
  // Note: renormalization scale here, factorization further down.
  // For now always convert to process 9999, instead of infoPtr->code(). 
  int    idprup = 9999;
  double xwgtup = infoPtr->weight();
  double scalup = infoPtr->QRen();
  double aqedup = infoPtr->alphaEM();
  double aqcdup = infoPtr->alphaS();
  setProcess(idprup, xwgtup, scalup, aqedup, aqcdup);

  // Read in particle info one by one, excluding zero and beams, and store it.
  // Note unusual C++ loop range, to better reflect LHA/Fortran standard.
  int nup   = processPtr->size() - 3;
  int    idup, statusup, istup, mothup1, mothup2, icolup1, icolup2; 
  double pup1, pup2, pup3, pup4, pup5, vtimup, spinup;
  for (int ip = 1; ip <= nup; ++ip) {
    Particle& particle = (*processPtr)[ip + 2];
    idup     = particle.id(); 
    // Convert from PYTHIA8 to LHA status codes.
    statusup = particle.status();
    if (ip < 3)            istup = -1;
    else if (statusup < 0) istup =  2;
    else                   istup =  1;
    mothup1  = max(0, particle.mother1() - 2); 
    mothup2  = max(0, particle.mother2() - 2); 
    icolup1  = particle.col();
    icolup2  = particle.acol();
    pup1     = particle.px();
    pup2     = particle.py();
    pup3     = particle.pz();
    pup4     = particle.e();
    pup5     = particle.m();
    vtimup   = particle.tau(); 
    spinup   = particle.pol();
    addParticle(idup, istup, mothup1, mothup2, icolup1, icolup2,
      pup1, pup2, pup3, pup4, pup5, vtimup, spinup) ;
  }

  // Also extract pdf information from Info class, and store it.
  int    id1up      = infoPtr->id1();
  int    id2up      = infoPtr->id2();
  double x1up       = infoPtr->x1();
  double x2up       = infoPtr->x2();
  double scalePDFup = infoPtr->QFac();
  double xpdf1up    = infoPtr->pdf1();
  double xpdf2up    = infoPtr->pdf2();
  setPdf(id1up, id2up, x1up, x2up, scalePDFup, xpdf1up, xpdf2up, true);  

  // Done.
  return true;

}

//--------------------------------------------------------------------------

//  Update cross-section information at the end of the run.

bool LHAupFromPYTHIA8::updateSigma() {

  // Read out information from PYTHIA 8 and send it in to LHA.
  double sigGen = CONVERTMB2PB * infoPtr->sigmaGen();
  double sigErr = CONVERTMB2PB * infoPtr->sigmaErr(); 
  setXSec(0, sigGen);
  setXErr(0, sigErr);

  // Done.
  return true;

}
 
//==========================================================================

} // end namespace Pythia8
