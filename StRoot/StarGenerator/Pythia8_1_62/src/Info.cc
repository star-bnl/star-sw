// Info.cc is a part of the PYTHIA event generator.
// Copyright (C) 2012 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the Info class.

#include "Info.h"

namespace Pythia8 {

//==========================================================================

// Info class.
// This class contains a mixed bag of information on the event generation 
// activity, especially on the current subprocess properties.

//--------------------------------------------------------------------------

// Constants: could be changed here if desired, but normally should not.
// These are of technical nature, as described for each.

// Number of times the same error message will be repeated at most.
const int Info::TIMESTOPRINT = 1; 

// LHA convention with cross section in pb may require conversion from mb.
const double Info::CONVERTMB2PB = 1e9;

//--------------------------------------------------------------------------

// List (almost) all information currently set.

void Info::list(ostream& os) const {

  // Header and beam info.
  os << "\n --------  PYTHIA Info Listing  ------------------------"
     << "---------------- \n \n" 
     << scientific << setprecision(3) 
     << " Beam A: id = " << setw(6) << idASave << ", pz = " << setw(10) 
     << pzASave << ", e = " << setw(10) << eASave << ", m = " << setw(10) 
     << mASave << ".\n"
     << " Beam B: id = " << setw(6) << idBSave << ", pz = " << setw(10) 
     << pzBSave << ", e = " << setw(10) << eBSave << ", m = " << setw(10) 
     << mBSave << ".\n\n";

  // Done if no subprocess has been defined.
  if (codeSave == 0 && nFinalSave == 0) {
    os << " No process has been set; something must have gone wrong! \n"
       << "\n --------  End PYTHIA Info Listing  --------------------"
       << "----------------" << endl; 
    return;
  }

  // Colliding parton info.
  if (isRes) 
    os << " In 1: id = " << setw(4) << id1Save << ", x = " << setw(10)
       << x1Save << ", pdf = " << setw(10) << pdf1Save << " at Q2 = " 
       << setw(10) << Q2FacSave << ".\n"  
       << " In 2: id = " << setw(4) << id2Save << ", x = " << setw(10)
       << x2Save << ", pdf = " << setw(10) << pdf2Save << " at same Q2.\n\n";  

  // Process name and code.
  os << ((isRes && !hasSubSave) ? " Subprocess " : " Process ") << nameSave 
     << " with code " << codeSave << " is 2 -> " << nFinalSave << ".\n";

  // Subprocess name and code for minimum bias processes.
  if (hasSubSave)
    os << " Subprocess " << nameSubSave << " with code " << codeSubSave 
       << " is 2 -> " << nFinalSubSave << ".\n";

  // Process-type-specific kinematics information.
  if (isRes && nFinalSave == 1) 
    os << " It has sHat = " << setw(10) << sH << ".\n";  
  else if ( isRes && nFinalSave == 2)  
    os << " It has sHat = " << setw(10) << sH << ",    tHat = " 
       << setw(10) << tH << ",    uHat = " << setw(10) << uH << ",\n"
       << "       pTHat = " << setw(10) << pTH << ",   m3Hat = " 
       << setw(10) << m3H << ",   m4Hat = " << setw(10) << m4H << ",\n"
       << "    thetaHat = " << setw(10) << thetaH << ",  phiHat = " 
       << setw(10) << phiH << ".\n";  
  else if ( nFinalSave == 2)  
    os << " It has s = " << setw(10) << sH << ",    t = " << setw(10) 
       << tH << ",    u = " << setw(10) << uH << ",\n"
       << "       pT = " << setw(10) << pTH << ",   m3 = " << setw(10) 
       << m3H << ",   m4 = " << setw(10) << m4H << ",\n" 
       << "    theta = " << setw(10) << thetaH << ",  phi = " << setw(10) 
       << phiH << ".\n";
  else if ( isRes && nFinalSave == 3)  
    os << " It has sHat = " << setw(10) << sH << ", <pTHat> = " 
       << setw(10) << pTH << ".\n";  

  // Couplings.
  if (isRes) os << "     alphaEM = " << setw(10) << alphaEMSave 
    << ",  alphaS = " << setw(10) << alphaSSave << "    at Q2 = " 
    << setw(10) << Q2RenSave << ".\n"; 

  // Impact parameter.
  if (bIsSet) os << "\n Impact parameter b = " << setw(10) << bMPISave 
    << " gives enhancement factor = " << setw(10) << enhanceMPISave 
    << ".\n";

  // Multiparton interactions and shower evolution.
  if (evolIsSet) os << " Max pT scale for MPI = " << setw(10) << pTmaxMPISave
    << ", ISR = " << setw(10) << pTmaxISRSave << ", FSR = " << setw(10) 
    << pTmaxISRSave << ".\n Number of MPI = " << setw(5) << nMPISave 
    << ", ISR = " << setw(5) << nISRSave << ", FSRproc = " << setw(5) 
    << nFSRinProcSave << ", FSRreson = " << setw(5) << nFSRinResSave 
    << ".\n"; 
       
  // Listing finished.
  os << "\n --------  End PYTHIA Info Listing  --------------------"
     << "----------------" << endl; 

}

//--------------------------------------------------------------------------

// Event weight and accumulated weight.

double Info::weight() const { return (abs(lhaStrategySave) == 4) 
  ? CONVERTMB2PB * weightSave : weightSave;
} 
  
double Info::weightSum() const {return (abs(lhaStrategySave) == 4) 
  ? CONVERTMB2PB * wtAccSum : wtAccSum;
}

//--------------------------------------------------------------------------
  
// Print a message the first few times. Insert in database.
 
  void Info::errorMsg(string messageIn, string extraIn, bool showAlways, 
    ostream& os) {
   
  // Recover number of times message occured. Also inserts new string.
  int times = messages[messageIn];
  ++messages[messageIn];

  // Print message the first few times.
  if (times < TIMESTOPRINT || showAlways) os << " PYTHIA " 
    << messageIn << " " << extraIn << endl;

}

//--------------------------------------------------------------------------

// Provide total number of errors/aborts/warnings experienced to date.

int Info::errorTotalNumber() {

  int nTot = 0;
  for ( map<string, int>::iterator messageEntry = messages.begin();
    messageEntry != messages.end(); ++messageEntry)
    nTot += messageEntry->second;
  return nTot;

}

//--------------------------------------------------------------------------

// Print statistics on errors/aborts/warnings.

void Info::errorStatistics(ostream& os) {

  // Header.
  os << "\n *-------  PYTHIA Error and Warning Messages Statistics  "
     << "----------------------------------------------------------* \n"
     << " |                                                       "
     << "                                                          | \n"
     << " |  times   message                                      "
     << "                                                          | \n" 
     << " |                                                       "
     << "                                                          | \n";

  // Loop over all messages
  map<string, int>::iterator messageEntry = messages.begin();
  if (messageEntry == messages.end()) 
    os << " |      0   no errors or warnings to report              "
       << "                                                          | \n";
  while (messageEntry != messages.end()) {
    // Message printout.
    string temp = messageEntry->first;
    int len = temp.length();
    temp.insert( len, max(0, 102 - len), ' ');
    os << " | " << setw(6) << messageEntry->second << "   " 
       << temp << " | \n";
    ++messageEntry;
  } 

  // Done. 
  os << " |                                                       "
     << "                                                          | \n"
     << " *-------  End PYTHIA Error and Warning Messages Statistics"
     << "  ------------------------------------------------------* " 
     << endl;

}

//==========================================================================

} // end namespace Pythia8

