// Pythia6Interface.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for the Pythia 6.4 f77 external linkage to C++.
// All required code is contained here, i.e. there is no matching .cc file.

#ifndef Pythia8_Pythia6Interface_H
#define Pythia8_Pythia6Interface_H

namespace Pythia8 {

//**************************************************************************

// Declare the f77 subroutines that may be used.

extern "C" {

  extern void pygive_(const char*, int);

  extern void pyinit_(const char*, const char*, const char*, double&,
    int, int, int);

  extern void pyupin_();

  extern void pyupev_();

  extern void pylist_(int&);

  extern void pystat_(int&);

}

//**************************************************************************

// Interfaces to the above routines, to make the C++ calls similar to f77.

class Pythia6Interface {

public:

  // Give in a command to change a setting.
  static void pygive(const string cmnd) { 
    const char* cstring = cmnd.c_str(); int len = cmnd.length(); 
    pygive_(cstring, len);
  }

  // Initialize the generation for the given beam confiuration.
  static void pyinit(const string frame, const string beam, 
    const string target, double wIn) { 
    const char* cframe = frame.c_str(); int lenframe = frame.length();
    const char* cbeam = beam.c_str(); int lenbeam = beam.length();
    const char* ctarget = target.c_str(); int lentarget = target.length();
    pyinit_(cframe, cbeam, ctarget, wIn, lenframe, lenbeam, lentarget); 
  }
  
  // Fill the initialization information in the HEPRUP commonblock.
  static void pyupin() {pyupin_();}

  // Generate the next hard process and 
  // fill the event information in the HEPEUP commonblock
  static void pyupev() {pyupev_();}

  // List the event at the process level.
  static void pylist(int mode) {pylist_(mode);}

  // Print statistics on the event generation process.
  static void pystat(int mode) {pystat_(mode);}

};

//**************************************************************************


} // end namespace Pythia8

#endif // Pythia8_Pythia6Interface_H
