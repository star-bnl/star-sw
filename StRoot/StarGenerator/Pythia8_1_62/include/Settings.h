// Settings.h is a part of the PYTHIA event generator.
// Copyright (C) 2012 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for the settings database.
// Flag: helper class with bool flags.
// Mode: helper class with int modes.
// Parm: (short for parameter) helper class with double parameters.
// Word: helper class with string words.
// Settings: maps of flags, modes, parms and words with input/output.

#ifndef Pythia8_Settings_H
#define Pythia8_Settings_H

#include "Info.h"
#include "PythiaStdlib.h"

namespace Pythia8 {

//==========================================================================

// Class for bool flags.

class Flag {

public:

  // Constructor
  Flag(string nameIn = " ", bool defaultIn = false) : name(nameIn), 
    valNow(defaultIn) , valDefault(defaultIn) { }

  // Data members.
  string name;
  bool   valNow, valDefault;

};

//==========================================================================

// Class for integer modes.

class Mode {

public:

  // Constructor
  Mode(string nameIn = " ", int defaultIn = 0, bool hasMinIn = false,
    bool hasMaxIn = false, int minIn = 0,  int maxIn = 0) :  name(nameIn), 
    valNow(defaultIn), valDefault(defaultIn), hasMin(hasMinIn),
    hasMax(hasMaxIn), valMin(minIn), valMax(maxIn) { }

  // Data members.
  string name;
  int    valNow, valDefault;
  bool   hasMin, hasMax;
  int    valMin, valMax;

};

//==========================================================================

// Class for double parms (where parm is shorthand for parameter).

class Parm {

public:

  // Constructor
  Parm(string nameIn = " ", double defaultIn = 0., 
    bool hasMinIn = false, bool hasMaxIn = false, double minIn = 0., 
    double maxIn = 0.) :  name(nameIn), valNow(defaultIn), 
    valDefault(defaultIn), hasMin(hasMinIn), hasMax(hasMaxIn), 
    valMin(minIn), valMax(maxIn) { }

  // Data members.
  string name;
  double valNow, valDefault;
  bool   hasMin, hasMax;
  double valMin, valMax;

};

//==========================================================================

// Class for string words.

class Word {

public:

  // Constructor
  Word(string nameIn = " ", string defaultIn = " ") : name(nameIn), 
    valNow(defaultIn) , valDefault(defaultIn) { }

  // Data members.
  string name, valNow, valDefault;

};

//==========================================================================

// This class holds info on flags (bool), modes (int), 
// parms (double) and words (string).

class Settings {

public:

  // Constructor.
  Settings() : isInit(false) {}

  // Initialize Info pointer.
  void initPtr(Info* infoPtrIn) {infoPtr = infoPtrIn;}
 
  // Read in database from specific file.
  bool init(string startFile = "../xmldoc/Index.xml", bool append = false, 
    ostream& os = cout) ;

  // Overwrite existing database by reading from specific file.
  bool reInit(string startFile = "../xmldoc/Index.xml", ostream& os = cout) ;

  // Read in one update from a single line.
  bool readString(string line, bool warn = true, ostream& os = cout) ; 
 
  // Write updates or everything to user-defined file.
  bool writeFile(string toFile, bool writeAll = false) ;
  bool writeFile(ostream& os = cout, bool writeAll = false) ;

  // Print out table of database, either all or only changed ones,
  // or ones containing a given string.
  void listAll(ostream& os = cout) { 
    list( true, false, " ", os); } 
  void listChanged(ostream& os = cout) { 
    list (false, false, " ", os); } 
  void list(string match, ostream& os = cout) { 
    list (false, true, match, os); } 

  // Reset all values to their defaults.
  void resetAll() ;

  // Query existence of an entry.
  bool isFlag(string keyIn) {
    return (flags.find(toLower(keyIn)) != flags.end()); }
  bool isMode(string keyIn) { 
    return (modes.find(toLower(keyIn)) != modes.end()); }
  bool isParm(string keyIn) {
    return (parms.find(toLower(keyIn)) != parms.end()); }
  bool isWord(string keyIn) {
    return (words.find(toLower(keyIn)) != words.end()); }
 
  // Add new entry.
  void addFlag(string keyIn, bool defaultIn) {
    flags[toLower(keyIn)] = Flag(keyIn, defaultIn); }  
  void addMode(string keyIn, int defaultIn, bool hasMinIn, 
    bool hasMaxIn, int minIn, int maxIn) { modes[toLower(keyIn)] 
    = Mode(keyIn, defaultIn, hasMinIn, hasMaxIn, minIn, maxIn); }      
  void addParm(string keyIn, double defaultIn, bool hasMinIn, 
    bool hasMaxIn, double minIn, double maxIn) { parms[toLower(keyIn)] 
    = Parm(keyIn, defaultIn, hasMinIn, hasMaxIn, minIn, maxIn); }  
  void addWord(string keyIn, string defaultIn) {
    words[toLower(keyIn)] = Word(keyIn, defaultIn); }  

  // Give back current value, with check that key exists. 
  bool   flag(string keyIn);
  int    mode(string keyIn);
  double parm(string keyIn);
  string word(string keyIn); 
    
  // Give back a map of all entries whose names match the string "match".
  map<string, Flag> getFlagMap(string match);
  map<string, Mode> getModeMap(string match);
  map<string, Parm> getParmMap(string match);
  map<string, Word> getWordMap(string match);

  // Change current value, respecting limits.
  void flag(string keyIn, bool nowIn); 
  void mode(string keyIn, int nowIn);
  void parm(string keyIn, double nowIn); 
  void word(string keyIn, string nowIn); 

  // Change current value, disregarding limits.
  void forceMode(string keyIn, int nowIn);
  void forceParm(string keyIn, double nowIn);
     
  // Restore current value to default. 
  void resetFlag(string keyIn);
  void resetMode(string keyIn);
  void resetParm(string keyIn);
  void resetWord(string keyIn);

private:

  // Pointer to various information on the generation.
  Info* infoPtr;

  // Map for bool flags.
  map<string, Flag> flags;

  // Map for integer modes.
  map<string, Mode> modes;

  // Map for double parms.
  map<string, Parm> parms;

  // Map for string words.
  map<string, Word> words;

  // Flag that initialization has been performed.
  bool isInit;

  // Print out table of database, called from listAll and listChanged.
  void list(bool doListAll, bool doListString, string match,
    ostream& os = cout); 

  // Initialize tunes to e+e- and pp/ppbar data.
  void initTuneEE(int eeTune);
  void initTunePP(int ppTune);

  // Useful functions for string handling.
  string toLower(const string& name);
  bool   boolString(string tag);
  string attributeValue(string line, string attribute);
  bool   boolAttributeValue(string line, string attribute);
  int    intAttributeValue(string line, string attribute);
  double doubleAttributeValue(string line, string attribute);

};

//==========================================================================

} // end namespace Pythia8

#endif // Pythia8_Settings_H
