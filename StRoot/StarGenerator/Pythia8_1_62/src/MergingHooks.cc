// MergingHooks.cc is a part of the PYTHIA event generator.
// Copyright (C) 2012 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This file is written by Stefan Prestel.
// Function definitions (not found in the header) for the HardProcess and
// MergingHooks classes.

#include "MergingHooks.h"

namespace Pythia8 {
 
//==========================================================================

// The HardProcess class.

//--------------------------------------------------------------------------

// Declaration of hard process class
// This class holds information on the desired hard 2->2 process to be merged
// This class is a container class for History class use.

// Initialisation on the process string

void HardProcess::initOnProcess( string process, ParticleData* particleData) {
  state.init("(hard process)", particleData);
  translateProcessString(process);
}

//--------------------------------------------------------------------------

// Initialisation on the path to LHE file

void HardProcess::initOnLHEF( string LHEfile, ParticleData* particleData) {
  state.init("(hard process)", particleData);
  translateLHEFString(LHEfile);
}

//--------------------------------------------------------------------------

// Function to access the LHE file and read relevant information.
// The merging scale will be read from the +1 jet sample, called
//   LHEpath_1.lhe
// while the hard process will be read from
//   LHEpath_0.lhe
// Currently, only read from MadEvent- or Sherpa-generated LHE files
// is automatic, else the user is asked to supply the necessary
// information.

void HardProcess::translateLHEFString( string LHEpath){

  // Open path to LHEF and extract merging scale
  ifstream infile;
  infile.open( (char*)( LHEpath +"_0.lhe").c_str());

  // Check with ME generator has been used
  int iLine =0;
  int nLinesMax = 200;
  string lineGenerator;
  while( iLine < nLinesMax
        && lineGenerator.find("SHERPA", 0) == string::npos
        && lineGenerator.find("MadGraph", 0) == string::npos){
    iLine++;
    lineGenerator = " ";
    getline(infile,lineGenerator);
  }
  infile.close();

  vector <int> incom;
  vector <int> inter;
  vector <int> outgo;
  // Particle identifiers, ordered in such a way that e.g. the "u"
  // in a mu is not mistaken for an u quark
  int inParticleNumbers[] = {
        // Leptons
        -11,11,-12,12,-13,13,-14,14,-15,15,-16,16,
        // Jet container
        2212,2212,0,0,0,0,
        // Quarks
        -1,1,-2,2,-3,3,-4,4,-5,5,-6,6};

  string inParticleNamesSH[] = {
        // Leptons
        "-11","11","-12","12","-13","13","-14","14","-15","15","-16","16",
        // Jet container
        "-93","93","-90","90","-91","91",
        // Quarks
        "-1","1","-2","2","-3","3","-4","4","-5","5","-6","6"};
  string inParticleNamesMG[] =  {
        // Leptons
        "e+","e-","ve~","ve","mu+","mu-","vm~","vm","ta+","ta-","vt~","vt",
        // Jet container
        "p~","p","l+","l-","vl~","vl",
        // Quarks
        "d~","d","u~","u","s~","s","c~","c","b~","b","t~","t"};

  // Declare intermediate particle identifiers
  int interParticleNumbers[] = {
         // Electroweak gauge bosons
         22,23,-24,24,
         // Top quarks
         -6,6,
         // Dummy index as back-up
         0,
         // All squarks
        -1000001,1000001,-1000002,1000002,-1000003,1000003,-1000004,1000004,
        -1000005,1000005,-1000006,1000006,-2000001,2000001,-2000002,2000002,
        -2000003,2000003,-2000004,2000004,-2000005,2000005,-2000006,2000006};
  // Declare names of intermediate particles
  string interParticleNamesMG[] = {
        // Electroweak gauge bosons
        "a","z","w-","w+",
         // Top quarks
         "t~","t",
        // Dummy index as back-up
        "xx",
         // All squarks
         "dl~","dl","ul~","ul","sl~","sl","cl~","cl","b1~","b1","t1~","t1",
         "dr~","dr","ur~","ur","sr~","sr","cr~","cr","b2~","b2","t2~","t2"}; 

  // Declare final state particle identifiers
  int outParticleNumbers[] = {
        // Leptons
        -11,11,-12,12,-13,13,-14,14,-15,15,-16,16,
        // Jet container
        2212,2212,0,0,0,0,
        // Quarks
        -1,1,-2,2,-3,3,-4,4,-5,5,-6,6,
        // Neutralino in SUSY
        1000022,
        // All squarks
        -1000001,1000001,-1000002,1000002,-1000003,1000003,-1000004,1000004,
        -1000005,1000005,-1000006,1000006,-2000001,2000001,-2000002,2000002,
        -2000003,2000003,-2000004,2000004,-2000005,2000005,-2000006,2000006};
  // Declare names of final state particles
  string outParticleNamesMG[] =  {
        // Leptons
        "e+","e-","ve~","ve","mu+","mu-","vm~","vm","ta+","ta-","vt~","vt",
        // Jet container
        "j~","j","l+","l-","vl~","vl",
        // Quarks
        "d~","d","u~","u","s~","s","c~","c","b~","b","t~","t",
        // Neutralino in SUSY
        "n1",
        // All squarks
        "dl~","dl","ul~","ul","sl~","sl","cl~","cl","b1~","b1","t1~","t1",
        "dr~","dr","ur~","ur","sr~","sr","cr~","cr","b2~","b2","t2~","t2"};

  string outParticleNamesSH[] = {
        // Leptons
        "-11","11","-12","12","-13","13","-14","14","-15","15","-16","16",
        // Jet container
        "-93","93","-90","90","-91","91",
        // Quarks
        "-1","1","-2","2","-3","3","-4","4","-5","5","-6","6",
        // Neutralino in SUSY
        "1000022",
        // All squarks
        "-1000001","1000001","-1000002","1000002","-1000003","1000003",
                   "-1000004","1000004",
        "-1000005","1000005","-1000006","1000006","-2000001","2000001",
                   "-2000002","2000002",
        "-2000003","2000003","-2000004","2000004","-2000005","2000005",
                   "-2000006","2000006"};

  // Declare size of particle name arrays
  int nIn   = 30;
  int nInt  = 31;
  int nOut  = 55;

  // Save type of the generator, in order to be able to extract
  // the tms definition
  meGenType = (lineGenerator.find("MadGraph", 0) != string::npos) ? -1
              : (lineGenerator.find("SHERPA", 0) != string::npos) ? -2 : 0;

  if(meGenType == -2){
    // Now read merging scale
    // Open path to LHEF and extract merging scale
    infile.open( (char*)( LHEpath +"_1.lhe").c_str());
    string lineTMS;
    while(lineTMS.find("NJetFinder ", 0) == string::npos){
      lineTMS = " ";
      getline(infile,lineTMS);
    }
    infile.close();
    lineTMS = lineTMS.substr(0,lineTMS.find(" 0.0 ",0));
    lineTMS = lineTMS.substr(lineTMS.find(" ", 0)+3,lineTMS.size());
    // Remove whitespaces
    while(lineTMS.find(" ", 0) != string::npos)
      lineTMS.erase(lineTMS.begin()+lineTMS.find(" ",0));
    // Replace d with e
    if( lineTMS.find("d", 0) != string::npos)
      lineTMS.replace(lineTMS.find("d", 0),1,1,'e');
    tms = atof((char*)lineTMS.c_str());

    // Now read hard process
    // Open path to LHEF and extract hard process
    infile.open( (char*)( LHEpath +"_0.lhe").c_str());
    string line;
    while(line.find("Process", 0) == string::npos){
      line = " ";
      getline(infile,line);
    }
    infile.close();
    line = line.substr(line.find(" ",0),line.size());

    // Cut string into incoming and outgoing pieces 
    vector <string> pieces;
    pieces.push_back( line.substr(0,line.find("->", 0)) );
    // Do not count additional final jets
    int end = (line.find("{", 0) != string::npos) ? line.find("{", 0)-2
            : line.size();
    pieces.push_back( line.substr(line.find("->", 0)+2, end) );

    // Get incoming particles
    for(int i=0; i < nIn; ++i) {
      for(int n = pieces[0].find(inParticleNamesSH[i], 0);
             n != int(string::npos);
             n = pieces[0].find(inParticleNamesSH[i], n)) {
        incom.push_back(inParticleNumbers[i]);
        pieces[0].erase(pieces[0].begin()+n,
                        pieces[0].begin()+n+inParticleNamesSH[i].size());
        n=0;
      }
    }
    // Get intermediate particles
    // If intermediates are still empty, fill intermediate with default value
    inter.push_back(0);
    // Get final particles
    for(int i=0; i < nOut; ++i) {
      for(int n = pieces[1].find(outParticleNamesSH[i], 0);
             n != int(string::npos);
             n = pieces[1].find(outParticleNamesSH[i], n)) {
        outgo.push_back(outParticleNumbers[i]);
        pieces[1].erase(pieces[1].begin()+n,
                        pieces[1].begin()+n+outParticleNamesSH[i].size());
        n=0;
      }
    }

  } else if(meGenType == -1){
    // Now read merging scale
    // Open path to LHEF and extract merging scale
    infile.open( (char*)( LHEpath +"_1.lhe").c_str());
    string lineTMS;
    while(lineTMS.find("ktdurham", 0) == string::npos){
      lineTMS = " ";
      getline(infile,lineTMS);
    }
    lineTMS = lineTMS.substr(0,lineTMS.find("=",0));
    infile.close();
    // Remove whitespaces
    while(lineTMS.find(" ", 0) != string::npos)
      lineTMS.erase(lineTMS.begin()+lineTMS.find(" ",0));
    // Replace d with e
    if( lineTMS.find("d", 0) != string::npos)
      lineTMS.replace(lineTMS.find("d", 0),1,1,'e');
    tms = atof((char*)lineTMS.c_str());

    // Now read hard process
    // Open path to LHEF and extract hard process
    infile.open( (char*)( LHEpath +"_0.lhe").c_str());
    string line;
    while(line.find("@1", 0) == string::npos){
      line = " ";
      getline(infile,line);
    }
    infile.close();
    line = line.substr(0,line.find("@",0));

    // Count number of resonances
    int appearances = 0;
    for(int n = line.find("(", 0); n != int(string::npos);
            n = line.find("(", n)) {
      appearances++;
      n++;
    }

    // Cut string in incoming, resonance+decay and outgoing pieces 
    vector <string> pieces;
    for(int i =0; i < appearances;++i) {
      int n = line.find("(", 0);
      pieces.push_back(line.substr(0,n));
      line = line.substr(n+1,line.size());
    }
    // Cut last resonance from rest
    if(appearances > 0) {
      pieces.push_back( line.substr(0,line.find(")",0)) );
      pieces.push_back( line.substr(line.find(")",0)+1,line.size()) );
    }

    // If the string was not cut into pieces, i.e. no resonance was
    // required, cut string using '>' as delimiter
    if(pieces.empty() ){
      appearances = 0;
      for(int n = line.find(">", 0); n != int(string::npos);
              n = line.find(">", n)) {
        appearances++;
        n++;
      }

      // Cut string in incoming and outgoing pieces 
      for(int i =0; i < appearances;++i) {
        int n = line.find(">", 0);
        pieces.push_back(line.substr(0,n));
        line = line.substr(n+1,line.size());
      }

      if(appearances == 1) pieces.push_back(line);
      if(appearances > 1) {
        pieces.push_back( line.substr(0,line.find(">",0)) );
        pieces.push_back( line.substr(line.find(">",0)+1,line.size()) );
      }
    }

    // Get incoming particles
    for(int i=0; i < nIn; ++i) {
      for(int n = pieces[0].find(inParticleNamesMG[i], 0);
             n != int(string::npos);
             n = pieces[0].find(inParticleNamesMG[i], n)) {
        incom.push_back(inParticleNumbers[i]);
        pieces[0].erase(pieces[0].begin()+n,
                        pieces[0].begin()+n+inParticleNamesMG[i].size());
        n=0;
      }
    }

    // Check intermediate resonances and decay products
    for(int i =1; i < int(pieces.size()); ++i){
      // Seperate strings into intermediate and outgoing, if not already done
      int k = pieces[i].find(">", 0);

      string intermediate = (pieces[i].find(">", 0) != string::npos) ? 
                             pieces[i].substr(0,k) : "";
      string outgoing = (pieces[i].find(">", 0) != string::npos) ? 
                         pieces[i].substr(k+1,pieces[i].size()) : pieces[i];

      // Get intermediate particles
      for(int j=0; j < nInt; ++j) {
        for(int n = intermediate.find(interParticleNamesMG[j], 0);
               n != int(string::npos);
               n = intermediate.find(interParticleNamesMG[j], n)) {
          inter.push_back(interParticleNumbers[j]);
          intermediate.erase(intermediate.begin()+n,
                      intermediate.begin()+n+interParticleNamesMG[j].size());
          n=0;
        }
      }

      // Get outgoing particles
      for(int j=0; j < nOut; ++j) {
        for(int n = outgoing.find(outParticleNamesMG[j], 0);
               n != int(string::npos);
               n = outgoing.find(outParticleNamesMG[j], n)) {
          outgo.push_back(outParticleNumbers[j]);
          outgoing.erase(outgoing.begin()+n,
                         outgoing.begin()+n+outParticleNamesMG[j].size());
          n=0;
        }
      }

      // For arbitrary or non-existing intermediate, remember zero for each 
      // two outgoing particles
      if(inter.empty()) {
        int nZeros = outgo.size()/2;
        for(int l=0; l < nZeros; ++l)
          inter.push_back(0);
      }

    }

  } else {

    cout << "Reading of tms and hard process information from LHEF currently"
         << " only automated for MadEvent- or SHERPA-produced LHEF" << endl;
    int tempInt      = 0;
    cout << "Use default process pp -> e+ve + jets? (0:no / 1:yes): ";
    cin >> tempInt;
    cout << endl;

    if(tempInt == 0){
      tempInt = 0;
      double tempDouble  = 0.0;
      cout << "Please specify merging scale (kT Durham, in GeV): ";
      cin >> tempDouble;
      tms = tempDouble;
      meGenType = -1;
      cout << endl;
      cout << "Please specify first incoming particle ";
      cout << "(p+/p- = 2212, e- = 11, e+ = -11): ";
      cin >> tempInt;
      incom.push_back(tempInt);
      tempInt = 0;
      cout << endl;
      cout << "Please specify second incoming particle ";
      cout << "(p+/p- = 2212, e- = 11, e+ = -11): ";
      cin >> tempInt;
      incom.push_back(tempInt);
      tempInt = 0;
      cout << endl;
      cout << "Please specify intermediate particle, if any ";
      cout << "(0 for none, else PDG code): ";
      cin >> tempInt;
      inter.push_back(tempInt);
      cout << endl;
      do {
        tempInt = 0;
        cout << "Please specify outgoing particle ";
        cout << "(jet=2212, else PDG code, exit with 99): ";
        cin >> tempInt;
        if(tempInt != 99) outgo.push_back(tempInt);
      } while(tempInt != 99);
      cout << endl;
    } else {
      cout << "LHE file not produced by SHERPA or MG/ME - ";
      cout << "Using default process and tms" << endl;
      incom.push_back(2212);
      incom.push_back(2212);
      inter.push_back(24);
      outgo.push_back(-11);
      outgo.push_back(12);
      tms = 10.;
      meGenType = -1;
    }
  }

  // Now store incoming, intermediate and outgoing
  // Set intermediate tags
  for(int i=0; i < int(inter.size()); ++i)
    hardIntermediate.push_back(inter[i]);

  // Set the incoming particle tags
  if(incom.size() != 2)
    cout << "Only two incoming particles allowed" << endl;
  else {
    hardIncoming1 = incom[0];
    hardIncoming2 = incom[1];
  }

  // Set final particle identifiers
  if(outgo.size()%2 == 1) {
    cout << "Only even number of outgoing particles allowed" << endl;
    for(int i=0; i < int(outgo.size()); ++i)
      cout << outgo[i] << endl;
  } else {

    // Push back particles / antiparticles
    for(int i=0; i < int(outgo.size()); ++i)
      if(outgo[i] > 0
        && outgo[i] != 2212
        && outgo[i] != 1000022)
        hardOutgoing2.push_back( outgo[i]);
      else if (outgo[i] < 0)
        hardOutgoing1.push_back( outgo[i]);

    // Push back jets, distribute evenly amongst particles / antiparticles
    // Push back majorana particles, distribute evenly
    int iNow = 0;
    for(int i=0; i < int(outgo.size()); ++i)
      if( (outgo[i] == 2212
        || outgo[i] == 1000022)
        && iNow%2 == 0 ){
        hardOutgoing2.push_back( outgo[i]);
        iNow++;
      } else if( (outgo[i] == 2212
               || outgo[i] == 1000022)
               && iNow%2 == 1 ){
        hardOutgoing1.push_back( outgo[i]);
        iNow++;
      }
  }

  // Done
}

//--------------------------------------------------------------------------

// Function to translate a string specitying the core process into the
// internal notation
// Currently, the input string has to be in MadEvent notation

void HardProcess::translateProcessString( string process){

  vector <int> incom;
  vector <int> inter;
  vector <int> outgo;
  // Particle identifiers, ordered in such a way that e.g. the "u"
  // in a mu is not mistaken for an u quark
  int inParticleNumbers[] = {
        // Leptons
        -11,11,-12,12,-13,13,-14,14,-15,15,-16,16,
        // Jet container
        2212,2212,0,0,0,0,
        // Quarks
        -1,1,-2,2,-3,3,-4,4,-5,5,-6,6};
  string inParticleNamesMG[] =  {
        // Leptons
        "e+","e-","ve~","ve","mu+","mu-","vm~","vm","ta+","ta-","vt~","vt",
        // Jet container
        "p~","p","l+","l-","vl~","vl",
        // Quarks
        "d~","d","u~","u","s~","s","c~","c","b~","b","t~","t"};

  // Declare intermediate particle identifiers
  int interParticleNumbers[] = {
         // Electroweak gauge bosons
         22,23,-24,24,
         // Top quarks
         -6,6,
         // Dummy index as back-up
         0,
         // All squarks
        -1000001,1000001,-1000002,1000002,-1000003,1000003,-1000004,1000004,
        -1000005,1000005,-1000006,1000006,-2000001,2000001,-2000002,2000002,
        -2000003,2000003,-2000004,2000004,-2000005,2000005,-2000006,2000006};
  // Declare names of intermediate particles
  string interParticleNamesMG[] = {
        // Electroweak gauge bosons
        "a","z","w-","w+",
         // Top quarks
         "t~","t",
        // Dummy index as back-up
        "xx",
         // All squarks
         "dl~","dl","ul~","ul","sl~","sl","cl~","cl","b1~","b1","t1~","t1",
         "dr~","dr","ur~","ur","sr~","sr","cr~","cr","b2~","b2","t2~","t2"}; 

  // Declare final state particle identifiers
  int outParticleNumbers[] = {
        // Leptons
        -11,11,-12,12,-13,13,-14,14,-15,15,-16,16,
        // Jet container
        2212,2212,0,0,0,0,
        // Quarks
        -1,1,-2,2,-3,3,-4,4,-5,5,-6,6,
        // Neutralino in SUSY
        1000022,
        // All squarks
        -1000001,1000001,-1000002,1000002,-1000003,1000003,-1000004,1000004,
        -1000005,1000005,-1000006,1000006,-2000001,2000001,-2000002,2000002,
        -2000003,2000003,-2000004,2000004,-2000005,2000005,-2000006,2000006};
  // Declare names of final state particles
  string outParticleNamesMG[] =  {
        // Leptons
        "e+","e-","ve~","ve","mu+","mu-","vm~","vm","ta+","ta-","vt~","vt",
        // Jet container
        "j~","j","l+","l-","vl~","vl",
        // Quarks
        "d~","d","u~","u","s~","s","c~","c","b~","b","t~","t",
        // Neutralino in SUSY
        "n1",
        // All squarks
        "dl~","dl","ul~","ul","sl~","sl","cl~","cl","b1~","b1","t1~","t1",
        "dr~","dr","ur~","ur","sr~","sr","cr~","cr","b2~","b2","t2~","t2"};

  // Declare size of particle name arrays
  int nIn   = 30;
  int nInt  = 31;
  int nOut  = 55;

  string line = process;

  // Count number of resonances
  int appearances = 0;
  for(int n = line.find("(", 0); n != int(string::npos);
          n = line.find("(", n)) {
    appearances++;
    n++;
  }

  // Cut string in incoming, resonance+decay and outgoing pieces 
  vector <string> pieces;
  for(int i =0; i < appearances;++i) {
    int n = line.find("(", 0);
    pieces.push_back(line.substr(0,n));
    line = line.substr(n+1,line.size());
  }
  // Cut last resonance from rest
  if(appearances > 0) {
    pieces.push_back( line.substr(0,line.find(")",0)) );
    pieces.push_back( line.substr(line.find(")",0)+1,line.size()) );
  }

  // If the string was not cut into pieces, i.e. no resonance was
  // required, cut string using '>' as delimiter
  if(pieces.empty() ){
    appearances = 0;
    for(int n = line.find(">", 0); n != int(string::npos);
            n = line.find(">", n)) {
      appearances++;
      n++;
    }

    // Cut string in incoming and outgoing pieces 
    for(int i =0; i < appearances;++i) {
      int n = line.find(">", 0);
      pieces.push_back(line.substr(0,n));
      line = line.substr(n+1,line.size());
    }

    if(appearances == 1) pieces.push_back(line);
    if(appearances > 1) {
      pieces.push_back( line.substr(0,line.find(">",0)) );
      pieces.push_back( line.substr(line.find(">",0)+1,line.size()) );
    }
  }

  // Get incoming particles
  for(int i=0; i < nIn; ++i) {
    for(int n = pieces[0].find(inParticleNamesMG[i], 0);
           n != int(string::npos);
           n = pieces[0].find(inParticleNamesMG[i], n)) {
      incom.push_back(inParticleNumbers[i]);
      pieces[0].erase(pieces[0].begin()+n,
                      pieces[0].begin()+n+inParticleNamesMG[i].size());
      n=0;
    }
  }

  // Check intermediate resonances and decay products
  for(int i =1; i < int(pieces.size()); ++i){
    // Seperate strings into intermediate and outgoing, if not already done
    int k = pieces[i].find(">", 0);

    string intermediate = (pieces[i].find(">", 0) != string::npos) ? 
                           pieces[i].substr(0,k) : "";
    string outgoing = (pieces[i].find(">", 0) != string::npos) ? 
                       pieces[i].substr(k+1,pieces[i].size()) : pieces[i];

    // Get intermediate particles
    for(int j=0; j < nInt; ++j) {
      for(int n = intermediate.find(interParticleNamesMG[j], 0);
             n != int(string::npos);
             n = intermediate.find(interParticleNamesMG[j], n)) {
        inter.push_back(interParticleNumbers[j]);
        intermediate.erase(intermediate.begin()+n,
                    intermediate.begin()+n+interParticleNamesMG[j].size());
        n=0;
      }
    }

    // Get outgoing particles
    for(int j=0; j < nOut; ++j) {
      for(int n = outgoing.find(outParticleNamesMG[j], 0);
             n != int(string::npos);
             n = outgoing.find(outParticleNamesMG[j], n)) {
        outgo.push_back(outParticleNumbers[j]);
        outgoing.erase(outgoing.begin()+n,
                       outgoing.begin()+n+outParticleNamesMG[j].size());
        n=0;
      }
    }

    // For arbitrary or non-existing intermediate, remember zero for each 
    // two outgoing particles
    if(inter.empty()) {
      int nZeros = outgo.size()/2;
      for(int l=0; l < nZeros; ++l)
        inter.push_back(0);
    }

  }

  // Now store incoming, intermediate and outgoing
  // Set intermediate tags
  for(int i=0; i < int(inter.size()); ++i)
    hardIntermediate.push_back(inter[i]);

  // Set the incoming particle tags
  if(incom.size() != 2)
    cout << "Only two incoming particles allowed" << endl;
  else {
    hardIncoming1 = incom[0];
    hardIncoming2 = incom[1];
  }

  // Set final particle identifiers
  if(outgo.size()%2 == 1) {
    cout << "Only even number of outgoing particles allowed" << endl;  
    for(int i=0; i < int(outgo.size()); ++i)
      cout << outgo[i] << endl;
  } else {

    // Push back particles / antiparticles
    for(int i=0; i < int(outgo.size()); ++i)
      if(outgo[i] > 0
        && outgo[i] != 2212
        && outgo[i] != 1000022)
        hardOutgoing2.push_back( outgo[i]);
      else if (outgo[i] < 0)
        hardOutgoing1.push_back( outgo[i]);

    // Push back jets, distribute evenly among particles / antiparticles
    // Push back majorana particles, distribute evenly
    int iNow = 0;
    for(int i=0; i < int(outgo.size()); ++i)
      if( (outgo[i] == 2212
        || outgo[i] == 1000022)
        && iNow%2 == 0 ){
        hardOutgoing2.push_back( outgo[i]);
        iNow++;
      } else if( (outgo[i] == 2212
               || outgo[i] == 1000022)
               && iNow%2 == 1 ){
        hardOutgoing1.push_back( outgo[i]);
        iNow++;
      }
  }

  // Done
}

//--------------------------------------------------------------------------

// Function to identify the hard subprocess in the current event

void HardProcess::storeCandidates( const Event& event){

  // Store the reference event
  state.clear();
  state = event;

  // Local copy of intermediate bosons
  vector<int> intermediates;
  for(int i =0; i < int(hardIntermediate.size());++i)
    intermediates.push_back( hardIntermediate[i]);

  // Local copy of outpoing partons
  vector<int> outgoing1;
  for(int i =0; i < int(hardOutgoing1.size());++i)
    outgoing1.push_back( hardOutgoing1[i]);
  vector<int> outgoing2;
  for(int i =0; i < int(hardOutgoing2.size());++i)
    outgoing2.push_back( hardOutgoing2[i]);

  // Clear positions of intermediate and outgoing particles
  PosIntermediate.resize(0);
  PosOutgoing1.resize(0);
  PosOutgoing2.resize(0);
  for(int i =0; i < int(hardIntermediate.size());++i)
    PosIntermediate.push_back(0);
  for(int i =0; i < int(hardOutgoing1.size());++i)
    PosOutgoing1.push_back(0);
  for(int i =0; i < int(hardOutgoing2.size());++i)
    PosOutgoing2.push_back(0);

  // If the hard process only consists of jets, and no intermediate
  // particle is required, then no candidates need to be fixed, since
  // all clusterings to the 2 -> 2 process should be allowed.
  // Check if hard process contains only jets
  bool hasOnlyJets = true;
  for(int i =0; i < int(hardOutgoing1.size());++i)
    if(hardOutgoing1[i] != 2212)
      hasOnlyJets = false;
  for(int i =0; i < int(hardOutgoing2.size());++i)
    if(hardOutgoing2[i] != 2212)
      hasOnlyJets = false;

  // If the hard process contains only of non-specified jets,
  // do not store any candidates, as to not discrimintate
  // clusterings
  if(hasOnlyJets){
    for(int i =0; i < int(hardOutgoing1.size());++i)
      PosOutgoing1[i] = 0;
    for(int i =0; i < int(hardOutgoing2.size());++i)
      PosOutgoing2[i] = 0;
    // Done
    return;
  }

  // Initialise vector of particles that were already identified as
  // hard process particles
  vector<int> iPosChecked;

  // First try to find particles coupled to intermediate bosons
  for(int i=0; i < int(intermediates.size()); ++i){

    // Do nothing if the intermediate boson is absent
    if(intermediates[i] == 0) continue;

    // Loop through event
    for(int j=0; j < int(event.size()); ++j) {
      // If the particle has a requested intermediate id, check if
      // daughters are hard process particles
      if(event[j].id() == intermediates[i]) {
        // If this particle is a potential intermediate
        PosIntermediate[i] = j;
        intermediates[i] = 0;
        // If id's of daughters are good, store position
        int iPos1 = event[j].daughter1();
        int iPos2 = event[j].daughter2();

        // Loop through daughters to check if these contain some hard
        // outgoing particles
        for( int k=iPos1; k <= iPos2; ++k){
          int id = event[k].id();

          // Check if daughter is hard outgoing particle
          for(int l=0; l < int(outgoing2.size()); ++l)
            if( outgoing2[l] != 99 ){
                // Found particle id
              if(id == outgoing2[l]
                // Found jet
                || (id > 0 && abs(id) < 10 && outgoing2[l] == 2212) ){
                // Store position
                PosOutgoing2[l] = k;
                // Remove the matched particle from the list
                outgoing2[l] = 99;
                iPosChecked.push_back(k);
                break;
              }
            }
 
          // Check if daughter is hard outgoing antiparticle
          for(int l=0; l < int(outgoing1.size()); ++l)
            if( outgoing1[l] != 99 ){
                // Found particle id
              if(id == outgoing1[l]
                // Found jet
                || (id < 0 && abs(id) < 10 && outgoing1[l] == 2212) ){
                // Store position
                PosOutgoing1[l] = k;
                // Remove the matched particle from the list
                outgoing1[l] = 99;
                iPosChecked.push_back(k);
                break;
            }
          }

        } // End loop through daughters
      } // End if ids match
    } // End loop through event
  } // End loop though requested intermediates 

  // If all outgoing particles were found, done
  bool done = true;
  for(int i=0; i < int(outgoing1.size()); ++i)
    if(outgoing1[i] != 99)
      done = false;
  for(int i=0; i < int(outgoing2.size()); ++i)
    if(outgoing2[i] != 99)
      done = false;
  // Return 
  if(done) return;

  // Leptons not associated with resonance are allowed.
  // Try to find all unmatched hard process leptons.
  // Loop through event to find outgoing lepton
  for(int i=0; i < int(event.size()); ++i){
    // Skip non-final particles and final partons
    if( !event[i].isFinal() || event[i].colType() != 0)
      continue;
    // Skip all particles that have already been identified
    bool skip = false;
    for(int k=0; k < int(iPosChecked.size()); ++k){
      if(i == iPosChecked[k])
        skip = true;
    }
    if(skip) continue;

    // Check if any hard outgoing leptons remain
    for(int j=0; j < int(outgoing2.size()); ++j){
      // Do nothing if this particle has already be found,
      // or if this particle is a jet or quark
      if(  outgoing2[j] == 99
        || outgoing2[j] == 2212
        || abs(outgoing2[j]) < 10)
        continue;

      // If the particle matches an outgoing lepton, save it
      if(event[i].id() == outgoing2[j]){
        PosOutgoing2[j] = i;
        outgoing2[j] = 99;
        iPosChecked.push_back(i);
      }
    }

    // Check if any hard outgoing antileptons remain
    for(int j=0; j < int(outgoing1.size()); ++j){
      // Do nothing if this particle has already be found,
      // or if this particle is a jet or quark
      if(  outgoing1[j] == 99
        || outgoing1[j] == 2212
        || abs(outgoing1[j]) < 10)
        continue;

      // If the particle matches an outgoing lepton, save it
      if(event[i].id() == outgoing1[j]){
        PosOutgoing1[j] = i;
        outgoing1[j] = 99;
        iPosChecked.push_back(i);
      }
    }
  }

  // It sometimes happens that MadEvent does not put a
  // heavy coloured resonance into the LHE file, even if requested.
  // This means that the decay products of this resonance need to be
  // found separately.
  // Loop through event to find hard process (anti)quarks
  for(int i=0; i < int(event.size()); ++i){

    // Skip non-final particles and final partons
    if( !event[i].isFinal() || event[i].colType() == 0)
      continue;

    // Skip all particles that have already been identified
    bool skip = false;
    for(int k=0; k < int(iPosChecked.size()); ++k){
      if(i == iPosChecked[k])
        skip = true;
    }
    if(skip) continue;

    // Check if any hard outgoing leptons remain
    for(int j=0; j < int(outgoing2.size()); ++j){
      // Do nothing if this particle has already be found,
      // or if this particle is a jet or quark
      if(  outgoing2[j] == 99
        || outgoing2[j] == 2212
        || abs(outgoing2[j]) > 10)
        continue;
      // If the particle matches an outgoing quarks, save it
      if(event[i].id() == outgoing2[j]){
        // Save parton
        PosOutgoing2[j] = i;
        // remove entry form lists
        outgoing2[j] = 99;
        iPosChecked.push_back(i);
      }
    }

    // Check if any hard outgoing antiquarks remain
    for(int j=0; j < int(outgoing1.size()); ++j){
      // Do nothing if this particle has already be found,
      // or if this particle is a jet or quark
      if(  outgoing1[j] == 99
        || outgoing1[j] == 2212
        || abs(outgoing1[j]) > 10)
        continue;
      // If the particle matches an outgoing lepton, save it
      if(event[i].id() == outgoing1[j]){
        // Save parton
        PosOutgoing1[j] = i;
        // Remove parton from list
        outgoing1[j] = 99;
        iPosChecked.push_back(i);
      }
    }
  }

  // Done
}

//--------------------------------------------------------------------------

// Function to check if the particle event[iPos] matches any of
// the stored outgoing particles of the hard subprocess

bool HardProcess::matchesAnyOutgoing(int iPos, const Event& event){

  // Match quantum numbers of any first outgoing candidate
  bool matchQN1 = false;
  // Match quantum numbers of any second outgoing candidate
  bool matchQN2 = false;
  // Match parton in the hard process,
  // or parton from decay of electroweak boson in hard process,
  // or parton from decay of electroweak boson from decay of top
  bool matchHP = false;

  // Check outgoing candidates
  for(int i=0; i < int(PosOutgoing1.size()); ++i)
    // Compare particle properties
    if( event[iPos].id()         == state[PosOutgoing1[i]].id()
     && event[iPos].colType()    == state[PosOutgoing1[i]].colType() 
     && event[iPos].chargeType() == state[PosOutgoing1[i]].chargeType() 
     && event[iPos].col()        == state[PosOutgoing1[i]].col() 
     && event[iPos].acol()       == state[PosOutgoing1[i]].acol()
     && event[iPos].charge()     == state[PosOutgoing1[i]].charge() )
      matchQN1 = true;

  // Check outgoing candidates
  for(int i=0; i < int(PosOutgoing2.size()); ++i)
    // Compare particle properties
    if( event[iPos].id()         == state[PosOutgoing2[i]].id()
     && event[iPos].colType()    == state[PosOutgoing2[i]].colType() 
     && event[iPos].chargeType() == state[PosOutgoing2[i]].chargeType() 
     && event[iPos].col()        == state[PosOutgoing2[i]].col() 
     && event[iPos].acol()       == state[PosOutgoing2[i]].acol()
     && event[iPos].charge()     == state[PosOutgoing2[i]].charge() )
      matchQN2 = true;

  // Check if maps to hard process:
  // Check that particle is in hard process
  if( event[iPos].mother1()*event[iPos].mother2() == 12
      // Or particle has taken recoil from first splitting
      || (  event[iPos].status() == 44
         && event[event[iPos].mother1()].mother1()
           *event[event[iPos].mother1()].mother2() == 12 )
      // Or particle has on-shell resonace as mother
      || (  event[iPos].status() == 23
         && event[event[iPos].mother1()].mother1()
           *event[event[iPos].mother1()].mother2() == 12 )
      // Or particle has on-shell resonace as mother,
      // which again has and on-shell resonance as mother
      || (  event[iPos].status() == 23
         && event[event[iPos].mother1()].status() == -22
         && event[event[event[iPos].mother1()].mother1()].status() == -22
         && event[event[event[iPos].mother1()].mother1()].mother1()
           *event[event[event[iPos].mother1()].mother1()].mother2() == 12 ) )
      matchHP = true;

  // Done
  return matchHP && ( matchQN1 || matchQN2 );

}


//--------------------------------------------------------------------------

// Function to return the type of the ME generator

int HardProcess::MEgenType(){ return meGenType;}

//--------------------------------------------------------------------------

// Function to get the number of coloured final state partons in the
// hard process

int HardProcess::nQuarksOut(){
  int nFin =0;
  for(int i =0; i< int(hardOutgoing1.size()); ++i){
    if(hardOutgoing1[i] == 2212 || abs(hardOutgoing1[i]) < 10) nFin++;
    if(hardOutgoing2[i] == 2212 || abs(hardOutgoing2[i]) < 10) nFin++;
  }
  return nFin;
}

//--------------------------------------------------------------------------

// Function to get the number of uncoloured final state particles in the
// hard process

int HardProcess::nLeptonOut(){
  int nFin =0;
  for(int i =0; i< int(hardOutgoing1.size()); ++i){
    if(abs(hardOutgoing1[i]) > 10 && abs(hardOutgoing1[i]) < 20) nFin++;
    if(abs(hardOutgoing2[i]) > 10 && abs(hardOutgoing2[i]) < 20) nFin++;
    // Bookkeep MSSM neutralinos as leptons
    if(abs(hardOutgoing1[i]) == 1000022) nFin++;
    if(abs(hardOutgoing2[i]) == 1000022) nFin++;
  }
  return nFin;
}

//--------------------------------------------------------------------------

// Function to get the number of coloured initial state partons in the
// hard process

int HardProcess::nQuarksIn(){
  int nIn =0;
  if(hardIncoming1 == 2212 || abs(hardIncoming1) < 10) nIn++;
  if(hardIncoming2 == 2212 || abs(hardIncoming2) < 10) nIn++;
  return nIn;
}

//--------------------------------------------------------------------------

// Function to get the number of uncoloured initial state particles in the
// hard process

int HardProcess::nLeptonIn(){
  int nIn =0;
  if(abs(hardIncoming1) > 10 && abs(hardIncoming1) < 20) nIn++;
  if(abs(hardIncoming2) > 10 && abs(hardIncoming2) < 20) nIn++;
  return nIn;
}

//--------------------------------------------------------------------------

// Function to report if a resonace decay was found in the
// 2->2 hard sub-process in the current state

int HardProcess::hasResInCurrent(){
  for(int i =0; i< int(PosIntermediate.size()); ++i)
    if(PosIntermediate[i] == 0) return 0;
  return 1;
}

//--------------------------------------------------------------------------

// Function to report the number of resonace decays in the 2->2 sub-process 
// of the  current state

int HardProcess::nResInCurrent(){
  int nRes = 0;
  for(int i =0; i< int(PosIntermediate.size()); ++i){
    if(PosIntermediate[i] != 0) nRes++;
  }
  return nRes;
}

//--------------------------------------------------------------------------

// Function to report if a resonace decay was found in the
// 2->2 hard core process

bool HardProcess::hasResInProc(){
  for(int i =0; i< int(hardIntermediate.size()); ++i)
    if(hardIntermediate[i] == 0) return false;
  return true;
}

//--------------------------------------------------------------------------

// Function to print the hard process (for debug)

void HardProcess::list() const {
  cout << "   Hard Process: ";
  cout << " \t " << hardIncoming1 << " + " << hardIncoming2;
  cout << " \t -----> \t ";
  for(int i =0; i < int(hardIntermediate.size());++i)
    cout << hardIntermediate[i] << " "; 
  cout << " \t -----> \t ";
  for(int i =0; i < int(hardOutgoing1.size());++i) {
    cout << hardOutgoing1[i] << " ";
    cout << hardOutgoing2[i] << " ";
  }
  cout << endl;
}

//--------------------------------------------------------------------------

// Function to list the hard process candiates in the matrix element state
// (for debug)

void HardProcess::listCandidates() const {
  cout << "   Hard Process candidates: ";
  cout << " \t " << hardIncoming1 << " + " << hardIncoming2;
  cout << " \t -----> \t ";
  for(int i =0; i < int(PosIntermediate.size());++i)
    cout << PosIntermediate[i] << " "; 
  cout << " \t -----> \t ";
  for(int i =0; i < int(PosOutgoing1.size());++i) {
    cout << PosOutgoing1[i] << " ";
    cout << PosOutgoing2[i] << " ";
  }
  cout << endl;
}

//--------------------------------------------------------------------------

// Function to clear hard process information

void HardProcess::clear() {

  // Clear flavour of the first incoming particle
  hardIncoming1 = hardIncoming2 = 0;
  // Clear outgoing particles
  hardOutgoing1.resize(0);
  hardOutgoing2.resize(0);
  // Clear intermediate bosons in the hard 2->2
  hardIntermediate.resize(0);

  // Clear reference event
  state.clear();

  // Clear potential positions of outgoing particles in reference event
  PosOutgoing1.resize(0);
  PosOutgoing2.resize(0);
  // Clear potential positions of intermediate bosons in reference event
  PosIntermediate.resize(0);

  // Clear merging scale read from LHE file
  tms = 0.;
  // Clear type of ME generator
  meGenType = 0;


}

//==========================================================================

// The MergingHooks class.

//--------------------------------------------------------------------------

// Initialise MergingHooks class

void MergingHooks::init( Settings& settings, Info* infoPtrIn, 
  ParticleData* particleDataPtrIn, ostream& os){

  // Save pointers
  infoPtr          = infoPtrIn;
  particleDataPtr  = particleDataPtrIn;

  // Initialise AlphaS objects for reweighting
  double alphaSvalueFSR = settings.parm("TimeShower:alphaSvalue");
  int    alphaSorderFSR = settings.mode("TimeShower:alphaSorder");
  AlphaS_FSRSave.init(alphaSvalueFSR,alphaSorderFSR);
  double alphaSvalueISR = settings.parm("SpaceShower:alphaSvalue");
  int    alphaSorderISR = settings.mode("SpaceShower:alphaSorder");
  AlphaS_ISRSave.init(alphaSvalueISR,alphaSorderISR);

  // Initialise merging switches
  doUserMergingSave     =  settings.flag("Merging:doUserMerging");

  // Initialise automated MadGraph kT merging
  doMGMergingSave       =  settings.flag("Merging:doMGMerging");

  // Initialise kT merging
  doKTMergingSave       =  settings.flag("Merging:doKTMerging");

  // Initialise exact definition of kT
  ktTypeSave            =  settings.mode("Merging:ktType");

  // Get core process from user input
  processSave = settings.word("Merging:Process");

  // Clear hard process
  hardProcess.clear();

  // Initialise the hard process
  if(doUserMergingSave || doKTMergingSave)
    hardProcess.initOnProcess(processSave, particleDataPtr);
  else
    hardProcess.initOnLHEF(lheInputFile, particleDataPtr);
   
  // Parameters for reconstruction of evolution scales
  includeMassiveSave    = settings.flag("Merging:includeMassive");
  enforceStrongOrderingSave = settings.flag("Merging:enforceStrongOrdering");
  scaleSeparationFactorSave = settings.parm("Merging:scaleSeparationFactor");
  orderInRapiditySave   = settings.flag("Merging:orderInRapidity");

  // Parameters for choosing history probabilistically
  nonJoinedNormSave     = settings.parm("Merging:nonJoinedNorm");
  fsrInRecNormSave      = settings.parm("Merging:fsrInRecNorm");
  pickByFullPSave       = settings.flag("Merging:pickByFullP");
  pickByPoPT2Save       = settings.flag("Merging:pickByPoPT2");
  includeRedundantSave  = settings.flag("Merging:includeRedundant");

  // Parameters for scale choices
  unorderedScalePrescipSave   =
    settings.mode("Merging:unorderedScalePrescrip");
  unorderedASscalePrescipSave =
    settings.mode("Merging:unorderedASscalePrescrip");
  incompleteScalePrescipSave  =
    settings.mode("Merging:incompleteScalePrescrip");

  // Parameter for allowing swapping of one colour index while reclustering
  allowColourShufflingSave  =
    settings.flag("Merging:allowColourShuffling");

  // Parameters for choosing history by sum(|pT|)
  pickBySumPTSave       = settings.flag("Merging:pickBySumPT");
  herwigAcollFSRSave    = settings.parm("Merging:aCollFSR");
  herwigAcollISRSave    = settings.parm("Merging:aCollISR");

  // Information on the shower cut-off scale
  pT0ISRSave            = settings.parm("SpaceShower:pT0Ref");
  pTcutSave             = settings.parm("SpaceShower:pTmin");
  pTcutSave             = max(pTcutSave,pT0ISRSave);

  // Initialise CKKWL weight
  weightSave = 1.;

  // Save merging scale on maximal number of jets
  // (Note: Not used at the moment)
  if(doKTMergingSave) {
    tmsValueSave = settings.parm("Merging:TMS");
    nJetMaxSave  = settings.mode("Merging:nJetMax");
  } else if (doMGMergingSave) {
    tmsValueSave = hardProcess.tms;
    nJetMaxSave  = settings.mode("Merging:nJetMax");
  } else {
    tmsValueSave = settings.parm("Merging:TMS");
    nJetMaxSave  = settings.mode("Merging:nJetMax");
  }

  // Write banner
  os << "\n"
     << " *---------- MEPS Merging Initialization  -----------------------*"
     << "\n"
     << " | We merge  " << processSave << "  with up to  " << nJetMaxSave
     << " additional jets \n";
  if(doKTMergingSave)
    os << " | Merging scale is defined in kT with the value ktMS = "
       << tmsValueSave << " GeV";
  else if(doMGMergingSave)
    os << " | Perform automanted MG/ME merging \n"
       << " | Merging scale is defined in kT with the value ktMS = "
       << tmsValueSave << " GeV";
  else
    os << " | Merging scale is defined by the user, with the value tMS = "
       << tmsValueSave;
  os << "\n *---------- END MEPS Merging Initialization  -------------------*"
     << "\n\n";

}

//--------------------------------------------------------------------------

// Function to return the number of clustering steps for the current event

int MergingHooks::getNumberOfClusteringSteps(const Event& event){

  // Count the number of final state partons
  int nFinalPartons = 0;
  for( int i=0; i < event.size(); ++i)
    if( event[i].isFinal() && event[i].isParton())
      nFinalPartons++;

  // Add tops to number of partons
  for( int i=0; i < event.size(); ++i)
    if( event[i].isFinal() && event[i].idAbs() == 6)
      nFinalPartons++;

  // Count the number of final state leptons
  int nFinalLeptons = 0;
  for( int i=0; i < event.size(); ++i)
    if( event[i].isFinal() && event[i].isLepton())
      nFinalLeptons++;

  // Add neutralinos to number of leptons
  for( int i=0; i < event.size(); ++i)
    if( event[i].isFinal()
      && event[i].idAbs() == 1000022)
      nFinalLeptons++;

  // Count the number of final state electroweak bosons
  int nFinalBosons = 0;
  for( int i=0; i < event.size(); ++i)
    if( event[i].isFinal()
      && ( event[i].idAbs() == 24
        || event[i].idAbs() == 23 ) )
      nFinalBosons++;

  // Save sum of all final state particles
  int nFinal = nFinalPartons + nFinalLeptons + 2*nFinalBosons;

  // Return the difference to the core process outgoing particles
  return (nFinal - nHardOutPartons() - nHardOutLeptons());

}

//--------------------------------------------------------------------------

double MergingHooks::kTms(const Event& event) {

  // Cut only on QCD partons!
  // Count particle types
  int nFinalColoured = 0;
  int nFinalNow =0;
  for( int i=0; i < event.size(); ++i) {
    if(event[i].isFinal()){
      if(event[i].id() != 23 && abs(event[i].id()) != 24)
        nFinalNow++;
      if( event[i].colType() != 0)
        nFinalColoured++;
    }
  }

  // Use MergingHooks in-built functions to get information on the hard
  // process
  int nLeptons = nHardOutLeptons();
  int nQuarks  = nHardOutPartons();
  int nResNow  = nResInCurrent();

  // Check if photons, electrons etc. have been produced. If so, do not veto
  if(nFinalNow - ( (nLeptons+nQuarks)/2 - nResNow)*2 != nFinalColoured){
    // Sometimes, Pythia detaches the decay products even though no
    // resonance was put into the LHE file, to catch this, add another
    // if statement
    if(nFinalNow != nFinalColoured) return 0.;
  }

  // Check that one parton has been produced. If not (e.g. in MPI), do not veto
  int nMPI = infoPtr->nMPI();
  if(nMPI > 1) return 0.;

  // Declare kT algorithm parameters
  double Dparam = 0.4;
  // Declare final parton vector
  vector <int> FinalPartPos;
  FinalPartPos.clear();
  // Search event record for final state partons
  for (int i=0; i < event.size(); ++i)
    if(event[i].isFinal() && event[i].colType() != 0)
      FinalPartPos.push_back(i);

  // Find minimal Durham kT in event, using own function: Check
  // definition of separation
  int type = (event[3].colType() == 0
           && event[4].colType() == 0) ? -1 : ktTypeSave;
  // Find minimal kT
  double ktmin = event[0].e();
  for(int i=0; i < int(FinalPartPos.size()); ++i){
    double kt12  = ktmin;
    // Compute separation to the beam axis for hadronic collisions
    if(type == 1 || type == 2) {
      double temp = event[FinalPartPos[i]].pT();
      kt12 = min(kt12, temp);
    }
    // Compute separation to other final state jets
    for(int j=i+1; j < int(FinalPartPos.size()); ++j) {
      double temp = kTdurham( event[FinalPartPos[i]], event[FinalPartPos[j]],
                      type, Dparam);
      kt12 = min(kt12, temp);
    }
    // Keep the minimal Durham separation
    ktmin = min(ktmin,kt12);
  }

  // Return minimal Durham kT
  return ktmin;

}

//--------------------------------------------------------------------------

// Function to compute durham y separation from Particle input

double MergingHooks::kTdurham(const Particle& RadAfterBranch,
  const Particle& EmtAfterBranch, int Type, double D ){

  // Declare return variable
  double ktdur;
  // Save 4-momenta of final state particles
  Vec4 jet1 = RadAfterBranch.p();
  Vec4 jet2 = EmtAfterBranch.p();

  if( Type == -1) {
    // Get angle between jets for e+e- collisions, make sure that
    // -1 <= cos(theta) <= 1
    double costh;
    if (jet1.pAbs()*jet2.pAbs() <=0.) costh = 1.;
    else {
      costh = costheta(jet1,jet2);
    }
    // Calculate kt durham separation between jets for e+e- collisions
    ktdur = 2.0*min( pow(jet1.e(),2) , (pow(jet2.e(),2)) )*(1.0 - costh);
  } else if( Type == 1 ){
    // Get delta_y for hadronic collisions:
    // Get mT of first jet
    double mT1sq = jet1.m2Calc() + jet1.pT2();
    double mT1 = 0.;
    if(mT1sq < 0) mT1 = - sqrt(-mT1sq);
    else mT1 = sqrt(mT1sq);
    // Get mT of second jet
    double mT2sq = jet2.m2Calc() + jet2.pT2();
    double mT2 = 0.;
    if(mT2sq < 0) mT2 = - sqrt(-mT2sq);
    else mT2 = sqrt(mT2sq);
    // Get rapidity of first jet
    double y1 = log( ( jet1.e() + abs(jet1.pz()) ) / mT1 );
    if(jet1.pz() < 0) y1 *= -1.;
    // Get rapidity of second jet
    double y2 = log( ( jet2.e() + abs(jet2.pz()) ) / mT2 );
    if(jet2.pz() < 0) y2 *= -1.;
    // Get delta_phi for hadronic collisions
    double pt1 = sqrt( pow(jet1.px(),2) + pow(jet1.py(),2) );
    double pt2 = sqrt( pow(jet2.px(),2) + pow(jet2.py(),2) );  
    double cosdPhi = ( jet1.px()*jet2.px() + jet1.py()*jet2.py() ) / (pt1*pt2);
    double dPhi = acos( cosdPhi );
    // Calculate kT durham like fastjet,
    // but with rapidity instead of pseudo-rapidity
    ktdur = min( pow(pt1,2),pow(pt2,2) )
          * ( pow(y1-y2,2) + pow(dPhi,2) ) / pow(D,2);
  } else if( Type == 2 ){
    // Get delta_eta for hadronic collisions
    double eta1 = 0.5*log( (jet1.e() + jet1.pz()) / (jet1.e() - jet1.pz()) );
    double eta2 = 0.5*log( (jet2.e() + jet2.pz()) / (jet2.e() - jet2.pz()) );
    // Get delta_phi and cos(Delta_phi) for hadronic collisions
    double pt1 = sqrt( pow(jet1.px(),2) + pow(jet1.py(),2) );
    double pt2 = sqrt( pow(jet2.px(),2) + pow(jet2.py(),2) );  
    double cosdPhi = ( jet1.px()*jet2.px() + jet1.py()*jet2.py() ) / (pt1*pt2);
    double dPhi = acos( cosdPhi );
    // Calculate kT durham like fastjet
    ktdur = min( pow(pt1,2),pow(pt2,2) )
          * ( pow(eta1-eta2,2) + pow(dPhi,2) ) / pow(D,2);
  } else if( Type == 3 ){
    // Get cosh(Delta_eta) for hadronic collisions
    double eta1 = 0.5*log( (jet1.e() + jet1.pz()) / (jet1.e() - jet1.pz()) );
    double eta2 = 0.5*log( (jet2.e() + jet2.pz()) / (jet2.e() - jet2.pz()) );
    double coshdEta = cosh( eta1 - eta2 );
    // Get delta_phi and cos(Delta_phi) for hadronic collisions
    double pt1 = sqrt( pow(jet1.px(),2) + pow(jet1.py(),2) );
    double pt2 = sqrt( pow(jet2.px(),2) + pow(jet2.py(),2) );  
    double cosdPhi = ( jet1.px()*jet2.px() + jet1.py()*jet2.py() ) / (pt1*pt2);
    // Calculate kT durham separation "SHERPA-like"
    ktdur = 2.0*min( pow(pt1,2),pow(pt2,2) )
          * ( coshdEta - cosdPhi ) / pow(D,2);
  } else {
    ktdur = 0.0;
  }
  // Return kT
  return sqrt(ktdur);
}

//--------------------------------------------------------------------------

// Function to set the path to the LHE file, so that more automated merging
// can be used

void MergingHooks::setLHEInputFile( string lheFile){
  // Remove "_1.lhe" suffix from LHE file name.
  // This is done so that the HarsProcess class can access both the +0 and +1
  // LHE files to find both the merging scale and the core process string
  // Store.
  lheInputFile = lheFile.substr(0,lheFile.size()-6);
}

//==========================================================================

} // end namespace Pythia8
