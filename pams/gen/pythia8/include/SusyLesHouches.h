// SusyLesHouches.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Peter Skands, Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

#ifndef SLHA_H
#define SLHA_H

// Stdlib header files for string and character manipulation.
#include <string>
#include <cctype>
// Stdlib header files for containers.
#include <vector>
#include <map>
// Stdlib header files for input/output.
#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
// Stdlib header files for mathematics.
#include <cmath>

// Stdlib namespace
using namespace std;

class SusyLesHouches {

public:

  //Constructor, with and without filename.
  SusyLesHouches(int verboseIn=1) : verbose(verboseIn), 
    headerPrinted(false), footerPrinted(false),
    slhaRead(false), lhefRead(false), lhefSlha(false) {};
  SusyLesHouches(string filename, int verboseIn=1) : verbose(verboseIn), 
    headerPrinted(false), footerPrinted(false),
    slhaRead(true), lhefRead(false), lhefSlha(false) {readFile(filename);};

  //***************************** SLHA FILE I/O *****************************//
  int readFile(string slhaFile="slha.spc");     // read in SLHA file 
  //int writeFile(string filename): write SLHA file on filename
  int checkSpectrum();

  //Output utilities
  void printHeader();   // print Header
  void printFooter();   // print Footer
  void printSpectrum(); // print Spectrum
  
  // Class for SLHA data entry
  class Entry {
    
  public:
    //Constructor. 
    Entry() : isIntP(false), isDoubleP(false), 
      isStringP(false), n(0), d(0.0), s(""), commentP("") {}
    
    // Generic functions to inquire whether an int, double, or string
    bool isInt(){return isIntP;}
    bool isDouble(){return isDoubleP;}
    bool isString(){return isStringP;}

    // = Overloading: Set entry to int, double, or string
    Entry& operator=(double& val)  {
      d=val;isIntP=false;isDoubleP=true;isStringP=false;
      return *this;      
    };
    Entry& operator=(int& val)  {
      n=val;isIntP=true;isDoubleP=false;isStringP=false;
      return *this;
    };
    Entry& operator=(string& val)  {
      s=val;isIntP=false;isDoubleP=false;isStringP=true;
      return *this;
    };
    
    // Set and Get comment
    void setComment(string comment) {commentP=comment;}
    void getComment(string comment) {comment=commentP;}

    // Generic functions to get value
    bool get(int& val) {val=n; return isIntP;}
    bool get(double& val) {val=d; return isDoubleP;}
    bool get(string& val) {val=s; return isStringP;}

  private:
    bool isIntP, isDoubleP, isStringP;    
    int n;
    double d;
    string s;
    string commentP;
    
  };

  //***************************** SLHA CLASSES *****************************//


  //class block: the generic SLHA block (see below for matrices)
  //Explicit typing required, e.g. block<double> minpar;
  template <class T> class block {    

  public: 

    //Constructor. 
    block<T>() : idnow(0) { } ;    

    //Does block exist?
    bool exists() { return entry.size() == 0 ? false : true ; };
    //Clear block
    void clear() { entry.clear(); };

    //set: set block entry values.
    //Possible return values from set:
    // 0: normal return. Entry did not previously exist and has been created.
    // 1: normal return. Entry did previously exist and has been overwritten.
    //-1: failure. 
    int set(int iIn,T valIn) { 
      int alreadyexisting=exists(iIn)?1:0;
      entry[iIn]=valIn; 
      return alreadyexisting;
    };
    // Read index and value from SLHA data line
    int set(istringstream& linestream) {
      linestream >> i >> val;
      return linestream ? set(i,val) : -1;
    };
    // With i already given, read value from remaining SLHA data line
    int set(int iIn,istringstream& linestream) {
      linestream >> val;
      return linestream ? set(iIn,val) : -1;
    };
    // Shorthand for entry[0]. Used e.g. for block ALPHA.
    void set(T valIn) { entry[0]=valIn; };

    // Does entry i already exist in this block?
    bool exists(int iIn) {return entry.find(iIn) != entry.end() 
      ? true : false;};

    // Indexing with (). Output only.
    T operator()(int iIn=0) {
      if (exists(iIn)) {return entry[iIn];} else {T dummy(0); return dummy;};
    };

    // Size of map
    int size() {return entry.size();};

    // First and next key code
    int first() { idnow = entry.begin()->first; return idnow; };
    int next() { 
      typename map<int,T>::iterator itnow;
      itnow = ++entry.find(idnow);
      if ( itnow == entry.end() ) itnow=entry.begin();
      return idnow = itnow->first;
    };

    // Simple print utility
    void print() {
      bool finished=false;
      int ibegin=first();
      i=ibegin;
      while (!finished) {
	cout << "  "<< i << " " << entry[i] <<endl;
	i=next();
	if (i == ibegin) finished=true;
      };       
    };

    // Special for DRbar running blocks.
    void setq(double qIn) { qDRbar=qIn; }
    double q() { return qDRbar; }
 
  private:
    map<int,T> entry;    
    int idnow;
    double qDRbar;
    //Auxiliary vars
    int i; 
    T val;
  };

  // class matrixblock: the generic SLHA matrix 
  // Explicit sizing required, e.g. matrixblock<4> nmix;
  template <int size> class matrixblock {    
  public: 
    //Constructor. Set uninitialized and explicitly zero.
    matrixblock<size>() { 
      initialized=false; 
      for (i=1;i<=size;i++) {
	for (j=1;j<=size;j++) {
	  entry[i][j]=0.0;
	};
      };
    };    

    // Assignment
    matrixblock& operator=(const matrixblock& m) { 
      if (this != &m) { 
	for (i=0;i<size;i++) for (j=0;j<=size;j++) entry[i][j] = m(i,j);
	qDRbar = m.qDRbar; 
	initialized = m.initialized; 
      } 
      return *this; };

    // Does this matrix contain any entries?
    bool exists() { return initialized; };
    // Clear initialized flag
    void clear() { initialized=false; };

    // Set matrix entry
    int set(int iIn,int jIn, double valIn) { 
      if (iIn>0 && jIn>0 && iIn<=size && jIn<=size) {
	entry[iIn][jIn]=valIn;
	initialized=true;
	return 0;
      } else {
	return -1;
      };
    };

    // Set entry from linestream (used during file read)
    int set(istringstream& linestream) {
      linestream >> i >> j >> val;
      return linestream ? set(i,j,val) : -1;
    };

    // () Overloading: Get entry
    double operator()(int iIn, int jIn) const {
      return (iIn <= size && jIn <= size && iIn > 0 && jIn > 0) ? 
	entry[iIn][jIn] : 0.0;
    };

    // Set and get scale for DRbar running blocks.
    void setq(double qIn) { qDRbar=qIn; }
    double q() { return qDRbar; }

    // Simple print utility, to be elaborated on.
    void print() {
      for (i=1;i<=size;i++) {
	cout << "   "<<i << " " ;
	for (j=1;j<=size;j++) cout << entry[i][j] << " ";
	cout << endl;
      };
    };

  private:
    bool initialized;
    double entry[size+1][size+1];
    double qDRbar;
    //Auxiliary vars
    int i,j; 
    double val;
  };

  // class tensorblock: the generic SLHA tensor
  // Explicit sizing required, e.g. tensorblock<3> rvlam;
  template <int size> class tensor3block {    
  public: 
    //Constructor. Set uninitialized and explicitly zero.
    tensor3block<size>() { 
      initialized=false; 
      for (i=1;i<=size;i++) {
	for (j=1;j<=size;j++) {
	  for (k=1;k<=size;k++) {
	    entry[i][j][k]=0.0;
	  };
	};
      };
    };    
    
    // Assignment
    tensor3block& operator=(const tensor3block& m) { 
      if (this != &m) { 
	for (i=0;i<size;i++) for (j=0;j<=size;j++) for (k=0;k<=size;k++) 
	  entry[i][j][k] = m(i,j,k);
	qDRbar = m.qDRbar; 
	initialized = m.initialized; 
      } 
      return *this; };
    
    // Does this matrix contain any entries?
    bool exists() { return initialized; };
    // Clear initialized flag
    void clear() { initialized=false; };
    
    // Set matrix entry
    int set(int iIn,int jIn, int kIn, double valIn) { 
      if (iIn>0 && jIn>0 && kIn>0 && iIn<=size && jIn<=size && kIn<=size) {
	entry[iIn][jIn][kIn]=valIn;
	initialized=true;
	return 0;
      } else {
	return -1;
      };
    };

    // Set entry from linestream (used during file read)
    int set(istringstream& linestream) {
      linestream >> i >> j >> k >> val;
      return linestream ? set(i,j,k,val) : -1;
    };

    // () Overloading: Get entry
    double operator()(int iIn, int jIn, int kIn) const {
      return (iIn <= size && jIn <= size && kIn <= size && iIn > 0 
	&& jIn > 0 && kIn > 0) ? entry[iIn][jIn][kIn] : 0.0;
    };

    // Set and get scale for DRbar running blocks.
    void setq(double qIn) { qDRbar=qIn; }
    double q() { return qDRbar; }

    // Simple print utility, to be elaborated on.
    void print() {
      for (i=1;i<=size;i++) {	
	for (j=1;j<=size;j++) {
	  cout << "   "<<i << " "<<j << " " ;
	  for (k=1;k<=size;k++) {
	    cout << entry[i][j][k] << " ";	   
	    cout << endl; 
	  };
	};
      };
    };

  private:
    bool initialized;
    double entry[size+1][size+1][size+1];
    double qDRbar;
    //Auxiliary vars
    int i,j,k; 
    double val;
  };

  //*************************** THE SLHA1 BLOCKS ***************************//
  //blocks for model definition:
  block<int> modsel;
  block<int> modsel21;
  block<double> modsel12;
  block<double> minpar;
  block<double> extpar;
  block<double> sminputs;
  //blocks for RGE program specific output
  block<string> spinfo;
  block<string> spinfo3;
  block<string> spinfo4;
  //blocks for DCY program specific output
  block<string> dcinfo;
  block<string> dcinfo3;
  block<string> dcinfo4;
  //blocks for mass and coupling spectrum
  block<double> mass;
  matrixblock<4> nmix;
  matrixblock<2> umix;
  matrixblock<2> vmix;
  matrixblock<2> stopmix;
  matrixblock<2> sbotmix;
  matrixblock<2> staumix;
  block<double> alpha;
  block<double> hmix;
  block<double> gauge;
  block<double> msoft;
  matrixblock<3> au;
  matrixblock<3> ad;
  matrixblock<3> ae;
  matrixblock<3> yu;
  matrixblock<3> yd;
  matrixblock<3> ye;

  //*************************** THE SLHA2 BLOCKS ***************************//
  //Additions to SLHA1
  block<double> qextpar;  

  //FLV Input
  block<double> vckmin;  // The input CKM Wolfenstein parms.
  block<double> upmnsin; // The input PMNS PDG parms.
  matrixblock<3> msq2in; // The input upper off-diagonal msq2
  matrixblock<3> msu2in; // The input upper off-diagonal msu2
  matrixblock<3> msd2in; // The input upper off-diagonal msd2
  matrixblock<3> msl2in; // The input upper off-diagonal msl2
  matrixblock<3> mse2in; // The input upper off-diagonal mse2
  matrixblock<3> tuin;   // The input upper off-diagonal TU
  matrixblock<3> tdin;   // The input upper off-diagonal TD
  matrixblock<3> tein;   // The input upper off-diagonal TE
  //FLV Output
  matrixblock<3> vckm;    // The output DRbar running Re{VCKM} at Q
  matrixblock<3> upmns;   // The output DRbar running Re{UPMNS} at Q
  matrixblock<3> msq2;    // The output DRbar running msq2 at Q
  matrixblock<3> msu2;    // The output DRbar running msu2 at Q
  matrixblock<3> msd2;    // The output DRbar running msd2 at Q
  matrixblock<3> msl2;    // The output DRbar running msl2 at Q
  matrixblock<3> mse2;    // The output DRbar running mse2 at Q
  matrixblock<3> tu;      // The output DRbar running TU at Q
  matrixblock<3> td;      // The output DRbar running TD at Q
  matrixblock<3> te;      // The output DRbar running TE at Q
  matrixblock<6> usqmix;  // The Re{} up squark mixing matrix
  matrixblock<6> dsqmix;   // The Re{} down squark mixing matrix
  matrixblock<6> selmix;   // The Re{} selectron mixing matrix
  matrixblock<3> snumix;   // The Re{} sneutrino mixing matrix
  matrixblock<3> snsmix;   // The scalar sneutrino mixing matrix
  matrixblock<3> snamix;   // The pseudoscalar neutrino mixing matrix

  //RPV Input
  tensor3block<3> rvlamllein; // The input LNV lambda couplings
  tensor3block<3> rvlamlqdin; // The input LNV lambda' couplings
  tensor3block<3> rvlamuddin; // The input BNV lambda'' couplings
  tensor3block<3> rvtllein;   // The input LNV T couplings
  tensor3block<3> rvtlqdin;   // The input LNV T' couplings
  tensor3block<3> rvtuddin;   // The input BNV T'' couplings
  block<double> rvkappain;    // The input LNV kappa couplings
  block<double> rvdin;        // The input LNV D terms
  block<double> rvm2lh1in;    // The input LNV m2LH1 couplings
  block<double> rvsnvevin;    // The input LNV sneutrino vevs
  //RPV Output
  tensor3block<3> rvlamlle;   // The output LNV lambda couplings
  tensor3block<3> rvlamlqd;   // The output LNV lambda' couplings
  tensor3block<3> rvlamudd;   // The output BNV lambda'' couplings
  tensor3block<3> rvtlle;     // The output LNV T couplings
  tensor3block<3> rvtlqd;     // The output LNV T' couplings
  tensor3block<3> rvtudd;     // The output BNV T'' couplings
  block<double> rvkappa;      // The output LNV kappa couplings
  block<double> rvd;          // The output LNV D terms
  block<double> rvm2lh1;      // The output LNV m2LH1 couplings
  block<double> rvsnvev;      // The output LNV sneutrino vevs
  matrixblock<7> rvnmix;      // The RPV neutralino mixing matrix
  matrixblock<5> rvumix;      // The RPV chargino L mixing matrix
  matrixblock<5> rvvmix;      // The RPV chargino R mixing matrix
  matrixblock<5> rvhmix;      // The RPV neutral scalar mixing matrix
  matrixblock<5> rvamix;      // The RPV neutral pseudoscalar mixing matrix
  matrixblock<7> rvlmix;      // The RPV charged fermion mixing matrix

  //CPV Input
  block<double> imminpar;
  block<double> imextpar;
  //CPV Output
  matrixblock<4> cvhmix;   // The CPV Higgs mixing matrix
  matrixblock<4> imcvhmix; // Optional: imaginary components
  matrixblock<3> imau,imad,imae; // Im{} of AU, AD, AE

  //CPV + FLV Input
  matrixblock<3> immsq2in;  // The Im{} input upper off-diagonal msq2
  matrixblock<3> immsu2in;  // The Im{} input upper off-diagonal msu2
  matrixblock<3> immsd2in;  // The Im{} input upper off-diagonal msd2
  matrixblock<3> immsl2in;  // The Im{} input upper off-diagonal msl2
  matrixblock<3> immse2in;  // The Im{} input upper off-diagonal mse2
  matrixblock<3> imtuin,imtdin,imtein; //  The Im{} input upper off-diagonal T
  //CPV + FLV Output
  matrixblock<3> imvckm;  // The output DRbar running Im{VCKM} at Q
  matrixblock<3> imupmns; // The output DRbar running Im{UPMNS} at Q
  matrixblock<3> immsq2;  // The output DRbar running msq2 at Q
  matrixblock<3> immsu2;  // The output DRbar running msu2 at Q
  matrixblock<3> immsd2;  // The output DRbar running msd2 at Q
  matrixblock<3> immsl2;  // The output DRbar running msl2 at Q
  matrixblock<3> immse2;  // The output DRbar running mse2 at Q
  matrixblock<3> imtu,imtd,imte; // Im{} of TU, TD, TE
  matrixblock<6> imusqmix;// The Im{} up squark mixing matrix
  matrixblock<6> imdsqmix; // The Im{} down squark mixing matrix
  matrixblock<6> imselmix; // The Im{} selectron mixing matrix
  matrixblock<3> imsnumix; // The Im{} sneutrino mixing matrix
  matrixblock<4> imnmix;   // The Im{} neutralino mixing matrix
  matrixblock<4> imumix;   // The Im{} chargino L mixing matrix
  matrixblock<4> imvmix;   // The Im{} chargino R mixing matrix

  //NMSSM Input
  //    All input is in EXTPAR
  //NMSSM Output
  block<double> nmssmrun;  // The block of NMSSM running parameters
  matrixblock<3> nmhmix;   // The NMSSM scalar Higgs mixing
  matrixblock<3> nmamix;   // The NMSSM pseudoscalar Higgs mixing
  matrixblock<5> nmnmix;   // The NMSSM neutralino mixing
  matrixblock<5> imnmnmix; //   Im{} (for future use)

  //*************************** SET BLOCK VALUE ****************************//
  template <class T> int set(string,T);
  template <class T> int set(string,int,T);
  template <class T> int set(string,int,int,T);
  template <class T> int set(string,int,int,int,T);

  //***************************** SLHA PRIVATE *****************************//
private:
  //SLHA I/O
  string spectrumFile;
  void message(int, string,string ,int line=0);
  int verbose;
  bool headerPrinted, footerPrinted;
  bool slhaRead, lhefRead, lhefSlha;

};

#endif


