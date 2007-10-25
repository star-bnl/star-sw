//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// March 19, 2006
//

#ifndef Histogram_hh
#define Histogram_hh

#include <iostream>
#include <vector>
#include <string>
using namespace std;

class Histogram {
public:
  Histogram();
  ~Histogram();
  Histogram(const string& name, const string& title, int nbins, float min, float max);

  string name() const;
  string title() const;
  int    numberOfBins() const;
  float  min() const;
  float  max() const;
  int    fill(float x, float w = 1);
  float  binContent(int i) const;
  float  entries() const;
  void   reset();

  //
  // Serialization
  //
  istream& read(istream& in);
  ostream& write(ostream& out);

private:
  string fName;
  string fTitle;
  int    fNbins;
  float  fMin;
  float  fMax;
  float  fEntries;
  vector<float> fBins;
};

inline Histogram::Histogram() {}
inline Histogram::~Histogram() {}
inline string Histogram::name() const { return fName; }
inline string Histogram::title() const { return fTitle; }
inline int Histogram::numberOfBins() const { return fNbins; }
inline float Histogram::min() const { return fMin; }
inline float Histogram::max() const { return fMax; }
inline float Histogram::binContent(int i) const { return fBins[i]; }
inline float Histogram::entries() const { return fEntries; }

#endif
