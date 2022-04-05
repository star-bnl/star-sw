#ifndef FRANKS1HISTOD__HH
#define FRANKS1HISTOD__HH

#ifndef __ROOT__

#include <Stiostream.h>
#include <cstdio>
#include <math.h>
#include <stddef.h>

class franks1HistoD {
private:
  char* mC1;
  char* mC2; 
  int mBins;
  int mPos;
  double mXmin;
  double mXmax;
  double mStep;
  double *vec; 
  int mEntries;

public:
  // constructor and deconstructor
  franks1HistoD(const char* c1, const char* c2, int bins, double xmin, double xmax);  
  ~franks1HistoD();  

  // member functions
  void Add( franks1HistoD* , franks1HistoD* ,    double w1=1., double w2=1., const char* c="");
  void Divide( franks1HistoD* , franks1HistoD* , double w1=1., double w2=1., const char* c="");
  void Fill( double value);
  void Fill( double value, double weight);
  int  GetBin(double value);
  void Scale(double scale);
  // methods without template arguments
  void Draw(const char* c="");
  void SetDirectory(int dummy) { /* no-op */};
  void Sumw2() {/* no-op */};
  int  GetNbinsX() { return mBins; }
  double    GetBinContent(int bin) { return vec[bin]; }
  double    GetBinCenter(int bin);
  double    GetMaximum();
  double    GetMinimum();
  double    GetMean();
  double    GetRMS();
  double    GetEntries();
  double    Integral();
  void Reset(const char* c="");
};

#endif // __ROOT__
#endif // FRANKS1HISTOD__HH
