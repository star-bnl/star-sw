#ifndef FRANKS1HISTOD__HH
#define FRANKS1HISTOD__HH

#include <iostream.h>
#include <stdio.h>
#include <math.h>

#ifdef __ROOT__
  #include "TObject.h"
#endif

class franks1HistoD 
#ifdef __ROOT__
  : public TObject 
#endif
{
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
  void   Add( franks1HistoD*, franks1HistoD*, double w1=1., double w2=2., const char* c="");
  void   Divide( franks1HistoD*, franks1HistoD*, double w1=1., double w2=2., const char* c="");
  void   Fill( double value);
  void   Fill( double value, double weight);
  void   Scale(double scale);

  void   Draw();
  void   Draw(const char*);
  int    GetBin(double x);
  double GetBinContent(int bin) { return vec[bin]; }
  double GetBinCenter(int bin);
  int    GetNbinsX() { return mBins; }
  double GetMean();
  double GetMaximum();
  double GetMinimum();
  double GetRMS();
  void   Reset(const char* c="");
  void   SetDirectory(int dummy) { /* no-op */};
  void   Sumw2() {/* no-op */};
  double Integral();
  double GetEntries() { return mEntries; }
#ifdef __ROOT__
  ClassDef(franks1HistoD,1)
#endif
};

#endif // FRANKS2DHISTO_HH
