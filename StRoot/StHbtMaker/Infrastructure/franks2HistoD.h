#ifndef FRANKS2HISTOD__HH
#define FRANKS2HISTOD__HH

#include <iostream.h>
#include <stdlib.h>
#include <math.h>

#ifdef __ROOT__
  #include "TObject.h"
#endif

class franks2HistoD 
#ifdef __ROOT__
  : public TObject 
#endif
{
private:
  // title
  char* mC1;
  char* mC2; 
  // X
  int    mXbins;
  double mXmin;
  double mXmax;
  double mXstep;
  int mXpos;
  // Y
  int    mYbins;
  double mYmin;
  double mYmax;
  double mYstep;
  double *vec; 
  int mYpos;
  // X*Y
  int mBins;
  //
  int mPos;
  int mEntries;

public:
  // constructor and deconstructor
  franks2HistoD(const char* c1, const char* c2, 
		int bins1, double xmin1, double xmax1,  
		int bins2, double xmin2, double xmax2);  
  ~franks2HistoD();  

  // member functions
  void   Add( franks2HistoD*, franks2HistoD*, double w1=1., double w2=2., const char* c="");
  void   Divide( franks2HistoD*, franks2HistoD*, double w1=1., double w2=2., const char* c="");
  void   Fill( double x, double y, double w=1.);
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
  ClassDef(franks2HistoD,1)
#endif
};

#endif // FRANKS2DHISTO_HH
