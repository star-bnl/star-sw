#ifndef Solaris

#ifndef FRANKS2HISTO__HH
#define FRANKS2HISTO__HH

#ifndef __CINT__
#include <Stiostream.h>
#include <math.h>
#ifdef GNU_GCC
#   include <stddef.h>
#endif
#endif

//#ifndef ST_NO_MEMBER_TEMPLATES
//#define ST_NO_MEMBER_TEMPLATES
//#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
template<class T>
#else
template<class T = double>
#endif

class franks2Histo {
private:
  // title
  char* mC1;
  char* mC2; 
  // X
  int    mXbins;
  T mXmin;
  T mXmax;
  T mXstep;
  int mXpos;
  // Y
  int    mYbins;
  T mYmin;
  T mYmax;
  T mYstep;
  T *vec; 
  int mYpos;
  // X*Y
  int mBins;
  //
  int mPos;
  int mEntries;

public:
  // constructor and deconstructor
  franks2Histo(const char* c1, const char* c2, 
		int bins1, T xmin1, T xmax1,  
		int bins2, T xmin2, T xmax2);  
  ~franks2Histo();  

  // member functions
#ifndef ST_NO_MEMBER_TEMPLATES
  template<class X, class Y, class Z>  void Add( franks2Histo<X>* , franks2Histo<X>* ,    Y w1=1., Z w2=1., const char* c="");
  template<class X, class Y, class Z>  void Divide( franks2Histo<X>* , franks2Histo<X>* , Y w1=1., Z w2=1., const char* c="");
  template<class X, class Y, class Z>  void Fill( X value1, Y value2, Z weight=1);
  template<class X>           int  GetBin(X value);
  template<class X>           void Scale(X scale);
#endif
  // methods without template arguments
  void   Draw(const char* c="");
  T GetBinContent(int bin) { return vec[bin]; }
  T GetBinCenter(int bin);
  int    GetNbinsX() { return mBins; }
  T GetMean();
  T GetMaximum();
  T GetMinimum();
  T GetRMS();
  void   Reset(const char* c="");
  void   SetDirectory(int dummy) { /* no-op */};
  void   Sumw2() {/* no-op */};
  T Integral();
  T GetEntries() { return mEntries; }
};
#endif // FRANKS2DHISTO_HH

#endif // Solaris
