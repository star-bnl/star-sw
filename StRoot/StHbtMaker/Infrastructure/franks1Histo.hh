#ifndef FRANKS1HISTO__HH
#define FRANKS1HISTO__HH


#ifndef __CINT__
#include <iostream.h>
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

class franks1Histo {
private:
  char* mC1;
  char* mC2; 
  int mBins;
  int mPos;
  T mXmin;
  T mXmax;
  T mStep;
  T *vec; 

public:
  // constructor and deconstructor
  franks1Histo(const char* c1, const char* c2, int bins, T xmin, T xmax);  
  ~franks1Histo();  

  // member functions
#ifndef ST_NO_MEMBER_TEMPLATES
  template<class X, class Y>  void Add( franks1Histo<X>* , franks1Histo<X>* ,    Y w1=1., Y w2=1., const char* c="");
  template<class X, class Y>  void Divide( franks1Histo<X>* , franks1Histo<X>* , Y w1=1., Y w2=1., const char* c="");
  template<class X>           void Fill( X value);
  template<class X, class Y>  void Fill( X value, Y weight);
  template<class X>           void Scale(X scale) { scale = scale*2; }
#else
  void Add( franks1Histo<double>* , franks1Histo<double>* , double, double, const char*);
  void Divide( franks1Histo<double>* , franks1Histo<double>* , double, double, const char*);
  void Fill( double value);
  void Fill( double value, double weight);
  void Scale(double scale) { scale = scale*2; }
#endif
  void Draw();
  void Draw(const char*);
  T    GetBinContent(int bin) { return vec[bin]; }
  int  GetNbinsX() { return mBins; }
  T    GetMean() { return 0; }
  T    GetRMS() { return 0; }
  void Reset()  { /* no-op */};
  void SetDirectory(int dummy) { /* no-op */};
  void Sumw2() {/* no-op */};
  T Integral() { T bla; return bla; }
  T GetEntries() { T bla; return bla; }

};

#ifndef __CINT__

// ***********
// constructor 
// ***********
template<class T>
inline franks1Histo<T>::franks1Histo(const char* c1, const char* c2, int bins, T xmin, T xmax) { 
  mC1 = c1;
  mC2 = c2;
  mBins = bins;
  mXmin = xmin;
  mXmax = xmax;
  mStep = (xmax-xmin)/bins;
  vec = new T[bins];
}
// *************
// deconstructor 
// *************
template<class T>
inline franks1Histo<T>::~franks1Histo() {
 delete vec;
}

// ******************************
// definition of member functions
// ******************************
#ifndef ST_NO_MEMBER_TEMPLATES
// *************************************************************************************************
  template<class T>
  template<class X, class Y>
  inline void franks1Histo<T>::Add( franks1Histo<X>* h1, franks1Histo<X>* h2, Y w1, Y w2, const char* c) {
    for (int i=0; i < mBins; i++) {
      vec[i] = h1->vec[i]*w1 + h2->vec[i]*w2;
    }
}
// *************************************************************************************************
  template<class T>
  template<class X, class Y>
  inline void franks1Histo<T>::Divide( franks1Histo<X>* h1, franks1Histo<X>* h2, Y w1, Y w2, const char* c) {
    for (int i=0; i < mBins; i++) {
      if (h2->vec[i]*w2 !=0 ) 
	vec[i] = h1->vec[i]*w1 / h2->vec[i]*w2;
      else
	vec[i]=0;
    }
  }
// *************************************************************************************************
template<class T>
template<class X>
inline void franks1Histo<T>::Fill( X value) {
  mPos = (int) abs( (value-mXmin)/mStep );
  if ( mPos>=0 && mPos < mBins) 
    vec[mPos]++;
  cout << ".";
}
// *************************************************************************************************
template<class T>
template<class X, class Y>
inline void franks1Histo<T>::Fill( X value, Y weight) {
  mPos = (int) abs( (value-mXmin)/mStep );
  if ( mPos>=0 && mPos < mBins) 
    vec[mPos] = vec[mPos] + weight;
  cout << ".";
}
#else
// *************************************************************************************************
  template<class T>
  inline void franks1Histo<T>::Add( franks1Histo<double>* h1, franks1Histo<double>* h2, double w1, double w2, const char* c) {
    for (int i=0; i < mBins; i++) {
      vec[i] = h1->vec[i]*w1 + h2->vec[i]*w2;
    }
}
// *************************************************************************************************
  template<class T>
  inline void franks1Histo<T>::Divide( franks1Histo<double>* h1, franks1Histo<double>* h2, double w1, double w2, const char* c) {
    for (int i=0; i < mBins; i++) {
      if (h2->vec[i]*w2 !=0 ) 
	vec[i] = h1->vec[i]*w1 / h2->vec[i]*w2;
      else
	vec[i]=0;
    }
}
// *************************************************************************************************
template<class T>
inline void franks1Histo<T>::Fill( double value) {
  mPos = (int) abs( (value-mXmin)/mStep );
  if ( mPos>=0 && mPos < mBins) 
    vec[mPos]++;
  cout << ".";
}
// *************************************************************************************************
template<class T>
inline void franks1Histo<T>::Fill( double value, double weight) {
  mPos = (int) abs( (value-mXmin)/mStep );
  if ( mPos>=0 && mPos < mBins) 
    vec[mPos] = vec[mPos] + weight;
  cout << ".";
}
#endif

// *************************************************************************************************
template<class T>
inline void franks1Histo<T>::Draw() { /* no-op */ };
template<class T>
inline void franks1Histo<T>::Draw(const char* c) { /* no-op */ };


#endif // __CINT__

#endif // FRANKS2DHISTO_HH
