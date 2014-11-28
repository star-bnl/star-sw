#ifndef FRANKS1HISTO__HH
#define FRANKS1HISTO__HH


#ifndef __CINT__
#include <Stiostream.h>
#include <stdio>
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
  int mEntries;

public:
  // constructor and deconstructor
  franks1Histo(const char* c1, const char* c2, 
	       int bins, T xmin, T xmax);  
  ~franks1Histo();  

  // member functions
#ifndef ST_NO_MEMBER_TEMPLATES
  template<class X, class Y>  void Add( franks1Histo<X>* , franks1Histo<X>* ,    Y w1=1., Y w2=1., const char* c="");
  template<class X, class Y>  void Divide( franks1Histo<X>* , franks1Histo<X>* , Y w1=1., Y w2=1., const char* c="");
  template<class X>           void Fill( X value);
  template<class X, class Y>  void Fill( X value, Y weight);
  template<class X>           int  GetBin(X value);
  template<class X>           void Scale(X scale);
#endif
  // methods without template arguments
  void Draw(const char* c="");
  void SetDirectory(int dummy) { /* no-op */};
  void Sumw2() {/* no-op */};
  int  GetNbinsX() { return mBins; }
  T    GetBinContent(int bin) { return vec[bin]; }
  T    GetBinCenter(int bin);
  T    GetMaximum();
  T    GetMinimum();
  T    GetMean();
  T    GetRMS();
  T    GetEntries();
  T    Integral();
  void Reset(const char* c="");
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
inline void franks1Histo<T>::Draw(const char* c) {
  cout << c << " " << mC1 << " " << endl;
  T min=GetMinimum();
  T max=GetMaximum();   
  T step = (max-min)/50.;
  cout << " minimum=" << min << " maximum=" << max << " step=" << step << endl;
  for (int i=0; i < mBins; i++) { 
    printf(" (%3i) %+e %+e ",i, GetBinCenter(i), vec[i]);
    for ( int j=0; j < floor( (vec[i]-min)/step ); j++) {
      cout << "*";
    }
    cout << endl;
  }
};
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
// *************************************************************************************************
template<class T>
template<class X>
inline int franks1Histo<T>::GetBin(X value) {
  int bin  = (int) floor( (value-mXmin)/mStep );
  if( !(bin >=0 && bin < mBins) ) bin=-1;
  return bin;
}
// *************************************************************************************************
template<class T>
inline T franks1Histo<T>::GetBinCenter(int bin) {
  double center=0;
  if ( bin >=0 && bin < mBins)
    center= mXmin + (0.5+bin)*mStep;
  return center;
}
// *************************************************************************************************
template<class T>
inline T franks1Histo<T>::GetMean() {
  T mean=0;
  for (int i=0; i< mBins; i++)
    mean+=vec[i]*GetBinCenter(i);
  mean/=mBins;
  return mean;
}
// *************************************************************************************************
template<class T>
inline T franks1Histo<T>::GetMaximum() {
  T max=vec[0];
  for (int i=0; i< mBins; i++) {
    if (vec[i] > max) 
      max=vec[i];
  }
  return max;
}
// *************************************************************************************************
template<class T>
inline T franks1Histo<T>::GetMinimum() {
  T min=vec[0];
  for (int i=0; i< mBins; i++) {
    if (vec[i] < min) 
      min=vec[i];
  }
  return min;
}
// *************************************************************************************************
template<class T>
inline T franks1Histo<T>::GetRMS() {
  T mean = GetMean();
  for (int i=0; i< mBins; i++)
    mean+=vec[i]*GetBinCenter(i);
  mean/=mBins;
  return mean;
}
// *************************************************************************************************
template<class T>
inline void franks1Histo<T>::Reset(const char*) {
  for (int i=0; i < mBins; i++) 
    vec[i] = 0;
}
// *************************************************************************************************
template<class T>
template<class X>
inline void franks1Histo<T>::Scale(X scale) {
  for (int i=0; i < mBins; i++) 
    vec[i] *=scale;
}
// *************************************************************************************************
template<class T>
inline T franks1Histo<T>::Integral() {
  T Integral=0;
  for (int i=0; i < mBins; i++) { 
    Integral+=vec[i];
    //cout << i << " " << vec[i] << " " << Integral << endl;
  }
  return Integral;
}
#endif

#endif // __CINT__

#endif // FRANKS2DHISTO_HH
