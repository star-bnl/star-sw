#ifndef Solaris
#include <stdlib.h>

#include "franks2Histo.hh"

// ***********
// constructor 
// ***********
template<class T>
franks2Histo<T>::franks2Histo(const char* c1, const char* c2, 
			     int xbins, T xmin, T xmax, 
			     int ybins, T ymin, T ymax) { 
  mC1 = (char*) c1;
  mC2 = (char*) c2;
  // X
  mXbins = xbins;
  mXmin = xmin;
  mXmax = xmax;
  mXstep = (xmax-xmin)/xbins;
  // Y
  mYbins = ybins;
  mYmin = ymin;
  mYmax = ymax;
  mYstep = (ymax-ymin)/ybins;
  // X*Y
  mBins = xbins*ybins;
  // check 
  if ( (xbins<=0 || xmax <= xmin)  || (ybins<=0 || ymax <= ymin) ) {
    cout << " franks2Histo<T>() can not create histogram with this parameters";
    cout << " xbins=" << xbins;
    cout << " xmin=" << xmin;
    cout << " xmax=" << xmax;
    cout << " ybins=" << ybins;
    cout << " ymin=" << ymin;
    cout << " ymax=" << ymax;
    exit(-1);
  }
  // create
  vec = new T[xbins*ybins];
}
// *************
// deconstructor 
// *************
template<class T>
franks2Histo<T>::~franks2Histo() {
  delete vec;
}
// ******************************
// definition of member functions
// ******************************
template<class T>
template<class X, class Y, class Z>
void franks2Histo<T>::Add( franks2Histo<X>* h1, franks2Histo<X>* h2, Y w1, Z w2, const char* c) {
  for (int i=0; i < mBins; i++) {
    vec[i] = h1->vec[i]*w1 + h2->vec[i]*w2;
  }
}
// *************************************************************************************************
template<class T>
template<class X, class Y, class Z>
void franks2Histo<T>::Divide( franks2Histo<X>* h1, franks2Histo<X>* h2, Y w1, Z w2, const char* c) {
  for (int i=0; i < mBins; i++) {
    if (h2->vec[i]*w2 !=0 ) 
      vec[i] = h1->vec[i]*w1 / h2->vec[i]*w2;
    else
      vec[i]=0;
  }
}
// *************************************************************************************************
template<class T>
void franks2Histo<T>::Draw(const char* c) { 
  cout << c << " " << mC1 << " " << mC2 << endl;
  T min=GetMinimum();
  T max=GetMaximum();
  T step = (max-min)/10.;
  cout << " minimum=" << min << " maximum=" << max << " step=" << step << endl;
  for (int y=mYbins-1; y>=0; y--) { 
    for (int x=0; x<mXbins; x++) { 
      T v = floor( (vec[x+y*mYbins]-min)/step );
      cout << v;
    }
    cout << endl;
  }
};
// *************************************************************************************************
// *************************************************************************************************
template<class T>
template<class X, class Y, class Z>
void franks2Histo<T>::Fill( X x, Y y, Z w) {
  if ( x>=mXmin && x<=mXmax  && y>=mYmin && y<=mYmax) {
    mXpos = (int) floor( (x-mXmin)/mXstep );
    mYpos = (int) floor( (y-mYmin)/mYstep );
    if ( mXpos+mYpos*mYbins <= mBins) {
      vec[mXpos+mYpos*mYbins] = vec[mXpos+mYpos*mYbins] + w;
    }
  }
}
// *************************************************************************************************
template<class T>
T franks2Histo<T>::Integral() {
  T Integral=0;
  for (int i=0; i < mBins; i++) { 
    Integral+=vec[i];
    cout << i << " " << vec[i] << " " << Integral << endl;
  }
  return Integral;
}
// *************************************************************************************************
template<class T>
template<class X>
int franks2Histo<T>::GetBin(X x) {
  int bin  = (int) floor( (x-mXmin)/mXstep );
  if( !(bin >=0 && bin < mXbins) ) bin=-1;
  return bin;
}
// *************************************************************************************************
template<class T>
T franks2Histo<T>::GetBinCenter(int bin) {
  T center=0;
  if ( bin >=0 && bin < mXbins)
    center= mXmin + (0.5+bin)*mXstep;
  return center;
}
// *************************************************************************************************
template<class T>
T franks2Histo<T>::GetMean() {
  T mean=0;
  for (int i=0; i< mBins; i++)
    mean+=vec[i]*GetBinCenter(i);
  mean/=mBins;
  return mean;
}
// *************************************************************************************************
template<class T>
T franks2Histo<T>::GetMaximum() {
  T max=vec[0];
  for (int i=0; i< mBins; i++) {
    if (vec[i] > max) 
      max=vec[i];
  }
  return max;
}
// *************************************************************************************************
template<class T>
T franks2Histo<T>::GetMinimum() {
  T min=vec[0];
  for (int i=0; i< mBins; i++) {
    if (vec[i] < min) 
      min=vec[i];
  }
  return min;
}
// *************************************************************************************************
template<class T>
T franks2Histo<T>::GetRMS() {
  T mean = GetMean();
  for (int i=0; i< mBins; i++)
    mean+=vec[i]*GetBinCenter(i);
  mean/=mBins;
  return mean;
}
// *************************************************************************************************
template<class T>
void franks2Histo<T>::Reset(const char* c) {
  for (int i=0; i < mBins; i++) 
    vec[i] = 0;
}
// *************************************************************************************************
template<class T>
template<class X>
void franks2Histo<T>::Scale(X scale) {
  for (int i=0; i < mBins; i++) 
    vec[i] *=scale;
}


#endif // Solaris
