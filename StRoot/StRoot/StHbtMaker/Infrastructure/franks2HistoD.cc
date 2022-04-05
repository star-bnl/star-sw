#ifndef __ROOT__

#include "StHbtMaker/Infrastructure/franks2HistoD.hh"

// ***********
// constructor 
// ***********
franks2HistoD::franks2HistoD(const char* c1, const char* c2, 
			     int xbins, double xmin, double xmax, 
			     int ybins, double ymin, double ymax) { 
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
    cout << " franks2HistoD can not create histogram with this parameters";
    cout << " xbins=" << xbins;
    cout << " xmin=" << xmin;
    cout << " xmax=" << xmax;
    cout << " ybins=" << ybins;
    cout << " ymin=" << ymin;
    cout << " ymax=" << ymax;
    exit(-1);
  }
  // create
  vec = new double[xbins*ybins];
}

// *************
// deconstructor 
// *************
franks2HistoD::~franks2HistoD() {
  delete vec;
}
// ******************************
// definition of member functions
// ******************************
void franks2HistoD::Add( franks2HistoD* h1, franks2HistoD* h2, double w1, double w2, const char* c) {
  for (int i=0; i < mBins; i++) {
    vec[i] = h1->vec[i]*w1 + h2->vec[i]*w2;
  }
}
// *************************************************************************************************
void franks2HistoD::Divide( franks2HistoD* h1, franks2HistoD* h2, double w1, double w2, const char* c) {
  for (int i=0; i < mBins; i++) {
    if (h2->vec[i]*w2 !=0 ) 
      vec[i] = h1->vec[i]*w1 / h2->vec[i]*w2;
    else
      vec[i]=0;
  }
}
// *************************************************************************************************
void franks2HistoD::Draw(const char* c) { 
  cout << c << " " << mC1 << " " << mC2 << endl;
  double min=GetMinimum();
  double max=GetMaximum();
  double step = (max-min)/10.;
  cout << " minimum=" << min << " maximum=" << max << " step=" << step << endl;
  for (int y=mYbins-1; y>=0; y--) { 
    for (int x=0; x<mXbins; x++) { 
      double v = floor( (vec[x+y*mYbins]-min)/step );
      cout << v;
    }
    cout << endl;
  }
};
// *************************************************************************************************
// *************************************************************************************************
void franks2HistoD::Fill( double x, double y, double w) {
  if ( x>=mXmin && x<=mXmax  && y>=mYmin && y<=mYmax) {
    mXpos = (int) floor( (x-mXmin)/mXstep );
    mYpos = (int) floor( (y-mYmin)/mYstep );
    if ( mXpos+mYpos*mYbins <= mBins) {
      vec[mXpos+mYpos*mYbins] = vec[mXpos+mYpos*mYbins] + w;
    }
  }
}
// *************************************************************************************************
double franks2HistoD::Integral() {
  double Integral=0;
  for (int i=0; i < mBins; i++) { 
    Integral+=vec[i];
    cout << i << " " << vec[i] << " " << Integral << endl;
  }
  return Integral;
}
// *************************************************************************************************
int franks2HistoD::GetBin(double x) {
  int bin  = (int) floor( (x-mXmin)/mXstep );
  if( !(bin >=0 && bin < mXbins) ) bin=-1;
  return bin;
}
// *************************************************************************************************
double franks2HistoD::GetBinCenter(int bin) {
  double center=0;
  if ( bin >=0 && bin < mXbins)
    center= mXmin + (0.5+bin)*mXstep;
  return center;
}
// *************************************************************************************************
double franks2HistoD::GetMean() {
  double mean=0;
  for (int i=0; i< mBins; i++)
    mean+=vec[i]*GetBinCenter(i);
  mean/=mBins;
  return mean;
}
// *************************************************************************************************
double franks2HistoD::GetMaximum() {
  double max=vec[0];
  for (int i=0; i< mBins; i++) {
    if (vec[i] > max) 
      max=vec[i];
  }
  return max;
}
// *************************************************************************************************
double franks2HistoD::GetMinimum() {
  double min=vec[0];
  for (int i=0; i< mBins; i++) {
    if (vec[i] < min) 
      min=vec[i];
  }
  return min;
}
// *************************************************************************************************
double franks2HistoD::GetRMS() {
  double mean = GetMean();
  for (int i=0; i< mBins; i++)
    mean+=vec[i]*GetBinCenter(i);
  mean/=mBins;
  return mean;
}
// *************************************************************************************************
void franks2HistoD::Reset(const char* c) {
  for (int i=0; i < mBins; i++) 
    vec[i] = 0;
}
// *************************************************************************************************
void franks2HistoD::Scale(double scale) {
  for (int i=0; i < mBins; i++) 
    vec[i] *=scale;
}

#endif // __ROOT__
