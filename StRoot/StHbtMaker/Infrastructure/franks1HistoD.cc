#ifndef __ROOT__

#include "StHbtMaker/Infrastructure/franks1HistoD.hh"
//#include "franks1HistoD.hh"

// ***********
// constructor 
// ***********
franks1HistoD::franks1HistoD(const char* c1, const char* c2, int bins, double xmin, double xmax) { 
  mC1 = (char*)c1;
  mC2 = (char*)c2;
  mBins = bins;
  mXmin = xmin;
  mXmax = xmax;
  mStep = (xmax-xmin)/bins;
  vec = new double[bins];
}
// *************
// deconstructor 
// *************
franks1HistoD::~franks1HistoD() {
 delete vec;
}

// ******************************
// definition of member functions
// ******************************
// *************************************************************************************************
void franks1HistoD::Add( franks1HistoD* h1, franks1HistoD* h2, double w1, double w2, const char* c) {
  for (int i=0; i < mBins; i++) {
    vec[i] = h1->vec[i]*w1 + h2->vec[i]*w2;
  }
}
// *************************************************************************************************
void franks1HistoD::Divide( franks1HistoD* h1, franks1HistoD* h2, double w1, double w2, const char* c) {
  for (int i=0; i < mBins; i++) {
    if (h2->vec[i]*w2 !=0 ) 
      vec[i] = h1->vec[i]*w1 / h2->vec[i]*w2;
    else
      vec[i]=0;
  }
}
// *************************************************************************************************
void franks1HistoD::Draw(const char* c) {
  cout << c << " " << mC1 << " " << endl;
  double min=GetMinimum();
  double max=GetMaximum();   
  double step = (max-min)/50.;
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
int franks1HistoD::GetBin(double value) {
  int bin  = (int) floor( (value-mXmin)/mStep );
  if( !(bin >=0 && bin < mBins) ) bin=-1;
  return bin;
}
// *************************************************************************************************
double franks1HistoD::GetBinCenter(int bin) {
  double center=0;
  if ( bin >=0 && bin < mBins)
    center= mXmin + (0.5+bin)*mStep;
  return center;
}
// *************************************************************************************************
double franks1HistoD::GetMean() {
  double mean=0;
  for (int i=0; i< mBins; i++)
    mean+=vec[i]*GetBinCenter(i);
  mean/=mBins;
  return mean;
}
// *************************************************************************************************
double franks1HistoD::GetMaximum() {
  double max=vec[0];
  for (int i=0; i< mBins; i++) {
    if (vec[i] > max) 
      max=vec[i];
  }
  return max;
}
// *************************************************************************************************
double franks1HistoD::GetMinimum() {
  double min=vec[0];
  for (int i=0; i< mBins; i++) {
    if (vec[i] < min) 
      min=vec[i];
  }
  return min;
}
// *************************************************************************************************
double franks1HistoD::GetRMS() {
  double mean = GetMean();
  for (int i=0; i< mBins; i++)
    mean+=vec[i]*GetBinCenter(i);
  mean/=mBins;
  return mean;
}
// *************************************************************************************************
void franks1HistoD::Reset(const char*) {
  for (int i=0; i < mBins; i++) 
    vec[i] = 0;
}
// *************************************************************************************************
void franks1HistoD::Scale(double scale) {
  for (int i=0; i < mBins; i++) 
    vec[i] *=scale;
}
// *************************************************************************************************
double franks1HistoD::Integral() {
  double Integral=0;
  for (int i=0; i < mBins; i++) { 
    Integral+=vec[i];
    //cout << i << " " << vec[i] << " " << Integral << endl;
  }
  return Integral;
} 
// *************************************************************************************************
double franks1HistoD::GetEntries() {
  return Integral();
} 
// *************************************************************************************************
void franks1HistoD::Fill( double value) {
  mPos = (int) fabs( (value-mXmin)/mStep );
  if ( mPos>=0 && mPos < mBins) 
    vec[mPos]++;
}
// *************************************************************************************************
void franks1HistoD::Fill( double value, double weight) {
  mPos = (int) fabs( (value-mXmin)/mStep );
  if ( mPos>=0 && mPos < mBins) 
    vec[mPos] = vec[mPos] + weight;
}

#endif // __ROOT__
