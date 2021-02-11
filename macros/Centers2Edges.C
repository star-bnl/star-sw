#include "Riostream.h"
#include "TMath.h"
#include "TArrayF.h"
const Int_t   NYARRAY  =  37;

Float_t YArray[NYARRAY] = { 50.0, 75.0,  100.0,
				     103.5, 104.0, 104.5, 
				     108.7, 109.2, 109.7,
				     113.9, 114.4, 114.9,
				     118.9, 119.6, 119.8, 
				     120.0, 120.25, 120.5, 120.75, 
				     121.0, 121.5, 122.1, 122.6, 
				     124.2, 125.2, 
				     126.2, 127.195, 
				     128.2, 129.195,
				     130.2, 131.195,
				     132.2, 133.195, 
				     137.195, 150., 
				     198., 200. } ;  
void Centers2Edges(Int_t nbins=NYARRAY, const Float_t *y=YArray) {
  if (!  nbins) return;
  TArrayF x(2*nbins+1);
  for (Int_t i = 0; i < nbins; i++) {cout << "\t" << y[i]; if ((i+1)%10 == 0) cout << endl;}
  cout << endl;
  Int_t j = 0;
  Float_t dy1, dy2;
  for (Int_t i = 0; i < nbins; i++) {
    if      (i == 0)         {dy1 = dy2 = 0.5* (y[i+1] - y[i]);}
    else if (i == nbins - 1) {dy1 = dy2 = 0.5*(y[i] - y[i-1]);}
    else {
      dy1 = 0.5*(y[i] - y[i-1]);
      dy2 = 0.5*(y[i+1] - y[i]);
    }
    Float_t dy = TMath::Min(dy1, dy2);
    Float_t xmin = y[i] - dy;
    Float_t xmax = y[i] + dy;
    if (i == 0) { x[0] = y[i] - dy; x[1] = y[i] + dy; j++;}
    else {
      if (x[j] < y[i] - dy) {
	j++;;
	x[j] = y[i] - dy;
      } 
      j++;
      x[j] = y[i] + dy;
    }
  }
  Int_t N = j + 1;
  x.Set(N);
  for (Int_t j = 1, i = 0; j < x.GetSize(); j++)  {
    cout << "\tx[" << j-1 << "] = " << x[j-1] << "\tx[" << j << "]= "<< x[j] << " => " << 0.5*(x[j-1] +x[j]) << "\ty = " << y[i] << endl;
    if (x[j-1] < y[i] && y[i] < x[j]) i++;
  }
  //  cout << endl;
  //  return x;
}
