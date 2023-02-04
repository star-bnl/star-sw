#include "string.h"
#include "TString.h"
#include "Riostream.h"
#include "TMath.h"
void WeightedAverage( const Char_t *pattern = "x0", const Char_t *FileName = "MakeRGraph.txt") {
  FILE *fp = fopen(FileName,"r");
  if (! fp) {
    cout << "Can't open" << FileName << endl;
    return;
  }
  TString Pattern(pattern);
  Pattern += " = ";
  Float_t z, dz;
  Char_t line[201];
  Int_t N = 0;
  Double_t zAv, dzAv;
  while (fgets(line,200,fp)) {
    /*
#02/03/2023
MakeRGraph(2013,"^WAO", -500,  500,    0,  200,0) x0 = -0.3303 +/-  0.0042 y0 = -0.2116 +/-  0.0044 R = 54.5527 +/-  0.0030 gamma = -0.01 +/-  0.03
MakeRGraph(2013,"^WBO", -500,  500,    0,  200,0) x0 = -0.3296 +/-  0.0041 y0 = -0.2153 +/-  0.0041 R =118.7558 +/-  0.0029 gamma = -0.05 +/-  0.02
MakeRGraph(2013,"^WDO", -500,  500,    0,  200,0) x0 = -0.3304 +/-  0.0060 y0 = -0.2021 +/-  0.0053 R =190.5705 +/-  0.0041 gamma = -0.04 +/-  0.02
    */
    cout << line;
    TString Line(line);
    Int_t indx = Line.Index(Pattern.Data());
    if (indx < 0) {
      if (N > 0) {
	zAv /= dzAv;
	dzAv = 1./TMath::Sqrt(dzAv);
	cout << Pattern.Data() << Form("%7.4f +/- %7.4f",zAv,dzAv) << "\tAveraged" << endl;
      }
      N = 0; zAv = dzAv = 0; 
      continue;
    }
    Int_t skip = indx + Pattern.Length();
    Int_t nr = sscanf(Line.Data()+skip,"%f +/- %f",&z,&dz);
    if (nr == 2 && dz > 0) {
      //      cout << pattern << " = " << z << " +/- " << dz << endl;
      Double_t WW = 1./(dz*dz);
      dzAv += WW;
      zAv  += z*WW;
      N++;
    }
  }
      if (N > 0) {
	zAv /= dzAv;
	dzAv = 1./TMath::Sqrt(dzAv);
	cout << Pattern.Data() << Form("%7.4f +/- %7.4f",zAv,dzAv) << "\tAveraged" << endl;
      }
  fclose(fp);
}
