const int NParameters=0;
struct BB_pars {
  Double_t c;
  Double_t d;
  Double_t e;
  Double_t f;
  Double_t m;
};
#if 1
BB_pars BB = {//The four parameters for the different TPCs are for the VTPC
   1.730798, // c 
   9.6,      // d 
   2.71,     // e 
   0.24,     // f 
   0.138     // m 
};
#else
BB_pars BB = {//and for the MTPC 
  1.597546, // c  
  9.8,	   // d 
  2.38,	   // e 
  0.2,  	   // f 
  0.138	   // m 
};
#endif
Double_t BetheBlochFunction(double *x,double *par) {
  Double_t bg = x[0];///BB.m; 
  Double_t x1 = pow(10,(BB.e - 1.0/3.0 * sqrt(2.0*log(10.0)/(3.0 * BB.f)))); 
  Double_t x2 = pow(10,(BB.e + 2.0/3.0 * sqrt(2.0*log(10.0)/(3.0 * BB.f)))); 
  Double_t p0 = BB.c/(BB.d + 2.0*log(10.0)*BB.e - 1.0); 
  Double_t dfq;
  if (bg < x1)     dfq = 0; 
    else  { 
      dfq = -2.0 * log(bg) + 2.0 * log(10.0) * BB.e; 
      if(bg < x2) { 
	Double_t d1 = 2.0/3.0*sqrt(2.0*log(10.0)/(3.0 * BB.f)); 
	Double_t d2 = log(bg)/log(10.0); 
	Double_t d3 = pow(( BB.e + d1 - d2),3); 
	dfq -= BB.f*d3; 
      } 
    } 
  return p0 * ( (1+ bg*bg)/(bg*bg) * (BB.d + log(1+(bg*bg)) + dfq) - 1.0 ); 
}
void bbna49() {
  TF1* pionPlusBandCenter 
    =new TF1("pionPlusBandCenter",BetheBlochFunction, 1.e-1,1.e4, NParameters); 
  //  pionPlusBandCenter->SetParameters(pars);
  pionPlusBandCenter->Draw();
}
