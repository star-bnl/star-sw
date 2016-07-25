const int NParameters=7;
double pars[NParameters]={1.072,0.3199,2.028e-07,1,1,2.69555e-7,5.0e-4};//for mean70
double BetheBlochFunction(double *x,double *par) {
  double rig[1];
  //  rig[0] = pow(10.,x[0]);
    rig[0] = x[0];     
         double charge=double(fabs(par[3]));
         double m=double(par[4]);//mass
         double calib=par[5];//calibFactor
         double satura=par[6];//saturation

          double prefactor=par[0];
          double postfactor=par[1];
          double mFactor=par[2];
          double myValue;
          double b2, gb2;
          double myDedx;



          myValue =5059.0;


          gb2=(rig[0]*charge/m)*(rig[0]*charge/m);
        
          if (gb2 > 0.0) b2=1.0/(1.0 + 1.0/gb2);
          else return 0.0;

	  myDedx=calib*charge*charge*(1.0/pow(b2,prefactor))*(pow(log(myValue*gb2),0.7) - postfactor*b2 )- mFactor;
          if (myDedx > satura) myDedx=satura;   

	  //          return log(myDedx);
          return 1.e6*myDedx;
      };
void Aihong() {
  TF1* pionPlusBandCenter 
    =new TF1("pionPlusBandCenter",BetheBlochFunction, 1.e-1,1.e3, NParameters); 
  pionPlusBandCenter->SetParameters(pars);
  pionPlusBandCenter->SetLineColor(6);
  pionPlusBandCenter->Draw("same");
}
