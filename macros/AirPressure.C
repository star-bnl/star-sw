TF1 *AirPressure() {
  //http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html 
  // norm[Pa] =  101325, x [cm]
  return new TF1("AirPressure","TMath::Power(1 - 2.25577e-5*(x/100.),5.25588)-1.0",-200,200);
};
