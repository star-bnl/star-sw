/* $ID;$
   $Log: RHIClock.C,v $
   Revision 1.6  2009/03/25 20:25:13  fisyak
   Add percentage for shift wrt 200 GeV

   Revision 1.5  2009/03/25 18:57:23  fisyak
   Fix spelling

   Revision 1.4  2009/03/25 18:56:53  fisyak
   Revision 1.3  2009/03/25 18:56:25  fisyak
   Add local clock

   Revision 1.2  2009/03/25 18:53:10  fisyak
   Add options for different beam configuration

__________________________________________________________________________________
Run    setup                 |run      |Ekin   | DB    |RunLog|Calc      |Comment|
                             |         |(GeV/u)|(Hz)   |(MHz) |(MHz)     |       |
__________________________________________________________________________________
local clock                                    |9215890|                 | -1.79%
pp500  lowLuminosity2009     | 10083143|249.729|9383499|9.383 | 9.383499 |   
pp200  ppProduction2008      |  9070006|100.135|9383160|9.383 | 9.383160 |   
AuAu9  lowEnergy2008         |  9071036|  4.545|9341730|9.342 | 9.246833 | -0.44% B = 121  
dAu200 production_dAu2008    |  9383169|101.934|9383169|9.346 | 9.383180 | d 
                                       | 99.908|              | 9.383164 | Au
                                                              | 9.383172 | Avr  
AuAu200 2007Production2      |  8154026| 99.908|9383149|9.383 | 9.383165 | -0.02%  
CuCu200 cuProductionHighTower|  6044049| 99.916|9383150|9.383 | 9.383165 | 
CuCu62  cu62productionMinBias|  6081022| 31.064|9381640|9.382 | 9.379588 |-0.02%  B = 120
CuCu22  cu20Test             |  6083002| 11.216|9351770|9.352 | 9.355936 |-0.34% 
AuAu200 productionMid        |  5084015| 99.908|9383149|9.383 | 9.383165 | 
AuAu62  production62GeV      |  5091003| 31.151|9379360|9.379 | 9.379609 |-0.04%
*/
Double_t RHIClock(Double_t e, Double_t m) {
  Double_t l = 3833.845*9.99998896969437556e-01;  // RHIC perimetr
  Double_t NB = 120;      // no. of buches
  Double_t beta = TMath::Sqrt((e+m)*(e+m) - m*m)/(e+m);
  Double_t f = NB*beta*TMath::C()/l;
  cout << "Freqency = " << f << " for kinetic energy = " << e << " mass = " << m <<endl;
  return f;
}
Double_t RHIClock(const Char_t *name="p", Double_t e = 250) {
  TString Name(name);
  Double_t M = 1;
  Int_t    Z = 1;
  Double_t A = 1.00795;
  Double_t u = 0.938271998/1.00727646688;
  if (Name == "p") {
    A = 1;
    M = 1.00727646688*u;
  } else if (Name == "d") {
    A = 2.0140;
    M = A*u;
  } else if (Name == "Cu") {
    A = 63.546;
    Z = 29;
    M = A*u;
  } else if (Name == "Au") {
    A = 196.96655;
    Z = 79;
    M = A*u;
  } else if (Name == "Pb") {
    A = 207.2;
    Z = 82;
    M = A*u;
  } else if (Name == "U") {
    A = 238.0289;
    Z = 92;
    M = A*u;
  } else if (Name == "pp500") {
    e = 249.729;
    A = 1;
    M = 1.00727646688*u;
  } else if (Name == "pp200") {
    e = 100.135;
    A = 1;
    M = 1.00727646688*u;
  } else if (Name == "AuAu9") {
    e = 4.545;
    A = 196.96655;
    Z = 79;
    M = A*u;
 } else if (Name == "AuAu200") {
    e = 99.908;
    A = 196.96655;
    Z = 79;
    M = A*u;
 } else if (Name == "AuAu62") {
    e = 31.151;
    A = 196.96655;
    Z = 79;
    M = A*u;
  } else if (Name == "CuCu200") {
    e = 99.916;
    A = 63.546;
    Z = 29;
    M = A*u;
  } else if (Name == "CuCu62") {
    e = 31.064;
    A = 63.546;
    Z = 29;
    M = A*u;
  } else if (Name == "CuCu22") {
    e = 11.216;
    A = 63.546;
    Z = 29;
    M = A*u;
  }

  cout << Name.Data() << "\tA\t" << A << "\tZ\t" << Z << "\tM\t" << M << "\tE\t" << e << endl; 
  return RHIClock(e*A,M);
}


