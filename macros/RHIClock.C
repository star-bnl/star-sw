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

2019         
glbSetup                            frequncy Ekin   Frequncy calc. Calc./Declared                                         
production_19GeV_2019                9341082 9.796  9348122        1.00075366001497468   9341045 9.99996039002762238e-01 9341035 9.99994968462968203e-01
production_14p5GeV_2019              9307122 7.309  9323422        1.00175134697922741   9307048 9.99992049099603486e-01 9307038 9.99990974653603981e-01
production_lzr_14p5GeV_2019          9307122 7.309
production_7.3GeV_fixedTarget_2019   9307129 7.309
production_lzr_7p7GeV_2019           9104511 7.309
production_7p7GeV_2019               9104506 3.847  9203553        1.01087889886612192   9104334 9.99981108255626361e-01 9104323 9.99979900062672233e-01
production_3p85GeV_fixedTarget_2019  9104506 3.947

   root.exe -q -b lBichsel.C pionMIP.root 'dEdxFit.C+("SecRow3C","GF")'
starClockOnl.20191207.235159.C:    row.frequency         =    9083344; // frequency in Hz  ;
starClockOnl.20191220.235718.C:    row.frequency         =    9337360; // frequency in Hz  ;
starClockOnl.20200127.212835.C:    row.frequency         =    9399987; // frequency in Hz  ;
starClockOnl.20200129.235637.C:    row.frequency         =    9361222; // frequency in Hz  ;
starClockOnl.20200130.220023.C:    row.frequency         =    9188680; // frequency in Hz  ;
starClockOnl.20200131.015235.C:    row.frequency         =    9188684; // frequency in Hz  ;
starClockOnl.20200131.210628.C:    row.frequency         =    9188685; // frequency in Hz  ;
starClockOnl.20200201.230354.C:    row.frequency         =    9372864; // frequency in Hz  ;
starClockOnl.20200202.232044.C:    row.frequency         =    9361228; // frequency in Hz  ;
starClockOnl.20200203.230525.C:    row.frequency         =    9188679; // frequency in Hz  ;
starClockOnl.20200204.230250.C:    row.frequency         =    9307123; // frequency in Hz  ;
starClockOnl.20200209.233751.C:    row.frequency         =    9188683; // frequency in Hz  ;
starClockOnl.20200212.234736.C:    row.frequency         =    9337363; // frequency in Hz  ;
starClockOnl.20200213.225906.C:    row.frequency         =    9337363; // frequency in Hz  ;
starClockOnl.20200223.234623.C:    row.frequency         =    9337360; // frequency in Hz  ;

*/
#if !defined(__CINT__) && !defined(__CLING__) && ! defined(__MAKECINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) && !defined(__CLING__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,18)
//#define __USE_ROOFIT__
//#endif
//________________________________________________________________________________
#if !defined(__CINT__) && !defined(__CLING__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#endif
//________________________________________________________________________________
Double_t RHIClock(Double_t ekin, Double_t m) {
  //Circumference =  3833.845 m => https://www.slac.stanford.edu/pubs/confproc/biw98/shea.pdf
  Double_t l       = 3833.845*9.99999253982746361e-01;// *9.99988614393081399e-01;// *9.99998896969437556e-01;  // RHIC perimetr
  Double_t NB = 120;      // no. of buches
  Double_t beta = TMath::Sqrt((ekin+m)*(ekin+m) - m*m)/(ekin+m);
  Double_t f = 1e-6*NB*beta*TMath::C()/l;
  cout << "Freqency = " << f << " (MHz) for kinetic energy = " << ekin << " mass = " << m <<endl;
  return f;
}
//________________________________________________________________________________
Int_t RHIClock(const Char_t *name="p", Double_t e = 250) {
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
  } else if (Name == "pp510") {
    e = 254.867; // 9.383512 
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
    e = 99.908;   // 9.383161 MHz
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
    e = 11.216; // 9.352 MHz => 9.35629
    A = 63.546;
    Z = 29;
    M = A*u;
  } else if (Name == "11p5GeV") {
    e = 5.761; // 9.337363 MHz
    A = 196.96655;
    Z = 79;
    M = A*u;
  } else if (Name == "9p2GeV") {
    e = 4.593; // 9.188684 MHz
    A = 196.96655;
    Z = 79;
    M = A*u;
  }
  Double_t N = TMath::Nint(A); // no. of nucleons
  Double_t E = e*N;
  //  Double_t E = e*A;
  //  e = TMath::Sqrt(p*p + M*M) - M;
  //  Double_t gamma = (e + M)/M;
  Double_t gamma = (E+M)/M;
  Double_t beta  = TMath::Sqrt(1 - 1./(gamma*gamma));
  cout << Name.Data() << "\tA\t" << A << "\tN\t" << N << "\tZ\t" << Z << "\tM\t" << M << "\tE\t" << e << "\tgamma = " << gamma << "\tbeta = " << beta  << endl; 
  return RHIClock(E,M);
}


