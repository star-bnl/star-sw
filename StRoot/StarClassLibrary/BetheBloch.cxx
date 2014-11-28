//
// $Id BetheBloch.cc $
//
// Description
// Using values of the dedx vs. kinetic energy curve.
// taken from GEANT.
//
// curve was generated using geometry from geant.
// need to transform to dedx vs. beta gamma
// this is done by
// 1) reading the kinetic energy and dedx value
// 2) obtain energy by energy = kinetic energy + mass
// 3) calculate beta*gamma = p/m where p = ::sqrt(e^2 - m^2)
// 4) insert into the map of beta gamma - dedx values
//
// The function returns a linear interpolation between
// the 2 closest bins.

// If betagamma < 2.5, the function returns a value proportional
// to 1/ beta^2
// There is an overall normalization factor obtained from a fit
// and there is a relative normalization factor between the simple
// 1/beta^2 region and the values from the table.
#include <Stiostream.h>
#include <vector>
#include "BetheBloch.h"
#include "TMath.h"
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
int BetheBloch::noWarn = 0;

BetheBloch::BetheBloch(){
    // it's more elegant to read the values from a file, but to put it
    // in the StarClassLibrary without extra files, we have to dump the numbers
    // here
    // could also fill them directly in the map, or transform the numbers directly
    // to beta gamma.
  if (! noWarn) {
  cout << "BetheBloch::BetheBloch =================================================================" << endl;
  cout << "Warning: please don't use BetheBloch::BetheBloch for any analysis after P00hm production" << endl;
  cout << "but use instead :                                                                       " << endl;
  cout << "for production before P03h static function 1.e-6*BetheBloch::Sirrf(Poverm)              " << endl;
  cout << "for production after  P03h (including P03h)                                             " << endl; 
  cout << "gSystem->Load(\"StBichsel\"); I70 = 1.e-6*mBichsel::Instance()->GetI70(TMath::Log10(poverm),1.)" << endl;
  cout << "see an example in  $STAR/StRoot/macros/analysis/bichsel.C                               " << endl;
  cout << "BetheBloch::BetheBloch =================================================================" << endl;
  noWarn = 1;
  }
    vector<double> kinVec;
    vector<double> ionizVec;   // Protons                      
    kinVec.push_back(0.216355);ionizVec.push_back(2.38171e-05);
    kinVec.push_back(0.22275 );ionizVec.push_back(2.25295e-05);
    kinVec.push_back(0.229145);ionizVec.push_back(2.13482e-05);
    kinVec.push_back(0.235539);ionizVec.push_back(2.02618e-05);
    kinVec.push_back(0.241934);ionizVec.push_back(1.92604e-05);
    kinVec.push_back(0.248329);ionizVec.push_back(1.83354e-05);
    kinVec.push_back(0.254723);ionizVec.push_back(1.74791e-05);
    kinVec.push_back(0.261118);ionizVec.push_back(1.6685e-05 );
    kinVec.push_back(0.267513);ionizVec.push_back(1.63046e-05);
    kinVec.push_back(0.273908);ionizVec.push_back(1.56024e-05);
    kinVec.push_back(0.280302);ionizVec.push_back(1.49478e-05);
    kinVec.push_back(0.286697);ionizVec.push_back(1.43364e-05); 
    kinVec.push_back(0.293092);ionizVec.push_back(1.37647e-05);
    kinVec.push_back(0.299487);ionizVec.push_back(1.32291e-05);
    kinVec.push_back(0.305881);ionizVec.push_back(1.27268e-05);
    kinVec.push_back(0.312276);ionizVec.push_back(1.2255e-05 );
    kinVec.push_back(0.318671);ionizVec.push_back(1.18114e-05);
    kinVec.push_back(0.325066);ionizVec.push_back(1.13592e-05);
    kinVec.push_back(0.33146 );ionizVec.push_back(1.09666e-05);
    kinVec.push_back(0.337855);ionizVec.push_back(1.05961e-05);
    kinVec.push_back(0.34425 );ionizVec.push_back(1.02462e-05);
    kinVec.push_back(0.350644);ionizVec.push_back(9.91509e-06);
    kinVec.push_back(0.357039);ionizVec.push_back(9.6016e-06 );
    kinVec.push_back(0.363434);ionizVec.push_back(9.30456e-06);
    kinVec.push_back(0.369829);ionizVec.push_back(9.02278e-06);
    kinVec.push_back(0.376223);ionizVec.push_back(8.81998e-06);
    kinVec.push_back(0.382618);ionizVec.push_back(8.56388e-06);
    kinVec.push_back(0.389013);ionizVec.push_back(8.32029e-06);  
    kinVec.push_back(0.395408);ionizVec.push_back(8.08844e-06);
    kinVec.push_back(0.401802);ionizVec.push_back(7.86756e-06);
    kinVec.push_back(0.408197);ionizVec.push_back(7.65699e-06);
    kinVec.push_back(0.414592);ionizVec.push_back(7.45608e-06);
    kinVec.push_back(0.420987);ionizVec.push_back(7.26426e-06);
    kinVec.push_back(0.427381);ionizVec.push_back(7.12715e-06);
    kinVec.push_back(0.433776);ionizVec.push_back(6.95077e-06);
    kinVec.push_back(0.440171);ionizVec.push_back(6.78204e-06);
    kinVec.push_back(0.446565);ionizVec.push_back(6.62049e-06);
    kinVec.push_back(0.45296 );ionizVec.push_back(6.46574e-06);
    kinVec.push_back(0.459355);ionizVec.push_back(6.3174e-06 );
    kinVec.push_back(0.46575 );ionizVec.push_back(6.17514e-06);
    kinVec.push_back(0.472144);ionizVec.push_back(6.03861e-06);
    kinVec.push_back(0.478539);ionizVec.push_back(5.90753e-06);
    kinVec.push_back(0.484934);ionizVec.push_back(5.82937e-06);
    kinVec.push_back(0.491329);ionizVec.push_back(5.70733e-06);
    kinVec.push_back(0.497723);ionizVec.push_back(5.58995e-06);
    kinVec.push_back(0.504118);ionizVec.push_back(5.47701e-06);
    kinVec.push_back(0.510513);ionizVec.push_back(5.36829e-06);  
    kinVec.push_back(0.516908);ionizVec.push_back(5.26358e-06);
    kinVec.push_back(0.523302);ionizVec.push_back(5.16268e-06);
    kinVec.push_back(0.529697);ionizVec.push_back(5.06542e-06);
    kinVec.push_back(0.536092);ionizVec.push_back(4.95505e-06);
    kinVec.push_back(0.542486);ionizVec.push_back(4.86486e-06);
    kinVec.push_back(0.548881);ionizVec.push_back(4.77779e-06);
    kinVec.push_back(0.555276);ionizVec.push_back(4.6937e-06 );
    kinVec.push_back(0.561671);ionizVec.push_back(4.61248e-06);
    kinVec.push_back(0.568065);ionizVec.push_back(4.53397e-06);
    kinVec.push_back(0.57446 );ionizVec.push_back(4.45809e-06);
    kinVec.push_back(0.580855);ionizVec.push_back(4.38469e-06);
    kinVec.push_back(0.58725 );ionizVec.push_back(4.32523e-06);
    kinVec.push_back(0.593644);ionizVec.push_back(4.25631e-06);
    kinVec.push_back(0.600039);ionizVec.push_back(4.18958e-06);
    kinVec.push_back(0.606434);ionizVec.push_back(4.12496e-06);
    kinVec.push_back(0.612828);ionizVec.push_back(4.06236e-06);
    kinVec.push_back(0.619223);ionizVec.push_back(4.00167e-06);
    kinVec.push_back(0.625618);ionizVec.push_back(3.94284e-06);
    kinVec.push_back(0.632013);ionizVec.push_back(3.88579e-06);
    kinVec.push_back(0.638407);ionizVec.push_back(3.83045e-06);
    kinVec.push_back(0.644802);ionizVec.push_back(3.78096e-06);
    kinVec.push_back(0.651197);ionizVec.push_back(3.72878e-06);
    kinVec.push_back(0.657592);ionizVec.push_back(3.6781e-06 );
    kinVec.push_back(0.663986);ionizVec.push_back(3.62888e-06);
    kinVec.push_back(0.670381);ionizVec.push_back(3.58106e-06);
    kinVec.push_back(0.676776);ionizVec.push_back(3.53459e-06);
    kinVec.push_back(0.683171);ionizVec.push_back(3.48941e-06);
    kinVec.push_back(0.689565);ionizVec.push_back(3.4455e-06 );
    kinVec.push_back(0.69596 );ionizVec.push_back(3.39852e-06);
    kinVec.push_back(0.702355);ionizVec.push_back(3.35701e-06);
    kinVec.push_back(0.708749);ionizVec.push_back(3.31663e-06);
    kinVec.push_back(0.715144);ionizVec.push_back(3.27733e-06);
    kinVec.push_back(0.721539);ionizVec.push_back(3.23906e-06);
    kinVec.push_back(0.727934);ionizVec.push_back(3.20181e-06);
    kinVec.push_back(0.734328);ionizVec.push_back(3.16551e-06);
    kinVec.push_back(0.740723);ionizVec.push_back(3.13016e-06);
    kinVec.push_back(0.747118);ionizVec.push_back(3.08976e-06);
    kinVec.push_back(0.753513);ionizVec.push_back(3.05624e-06);
    kinVec.push_back(0.759907);ionizVec.push_back(3.02358e-06);
    kinVec.push_back(0.766302);ionizVec.push_back(2.99172e-06);
    kinVec.push_back(0.772697);ionizVec.push_back(2.96065e-06);
    kinVec.push_back(0.779092);ionizVec.push_back(2.93033e-06);
    kinVec.push_back(0.785486);ionizVec.push_back(2.90077e-06);
    kinVec.push_back(0.791881);ionizVec.push_back(2.87191e-06);
    kinVec.push_back(0.798276);ionizVec.push_back(2.84375e-06);
    kinVec.push_back(0.80467 );ionizVec.push_back(2.80652e-06);
    kinVec.push_back(0.811065);ionizVec.push_back(2.77976e-06);
    kinVec.push_back(0.81746 );ionizVec.push_back(2.75364e-06);
    kinVec.push_back(0.823855);ionizVec.push_back(2.72812e-06);
    kinVec.push_back(0.830249);ionizVec.push_back(2.70318e-06);
    kinVec.push_back(0.836644);ionizVec.push_back(2.67881e-06);
    kinVec.push_back(0.843039);ionizVec.push_back(2.65499e-06);
    kinVec.push_back(0.849434);ionizVec.push_back(2.63171e-06);

    // Pions                  // Pions                                                          
    kinVec.push_back(0.92068);ionizVec.push_back(2.44711e-06);//ionizVec.push_back(2.44711e-06);
    kinVec.push_back(1.05682);ionizVec.push_back(2.12787e-06);//ionizVec.push_back(2.12787e-06);
    kinVec.push_back(1.19295);ionizVec.push_back(1.91688e-06);//ionizVec.push_back(1.91688e-06);
    kinVec.push_back(1.32908);ionizVec.push_back(1.76312e-06);//ionizVec.push_back(1.76312e-06);
    kinVec.push_back(1.46522);ionizVec.push_back(1.65432e-06);//ionizVec.push_back(1.65432e-06);
    kinVec.push_back(1.60135);ionizVec.push_back(1.56874e-06);//ionizVec.push_back(1.56874e-06);
    kinVec.push_back(1.73748);ionizVec.push_back(1.50247e-06);//ionizVec.push_back(1.50247e-06);
    kinVec.push_back(1.87361);ionizVec.push_back(1.46839e-06);//ionizVec.push_back(1.46839e-06);
    kinVec.push_back(2.00974);ionizVec.push_back(1.42577e-06);//ionizVec.push_back(1.42577e-06);
    kinVec.push_back(2.14588);ionizVec.push_back(1.39101e-06);//ionizVec.push_back(1.39101e-06);
    kinVec.push_back(2.28201);ionizVec.push_back(1.38415e-06);//ionizVec.push_back(1.38415e-06);
    kinVec.push_back(2.41814);ionizVec.push_back(1.35975e-06);//ionizVec.push_back(1.35975e-06  
    kinVec.push_back(2.55427);ionizVec.push_back(1.34999e-06);//ionizVec.push_back(1.34999e-06);
    kinVec.push_back(2.69041);ionizVec.push_back(1.34375e-06);//ionizVec.push_back(1.34375e-06);
    kinVec.push_back(2.82654);ionizVec.push_back(1.3375e-06 );//ionizVec.push_back(1.3375e-06 );
    kinVec.push_back(2.96267);ionizVec.push_back(1.32839e-06);//ionizVec.push_back(1.32839e-06);
    kinVec.push_back(3.0988 );ionizVec.push_back(1.32689e-06);//ionizVec.push_back(1.32689e-06);
    kinVec.push_back(3.23494);ionizVec.push_back(1.32366e-06);//ionizVec.push_back(1.32366e-06);
    kinVec.push_back(3.37107);ionizVec.push_back(1.32239e-06);//ionizVec.push_back(1.32239e-06);
    kinVec.push_back(3.5072 );ionizVec.push_back(1.32112e-06);//ionizVec.push_back(1.32112e-06);
    kinVec.push_back(3.64333);ionizVec.push_back(1.31949e-06);//ionizVec.push_back(1.31949e-06);
    kinVec.push_back(3.77947);ionizVec.push_back(1.32095e-06);//ionizVec.push_back(1.32095e-06);
    kinVec.push_back(3.9156 );ionizVec.push_back(1.32241e-06);//ionizVec.push_back(1.32241e-06);
    kinVec.push_back(4.18786);ionizVec.push_back(1.3238e-06 );//ionizVec.push_back(1.3228e-06 );
    kinVec.push_back(4.46013);ionizVec.push_back(1.32547e-06);//ionizVec.push_back(1.32547e-06);
    kinVec.push_back(4.59626);ionizVec.push_back(1.32823e-06);//ionizVec.push_back(1.32823e-06);
    kinVec.push_back(5.14079);ionizVec.push_back(1.3315e-06 );//ionizVec.push_back(1.3435e-06 );//ionizVec.push_back(1.3435e-06 );
    kinVec.push_back(5.27692);ionizVec.push_back(1.33467e-06);//ionizVec.push_back(1.34667e-06);//ionizVec.push_back(1.34667e-06);
    kinVec.push_back(5.41306);ionizVec.push_back(1.33871e-06);//ionizVec.push_back(1.36661e-06);//ionizVec.push_back(1.36617e-06);
    kinVec.push_back(5.54919);ionizVec.push_back(1.34288e-06);//ionizVec.push_back(1.36988e-06);//ionizVec.push_back(1.36988e-06);
    kinVec.push_back(5.68532);ionizVec.push_back(1.34616e-06);//ionizVec.push_back(1.37316e-06);//ionizVec.push_back(1.37316e-06);
    kinVec.push_back(5.82145);ionizVec.push_back(1.35068e-06);//ionizVec.push_back(1.37468e-06);//ionizVec.push_back(1.37468e-06);
    kinVec.push_back(5.95759);ionizVec.push_back(1.35494e-06);//ionizVec.push_back(1.37795e-06);//ionizVec.push_back(1.37795e-06);
    kinVec.push_back(6.09372);ionizVec.push_back(1.36034e-06);//ionizVec.push_back(1.37614e-06);//ionizVec.push_back(1.37614e-06);
    kinVec.push_back(6.22985);ionizVec.push_back(1.36459e-06);//ionizVec.push_back(1.37939e-06);//ionizVec.push_back(1.37939e-06);
    kinVec.push_back(6.36598);ionizVec.push_back(1.36865e-06);//ionizVec.push_back(1.38265e-06);//ionizVec.push_back(1.38265e-06);
    kinVec.push_back(6.50212);ionizVec.push_back(1.37391e-06);//ionizVec.push_back(1.38038e-06);//ionizVec.push_back(1.38038e-06);
    kinVec.push_back(6.63825);ionizVec.push_back(1.37861e-06);//ionizVec.push_back(1.38361e-06);//ionizVec.push_back(1.38361e-06);
    kinVec.push_back(6.77438);ionizVec.push_back(1.38285e-06);//ionizVec.push_back(1.38685e-06);//ionizVec.push_back(1.38685e-06);
    kinVec.push_back(6.91051);ionizVec.push_back(1.38648e-06);//ionizVec.push_back(1.38748e-06);//ionizVec.push_back(1.38748e-06);
    kinVec.push_back(7.04665);ionizVec.push_back(1.38971e-06);//ionizVec.push_back(1.39071e-06);
    kinVec.push_back(7.31891);ionizVec.push_back(1.39186e-06);//ionizVec.push_back(1.39186e-06);
    kinVec.push_back(7.45504);ionizVec.push_back(1.39507e-06);//ionizVec.push_back(1.39507e-06);
    kinVec.push_back(7.72731);ionizVec.push_back(1.39519e-06);//ionizVec.push_back(1.39519e-06);
    kinVec.push_back(7.86344);ionizVec.push_back(1.39713e-06);//ionizVec.push_back(1.39813e-06);
    kinVec.push_back(8.13571);ionizVec.push_back(1.39835e-06);//ionizVec.push_back(1.39835e-06);
    kinVec.push_back(8.27184);ionizVec.push_back(1.39978e-06);//ionizVec.push_back(1.39778e-06);
    kinVec.push_back(8.40797);ionizVec.push_back(1.40071e-06);//ionizVec.push_back(1.40071e-06);
    kinVec.push_back(8.5441 );ionizVec.push_back(1.40363e-06);//ionizVec.push_back(1.40363e-06);
    kinVec.push_back(8.81637);ionizVec.push_back(1.40651e-06);//ionizVec.push_back(1.40651e-06);
    kinVec.push_back(8.9525 );ionizVec.push_back(1.40943e-06);//ionizVec.push_back(1.40943e-06);
    kinVec.push_back(9.08863);ionizVec.push_back(1.41462e-06);//ionizVec.push_back(1.41462e-06);
    kinVec.push_back(9.22477);ionizVec.push_back(1.41753e-06);//ionizVec.push_back(1.41753e-06);
    kinVec.push_back(9.76929);ionizVec.push_back(1.42957e-06);//ionizVec.push_back(1.42957e-06);
    kinVec.push_back(9.90543);ionizVec.push_back(1.43212e-06);//ionizVec.push_back(1.43212e-06);
    kinVec.push_back(10.0416);ionizVec.push_back(1.4451e-06 );//ionizVec.push_back(1.4451e-06 );
    kinVec.push_back(10.1777);ionizVec.push_back(1.44767e-06);//ionizVec.push_back(1.44767e-06);
    kinVec.push_back(10.3138);ionizVec.push_back(1.45024e-06);//ionizVec.push_back(1.45024e-06);
    kinVec.push_back(10.7222);ionizVec.push_back(1.45164e-06);//ionizVec.push_back(1.45164e-06);
    kinVec.push_back(10.8584);ionizVec.push_back(1.45502e-06);//ionizVec.push_back(1.45502e-06);
    kinVec.push_back(10.9945);ionizVec.push_back(1.45758e-06);//ionizVec.push_back(1.45758e-06);
    kinVec.push_back(12.0835);ionizVec.push_back(1.45772e-06);//ionizVec.push_back(1.45772e-06);

    // extrapolation
//     kinVec.push_back(15.8304);
//     kinVec.push_back(18.6956);
//     kinVec.push_back(21.5609);
//     kinVec.push_back(24.4261);
//     kinVec.push_back(27.2913);
//     kinVec.push_back(30.1566);
//     kinVec.push_back(33.0218);
//     kinVec.push_back(35.887	);
//     kinVec.push_back(38.7523);
//     kinVec.push_back(41.6175);
//     kinVec.push_back(44.4827);
//     kinVec.push_back(47.3479);
//     kinVec.push_back(50.2132);
//     kinVec.push_back(53.0784);
//     kinVec.push_back(55.9436);
//     kinVec.push_back(58.8089);
//     kinVec.push_back(61.6741);
//     kinVec.push_back(64.5393);
//     kinVec.push_back(67.4046);
//     kinVec.push_back(70.2698);
//     kinVec.push_back(73.135 );
//     kinVec.push_back(76.0002);
//     kinVec.push_back(78.8655);
//     kinVec.push_back(81.7307);
//     kinVec.push_back(84.5959);
//     kinVec.push_back(87.4612);
//     kinVec.push_back(90.3264);
//     kinVec.push_back(93.1916);
//     kinVec.push_back(96.0569);
//     kinVec.push_back(98.9221);
//     kinVec.push_back(101.787);
//     kinVec.push_back(104.653);
//     kinVec.push_back(107.518);
//     kinVec.push_back(110.383);
//     kinVec.push_back(113.248);
//     kinVec.push_back(116.113);
//     kinVec.push_back(118.979);
//     kinVec.push_back(121.844);
//     kinVec.push_back(124.709);
//     kinVec.push_back(127.574);
//     kinVec.push_back(130.44 );
//     kinVec.push_back(133.305);
//     kinVec.push_back(136.17 );
//     kinVec.push_back(139.035);
//     kinVec.push_back(141.901);
//     kinVec.push_back(144.766);
//     kinVec.push_back(147.631);
//     kinVec.push_back(150.496);
//     kinVec.push_back(153.361);
//     kinVec.push_back(156.227);
//     kinVec.push_back(159.092);
//     kinVec.push_back(161.957);
//     kinVec.push_back(164.822);
//     kinVec.push_back(167.688);
//     kinVec.push_back(170.553);
//     kinVec.push_back(173.418);
//     kinVec.push_back(176.283);
//     kinVec.push_back(179.149);
//     kinVec.push_back(182.014);
//     kinVec.push_back(184.879);
//     kinVec.push_back(187.744);
//     kinVec.push_back(190.609);
//     kinVec.push_back(193.475);
//     kinVec.push_back(196.34 );
//     kinVec.push_back(199.205);

    // electrons                // electrons                           
    kinVec.push_back(214.286);	ionizVec.push_back(1.84391e-06*1.0583);
    kinVec.push_back(251.468);	ionizVec.push_back(1.865e-06  *1.0583);
    kinVec.push_back(288.65 );	ionizVec.push_back(1.88321e-06*1.0583);
    kinVec.push_back(325.832);	ionizVec.push_back(1.87983e-06*1.0583);
    kinVec.push_back(363.014);	ionizVec.push_back(1.89258e-06*1.0583);
    kinVec.push_back(400.196);	ionizVec.push_back(1.88496e-06*1.0583);
    kinVec.push_back(437.378);	ionizVec.push_back(1.8953e-06 *1.0583);
    kinVec.push_back(474.56 );	ionizVec.push_back(1.90352e-06*1.0583);
    kinVec.push_back(511.742);	ionizVec.push_back(1.89112e-06*1.0583);
    kinVec.push_back(548.924);	ionizVec.push_back(1.89855e-06*1.0583);
    kinVec.push_back(586.106);	ionizVec.push_back(1.90452e-06*1.0583);
    kinVec.push_back(623.288);	ionizVec.push_back(1.90318e-06*1.0583);
    kinVec.push_back(660.47 );	ionizVec.push_back(1.90914e-06*1.0583);  
    kinVec.push_back(697.652);	ionizVec.push_back(1.91306e-06*1.0583);
    kinVec.push_back(734.834);	ionizVec.push_back(1.91746e-06*1.0583);
    kinVec.push_back(772.016);	ionizVec.push_back(1.92184e-06*1.0583);
    kinVec.push_back(809.198);	ionizVec.push_back(1.92571e-06*1.0583);
    kinVec.push_back(846.38 );	ionizVec.push_back(1.93009e-06*1.0583);
    kinVec.push_back(883.562);	ionizVec.push_back(1.93345e-06*1.0583);
    kinVec.push_back(920.744);	ionizVec.push_back(1.93669e-06*1.0583);
    kinVec.push_back(957.926);	ionizVec.push_back(1.93992e-06*1.0583);
    kinVec.push_back(995.108);	ionizVec.push_back(1.94317e-06*1.0583);
    kinVec.push_back(1032.29);	ionizVec.push_back(1.94641e-06*1.0583);
    kinVec.push_back(1069.47);	ionizVec.push_back(1.94964e-06*1.0583);
    kinVec.push_back(1106.65);	ionizVec.push_back(1.95206e-06*1.0583);
    kinVec.push_back(1143.84);	ionizVec.push_back(1.95445e-06*1.0583);
    kinVec.push_back(1181.02);	ionizVec.push_back(1.95684e-06*1.0583);
    kinVec.push_back(1218.2 );	ionizVec.push_back(1.95924e-06*1.0583);
    kinVec.push_back(1255.38);	ionizVec.push_back(1.96162e-06*1.0583);  
    kinVec.push_back(1292.56);	ionizVec.push_back(1.96402e-06*1.0583);
    kinVec.push_back(1329.75);	ionizVec.push_back(1.96641e-06*1.0583);
    kinVec.push_back(1366.93);	ionizVec.push_back(1.96846e-06*1.0583);
    kinVec.push_back(1404.11);	ionizVec.push_back(1.97023e-06*1.0583);
    kinVec.push_back(1441.29);	ionizVec.push_back(1.972e-06  *1.0583);
    kinVec.push_back(1478.47);	ionizVec.push_back(1.97379e-06*1.0583);
    kinVec.push_back(1515.66);	ionizVec.push_back(1.97555e-06*1.0583);
    kinVec.push_back(1552.84);	ionizVec.push_back(1.97732e-06*1.0583);
    kinVec.push_back(1590.02);	ionizVec.push_back(1.9791e-06 *1.0583);
    kinVec.push_back(1627.2 );	ionizVec.push_back(1.98087e-06*1.0583);
    kinVec.push_back(1664.38);	ionizVec.push_back(1.98265e-06*1.0583);
    kinVec.push_back(1701.57);	ionizVec.push_back(1.98434e-06*1.0583);
    kinVec.push_back(1738.75);	ionizVec.push_back(1.98567e-06*1.0583);
    kinVec.push_back(1775.93);	ionizVec.push_back(1.98699e-06*1.0583);
    kinVec.push_back(1813.11);	ionizVec.push_back(1.98832e-06*1.0583);
    kinVec.push_back(1850.29);	ionizVec.push_back(1.98964e-06*1.0583);
    kinVec.push_back(1887.48);	ionizVec.push_back(1.99098e-06*1.0583);
    kinVec.push_back(1924.66);	ionizVec.push_back(1.99229e-06*1.0583);
    kinVec.push_back(1961.84);	ionizVec.push_back(1.99361e-06*1.0583); 
    kinVec.push_back(1999.02);	ionizVec.push_back(1.99493e-06*1.0583);
    kinVec.push_back(2036.2 );	ionizVec.push_back(1.99625e-06*1.0583);
    kinVec.push_back(2073.39);	ionizVec.push_back(1.99759e-06*1.0583);
    kinVec.push_back(2110.57);	ionizVec.push_back(1.99891e-06*1.0583);
    kinVec.push_back(2147.75);	ionizVec.push_back(2.00012e-06*1.0583);
    kinVec.push_back(2184.93);	ionizVec.push_back(2.0011e-06 *1.0583);
    kinVec.push_back(2222.11);	ionizVec.push_back(2.00209e-06*1.0583);
    kinVec.push_back(2259.3 );	ionizVec.push_back(2.00309e-06*1.0583);
    kinVec.push_back(2296.48);	ionizVec.push_back(2.00407e-06*1.0583);
    kinVec.push_back(2333.66);	ionizVec.push_back(2.00508e-06*1.0583);
    kinVec.push_back(2370.84);	ionizVec.push_back(2.00606e-06*1.0583);
    kinVec.push_back(2408.02);	ionizVec.push_back(2.00706e-06*1.0583);
    kinVec.push_back(2445.21);	ionizVec.push_back(2.00805e-06*1.0583);
    kinVec.push_back(2482.39);	ionizVec.push_back(2.00903e-06*1.0583);
    kinVec.push_back(2519.57);	ionizVec.push_back(2.01003e-06*1.0583);
    kinVec.push_back(2556.75);	ionizVec.push_back(2.01102e-06*1.0583);
    kinVec.push_back(2593.93);	ionizVec.push_back(2.01202e-06*1.0583);
    kinVec.push_back(2631.12);	ionizVec.push_back(2.01301e-06*1.0583);
    kinVec.push_back(2668.3 );	ionizVec.push_back(2.014e-06  *1.0583);
    kinVec.push_back(2705.48);	ionizVec.push_back(2.01486e-06*1.0583);
    kinVec.push_back(2742.66);	ionizVec.push_back(2.01561e-06*1.0583);
    kinVec.push_back(2779.84);	ionizVec.push_back(2.01635e-06*1.0583);
    kinVec.push_back(2817.03);	ionizVec.push_back(2.0171e-06 *1.0583);
    kinVec.push_back(2854.21);	ionizVec.push_back(2.01784e-06*1.0583);
    kinVec.push_back(2891.39);	ionizVec.push_back(2.01859e-06*1.0583);
    kinVec.push_back(2928.57);	ionizVec.push_back(2.01933e-06*1.0583);
    kinVec.push_back(2965.75);	ionizVec.push_back(2.02008e-06*1.0583);
    kinVec.push_back(3002.94);	ionizVec.push_back(2.02081e-06*1.0583);
    kinVec.push_back(3040.12);	ionizVec.push_back(2.02156e-06*1.0583);
    kinVec.push_back(3077.3 );	ionizVec.push_back(2.02231e-06*1.0583);
    kinVec.push_back(3114.48);	ionizVec.push_back(2.02306e-06*1.0583);
    kinVec.push_back(3151.66);	ionizVec.push_back(2.0238e-06 *1.0583);
    kinVec.push_back(3188.85);	ionizVec.push_back(2.02454e-06*1.0583);
    kinVec.push_back(3226.03);	ionizVec.push_back(2.02529e-06*1.0583);
    kinVec.push_back(3263.21);	ionizVec.push_back(2.02604e-06*1.0583);
    kinVec.push_back(3300.39);	ionizVec.push_back(2.02678e-06*1.0583);
    kinVec.push_back(3337.57);	ionizVec.push_back(2.02752e-06*1.0583);
    kinVec.push_back(3374.76);	ionizVec.push_back(2.02826e-06*1.0583);
    kinVec.push_back(3411.94);	ionizVec.push_back(2.02887e-06*1.0583);
    kinVec.push_back(3449.12);	ionizVec.push_back(2.02943e-06*1.0583);
    kinVec.push_back(3486.3 );	ionizVec.push_back(2.03e-06   *1.0583);
    kinVec.push_back(3523.48);	ionizVec.push_back(2.03056e-06*1.0583);
    kinVec.push_back(3560.67);	ionizVec.push_back(2.03113e-06*1.0583);
    kinVec.push_back(3597.85);	ionizVec.push_back(2.03169e-06*1.0583);
    kinVec.push_back(3635.03);	ionizVec.push_back(2.03227e-06*1.0583);
    kinVec.push_back(3672.21);	ionizVec.push_back(2.03283e-06*1.0583);
    kinVec.push_back(3709.39);	ionizVec.push_back(2.03339e-06*1.0583);
    kinVec.push_back(3746.58);	ionizVec.push_back(2.03395e-06*1.0583);
    kinVec.push_back(3783.76);	ionizVec.push_back(2.03452e-06*1.0583);
    kinVec.push_back(3820.94);	ionizVec.push_back(2.03509e-06*1.0583);
    kinVec.push_back(3858.12);	ionizVec.push_back(2.03565e-06*1.0583);
    kinVec.push_back(3895.3 );	ionizVec.push_back(2.03622e-06*1.0583);
    
    
    // extrapolation		    
//     ionizVec.push_back(1.47415e-06);
//     ionizVec.push_back(1.51035e-06);
//     ionizVec.push_back(1.54178e-06);
//     ionizVec.push_back(1.569e-06  );
//     ionizVec.push_back(1.59476e-06);
//     ionizVec.push_back(1.61668e-06);
//     ionizVec.push_back(1.63772e-06);
//     ionizVec.push_back(1.65727e-06);
//     ionizVec.push_back(1.67439e-06);
//     ionizVec.push_back(1.69155e-06);
//     ionizVec.push_back(1.70766e-06);
//     ionizVec.push_back(1.72162e-06);
//     ionizVec.push_back(1.73558e-06);
//     ionizVec.push_back(1.74958e-06);
//     ionizVec.push_back(1.76232e-06);
//     ionizVec.push_back(1.77371e-06);
//     ionizVec.push_back(1.78513e-06);
//     ionizVec.push_back(1.79656e-06);
//     ionizVec.push_back(1.80799e-06);
//     ionizVec.push_back(1.81811e-06);
//     ionizVec.push_back(1.82747e-06);
//     ionizVec.push_back(1.83683e-06);
//     ionizVec.push_back(1.8462e-06 );
//     ionizVec.push_back(1.85558e-06);
//     ionizVec.push_back(1.86498e-06);
//     ionizVec.push_back(1.87347e-06);
//     ionizVec.push_back(1.8812e-06 );
//     ionizVec.push_back(1.88893e-06);
//     ionizVec.push_back(1.89667e-06);
//     ionizVec.push_back(1.90442e-06);
//     ionizVec.push_back(1.91216e-06);
//     ionizVec.push_back(1.91992e-06);
//     ionizVec.push_back(1.92768e-06);
//     ionizVec.push_back(1.93402e-06);
//     ionizVec.push_back(1.94015e-06);
//     ionizVec.push_back(1.94627e-06);
//     ionizVec.push_back(1.9524e-06 );
//     ionizVec.push_back(1.95853e-06);
//     ionizVec.push_back(1.96466e-06);
//     ionizVec.push_back(1.97079e-06);
//     ionizVec.push_back(1.97692e-06);
//     ionizVec.push_back(1.98305e-06);
//     ionizVec.push_back(1.98894e-06);
//     ionizVec.push_back(1.99384e-06);
//     ionizVec.push_back(1.99875e-06);
//     ionizVec.push_back(2.00366e-06);
//     ionizVec.push_back(2.00857e-06);
//     ionizVec.push_back(2.01347e-06);
//     ionizVec.push_back(2.01837e-06);
//     ionizVec.push_back(2.02328e-06);
//     ionizVec.push_back(2.02817e-06);
//     ionizVec.push_back(2.03307e-06);
//     ionizVec.push_back(2.03797e-06);
//     ionizVec.push_back(2.04287e-06);
//     ionizVec.push_back(2.04772e-06);
//     ionizVec.push_back(2.05173e-06);
//     ionizVec.push_back(2.05575e-06);
//     ionizVec.push_back(2.05975e-06);
//     ionizVec.push_back(2.06375e-06);
//     ionizVec.push_back(2.06775e-06);
//     ionizVec.push_back(2.07175e-06);
//     ionizVec.push_back(2.07574e-06);
//     ionizVec.push_back(2.07973e-06);
//     ionizVec.push_back(2.08371e-06);
//     ionizVec.push_back(2.08771e-06);


 
    for(size_t i=0; i<ionizVec.size(); ++i) {
// 	double energy = kinVec[i];
// 	energy +=muonmass; // kinetic energy = energy - mass
// 	double betagamma = ::sqrt(energy*energy-muonmass*muonmass)/muonmass;
// 	mMap.insert(map<double,double>::value_type(betagamma,ionizVec[i]));
	mMap.insert(map<double,double>::value_type(kinVec[i],ionizVec[i]));
	
    }
    //    ifs.close(); 
    
}
BetheBloch::~BetheBloch(){
    mMap.clear();
}
double BetheBloch::operator() (double betagamma) {

    // if betagamma < 2.5, result should be proportional to
    // 1/beta^2
    double unnormalized = 0;
    if (betagamma < .217) {
	double bg2 = betagamma*betagamma;
	// Absolute Normalization * relative normalization
	unnormalized = (5.39e-4*2.0885e-3)*((1+bg2)/bg2);
    }
    else {
	//
	// betagamma > .8, use table
	//
	map<double,double>::iterator lowb = mMap.lower_bound(betagamma);
	if (lowb == mMap.end())
	    unnormalized =  (*(--lowb)).second; // If past the range, return last value

	else {
	    double bghigh   =  (*lowb).first;
	    double dedxhigh =  (*lowb).second;
	    if (bghigh == betagamma || lowb==mMap.begin())
		unnormalized = dedxhigh;
	    else {
		--lowb;
		double bglow   =  (*lowb).first;   // These variables are not really needed, but  
		double dedxlow =  (*lowb).second;  // these steps make the code more readable.
		double slope = (dedxhigh - dedxlow)/(bghigh - bglow);
		unnormalized = (dedxlow + slope * (betagamma - bglow));	
	    }
	}
    }
	    
    // Normalization factor
    // obtained from fit. 5.3e-4
    //
//     return 5.31e-4*unnormalized;
//     return 5.39e-4*unnormalized;
    // prod period     scale factor from dedxfit.C    normalization
    // P00he           1.1257e-6                      1.0000 (reference)
    // P00hg           1.19346e-6                     1.055

    return 1.055*unnormalized;
}
    
// Double_t BetheBloch::operator() (Double_t betagamma){
//     return (Double_t) operator()((double) betagamma);
// }
Double_t BetheBloch::Sirrf(Double_t Poverm, Double_t Length, Int_t k) {
  Double_t Scale2keV = 1.67180; // scale to get  2.40 keV/cm at b*g = 4
  Double_t par[7] = {
    2.12188e-01,//2.33912e-01, // Scale  
    1.83678e-05, // I      
    1.17380e+01, // Delta  
   -3.52538e-01, // a0     
    9.38373e-02, // a1     
   -7.95122e-03, // a2     
    1.13168e+01  // Delta_e
  };
  Double_t poverm = Poverm;
  if (poverm > 527.5) poverm = 527.5;
  Double_t beta2inv = 1. + 1./(poverm*poverm);
  Double_t gamma  = TMath::Sqrt(poverm*poverm + 1);
  Double_t Lpoverm = TMath::Log(poverm);
  Double_t K      = 0.307075e+3;// keV/g cm^2 
  Double_t A      = 38.691;
  Double_t Z      = 17.436;
  Double_t rho    = 1.5607e-03;//  0.9*0.00166+0.1*0.000667
  Double_t I      = par[1]; //15.8e-6; //15.8; // MeV for Ar, 13.1e-6 MeV for CH4
  Double_t m      =   0.510998902;// MeV electron Mass
  Double_t pim    = 139.570180;// MeV pion Mass
  Double_t Delta;
  Double_t M = pim;
  if (k) {M = m;   Delta = par[6];}
  else   {M = pim; Delta = par[2];}
  Double_t r = m/M;
  Double_t Tmax = 2*m*poverm*poverm/(1. + r*(2*gamma + r));
  Double_t Tupper = Tmax;
  Double_t si = K*Z/A*rho/2*beta2inv*
    (TMath::Log(2*m*poverm*poverm*Tupper/(I*I)) 
     - (1 + Tupper/Tmax)/beta2inv - Delta);
  if (si <= 0) si = 1.e-12;
  Double_t value = par[0] + TMath::Log(si) + 
    Lpoverm*(par[3] + Lpoverm*(par[4] + Lpoverm*par[5]));
  Double_t sirrf =  TMath::Exp(value)*Scale2keV;
   const Int_t Nm = 12;
   Double_t coeff[Nm] = { 
   -3.16420e-01, 6.54653e-02,-4.01169e-03, 1.10047e-04,-1.18392e-06,-7.18814e-09, //hist112
    3.06893e-10,-2.33023e-12,-7.70897e-15, 2.13252e-16,-1.18276e-18, 2.24178e-21
   };
   Double_t X = Length;
  if (X > 130) X = 130;
  Double_t FPARAM = 0;
  for (int i = Nm-1; i >= 0; i--) FPARAM = coeff[i] + X*FPARAM;
  sirrf *= TMath::Exp(FPARAM);
  return sirrf;
}
//________________________________________________________________________________
Double_t  BetheBloch::Girrf(Double_t poverm,  Double_t Tmin, Int_t k) {
  //        returns value of relative ionisation normalised to value
  //        at p/m=4 poverm    p/m (=beta gamma)             (input)
  //        k = 0 (default) pi+/-,K+/-,P/pbar, deuteron, ..
  //        k != 0   e+-
  const Int_t NPart = 2;
  const Int_t NTmin = 7;
  const Int_t Nbg   = 51;
  Double_t si[NPart][NTmin][Nbg] = {
    {
{// PROTON Tmin = 1.e-8 GeV
   0.101943,   0.131933,   0.164116,   0.193259,   0.216897,
   0.235737,   0.250576,   0.261979,   0.270347,   0.276080,
   0.281042,   0.286340,   0.290173,   0.295568,   0.300482,
   0.306566,   0.312465,   0.318372,   0.324223,   0.329744,
   0.335023,   0.340058,   0.344848,   0.349393,   0.353700,
   0.357779,   0.361643,   0.365304,   0.368776,   0.372072,
   0.374913,   0.376389,   0.376593,   0.375846,   0.374266,
   0.371957,   0.369016,   0.365532,   0.361584,   0.357247,
   0.352590,   0.347674,   0.342559,   0.337297,   0.331938,
   0.326529,   0.321112,   0.315727,   0.310410,   0.305197,
   0.300120 },
{// PROTON Tmin = 1.e-7 GeV
   0.347994,   0.357454,   0.371919,   0.385717,   0.395994,
   0.403121,   0.407599,   0.409719,   0.409620,   0.407459,
   0.405736,   0.405501,   0.403439,   0.404088,   0.404118,
   0.406217,   0.408238,   0.410514,   0.413032,   0.415505,
   0.417925,   0.420282,   0.422560,   0.424747,   0.426835,
   0.428824,   0.430713,   0.432507,   0.434210,   0.435828,
   0.437075,   0.437035,   0.435794,   0.433671,   0.430777,
   0.427212,   0.423070,   0.418436,   0.413386,   0.407992,
   0.402319,   0.396428,   0.390375,   0.384211,   0.377984,
   0.371738,   0.365513,   0.359349,   0.353280,   0.347341,
   0.341561 },
{// PROTON Tmin = 1.e-6 GeV
   0.593960,   0.582899,   0.579653,   0.578112,   0.575033,
   0.570452,   0.564575,   0.557418,   0.548858,   0.538810,
   0.530407,   0.524646,   0.516695,   0.512600,   0.507748,
   0.505866,   0.504008,   0.502654,   0.501840,   0.501266,
   0.500827,   0.500505,   0.500272,   0.500101,   0.499971,
   0.499868,   0.499783,   0.499710,   0.499644,   0.499584,
   0.499237,   0.497681,   0.494996,   0.491496,   0.487287,
   0.482467,   0.477124,   0.471339,   0.465187,   0.458736,
   0.452049,   0.445183,   0.438192,   0.431126,   0.424030,
   0.416946,   0.409914,   0.402971,   0.396151,   0.389484,
   0.383002 },
{// PROTON Tmin = 1.e-5 GeV
   0.833507,   0.807579,   0.786688,   0.769868,   0.753491,
   0.737259,   0.721083,   0.704710,   0.687752,   0.669883,
   0.654863,   0.643630,   0.629838,   0.621036,   0.611330,
   0.605482,   0.599758,   0.594782,   0.590641,   0.587023,
   0.583726,   0.580727,   0.577983,   0.575454,   0.573106,
   0.570912,   0.568853,   0.566912,   0.565078,   0.563340,
   0.561399,   0.558327,   0.554198,   0.549321,   0.543798,
   0.537722,   0.531178,   0.524243,   0.516989,   0.509480,
   0.501778,   0.493937,   0.486009,   0.478040,   0.470076,
   0.462155,   0.454316,   0.446594,   0.439021,   0.431628,
   0.424443 },
{// PROTON Tmin = 1.e-4 GeV
   0.839440,   0.852428,   0.868749,   0.883187,   0.893099,
   0.894966,   0.872911,   0.847920,   0.823201,   0.798172,
   0.777163,   0.761008,   0.741863,   0.728709,   0.714420,
   0.704779,   0.695307,   0.686786,   0.679366,   0.672732,
   0.666596,   0.660931,   0.655683,   0.650801,   0.646237,
   0.641954,   0.637921,   0.634114,   0.630511,   0.627096,
   0.623560,   0.618972,   0.613400,   0.607146,   0.600309,
   0.592977,   0.585231,   0.577147,   0.568790,   0.560225,
   0.551508,   0.542692,   0.533826,   0.524955,   0.516121,
   0.507363,   0.498717,   0.490216,   0.481891,   0.473772,
   0.465884 },
{// PROTON Tmin = 1.e-3 GeV
   0.839440,   0.852428,   0.868749,   0.883187,   0.893099,
   0.898899,   0.900993,   0.899481,   0.894283,   0.885457,
   0.877116,   0.862326,   0.842699,   0.828748,   0.812605,
   0.800885,   0.788845,   0.777552,   0.767334,   0.757968,
   0.749173,   0.740953,   0.733272,   0.726078,   0.719325,
   0.712969,   0.706973,   0.701306,   0.695938,   0.690847,
   0.685720,   0.679616,   0.672600,   0.664970,   0.656819,
   0.648232,   0.639285,   0.630050,   0.620592,   0.610969,
   0.601237,   0.591446,   0.581643,   0.571870,   0.562167,
   0.552572,   0.543118,   0.533838,   0.524761,   0.515915,
   0.507326 },
{// PROTON Tmin = 1.e1 GeV
   0.839440,   0.852428,   0.868749,   0.883187,   0.893099,
   0.898899,   0.900993,   0.899481,   0.894283,   0.885457,
   0.878268,   0.874738,   0.867393,   0.866105,   0.863513,
   0.865548,   0.867478,   0.870049,   0.873256,   0.876516,
   0.879759,   0.882956,   0.886067,   0.889055,   0.891893,
   0.894564,   0.897060,   0.899372,   0.901496,   0.903427,
   0.904867,   0.904887,   0.903560,   0.901187,   0.897863,
   0.893675,   0.888699,   0.883011,   0.876680,   0.869776,
   0.862371,   0.854537,   0.846347,   0.837877,   0.829201,
   0.820390,   0.811514,   0.802638,   0.793821,   0.785117,
   0.776576 }},
{
{// ELECTRON Tmin = 1.e-8 GeV
   0.061287,   0.087795,   0.126397,   0.181703,   0.202595,
   0.220138,   0.234761,   0.246683,   0.256100,   0.263201,
   0.268300,   0.272757,   0.278553,   0.283217,   0.289664,
   0.295870,   0.302224,   0.308683,   0.314918,   0.320935,
   0.326709,   0.332211,   0.337422,   0.342334,   0.346949,
   0.351274,   0.355322,   0.359110,   0.362619,   0.365927,
   0.369046,   0.371274,   0.372309,   0.372300,   0.371368,
   0.369625,   0.367170,   0.364099,   0.360497,   0.356446,
   0.352020,   0.347288,   0.342316,   0.337162,   0.331881,
   0.326526,   0.321143,   0.315777,   0.310469,   0.305257,
   0.300178 },
{// ELECTRON Tmin = 1.e-7 GeV
   0.125786,   0.180191,   0.259418,   0.372493,   0.379818,
   0.385913,   0.390665,   0.393878,   0.395446,   0.395244,
   0.393315,   0.391397,   0.392162,   0.391340,   0.393547,
   0.395422,   0.397651,   0.400503,   0.403317,   0.406129,
   0.408914,   0.411632,   0.414253,   0.416758,   0.419139,
   0.421530,   0.423843,   0.426048,   0.428093,   0.430022,
   0.431810,   0.432729,   0.432457,   0.431133,   0.428882,
   0.425818,   0.422050,   0.417685,   0.412824,   0.407566,
   0.401998,   0.396197,   0.390223,   0.384127,   0.377956,
   0.371751,   0.365554,   0.359406,   0.353343,   0.347404,
   0.341621 },
{// ELECTRON Tmin = 1.e-6 GeV
   0.188065,   0.269407,   0.387860,   0.556745,   0.553215,
   0.549427,   0.545222,   0.540270,   0.534318,   0.527012,
   0.518178,   0.509957,   0.505733,   0.499451,   0.497444,
   0.495072,   0.493300,   0.492673,   0.492196,   0.491928,
   0.491837,   0.491870,   0.491977,   0.492123,   0.492286,
   0.492451,   0.492611,   0.492762,   0.492848,   0.492941,
   0.493006,   0.492318,   0.490554,   0.487856,   0.484391,
   0.480299,   0.475593,   0.470347,   0.464633,   0.458521,
   0.452078,   0.445373,   0.438474,   0.431444,   0.424347,
   0.417240,   0.410176,   0.403202,   0.396356,   0.389668,
   0.383168 },
{// ELECTRON Tmin = 1.e-5 GeV
   0.225341,   0.322804,   0.464735,   0.667397,   0.688362,
   0.693097,   0.686526,   0.678489,   0.668279,   0.655910,
   0.641449,   0.627702,   0.618939,   0.607496,   0.601436,
   0.594923,   0.589212,   0.585138,   0.581386,   0.578045,
   0.575080,   0.572423,   0.570010,   0.567790,   0.565726,
   0.563791,   0.561968,   0.560244,   0.558563,   0.556967,
   0.555428,   0.553212,   0.549991,   0.545900,   0.541051,
   0.535547,   0.529485,   0.522951,   0.516028,   0.508790,
   0.501306,   0.493641,   0.485852,   0.477989,   0.470100,
   0.462227,   0.454414,   0.446703,   0.439133,   0.431741,
   0.424562 },
{// ELECTRON Tmin = 1.e-4 GeV
   0.225341,   0.322804,   0.464735,   0.667399,   0.688502,
   0.707020,   0.722967,   0.736155,   0.746477,   0.753312,
   0.754001,   0.738000,   0.728293,   0.714387,   0.705829,
   0.696189,   0.687171,   0.679993,   0.673170,   0.666869,
   0.661084,   0.655754,   0.650813,   0.646201,   0.641872,
   0.637792,   0.633933,   0.630274,   0.626755,   0.623409,
   0.620202,   0.616395,   0.611656,   0.606113,   0.599875,
   0.593041,   0.585702,   0.577942,   0.569838,   0.561459,
   0.552871,   0.544134,   0.535304,   0.526432,   0.517565,
   0.508750,   0.500027,   0.491434,   0.483015,   0.474795,
   0.466821 },
{// ELECTRON Tmin = 1.e-3 GeV
   0.225341,   0.322804,   0.464735,   0.667399,   0.688502,
   0.707020,   0.722967,   0.736155,   0.746477,   0.753660,
   0.757573,   0.761068,   0.768750,   0.772628,   0.781271,
   0.788131,   0.793997,   0.796536,   0.785481,   0.776785,
   0.769068,   0.761861,   0.754986,   0.748371,   0.741985,
   0.735813,   0.729846,   0.724079,   0.718458,   0.713030,
   0.707767,   0.701933,   0.695202,   0.687704,   0.679552,
   0.670848,   0.661690,   0.652161,   0.642342,   0.632307,
   0.622124,   0.611854,   0.601555,   0.591279,   0.581077,
   0.570992,   0.561067,   0.551338,   0.541840,   0.532602,
   0.523648 },
{// ELECTRON Tmin = 1.e1 GeV
   0.225341,   0.322804,   0.464735,   0.667399,   0.688502,
   0.707020,   0.722967,   0.736155,   0.746477,   0.753660,
   0.757573,   0.761068,   0.768750,   0.772628,   0.781271,
   0.788131,   0.793997,   0.799402,   0.803492,   0.806548,
   0.808743,   0.810210,   0.811067,   0.811423,   0.811374,
   0.811006,   0.810387,   0.809573,   0.808541,   0.807405,
   0.806174,   0.804141,   0.801002,   0.796909,   0.791990,
   0.786363,   0.780137,   0.773409,   0.766268,   0.758796,
   0.751070,   0.743158,   0.735125,   0.727027,   0.718920,
   0.710854,   0.702875,   0.695023,   0.687337,   0.679848,
   0.672586 }}
  };
	       
  Double_t beta2inv = 1. + 1./(poverm*poverm);
  Double_t bgL = TMath::Log10 (poverm);
  Double_t  X   = 10*(bgL+1);
  if (X < 0) X = 0;
  if (X > Nbg) X = Nbg;
  Int_t    iX   = (int) X;
  if (iX < 0) iX = 0;
  if (iX > Nbg - 2) iX = Nbg - 2;
  Double_t dX   = X - iX;
  Int_t l = 0;
  if (k != 0) l = 1; // e+/-
  Double_t Y = 8;
  if (Tmin > 0) Y = 8 + TMath::Log10(Tmin);
  if (Y < 0) Y = 0;
  if (Y > 6) Y = 6;
  Int_t iY = (int) Y;
  if (iY < 0) iY = 0;
  if (iY > NTmin-2) iY = NTmin-2;
  Double_t dY = Y - iY;
  Double_t Dx = 1 - dX;
  Double_t Dy = 1 - dY;
  Double_t sirrf =  
    Dx*Dy*si[l][iY][iX]   + dX*Dy*si[l][iY][iX+1] +
    Dx*dY*si[l][iY+1][iX] + dX*dY*si[l][iY+1][iX+1];
  sirrf *=   beta2inv*(bgL + 2.);
#if 0
printf("poverm : %f l : %i iY : %i iX : %i dX = %f dY = %f sirrf = %f Tmin: %f\n",
       poverm,l,iY,iX,dX,dY,sirrf, Tmin);
printf("Dx:%f Dy:%f si[l][iY][iX]:%f\n",Dx,Dy,si[l][iY][iX]);
#endif
  return sirrf;
}
    
