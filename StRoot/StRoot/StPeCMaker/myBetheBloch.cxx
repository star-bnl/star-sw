//
// $Id myBetheBloch.cc $
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

#include <vector>
#include "myBetheBloch.h"

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

ClassImp(myBetheBloch)

myBetheBloch::myBetheBloch(){
    // it's more elegant to read the values from a file, but to put it
    // in the StarClassLibrary without extra files, we have to dump the numbers
    // here
    // could also fill them directly in the map, or transform the numbers directly
    // to beta gamma.
    
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
myBetheBloch::~myBetheBloch(){
    mMap.clear();
}
double myBetheBloch::operator() (double betagamma) {

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
    
// Double_t myBetheBloch::operator() (Double_t betagamma){
//     return (Double_t) operator()((double) betagamma);
// }
