#include "CalibrationHelperFunctions.h"
#include <cmath>
#include <iostream>
#include <fstream>
using namespace std;

CalibrationHelperFunctions::CalibrationHelperFunctions(){	
  ifstream read_mapping;
	
  //try to read mapping from local disk
  read_mapping.open("btow_mapping.20030401.0.txt");
  if(!read_mapping.good()){
    cout<<"CalibrationHelperFunctions - FATAL ERROR: could not read mapping!.  Answers will be nonsense"<<endl;
  }
  else{
    while (1) {
      int id;
      double eta, phi, daqId, crate, crate_seq, module, etaDiv, subDiv, tdc, pmt, position, patchId;
      read_mapping >> id >> eta >> phi >> daqId >> crate >> crate_seq >> module >> etaDiv >> subDiv >> tdc >> pmt >> position >> patchId;
      if (!read_mapping.good() || id > ntowers) break;
      tower_eta[id-1] = eta;
      tower_phi[id-1] = phi;
      tower_theta[id-1] = 2*atan(exp(-1*eta));
    }
  }
}

CalibrationHelperFunctions::~CalibrationHelperFunctions(){
	
}

bool CalibrationHelperFunctions::isGoodTower2004(int towerid){
  switch(towerid){
  default:
    return true;
  }
}

bool CalibrationHelperFunctions::isGoodTower2005(int towerid){
  if(isBadIso2005(towerid) || isBadPoverE2005(towerid)) return false;
	
  switch(towerid){
    //bad towers
  case(36): case(45): case(46): case(47): case(48): case(50): case(59): case(63): case(139): case(220):
  case(297): case(303): case(389): case(411): case(426): case(446): case(448): case(485): case(492):
  case(528): case(559): case(565): case(638): case(671): case(691): case(738): case(744): case(761):
  case(846): case(855): case(875): case(897): case(916): case(1028): case(1080): case(1100): case(1104):
  case(1160): case(1176): case(1220): case(1280): case(1317): case(1397): case(1398): case(1400): 
  case(1417): case(1419): case(1420): case(1440): case(1471): case(1505): case(1612): case(1668):
  case(1676): case(1679): case(1720): case(1764): case(1856): case(1866): case(1880): case(1909): 
  case(2069): case(2074): case(2075): case(2079): case(2092): case(2161): case(2168): case(2241):
  case(2257): case(2285): case(2378): case(2394): case(2403): case(2409): case(2458): case(2460):
  case(2470): case(2504): case(2529): case(2592): case(2610): case(2658): case(2794): case(2834):
  case(2835): case(2863): case(2865): case(2897): case(2961): case(2969): case(2972): case(3069):
  case(3086): case(3093): case(3097): case(3232): case(3255): case(3283): case(3289): case(3309): 
  case(3372): case(3407): case(3515): case(4171): case(4217): case(4232): case(4240): case(4312): 
  case(4357): case(4377): case(4506): case(4507): case(4508): case(4514): case(4519): case(4543): 
  case(4558): case(4560): case(4580): case(4585): case(4639): case(4660): case(4671): case(4678): 
  case(4766): case(4768):
    return false;
			
  default:
    return true;
  }
}

bool CalibrationHelperFunctions::isBadIso2005(int id){
  switch(id){
    //isolation cut failures
  case(34): case(35): case(266): case(267): case(286): case(287): case(561): case(562):
  case(615): case(616): case(633): case(653): case(637): case(657): case(649): case(650):
  case(673): case(674): case(813): case(814): case(837): case(857): case(953): case(954): 
  case(1026): case(1046): case(1353): case(1354): case(1574): case(1575): case(1753): case(1773):
  case(1765): case(1766): case(1897): case(1898): case(2073): case(2093): case(2077): case(2097): 
  case(2440): case(2460): case(2589): case(2590): case(3070): case(3071): case(3494): case(3495):
  case(4677): case(4678):
    return true;
			
  default:
    return false;
  }
}

bool CalibrationHelperFunctions::isBadPoverE2005(int id){
  switch(id){
    //poverE failures
  case(30): case(95): case(108): case(162): case(308): case(533): case(555): case(750):
  case(762): case(779): case(873): case(882): case(899): case(1024): case(1130): case(1132): 
  case(1197): case(1204): case(1217): case(1237): case(1257): case(1294): case(1306): case(1375):
  case(1434): case(1487): case(1537): case(1709): case(1984): case(2043): case(2162): case(2339):
  case(2392):
    return true;
			
  default:
    return false;
  }
}

bool CalibrationHelperFunctions::isGoodTower2006(int id){
  switch(id){
    //copy bad towers from 2005, remove a bunch of good ones
  case(50): case(139): case(220):
  case(389): case(411): case(426): case(446): case(485): case(492):
  case(638): case(671):
  case(846): case(855): case(875): case(916): case(1028): case(1080): case(1100):
  case(1160): case(1176): case(1220): case(1280): case(1397):  
  case(1612): case(1668):
  case(1720): case(1856): case(1880):  
  case(2074): case(2075): case(2079): case(2092): case(2168):
  case(2257): case(2394): case(2403): case(2409): case(2458):
  case(2470): case(2504): case(2529): case(2610): case(2658): case(2794): case(2834):
  case(2865): case(2897): case(2961): case(2969): case(2972):
  case(3097): case(3255): case(3289):
  case(3372): case(3515): case(4171): case(4217): case(4240):
  case(4357): case(4377): case(4506): case(4507): case(4508): case(4514): case(4543): 
  case(4560): case(4580): case(4585): case(4671):
  case(4766): case(4768):
    return false;
			
    //copy bad iso from 2005, dropped 1897-8 (should this have been 1877-8?)
  case(34): case(35): case(266): case(267): case(286): case(287): case(561): case(562):
  case(615): case(616): case(633): case(653): case(637): case(657): case(649): case(650):
  case(673): case(674): case(813): case(814): case(837): case(857): case(953): case(954): 
  case(1026): case(1046): case(1353): case(1354): case(1574): case(1575): case(1753): case(1773):
  case(1765): case(1766): case(2073): case(2093): case(2077): case(2097): 
  case(2440): case(2460): case(2589): case(2590): case(3070): case(3071): case(3494): case(3495):
  case(4677): case(4678):
    return false;
		
    //new bad towers
  case(240): case(390): case(391): case(392): case(409): case(410): case(412): case(504): case(541): case(594):
  case(629): case(639): case(647): case(681): case(749): case(760): case(839): case(840): case(844): 
  case(859): case(873): case(880): case(933): case(1018): case(1125): case(1142): case(1143): 
  case(1158): case(1159): case(1161): case(1162): case(1163): case(1171): case(1180): case(1198):
  case(1217): case(1224): case(1225): case(1237): case(1240): case(1244): case(1250):
  case(1301): case(1319): case(1321): case(1341): case(1342): case(1348): case(1375): case(1381): case(1401):
  case(1422): case(1507): case(1588): case(1608): case(1654): case(1732): case(1779): case(1838): 
  case(1892): case(1893): case(1949): case(1985): case(2005): case(2021): case(2025): case(2070): case(2085):
  case(2094): case(2095): case(2101): case(2105): case(2108): case(2116): case(2177): case(2188): case(2196):
  case(2222): case(2262): case(2301): case(2305): case(2337): case(2453): case(2580): case(2633): case(2652):
  case(2727): case(3007): case(3017): case(3154): case(3171): case(3220): case(3231): case(3287): case(3290):
  case(3296): case(3328): case(3333): case(3493): case(3508): case(3544): case(3557): case(3588): case(3604):
  case(3611): case(3653): case(3678): case(3679): case(3709): case(3715): case(3727): case(3745): case(3746):
  case(3761): case(3769): case(3795): case(3803): case(3986): case(4014): case(4105): case(4016): case(4107): 
  case(4018): case(4019): case(4020): case(4053): case(4054): case(4055): case(4056): case(4057): case(4075): 
  case(4077): case(4078): case(4080): case(4100): case(4119): case(4120): case(4130): case(4174): case(4326):
  case(4388): case(4459): case(4464): case(4505): case(4549): case(4569): case(4596): case(4672): case(4684):
  case(4765):
    return false;
			
    //a few more I missed the first time around
  case(341):  case(900):  case(1044): case(1063): case(1078): case(1850): case(1877): case(1878): case(3738):
  case(4015): case(4017): case(1048): case(3690): case(3718):
    return false;
			
  default:
    return true;
  }
}	
