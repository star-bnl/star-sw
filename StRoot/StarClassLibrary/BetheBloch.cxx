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
// 3) calculate beta*gamma = p/m where p = sqrt(e^2 - m^2)
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
#include "BetheBloch.h"
#include "StMuonMinus.hh"

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

ClassImp(BetheBloch)

BetheBloch::BetheBloch(){
    // it's more elegant to read the values from a file, but to put it
    // in the StarClassLibrary without extra files, we have to dump the numbers
    // here
    // could also fill them directly in the map, or transform the numbers directly
    // to beta gamma.
    
    const double muonmass = StMuonMinus::instance()->mass();
    vector<double> kinVec;
    kinVec.push_back(.11295E-04 );
    kinVec.push_back(.14219E-04 );
    kinVec.push_back(.17901E-04 );
    kinVec.push_back(.22536E-04 );
    kinVec.push_back(.28371E-04 );
    kinVec.push_back(.35717E-04 );
    kinVec.push_back(.44965E-04 );
    kinVec.push_back(.56607E-04 );
    kinVec.push_back(.71264E-04 );
    kinVec.push_back(.89716E-04 );
    kinVec.push_back(.11295E-03 );
    kinVec.push_back(.14219E-03 );
    kinVec.push_back(.17901E-03 );
    kinVec.push_back(.22536E-03 );
    kinVec.push_back(.28371E-03 );
    kinVec.push_back(.35717E-03 );
    kinVec.push_back(.44965E-03 );
    kinVec.push_back(.56607E-03 );
    kinVec.push_back(.71264E-03 );
    kinVec.push_back(.89716E-03 );
    kinVec.push_back(.11295E-02 );
    kinVec.push_back(.14219E-02 );
    kinVec.push_back(.17901E-02 );
    kinVec.push_back(.22536E-02 );
    kinVec.push_back(.28371E-02 );
    kinVec.push_back(.35717E-02 );
    kinVec.push_back(.44965E-02 );
    kinVec.push_back(.56607E-02 );
    kinVec.push_back(.71264E-02 );
    kinVec.push_back(.89716E-02 );
    kinVec.push_back(.11295E-01 );
    kinVec.push_back(.14219E-01 );
    kinVec.push_back(.17901E-01 );
    kinVec.push_back(.22536E-01 );
    kinVec.push_back(.28371E-01 );
    kinVec.push_back(.35717E-01 );
    kinVec.push_back(.44965E-01 );
    kinVec.push_back(.56607E-01 );
    kinVec.push_back(.71264E-01 );
    kinVec.push_back(.89716E-01 );
    kinVec.push_back(.11295     );
    kinVec.push_back(.14219     );
    kinVec.push_back(.17901     );
    kinVec.push_back(.22536     );
    kinVec.push_back(.28371     );
    kinVec.push_back(.35717     );
    kinVec.push_back(.44965     );
    kinVec.push_back(.56607     );
    kinVec.push_back(.71264     );
    kinVec.push_back(.89716     );
    kinVec.push_back(1.1295     );
    kinVec.push_back(1.4219     );
    kinVec.push_back(1.7901     );
    kinVec.push_back(2.2536     );
    kinVec.push_back(2.8371     );
    kinVec.push_back(3.5717     );
    kinVec.push_back(4.4965     );
    kinVec.push_back(5.6607     );
    kinVec.push_back(7.1264     );
    kinVec.push_back(8.9716     );
    kinVec.push_back(11.295     );
    kinVec.push_back(14.219     );
    kinVec.push_back(17.901     );
    kinVec.push_back(22.536     );
    kinVec.push_back(28.371     );
    kinVec.push_back(35.717     );
    kinVec.push_back(44.965     );
    kinVec.push_back(56.607     );
    kinVec.push_back(71.264     );
    kinVec.push_back(89.716     );
    kinVec.push_back(112.95     );
    kinVec.push_back(142.19     );
    kinVec.push_back(179.01     );
    kinVec.push_back(225.36     );
    kinVec.push_back(283.71     );
    kinVec.push_back(357.17     );
    kinVec.push_back(449.65     );
    kinVec.push_back(566.07     );
    kinVec.push_back(712.64     );
    kinVec.push_back(897.16     );
    kinVec.push_back(1129.5     );
    kinVec.push_back(1421.9     );
    kinVec.push_back(1790.1     );
    kinVec.push_back(2253.6     );
    kinVec.push_back(2837.1     );
    kinVec.push_back(3571.7     );
    kinVec.push_back(4496.5     );
    kinVec.push_back(5660.7     );
    kinVec.push_back(7126.4     );
    //
    vector<double> ionizVec;
    
    ionizVec.push_back(.75591    );
    ionizVec.push_back(.73857    );
    ionizVec.push_back(.69776    );
    ionizVec.push_back(.63873    );
    ionizVec.push_back(.57037    );
    ionizVec.push_back(.50158    );
    ionizVec.push_back(.43821    );
    ionizVec.push_back(.38250    );
    ionizVec.push_back(.33417    );
    ionizVec.push_back(.29199    );
    ionizVec.push_back(.25472    );
    ionizVec.push_back(.22149    );
    ionizVec.push_back(.19175      );   
    ionizVec.push_back(.16518    );
    ionizVec.push_back(.14223    );
    ionizVec.push_back(.12192    );
    ionizVec.push_back(.10391    );
    ionizVec.push_back(.88120E-01);
    ionizVec.push_back(.74425E-01);
    ionizVec.push_back(.62643E-01);
    ionizVec.push_back(.51996E-01);
    ionizVec.push_back(.43716E-01);
    ionizVec.push_back(.36686E-01);
    ionizVec.push_back(.30732E-01);
    ionizVec.push_back(.25709E-01);
    ionizVec.push_back(.21490E-01);
    ionizVec.push_back(.17960E-01);
    ionizVec.push_back(.15019E-01);
    ionizVec.push_back(.12577E-01  );   
    ionizVec.push_back(.10556E-01);
    ionizVec.push_back(.88891E-02);
    ionizVec.push_back(.75194E-02);
    ionizVec.push_back(.63980E-02);
    ionizVec.push_back(.54839E-02);
    ionizVec.push_back(.47424E-02);
    ionizVec.push_back(.41449E-02);
    ionizVec.push_back(.36676E-02);
    ionizVec.push_back(.32904E-02);
    ionizVec.push_back(.29971E-02);
    ionizVec.push_back(.27741E-02);
    ionizVec.push_back(.26101E-02);
    ionizVec.push_back(.24955E-02);
    ionizVec.push_back(.24225E-02);
    ionizVec.push_back(.23841E-02);
    ionizVec.push_back(.23744E-02);
    ionizVec.push_back(.23883E-02);
    ionizVec.push_back(.24211E-02);
    ionizVec.push_back(.24690E-02  );   
    ionizVec.push_back(.25288E-02);
    ionizVec.push_back(.25977E-02);
    ionizVec.push_back(.26733E-02);
    ionizVec.push_back(.27540E-02);
    ionizVec.push_back(.28383E-02);
    ionizVec.push_back(.29251E-02);
    ionizVec.push_back(.30134E-02);
    ionizVec.push_back(.31026E-02);
    ionizVec.push_back(.31921E-02);
    ionizVec.push_back(.32814E-02);
    ionizVec.push_back(.33702E-02);
    ionizVec.push_back(.34582E-02);
    ionizVec.push_back(.35453E-02);
    ionizVec.push_back(.36259E-02);
    ionizVec.push_back(.37002E-02);
    ionizVec.push_back(.37693E-02);
    ionizVec.push_back(.38333E-02);
    ionizVec.push_back(.38926E-02);
    ionizVec.push_back(.39476E-02);
    ionizVec.push_back(.39984E-02);
    ionizVec.push_back(.40454E-02);
    ionizVec.push_back(.40890E-02);
    ionizVec.push_back(.41296E-02);
    ionizVec.push_back(.41673E-02);
    ionizVec.push_back(.42025E-02);
    ionizVec.push_back(.42356E-02);
    ionizVec.push_back(.42668E-02);
    ionizVec.push_back(.42963E-02);
    ionizVec.push_back(.43245E-02);
    ionizVec.push_back(.43516E-02);
    ionizVec.push_back(.43779E-02);
    ionizVec.push_back(.44036E-02);
    ionizVec.push_back(.44289E-02);
    ionizVec.push_back(.44540E-02);
    ionizVec.push_back(.44792E-02);
    ionizVec.push_back(.45043E-02);
    ionizVec.push_back(.45293E-02);
    ionizVec.push_back(.45544E-02);
    ionizVec.push_back(.45794E-02);
    ionizVec.push_back(.46044E-02);
    ionizVec.push_back(.46294E-02);
   
    for(size_t i=0; i<ionizVec.size(); ++i) {
	double energy = kinVec[i];
	energy +=muonmass; // kinetic energy = energy - mass
	double betagamma = sqrt(energy*energy-muonmass*muonmass)/muonmass;
	mMap.insert(map<double,double>::value_type(betagamma,ionizVec[i]));
	
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
    if (betagamma < 2.5) {
	double bg2 = betagamma*betagamma;
	// Absolute Normalization * relative normalization
	unnormalized = (2.0885e-3)*((1+bg2)/bg2);
    }
    else {
	//
	// betagamma > 2.5, use table
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
    return 5.3e-4*unnormalized;
}
    
// Double_t BetheBloch::operator() (Double_t betagamma){
//     return (Double_t) operator()((double) betagamma);
// }
