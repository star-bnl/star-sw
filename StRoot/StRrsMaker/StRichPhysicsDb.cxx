/*******************************************************************
 * $Id: StRichPhysicsDb.cxx,v 1.3 2000/02/08 16:29:33 lasiuk Exp $
 *
 * Description:
 *  Implementation of the two databases modules
 *
 *******************************************************************
 * $Log: StRichPhysicsDb.cxx,v $
 * Revision 1.3  2000/02/08 16:29:33  lasiuk
 * include gasGainAmplification factor here instead of geometry
 *
 *
 * Revision 1.3  2000/02/08 16:29:33  lasiuk
 * include gasGainAmplification factor here instead of geometry
 *
 * Revision 1.2  2000/01/25 22:02:22  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:03  lasiuk
 * Initial Revision
 *
 *******************************************************************/

// STL
#include <algorithm>
#include <iostream.h>

#ifndef ST_NO_NAMESPACES
using std::max_element;
#endif

//SCL
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif

//RRS
#include "StRichPhysicsDb.h"

StRichPhysicsDb  *StRichPhysicsDb::p2Db  = 0;
    
StRichPhysicsDb::StRichPhysicsDb()
    : e_distribut(21)
{    
    //star_fill();
    my_fill();
    common_fill();
}


void StRichPhysicsDb::my_fill()
    avg_n_inter     = 25/centimeter;
    mGasGainAmplification = 1.0e4;
    
    mPolia           = 0.3;
    avg_n_inter     = 15.3/centimeter;
    mGasGainAmplification = 1.0e5;
    
    phot2elec       = 0.27;
    phot2pad        = 0.75;
    avl2phot        = 7.7e-6;
    
    electric_noise  = 800.0;
    pedestal        = 50;
    adc_factor      = 0.16556*(1.e-15*coulomb);      // adc/femtocoulomb
    adc_threshold   = 5;
    channel_width   = 10;           // bits
    //e_charge        = 1.602e-19;
    
}

void StRichPhysicsDb::common_fill()
{
    e_distribut[0]  = 79.4;
    e_distribut[1]  = 91.4;
    e_distribut[2]  = 94.8;
    e_distribut[3]  = 96.4;
    e_distribut[4]  = 97.35;
    e_distribut[5]  = 97.95;
    e_distribut[6]  = 98.39;
    e_distribut[7]  = 98.73;
    e_distribut[8]  = 99.00;
    e_distribut[9]  = 99.21;
    e_distribut[10] = 99.38;
    e_distribut[11] = 99.51;
    e_distribut[12] = 99.61;
    e_distribut[13] = 99.69;
    e_distribut[14] = 99.75;
    e_distribut[15] = 99.80;
    e_distribut[16] = 99.842;
    e_distribut[17] = 99.879;
    e_distribut[18] = 99.912;
    e_distribut[19] = 99.941;
    e_distribut[20] = 99.941;
    
    // find max of e_distribut
    e_max = *(max_element( &e_distribut[0], &e_distribut[20] ));
    
}

StRichPhysicsDb* StRichPhysicsDb::getDb()
{
    if(!p2Db)
	p2Db = new StRichPhysicsDb();
    return p2Db;
}

void StRichPhysicsDb::print(ostream& os) const
{
    os << "** StRichPhysicsDb::print() ** " << endl;
    os << " version: " << version() << endl;
    os << "maximumElectronEnergy=     "  << maximumElectronEnergy() << endl;
    os << "Ionization:" << endl;
    os << "polia= " << polia() << endl;
    os << "averageNumberOfInteraction= " << (averageNumberOfInteractions()/centimeter) << " /cm" << endl;
    os << "maximumElectronEnergyProbability=     "  << maximumElectronEnergyProbability() << endl;
    os << "gas Gain Amplification=    "  << gasGainAmplification()  << endl;
    for(int i=0; i<e_distribut.size(); i++) {
	os << "electronDistribution[" << i << "]= " << electronDistribution(i) << endl;
    }

    os << "\nEfficiency:" << endl;
    os << "photonToPadEfficiency=     " << photonToPadEfficiency() << endl;
    os << "photoConversionEfficiency= " << photoConversionEfficiency() << endl;
    os << "feedBackPhotonProbability= " << feedBackPhotonProbability() << endl;

    os << "\nElectronics:" << endl;
    os << "averagePedestal= "      << averagePedestal()               << " channels" << endl;
    os << "adcConversion= "        << (adcConversion()/(1.e-15*coulomb)) << " ADC/fC"   << endl;
    os << "adcThreshold= "         << adcThreshold()                  << " channels" << endl;
    os << "adcChannelWidth= "      << adcChannelWidth()               << " channels" << endl;
    os << "electronicNoiseLevel= " << electronicNoiseLevel() << endl;
    //os << "electronCharge= " << electronCharge() << endl;
    os << "\n***************** End of Physics DB ********************************\n" << endl;
}
#ifndef ST_NO_NAMESPACES
//}
#endif
