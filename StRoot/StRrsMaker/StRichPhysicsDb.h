/****************************************************************
 * $Id: StRichPhysicsDb.h,v 1.4 2000/02/11 21:10:54 lasiuk Exp $
 *
 * Description:
 *  The two classes defined below, geometryDB and physicsDB,
 *  are interfaces to Star databases for the Rich Raw Data
 *  module. In other words, they hold all the constants 
 *  assossiated with the geometry or the physics of the 
 *  detector.  
 *
 *  Both have common_fill,star_fill and my_fill private
 *  functions, that are called from the ctors. The firsts
 *  hold any member initializations. The second is the 
 *  main way to fill the DBs - from STAR central database.
 *  The last fill is for times where the central DB is
 *  not accessible. 
 *
 *  Both classes are accessible through a static 
 *  member, getDB(), that returns a pointer to the 
 *  only instance. 
 *
 **************************************************************
 * $Log: StRichPhysicsDb.h,v $
 * Revision 1.4  2000/02/11 21:10:54  lasiuk
 * maximum energy probability in access function
 * change electrons/cm and gas gain to 10 pwer 5
 *
 * Revision 1.4  2000/02/11 21:10:54  lasiuk
 * maximum energy probability in access function
 * change electrons/cm and gas gain to 10 pwer 5
 *
 * Revision 1.3  2000/02/08 16:29:35  lasiuk
 * include gasGainAmplification factor here instead of geometry
 *
 * Revision 1.2  2000/01/25 22:02:22  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:04  lasiuk
 * Initial Revision
 *
 *  Revision history:
 *    7/27/1999 First Approach, C & A
 *    8/8/1999 Secondary Revision, C & A
 *    8/10/1999 Static access method getDB() added,
 *                                with help of Valery Fine
 ***********************************************************/

#ifndef ST_RICH_PHYSICS_H
#define ST_RICH_PHYSICS_H

#include <vector>
//#include <memory>

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#ifndef ST_NO_NAMESPACES
//namespace StRichRaw {
#endif
#include "StRichRrsMacros.h"
#include "StRichPhysicsDbInterface.h"

class StRichPhysicsDb : public StRichPhysicsDbInterface {
public:        
    static StRichPhysicsDb* getDb();

    double version() const;

    // Efficiency
    double photonToPadEfficiency() const;
    double photoConversionEfficiency() const;
    double feedBackPhotonProbability() const;

    
    // Ionization
    double polia() const;
    double averageNumberOfInteractions() const;
    double electronDistribution(int)     const;    
    double maximumElectronEnergyProbability()       const;
    double gasGainAmplification()        const;
    // Electronics
    int    averagePedestal()             const;
    double adcConversion()               const;
    int    adcThreshold()                const;
    int    adcChannelWidth()             const;
    double electronicNoiseLevel()        const;    
    double electronCharge()              const;    

    void   print(ostream& os = cout)     const;
    
public:
    double mVersion;
	
    double mPolia;                 // parameter of Polia distribution
    double avg_n_inter;            // average number of interactions
    double mGasGainAmplification;
    double phot2pad;               // efficiency of secondary photon hiting CsI pad
    double phot2elec;              // efficency of a secondary photon kicking out
                                   // an electron from CsI
    double avl2phot;               // probability of feedback photons from avalanche
    double electric_noise;         // electronic noise simulation 
    //double e_charge;               // charge of electron [coulomb]
    
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<double> e_distribut;  // distribution of e- per interactions
#else
    vector<double, allocator<double> > e_distribut;
#endif
	
    // contains a serie of increasing numbers related 
    // to the distribution. (cf.definition in .cxx) 
    double e_max;                  // maximum value in e_distribut 
	
    int    pedestal;                  // average channel pedestal
    double adc_factor;                // multiplication factor for ADC conversion
    int    adc_threshold;             // lower threshold for adc counts
    int    channel_width;             // adc channel width in bits
    
protected:
    StRichPhysicsDb();
	
private: 
    void common_fill();            // common fill between my_fill and star_fill
    void star_fill();              // fill DB from star central DB 
    void my_fill();                // fill DB with my own stuff 
	
    static StRichPhysicsDb* p2Db;       // handle to only instance 
}; 
inline double StRichPhysicsDb::version() const {return mVersion;}
inline double StRichPhysicsDb::polia() const { return mPolia;}
inline double StRichPhysicsDb::averageNumberOfInteractions() const { return avg_n_inter;}
inline double StRichPhysicsDb::photonToPadEfficiency() const { return phot2pad;}
inline double StRichPhysicsDb::photoConversionEfficiency() const { return phot2elec;}
inline double StRichPhysicsDb::feedBackPhotonProbability() const { return avl2phot;}
inline double StRichPhysicsDb::electronicNoiseLevel() const { return electric_noise;}    
//inline double StRichPhysicsDb::electronCharge() const { return e_charge;}    
inline double StRichPhysicsDb::electronDistribution(int i) const { return e_distribut[i];}    
inline double StRichPhysicsDb::gasGainAmplification() const { return mGasGainAmplification;}

inline double StRichPhysicsDb::maximumElectronEnergyProbability() const { return e_max;}
inline int StRichPhysicsDb::averagePedestal() const { return pedestal;}
inline double StRichPhysicsDb::adcConversion() const { return adc_factor;}	
inline int StRichPhysicsDb::adcThreshold() const { return adc_threshold;}
inline int StRichPhysicsDb::adcChannelWidth() const { return channel_width;}

#ifndef ST_NO_NAMESPACES
//}
#endif

#endif
