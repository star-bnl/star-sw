/****************************************************************
 * $Id: StRichPhysicsDb.h,v 1.1 2000/01/18 21:32:04 lasiuk Exp $
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
 * Revision 1.1  2000/01/18 21:32:04  lasiuk
 * Initial Revision
 *
 *
 * Revision 1.2  2000/01/25 22:02:22  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:04  lasiuk
 * 
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
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
#include <vector>
//#include <memory>

class StRichPhysicsDb : public StRichPhysicsDbInterface {
    class StRichPhysicsDb {
    public:    
    double maximumElectronEnergy() const;
	static StRichPhysicsDb* getDb();
	
	double version;
    double electronicNoiseLevel()        const;    
	double polia;                  // parameter of Polia distribution
	double mVersion;
	
	double mPolia;                  // parameter of Polia distribution
	double avg_n_inter;            // average number of interactions
	
	double phot2pad;               // efficiency of secondary photon hiting CsI pad
	
	double phot2elec;              // efficency of a secondary photon kicking out
	// an electron from CsI
	
	double avl2phot;               // probability of feedback photons from avalanche
	double electric_noise;         // electronic noise simulation 
	double e_charge;               // charge of electron [coulomb]
    void   print(ostream& os = cout)     const;
    double avl2phot;               // probability of feedback photons from avalanche
	vector<double> e_distribut;  // distribution of e- per interactions
    //double e_charge;               // charge of electron [coulomb]
	vector<double, allocator<double> > e_distribut;
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<double> e_distribut;  // distribution of e- per interactions
	// contains a serie of increasing numbers related 
	// to the distribution. (cf.definition in .cxx) 
	double e_max;                  // maximum value in e_distribut 
	
	int pedestal;                  // average channel pedestal
	double adc_factor;             // multiplication factor for ADC conversion
	int adc_threshold;             // lower threshold for adc counts
	int channel_width;             // adc channel width in bits
	
    protected:
	StRichPhysicsDb();
    int    channel_width;             // adc channel width in bits
    private: 
	void common_fill();            // common fill between my_fill and star_fill
	void star_fill();              // fill DB from star central DB 
    }; 
    

inline double StRichPhysicsDb::maximumElectronEnergyProbability() const { return e_max;}
inline int StRichPhysicsDb::averagePedestal() const { return pedestal;}
inline double StRichPhysicsDb::adcConversion() const { return adc_factor;}	
inline int StRichPhysicsDb::adcThreshold() const { return adc_threshold;}
inline int StRichPhysicsDb::adcChannelWidth() const { return channel_width;}

#ifndef ST_NO_NAMESPACES
//}
#endif

#endif
