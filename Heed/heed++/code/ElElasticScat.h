#ifndef ELELASCTICSCAT_H
#define ELELASCTICSCAT_H
#include "wcpplib/util/String.h"
#include "wcpplib/safetl/AbsArr.h"
#include "wcpplib/safetl/AbsPtr.h"

/*
Definition of elastic scattering for low-energy delta-electron.

2003, I. Smirnov
*/

// People, who does not want to link with graphics,
// can uncomment the following macro.
// The result will be the disappearance of the two functions
// from class ElElasticScat, which are the only functions in heed++,
// which call histdef.
// These functions were used at preparation of the condensed simulation
// scheme, namely fitting condensed cross-sections.
// During actual simulations these two functions are not necessary.
//#define EXCLUDE_FUNCTIONS_WITH_HISTDEF

// Fit parameters for particular element and energy,
// The fit gives dependence of cross section on angle.
class ElElasticScatDataStruct
{public:
  double A[4];  // in -1.0 then the combination is not valid
  double C[7];
  double B;
  //double D[2];  // not necessary - it is quality of fit
  double CS(double theta);  // return -1 if not valid
};

// Contains array of the structures defined above for a set of energies.
class ElElasticScatData
{public:
  long Z;
  DynLinArr< ElElasticScatDataStruct > data;  // dimension: different energies
  ElElasticScatData(void): Z(0) {;}
  ElElasticScatData(long fZ, long qe): Z(fZ), data(qe) {;}
};


const double low_cut_angle_deg = 20.0; // degrees

// The following class presentes the data for any atom.
// One object contains all data and presents them by request.

class ElElasticScat: public RegPassivePtr
{public:
  double get_CS(long Z, double energy,  // kinetic energy in MeV 
		double angle, 
		int s_interp=0);  // the last parameter is only for
  // debug and for various checks.
  // In particular, fill_hist call this function with s_interp=1
  // for histograms "int...".
  // Return value is in angstrom^2/srad

  double get_CS_Rutherford(long Z, double energy,  // kinetic energy in MeV 
			   double angle);  // internal units (radian)
  // Return value is in angstrom^2
  long get_qe(void) const { return qe;}
  double get_energy_mesh(long ne) const { return energy_mesh[ne];}

  ElElasticScat(void): atom(0) {;}
  ElElasticScat(const String& file_name);
  void print(std::ostream& file, int l) const ;
#ifndef EXCLUDE_FUNCTIONS_WITH_HISTDEF
  void fill_hist(void);
  // Makes a package of histograms for all atoms for which the
  // fit is presented in the data file.
  // There are 6 types of histograms:
  // raw..., cor..., corrad, int..., rut..., rutrad
  // Difference between raw and cor I forgot for the moment.
  // The plots looks the same but created by diffeent manner.
  // raw by the call of atom[na].data[ne].CS(angle/180.0 * M_PI );
  // cor by the normal call: 
  // get_CS(atom[na].Z, energyMeV, angle/180.0 * M_PI );
  // int is produced by interpolation between neighboring presented atoms.
  // It is useful, in particular, to check the precision of interpolation.
  // rut is Rutherford cross section.
  // histograms with suffix rad are the same but with factor 
  //  2.0 * M_PI * sin(anglerad)
  // path length is inverse linear coefficient of absorption
  // for unit A and dencity ( they are not known in this program)
  // So you should multiply by A and divide by dencity in gr/cm3.

  void fill_hist_low_scat(const String& file_name,
			  const String& file_name_dist);
  // It fills some histograms and write file with tables
  // energy vs coefficient which gives dependency (proportional) 
  // of root from dispertion
  // on number of interactions.
  // If file_name_dist != "" and "none",
  // the program will write there the shapes of distributions.
  // As far as I now understood, the second file is not used.
#endif
private:
  long qe;   // number of energies (local mesh)
  DynLinArr< double > energy_mesh;  // KeV
  DynLinArr< double > gamma_betta2;  // gamma * betta2 for electron of 
  // this energy
  DynLinArr< ElElasticScatData > atom;   // dimension: different atoms
  double get_CS_for_presented_atom(long na, 
				   double energy,  // kinetic energy in MeV 
				   double angle);
};

class ElElasticScatLowSigma: public RegPassivePtr
{public:
  /*
  // perhaps it is not necessary right here.
  // For real matter in needs to mix the atoms.
  // It is more conveniently to do in HeedDeltaElectronCS.*
  // for new format:
  double get_mean(  // return value in radians
		   long Z, double energy,  // MeV 
		   double nscat) const;  // number of scatterings may be 
                                         // not entire
  // old method:
  double get_sigma(  // return value in radians
		   long Z, double energy,  // MeV 
		   double nscat) const;  // number of scatterings may be 
                                         // not entire
  // this is more precise for little stept
  */

  double get_mean_coef(long Z, long ne)const { return mean_coef[Z-1][ne]; }
  double get_coef(long Z, long ne)const { return coef[Z-1][ne]; }
  long get_qscat(void) const {return qscat; }
  ElElasticScat* get_ees(void) const {return ees.get(); } 
  ElElasticScatLowSigma(void){;}
  ElElasticScatLowSigma(ElElasticScat* fees, const String& file_name);
private:
  PassivePtr<ElElasticScat> ees;
  long qat;  // number of atoms registered in this class (Z is sequencial)
  long qscat;  // maximal number of scaterings
  // qe and energies are taken form ElElasticScat
  //DynLinArr< long > zat;


  // addition for new format:  
  DynLinArr <DynLinArr< double > > mean_coef; // mean( (1-cos(theta)) )
  // old format:
  DynLinArr <DynLinArr< double > > coef;      // sqrt(mean( (1-cos(theta))^2 ))
  // first index - number of atom = z - 1
  // second index - energy

};

/*
class ElElasticScatLowSigma: public RegPassivePtr
{public:
  double get_sigma(  // return value in radians
		   long Z, double energy,  // MeV 
		   long nscat) const;
  ElElasticScatLowSigma(void){;}
  ElElasticScatLowSigma(ElElasticScat* fees, const String& file_name);
private:
  PassivePtr<ElElasticScat> ees;
  long qat;
  long qscat;
  // qe and energies are taken form ElElasticScat
  //DynLinArr< long > zat;
  DynLinArr< DynLinArr <DynLinArr< double > > > sigma;
  // first index - number of atom = z - 1
  // second index - energy
  // third index - scattering number - 1
};
*/

#endif
