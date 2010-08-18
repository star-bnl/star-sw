#ifndef ENTRANFCS_H
#define ENTRANFCS_H
#include "heed++/code/HeedMatterDef.h"

/*
The PAI cross section of energy transfers from charged particle
to media.
The particle has fixed parameters: energy, speed, etc., which
are not affected to energy transfers, since they are considered
too little if compared with the particle energy.
 
2003, I. Smirnov
*/

//#define DEBUG_EnTransfCS  // allows to print additional information
// and keeps it in class, which makes it bigger

#define EXCLUDE_A_VALUES  // exclude absorption values
#define EXCLUDE_VAL_FADDA  // exclude unecessary for MC values
//#define EXCLUDE_MEAN  // exclude calculations of means, which are 
// unnecessary for MC

#define CLEAR_ARRAYS

class EnTransfCS: public RegPassivePtr  // Energy Transfer Cross Section
{public:
  double particle_mass;     // MeV
  double particle_tkin;     // MeV  kinetic energy
  double particle_ener;     // MeV  total energy
  long particle_charge;     // in the charge of electron units.
  // it is squared, therefore the sign does not matter.
 
  double betta;
  double betta2;   // betta^2
  double betta12;  // 1 - betta^2
  double gamma;  // 
  double gamma_1;  // gamma - 1 : the best dimensionless measurement of speed  
  double maximal_energy_trans;  // MeV
  int s_simple_form;   // controls the form of Rezerford scattering
  // For our purposes it is good to have simple form, 
  // so this variable is initialized to 1.
  // Simple form means that there are two terms.
  // The third term is assumed zero.

  int s_primary_electron;  // sign that the primary particle is the electron
  PassivePtr< HeedMatterDef > hmd;
  // In the following arrays there is the only index: the energy.
  // The meaning: the average value on the energy interval.
  DynLinArr< double >  log1C;  // common first log without cs
  DynLinArr< double >  log2C;  // common second log without cs
  DynLinArr< double >  chereC;  // Cherenkov's radiation
  DynLinArr< double >  chereCangle;  // angle of Cherenkov's radiation
                                     // (for print)
  DynLinArr< double >  Rreser; // the thing which is called R in my paper
#ifdef DEBUG_EnTransfCS
  DynLinArr< double > treser; // total rezer, sum of frezer
  // by atoms and shells, per one electron ( in the paper it is per atom)
#endif

  DynLinArr< double >  addaC;  // sum: energy tranfer cross section to ioniz
#ifndef EXCLUDE_A_VALUES
  DynLinArr< double >  addaC_a;  // sum: energy tranfer cross section to abs
#endif
  double  quanC;   // it's integral, or quantity of energy transfers,
                   // or primary cluster number.
#ifndef EXCLUDE_A_VALUES
  double  quanC_a;
#endif
#ifndef EXCLUDE_MEAN
  double  meanC;   // first moment,
                   // or restricted mean energy loss, Mev.
#ifndef EXCLUDE_A_VALUES
  double  meanC_a;
#endif

  double  meanCleft;   // reduced first moment,
                   // or restricted mean energy loss, Mev.
                   // Instead of central energy for each interval,
                   // the left side of interval is taken here.
                   // The value should be less than above.
                   // The difference shows the uncertainty of the value above
                   // arising from the finite number of intyervals
  double  meanC1;  //first moment with whole additional tail
		   // to emax - kinematically allowed transition.
		   // Now it is calculated only for heavy particles
		   // because the integral for electrons is not
		   // trivial,
		   // or mean energy loss, Mev.
#ifndef EXCLUDE_A_VALUES
  double  meanC1_a; 
#endif

  double  meaneleC; // expected restricted quantity of  secondary ionization.
  double  meaneleC1; // expected quantity of secondary ionization.
  // In the following arrays there are three indexes:
  // the atom number in the matter, 
  // the shell number in atom,
  // the energy.
#endif   // for ifndef EXCLUDE_MEAN
  //DynLinArr< DynLinArr< DynLinArr< double > > > flog1; // first log with cs
  //DynLinArr< DynLinArr< DynLinArr< double > > > flog2; // second log with cs
  DynLinArr< DynLinArr< DynLinArr< double > > > cher;  // fraction of Cher.
#ifndef EXCLUDE_A_VALUES
  DynLinArr< DynLinArr< DynLinArr< double > > > cher_a;  // fraction of Cher.
#endif
  //DynLinArr< DynLinArr< DynLinArr< double > > > rezer;  // integral of cs
  DynLinArr< DynLinArr< DynLinArr< double > > > frezer; // total last item

  // addaC is sum of adda
  DynLinArr< DynLinArr< DynLinArr< double > > > adda;   // sum
#ifndef EXCLUDE_A_VALUES
  DynLinArr< DynLinArr< DynLinArr< double > > > adda_a;   // sum
#endif
  // The following two integrals are normalized to unity
  DynLinArr< DynLinArr< DynLinArr< double > > > fadda; // integral
#ifndef EXCLUDE_A_VALUES
  DynLinArr< DynLinArr< DynLinArr< double > > > fadda_a; // integral
#endif
#ifndef EXCLUDE_VAL_FADDA 
  // The true values of the integral (should be equal to quan)
  DynLinArr< DynLinArr< double > > val_fadda; // integral * hmd->xeldens;
#ifndef EXCLUDE_A_VALUES
  DynLinArr< DynLinArr< double > > val_fadda_a; // integral * hmd->xeldens;
#endif
  // Integrated cross section;
  // first element: integral from e[0] to e[1], etc.
#endif

  // In the following arrays there are two indexes:
  // the atom number in the matter, 
  // the shell number in atom
  DynLinArr< DynLinArr< double > > quan;  // per 1 cm, used for path length
#ifndef EXCLUDE_A_VALUES
  DynLinArr< DynLinArr< double > > quan_a;  // per 1 cm, used for path length
#endif
#ifndef EXCLUDE_MEAN
  DynLinArr< DynLinArr< double > > mean;
#ifndef EXCLUDE_A_VALUES
  DynLinArr< DynLinArr< double > > mean_a;
#endif
#endif

  DynLinArr< double >  length_y0; 

  /* obliterate: the particle charge was missed.
     The bode of constructor as well as the data volume are very large.
 
  EnTransfCS(double fparticle_mass, double fgamma_1, 
	    int fs_primary_electron, 
	    HeedMatterDef* fhmd);
  */
  EnTransfCS(void) {};
  EnTransfCS(double fparticle_mass, double fgamma_1, 
	    int fs_primary_electron, 
	    HeedMatterDef* fhmd, long fparticle_charge=1);
 
  virtual void print(ostream& file, int l) const ;
  macro_copy_total(EnTransfCS);





};

class EnTransfCSType
{public:
  PassivePtr<EnTransfCS> etcs;
  EnTransfCSType(void): etcs() {;}
  EnTransfCSType(EnTransfCS* md): etcs(md) {;}
};
ostream & operator << (ostream & file, const EnTransfCSType & f);


#endif
