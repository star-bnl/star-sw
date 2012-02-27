#ifndef MOLECULE_DEF_H
#define MOLECULE_DEF_H

#include "wcpplib/matter/AtomDef.h"

/*
Definition of molecule as a mixture of atoms. 
Only the basic information: the name, the notation,
the mean charge and atomic weight and the parameters of mixture class.

The principle of definitions of matters is the same as for atoms:
a dictionary or a database. See details there. But the logbook is different,
of course.

1998-2004 I. Smirnov
*/ 

//This is not finished

class VanDerVaals virt_common_base_col
{private:
  double ah;
  double bh;
  double Vkh;
  double Pkh;
  double Tkh;
public:
  VanDerVaals( double fPk, double fTk);
  virtual ~VanDerVaals() {}
  double a(void) const {return ah;}
  double b(void) const {return bh;}
  double Vk(void) const {return Vkh;}
  double Pk(void) const {return Pkh;}
  double Tk(void) const {return Tkh;}
  /*
  double pressure(double M, // the number of moles
                  double volume,
                  double T);
  double volume(double T,  // relative to T_k 
                double p,  // relative to p_k
                int &s_not_single);  
  */
  // Return number of moles in the unit volume 
  double volume_of_mole(double T,  
                        double p,
                        int &s_not_single);

  macro_copy_header(VanDerVaals);
};
std::ostream& operator << (std::ostream& file, const VanDerVaals& f);

class MoleculeDef: public AtomMixDef
{
  String nameh;
  String notationh;
  // Number of atoms of particular sort in the molecule
  // Obviously it is not normalized to one, but instead
  // the sum is equal to tqatomh
  DynLinArr< long > qatom_psh; 
  long Z_totalh;
  double A_totalh;
  // Total number of atoms in molecule 
  // Attention: this is not the number of different sorts of atoms
  // The latter is qatom() from AtomMixDef
  long tqatomh;  
  ActivePtr< VanDerVaals > awlsh;
public:
  inline const String& name(void) const  {return nameh;}
  inline const String& notation(void) const  {return notationh;}
  inline const DynLinArr< long >& qatom_ps(void) const 
    {return qatom_psh; }
  inline long qatom_ps(long n) const 
    {return qatom_psh[n]; }
  inline long Z_total(void) const {return Z_totalh;}
  inline double A_total(void) const {return A_totalh;}
  inline long tqatom(void) const {return tqatomh;}
  inline const ActivePtr< VanDerVaals >& awls(void) const {return awlsh;}
  MoleculeDef(void);
  MoleculeDef(const String& fname, const String& fnotation,
              long fqatom, const DynLinArr< String >& fatom_not,
              const DynLinArr< long >& fqatom_ps,
              ActivePtr< VanDerVaals > fawls=ActivePtr< VanDerVaals >());
  MoleculeDef(const String& fname, const String& fnotation,
              const String& fatom_not, long fqatom_ps,
              ActivePtr< VanDerVaals > fawls=ActivePtr< VanDerVaals >());
  MoleculeDef(const String& fname, const String& fnotation,
              const String& fatom_not1, long fqatom_ps1,
              const String& fatom_not2, long fqatom_ps2,
              ActivePtr< VanDerVaals > fawls=ActivePtr< VanDerVaals >());
  MoleculeDef(const String& fname, const String& fnotation,
              const String& fatom_not1, long fqatom_ps1,
              const String& fatom_not2, long fqatom_ps2,
              const String& fatom_not3, long fqatom_ps3,
              ActivePtr< VanDerVaals > fawls=ActivePtr< VanDerVaals >());
  ~MoleculeDef();

  void print(std::ostream& file, int l) const;
  static void printall(std::ostream& file);
  // Check that there is no molecule with the same name in the container
  void verify(void); 
  static AbsList< MoleculeDef* >& get_logbook(void);
  // Initialize the logbook at the first request
  // and keep it as internal static variable.
  static const AbsList< MoleculeDef* >& get_const_logbook(void);
  // Return the address of the molecule with this name. 
  // If there is no molecule with this notation, the function returns NULL
  // but does not terminate the program as that for AtomDef. Be careful.
  static MoleculeDef* get_MoleculeDef(const String& fnotation);

  macro_copy_total(MoleculeDef);
};
std::ostream& operator << (std::ostream& file, const MoleculeDef& f);


#endif
