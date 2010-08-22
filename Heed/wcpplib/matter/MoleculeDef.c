#include <iomanip>
#include "wcpplib/matter/MoleculeDef.h"
#include "wcpplib/util/FunNameStack.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/math/cubic.h" 

/*
1998-2004 I. Smirnov
*/ 

VanDerVaals::VanDerVaals( double fPk, double fTk):Pkh(fPk), Tkh(fTk)
{
  double R = k_Boltzmann * Avogadro;  // more precise

  Vkh = R * 3.0 / 8.0 * Tkh / Pkh; 
  ah = 3 * Pkh * Vkh * Vkh;
  bh = 1.0 / 3.0 * Vkh;
  
}

double VanDerVaals::volume_of_mole(double T,  
				   double p,
				   int &s_not_single)
{
  mfunname("VanDerVaals::volume_of_mole(...)");

  double Tr = T / Tkh;
  double Pr = p / Pkh;
  Iprint2n(mcout, Tr, Pr);
  Cubic cb(Pr, -1.0/3.0 * (Pr + 8 * Tr), 3, -1);
  double r[3];
  int q = cb.find_real_zero(r);
  check_econd11(q , <= 0 , mcerr);
  //check_econd11a(q , == 0 , "uncertainty: at horizontal plato\n", mcerr);
  double x = r[q-1]; // this is the relative volume taken by one mole
  double res = x * Vkh; // this is the absolute volume taken by one mole
  Iprint2n(mcout, x, res);
  if(q == 2) s_not_single = 1;
  else s_not_single = 0;
  return res;
}

macro_copy_body(VanDerVaals)

ostream & operator << (ostream & file, const VanDerVaals& f)
{
  mfunname("ostream & operator << (ostream & file, const VanDerVaals& f)");
  Ifile<<"VanDerVaals:\n";
  indn.n+=2;
  Iprintn(file, f.Pk()/(atmosphere));
  Iprintn(file, f.Tk()/(kelvin));
  Iprintn(file, f.Vk()/(cm3));
  Ifile<<"For comparison, the volume of a mole of ideal gas\n";
  Ifile<<"at the same conditions takes\n";
  Iprintn(file, (k_Boltzmann * Avogadro * f.Tk()/f.Pk())/(cm3*mole) );
  Iprintn(file, f.a()/(atmosphere * cm3*cm3));
  Iprintn(file, f.b()/(cm3));
  indn.n-=2;
  return file;
}

/*
//This is not finished

double VanDerVaals::pressure(double M, // the number of moles
			     double volume,
			     double T)
{
  mfunname("double VanDerVaals::pressure(double M, double volume, double T)");
  
  double ridberg1=8.314 * (joule/(kelvin*mole));  // for debug
  double ridberg2 = k_Boltzmann * Avogadro;  // more precise
  mcout<<"ridberg1/(joule/(kelvin*mole)) ="
       << ridberg1/(joule/(kelvin*mole))<<'\n';
  mcout<<"ridberg2/(joule/(kelvin*mole)) ="
       << ridberg2/(joule/(kelvin*mole))<<'\n';

  double pa = M * (

  Parabol par( 
*/


/*
molecule_def::molecule_def(const String& fname, const String fnotation,
			   long fqatom, 
			   const DynLinArr< ProtPtr<atom_def> >& fatom,
			   const DynLinArr< double > fweight_quan):
  nameh(fname), notationh(fnotation), 
  qatomh(fqatom), atomh(fatom), weight_quanh(fweight_quan),
  weight_massh(fqatom), Z_meanh(0.0), A_meanh(0.0), inv_A_meanh(0.0),
  mean_ratio_Z_to_Ah(0.0)
*/

MoleculeDef::MoleculeDef(void):nameh("none"), notationh("none") 
{
  MoleculeDef::get_logbook().append(this);
}
/*
MoleculeDef::MoleculeDef(const MoleculeDef& f)
{
  mfunnamep("MoleculeDef::MoleculeDef(const MoleculeDef& f)");
  funnw.ehdr(cerr);
  mcerr<<"The copy constructor is not allowed, "
       <<"since it would create second molecula with the same name and notation\n";
  spexit(mcerr);
}

MoleculeDef& MoleculeDef::operator=(const MoleculeDef& f)
{
  mfunnamep("MoleculeDef& MoleculeDef::operator=(const MoleculeDef& f)");
  funnw.ehdr(cerr);
  mcerr<<"The assignment operator is not allowed, "
       <<"since it would create second molecula with the same name and notation\n";
  spexit(mcerr);
}
*/
MoleculeDef::MoleculeDef(const String& fname, const String& fnotation,
			 long fqatom, 
			 const DynLinArr< String >& fatom_not,
			 const DynLinArr< long >& fqatom_ps,
			 ActivePtr< VanDerVaals > fawls):
  AtomMixDef(fqatom, fatom_not, fqatom_ps),
  nameh(fname), notationh(fnotation), 
  qatom_psh(fqatom_ps),
  Z_totalh(0), A_totalh(0.0), tqatomh(0), awlsh(fawls)
{ 
  mfunname("MoleculeDef::MoleculeDef(...)");
  long n, k;

  for( n=0; n<qatom(); n++)
  {
    Z_totalh += qatom_psh[n] * atom(n)->Z();
    A_totalh += qatom_psh[n] * atom(n)->A();
    tqatomh += qatom_psh[n];
    check_econd11( qatom_psh[n] , <= 0 , mcerr );
  }
  check_econd11( s , <= 0 , mcerr );

  verify();
  MoleculeDef::get_logbook().append(this);
  //logbook.push_back(this);
}

  // one atom in molecule
MoleculeDef::MoleculeDef(const String& fname, const String& fnotation,
			 const String& fatom_not, long fqatom_ps,
			 ActivePtr< VanDerVaals > fawls):
  AtomMixDef(fatom_not),
  nameh(fname), notationh(fnotation), 
  qatom_psh(1, fqatom_ps),
  Z_totalh(0), A_totalh(0.0), tqatomh(fqatom_ps), awlsh(fawls)
{ 
  mfunname("MoleculeDef::MoleculeDef(...)");
  long n;

  Z_totalh = atom(0)->Z() * fqatom_ps;
  A_totalh = atom(0)->A() * fqatom_ps;

  verify();
  MoleculeDef::get_logbook().append(this);
  //logbook.push_back(this);
}

// two atoms
MoleculeDef::MoleculeDef(const String& fname, const String& fnotation,
			 const String& fatom_not1, long fqatom_ps1,
			 const String& fatom_not2, long fqatom_ps2,
			 ActivePtr< VanDerVaals > fawls
			 ):
  AtomMixDef(fatom_not1, fqatom_ps1, fatom_not2, fqatom_ps2),
  nameh(fname), notationh(fnotation), 
  qatom_psh(2),
  Z_totalh(0), A_totalh(0.0), tqatomh(0), awlsh(fawls)
{ 
  mfunname("MoleculeDef::MoleculeDef(...)");

  long n;
  qatom_psh[0] = fqatom_ps1;
  qatom_psh[1] = fqatom_ps2;
  for( n=0; n<qatom(); n++)
  {
    check_econd11( qatom_psh[n] , <= 0 , mcerr );
    Z_totalh += qatom_psh[n] * atom(n)->Z();
    A_totalh += qatom_psh[n] * atom(n)->A();
    tqatomh += qatom_psh[n];
  }
  verify();
  MoleculeDef::get_logbook().append(this);
  //logbook.push_back(this);
}

// three atoms
MoleculeDef::MoleculeDef(const String& fname, const String& fnotation,
			 const String& fatom_not1, long fqatom_ps1,
			 const String& fatom_not2, long fqatom_ps2,
			 const String& fatom_not3, long fqatom_ps3,
			 ActivePtr< VanDerVaals > fawls
			 ):
  AtomMixDef(fatom_not1, fqatom_ps1, fatom_not2, fqatom_ps2, 
	     fatom_not3, fqatom_ps3),
  nameh(fname), notationh(fnotation), 
  qatom_psh(3),
  Z_totalh(0), A_totalh(0.0), tqatomh(0), awlsh(fawls)
{ 
  mfunname("MoleculeDef::MoleculeDef(...)");

  long n;
  qatom_psh[0] = fqatom_ps1;
  qatom_psh[1] = fqatom_ps2;
  qatom_psh[2] = fqatom_ps3;
  for( n=0; n<qatom(); n++)
  {
    check_econd11( qatom_psh[n] , <= 0 , mcerr );
    Z_totalh += qatom_psh[n] * atom(n)->Z();
    A_totalh += qatom_psh[n] * atom(n)->A();
    tqatomh += qatom_psh[n];
  }
  verify();
  MoleculeDef::get_logbook().append(this);
  //logbook.push_back(this);
}

/*
MoleculeDef* MoleculeDef::get_MoleculeDef(long n)
{
  return MoleculeDef::cont[n];
}
*/
/*
const AbsList< MoleculeDef* >& MoleculeDef::get_MoleculeDefLogbook(void)
{
  return logbook;
}

MoleculeDef* MoleculeDef::get_MoleculeDef(const String& fnotation)
{
  AbsListNode<MoleculeDef*>* an=NULL;
  while( (an = logbook.get_next_node(an)) != NULL)
  { 
    if(an->el->notation() == fnotation)
    {
      return an->el;
    }
  }
}
*/  
void MoleculeDef::print(ostream & file)
{
  file<<(*this);
}
void MoleculeDef::printall(ostream & file)
{
  Ifile<<"MoleculeDef::printall:\n";
  AbsListNode<MoleculeDef*>* an=NULL;
  AbsList< MoleculeDef* >& logbook = MoleculeDef::get_logbook();
  while( (an = logbook.get_next_node(an)) != NULL)
  { 
    file<<(*(an->el));
  }
}
void MoleculeDef::verify(void)
{
  mfunnamep("void MoleculeDef::verify(void)");
  if(nameh == "none" && notationh == "none")
    return;
  AbsListNode<MoleculeDef*>* an=NULL;
  while( (an = MoleculeDef::get_logbook().get_next_node(an)) != NULL)
  { 
    if(an->el->nameh == nameh || an->el->notationh == notationh)
    {
      funnw.ehdr(mcerr);
      mcerr<<"can not initialize two molecules "
	   <<"with the same name or notation\n";
      mcerr<<"name="<<nameh<<" notation="<<notationh<<'\n';
      spexit(mcerr);
    }
  }
}



AbsList< MoleculeDef* >& MoleculeDef::get_logbook(void)
{
  static AbsList< MoleculeDef* > logbook;
  return logbook;
}

const AbsList< MoleculeDef* >& MoleculeDef::get_const_logbook(void)
{
  return MoleculeDef::get_logbook();
}


MoleculeDef* MoleculeDef::get_MoleculeDef(const String& fnotation)
{
  AbsList< MoleculeDef* >& logbook = MoleculeDef::get_logbook();
  AbsListNode<MoleculeDef*>* an=NULL;
  while( (an = logbook.get_next_node(an)) != NULL)
  { 
    if(an->el->notation() == fnotation)
    {
      return an->el;
    }
  }
  return NULL;
}




ostream & operator << (ostream & file, const MoleculeDef& f)
{
  mfunnamep("ostream & operator << (ostream & file, const MoleculeDef& f)");
  Ifile<<"MoleculeDef: name="<<setw(10)<<f.name()
       <<" notation="<<setw(3)<<f.notation()<<'\n';
  indn.n+=2;
  Ifile<<"Z_total()="<<setw(3)<<f.Z_total()
       <<" A_total()/(gram/mole)="<<f.A_total()/(gram/mole)
       <<" tqatom()="<<f.tqatom()
       <<'\n';
  Iprintn(file, f.qatom());
  long n;
  indn.n+=2;
  for( n=0; n<f.qatom(); n++)
  {
    //Ifile<<"n="<<n<<'\n';
    //Ifile<<"atom(n)="<<f.atom(n);
    Ifile<<"n="<<n<<" atom(n)->notation="<<f.atom(n)->notation()
	 <<" qatom_ps(n)="<<f.qatom_ps(n)<<'\n';
    //indn.n+=2;
    //Ifile<<" weight_quan(n)="<<f.weight_quan(n)
    //	 <<" weight_mass(n)="<<f.weight_mass(n)<<'\n';
    //indn.n-=2;
  }
  indn.n-=2;
  f.AtomMixDef::print(file); 
  Iprintn(mcout, f.awls());
  VanDerVaals* at = f.awls().get();
  if(at != NULL)
  {
    Ifile<<"Density at the crutial conditions for ideal gas (for debug):\n";
    double ridberg = k_Boltzmann * Avogadro;  // more precise
    //mcout<<"ridberg/(joule/(kelvin*mole)) ="
    //     << ridberg/(joule/(kelvin*mole))<<'\n';
    //double sa = f.A_total();
    Iprintn(mcout, f.A_total() * at->Pk() /(ridberg * at->Tk() ) / (gram/cm3));
    Ifile<<"For the Waals:\n";
    Iprintn(mcout, f.A_total()/at->Vk() / (gram/cm3));

  }

  indn.n-=2;
  return file;
}

MoleculeDef::~MoleculeDef() 
{ 
  //logbook.remove(this); 
  MoleculeDef::get_logbook().remove(this); 

}

