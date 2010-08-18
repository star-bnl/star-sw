#include <iomanip>
#include "wcpplib/matter/AtomDef.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/stream/prstream.h"
#include "wcpplib/util/FunNameStack.h"
/*
1998-2004, I. Smirnov.
*/

void AtomDef::print(ostream & file, int l) const
{
  file<<(*this);
}
void AtomDef::printall(ostream & file)
{
  Ifile<<"AtomDef::printall:\n";

  AbsList< AtomDef* >& logbook = AtomDef::get_logbook();
  AbsListNode<AtomDef*>* an=NULL;
  while( (an = logbook.get_next_node(an)) != NULL)
  { 
    file<<(*(an->el));
  }
}

AtomDef::AtomDef(void):nameh("none"), notationh("none") 
{
  AtomDef::get_logbook().append(this);
}
/*
AtomDef::AtomDef(const AtomDef& f)
{
  mfunnamep("AtomDef::AtomDef(const AtomDef& f)");
  funnw.ehdr(cerr);
  mcerr<<"The copy constructor is not allowed, "
       <<"since it would create second atom with the same name and notation\n";
  spexit(mcerr);
}

AtomDef& AtomDef::operator=(const AtomDef& f)
{
  mfunnamep("AtomDef& AtomDef::operator=(const AtomDef& f)");
  funnw.ehdr(cerr);
  mcerr<<"The assignment operator is not allowed, "
       <<"since it would create second atom with the same name and notation\n";
  spexit(mcerr);
}
*/

AtomDef::AtomDef(const String& fnameh, const String& fnotationh,
		 int fZh, double fAh):
  nameh(fnameh), notationh(fnotationh), Zh(fZh), Ah(fAh)
{ 
  mfunname("AtomDef::AtomDef(...)");
  check_econd21(fZh , < 1 || , > max_poss_atom_z , mcerr); 
  verify();
  AtomDef::get_logbook().append(this);
}

double AtomDef::get_A(int fZ)
{
  mfunnamep("double AtomDef::get_A(int fZ)");
  AbsList< AtomDef* >& logbook = AtomDef::get_logbook();
  AbsListNode<AtomDef*>* an=NULL;
  while( (an = logbook.get_next_node(an)) != NULL)
  { 
    if(an->el->Z() == fZ)
    {
      return an->el->A();
    }
  }
  funnw.ehdr(mcerr);
  mcerr<<"Atom is not found, Z="<<fZ<<'\n';
  spexit(mcerr);
  return 0.0; // to quiet compiler
}

AtomDef* AtomDef::get_AtomDef(int fZ)
{
  mfunnamep("AtomDef* AtomDef::get_AtomDef(int fZ)");
  AbsList< AtomDef* >& logbook = AtomDef::get_logbook();
  AbsListNode<AtomDef*>* an=NULL;
  while( (an = logbook.get_next_node(an)) != NULL)
  { 
    if(an->el->Z() == fZ)
    {
      return an->el;
    }
  }
  funnw.ehdr(mcerr);
  mcerr<<"Atom is not found, Z="<<fZ<<'\n';
  spexit(mcerr);
  return NULL; // to quiet compiler
}


void AtomDef::verify(void)
{
  mfunnamep("void AtomDef::verify(void)");
  if(nameh == "none" && notationh == "none")
    return;
  AbsList< AtomDef* >& logbook = AtomDef::get_logbook();
  AbsListNode<AtomDef*>* an=NULL;
  while( (an = logbook.get_next_node(an)) != NULL)
  { 
    if(an->el->nameh == nameh || an->el->notationh == notationh)
    {
      funnw.ehdr(mcerr);
      mcerr<<"can not initialize two atoms with the same name or notation\n";
      mcerr<<"name="<<nameh<<" notation="<<notationh<<'\n';
      spexit(mcerr);
    }
  }
}

ostream & operator << (ostream & file, const AtomDef& f)
{
  Ifile<<"AtomDef: name="<<setw(10)<<f.name()
       <<" notation="<<setw(3)<<f.notation();
  Ifile<<" Z()="<<setw(3)<<f.Z()<<" A()/(gram/mole)="<<f.A()/(gram/mole)<<'\n';
  return file;
}


AbsList< AtomDef* >& AtomDef::get_logbook(void)
{
  static AbsList< AtomDef* > logbook;
  return logbook;
}

const AbsList< AtomDef* >& AtomDef::get_const_logbook(void)
{
  return AtomDef::get_logbook();
}


AtomDef* AtomDef::get_AtomDef(const String& fnotation)
{
  AbsList< AtomDef* >& logbook = AtomDef::get_logbook();
  AbsListNode<AtomDef*>* an=NULL;
  while( (an = logbook.get_next_node(an)) != NULL)
  { 
    if(an->el->notation() == fnotation)
    {
      return an->el;
    }
  }
  return NULL;
}

AtomDef::~AtomDef() 
{ 
  AtomDef::get_logbook().remove(this); 
}

AtomMixDef::AtomMixDef(long fqatom, const DynLinArr< String >& fatom_not,
		       const DynLinArr< double >& fweight_quan):
  qatomh(fqatom), atomh(fqatom), 
  weight_quanh(fqatom, 0.0),
  weight_massh(fqatom, 0.0),
  Z_meanh(0.0), A_meanh(0.0), inv_A_meanh(0.0),
  mean_ratio_Z_to_Ah(0.0)
{ 
  mfunnamep("AtomMixDef::AtomMixDef(...)");
  long n, k;
  check_econd11( fqatom , <=0  , mcerr );
  check_econd12( fqatom , > , fatom_not.get_qel() , mcerr );
  check_econd12( fqatom , > , fweight_quan.get_qel() , mcerr );

  for( k=0; k<qatomh; k++)
  {
    AtomDef* ad = AtomDef::get_AtomDef(fatom_not[k]);
    if(ad == NULL)
    {
      funnw.ehdr(mcerr);
      mcerr<<"can not find atom with notation "<<fatom_not[k]
	   <<"\nIn particular, check the sequance of initialization\n";
      spexit(mcerr);
    }
    atomh[k].put(ad);
  }

  double s = 0.0;
  for( n=0; n<qatomh; n++)
  {
    weight_quanh[n] = fweight_quan[n]; 
    check_econd11( weight_quanh[n] , <= 0 , mcerr );
    s += weight_quanh[n];
  }
  check_econd11( s , <= 0 , mcerr );
  if( s != 1.0)
  {
    for( n=0; n<qatomh; n++)
    {
      weight_quanh[n] /= s;
    }
  }
  for( n=0; n<qatomh; n++)
  {
    weight_massh[n]=weight_quanh[n] * atomh[n]->A();
  }
  s = 0.0;
  for( n=0; n<qatomh; n++)
  {
    s += weight_massh[n];
  }
  check_econd11( s , <= 0 , mcerr );
  if( s != 1.0)
  {
    for( n=0; n<qatomh; n++)
    {
      weight_massh[n] /= s;
    }
  }
  //double mean_ratio_Z_to_Ahh = 0.0; 
  for( n=0; n<qatomh; n++)
  {
    Z_meanh += atomh[n]->Z() * weight_quanh[n];
    A_meanh += atomh[n]->A() * weight_quanh[n];
    inv_A_meanh += (1.0 / atomh[n]->A()) * weight_quanh[n];
    //mean_ratio_Z_to_Ahh += atomh[n]->Z() / atomh[n]->A() *  weight_massh[n];
  } 
  mean_ratio_Z_to_Ah = Z_meanh / A_meanh;
  //mcout<<"mean_ratio_Z_to_Ahh="<<mean_ratio_Z_to_Ahh
  //     <<" mean_ratio_Z_to_Ah="<<mean_ratio_Z_to_Ah<<'\n';
  NumberOfElectronsInGramh = mean_ratio_Z_to_Ah* (gram/mole) * Avogadro;

}

AtomMixDef::AtomMixDef(long fqatom, const DynLinArr< String >& fatom_not,
		       const DynLinArr< long >& fweight_quan):
  qatomh(fqatom), atomh(fqatom), 
  weight_quanh(fqatom, 0.0),
  weight_massh(fqatom, 0.0),
  Z_meanh(0.0), A_meanh(0.0), inv_A_meanh(0.0),
  mean_ratio_Z_to_Ah(0.0)
{ 
  mfunnamep("AtomMixDef::AtomMixDef(...)");
  long n, k;
  check_econd11( fqatom , <=0  , mcerr );
  check_econd12( fqatom , > , fatom_not.get_qel() , mcerr );
  check_econd12( fqatom , > , fweight_quan.get_qel() , mcerr );

  for( k=0; k<qatomh; k++)
  {
    AtomDef* ad = AtomDef::get_AtomDef(fatom_not[k]);
    if(ad == NULL)
    {
      funnw.ehdr(mcerr);
      mcerr<<"can not find atom with notation "<<fatom_not[k]
	   <<"\nIn particular, check the sequance of initialization\n";
      spexit(mcerr);
    }
    atomh[k].put(ad);
  }

  double s = 0.0;
  for( n=0; n<qatomh; n++)
  {
    weight_quanh[n] = fweight_quan[n]; 
    check_econd11( weight_quanh[n] , <= 0 , mcerr );
    s += weight_quanh[n];
  }
  check_econd11( s , <= 0 , mcerr );
  if( s != 1.0)
  {
    for( n=0; n<qatomh; n++)
    {
      weight_quanh[n] /= s;
    }
  }
  for( n=0; n<qatomh; n++)
  {
    weight_massh[n]=weight_quanh[n] * atomh[n]->A();
  }
  s = 0.0;
  for( n=0; n<qatomh; n++)
  {
    s += weight_massh[n];
  }
  check_econd11( s , <= 0 , mcerr );
  if( s != 1.0)
  {
    for( n=0; n<qatomh; n++)
    {
      weight_massh[n] /= s;
    }
  }
  //double mean_ratio_Z_to_Ahh = 0.0; 
  for( n=0; n<qatomh; n++)
  {
    Z_meanh += atomh[n]->Z() * weight_quanh[n];
    A_meanh += atomh[n]->A() * weight_quanh[n];
    inv_A_meanh += (1.0 / atomh[n]->A()) * weight_quanh[n];
    //mean_ratio_Z_to_Ahh += atomh[n]->Z() / atomh[n]->A() *  weight_massh[n];
  } 
  mean_ratio_Z_to_Ah = Z_meanh / A_meanh;
  //mcout<<"mean_ratio_Z_to_Ahh="<<mean_ratio_Z_to_Ahh
  //     <<" mean_ratio_Z_to_Ah="<<mean_ratio_Z_to_Ah<<'\n';
  NumberOfElectronsInGramh = mean_ratio_Z_to_Ah* (gram/mole) * Avogadro;

}


  // one atom in mixture
AtomMixDef::AtomMixDef(const String& fatom_not):
  qatomh(1), atomh(1), 
  weight_quanh(1),
  weight_massh(1), 
  Z_meanh(0.0), A_meanh(0.0), inv_A_meanh(0.0),
  mean_ratio_Z_to_Ah(0.0)
{ 
  mfunnamep("AtomMixDef::AtomMixDef(...)");
  long n;
  AtomDef* ad = AtomDef::get_AtomDef(fatom_not);
  if(ad == NULL)
  {
    funnw.ehdr(mcerr);
    mcerr<<"can not find atom with notation "<<fatom_not
	 <<"\nIn particular, check the sequance of initialization\n";
    spexit(mcerr);
  }
  atomh[0].put(ad);

  weight_quanh[0] = 1.0;
  weight_massh[0] = 1.0;

  //double mean_ratio_Z_to_Ahh = 0.0; 
  Z_meanh += atomh[0]->Z();
  A_meanh += atomh[0]->A();
  inv_A_meanh += (1.0 / atomh[0]->A());
  //mean_ratio_Z_to_Ahh += atomh[0]->Z() / atomh[0]->A();

  mean_ratio_Z_to_Ah = Z_meanh / A_meanh;
  NumberOfElectronsInGramh = mean_ratio_Z_to_Ah* (gram/mole) * Avogadro;

}

// two atoms
AtomMixDef::AtomMixDef(const String& fatom_not1, double  fweight_quan1,
		       const String& fatom_not2, double  fweight_quan2):
  qatomh(2), atomh(2), 
  weight_quanh(2),
  weight_massh(2), 
  Z_meanh(0.0), A_meanh(0.0), inv_A_meanh(0.0),
  mean_ratio_Z_to_Ah(0.0)
{ 
  mfunnamep("AtomMixDef::AtomMixDef(...)");
  DynLinArr< String > fatom_not(2);
  fatom_not[0] = fatom_not1;
  fatom_not[1] = fatom_not2;

  long n, k;

  for( k=0; k<qatomh; k++)
  {
    AtomDef* ad = AtomDef::get_AtomDef(fatom_not[k]);
    if(ad == NULL)
    {
      funnw.ehdr(mcerr);
      mcerr<<"can not find atom with notation "<<fatom_not[k]
	   <<"\nIn particular, check the sequance of initialization\n";
      spexit(mcerr);
    }
    atomh[k].put(ad);
  }
  weight_quanh[0] = fweight_quan1;
  weight_quanh[1] = fweight_quan2;
  double s = 0.0;
  for( n=0; n<qatomh; n++)
  {
    check_econd11( weight_quanh[n] , <= 0 , mcerr );
    s += weight_quanh[n];
  }
  check_econd11( s , <= 0 , mcerr );
  if( s != 1.0)
  {
    for( n=0; n<qatomh; n++)
    {
      weight_quanh[n] /= s;
    }
  }
  for( n=0; n<qatomh; n++)
  {
    weight_massh[n]=weight_quanh[n] * atomh[n]->A();
  }
  s = 0.0;
  for( n=0; n<qatomh; n++)
  {
    s += weight_massh[n];
  }
  check_econd11( s , <= 0 , mcerr );
  if( s != 1.0)
  {
    for( n=0; n<qatomh; n++)
    {
      weight_massh[n] /= s;
    }
  }
  double mean_ratio_Z_to_Ahh = 0.0; 
  for( n=0; n<qatomh; n++)
  {
    Z_meanh += atomh[n]->Z() * weight_quanh[n];
    A_meanh += atomh[n]->A() * weight_quanh[n];
    inv_A_meanh += (1.0 / atomh[n]->A()) * weight_quanh[n];
    mean_ratio_Z_to_Ahh += atomh[n]->Z() / atomh[n]->A() *  weight_massh[n];
  } 
  mean_ratio_Z_to_Ah = Z_meanh / A_meanh;
  //mcout<<"mean_ratio_Z_to_Ahh="<<mean_ratio_Z_to_Ahh
  //     <<"mean_ratio_Z_to_Ah="<<mean_ratio_Z_to_Ah<<'\n';
  NumberOfElectronsInGramh = mean_ratio_Z_to_Ah* (gram/mole)  * Avogadro;

}




// three atoms
AtomMixDef::AtomMixDef(const String& fatom_not1, double  fweight_quan1,
		       const String& fatom_not2, double  fweight_quan2,
		       const String& fatom_not3, double  fweight_quan3):
  qatomh(3), atomh(3), 
  weight_quanh(3),
  weight_massh(3), 
  Z_meanh(0.0), A_meanh(0.0), inv_A_meanh(0.0),
  mean_ratio_Z_to_Ah(0.0)
{ 
  mfunnamep("AtomMixDef::AtomMixDef(...)");
  DynLinArr< String > fatom_not(3);
  fatom_not[0] = fatom_not1;
  fatom_not[1] = fatom_not2;
  fatom_not[2] = fatom_not3;

  long n, k;

  for( k=0; k<qatomh; k++)
  {
    AtomDef* ad = AtomDef::get_AtomDef(fatom_not[k]);
    if(ad == NULL)
    {
      funnw.ehdr(mcerr);
      mcerr<<"can not find atom with notation "<<fatom_not[k]
	   <<"\nIn particular, check the sequance of initialization\n";
      spexit(mcerr);
    }
    atomh[k].put(ad);
  }
  weight_quanh[0] = fweight_quan1;
  weight_quanh[1] = fweight_quan2;
  weight_quanh[2] = fweight_quan3;
  double s = 0.0;
  for( n=0; n<qatomh; n++)
  {
    check_econd11( weight_quanh[n] , <= 0 , mcerr );
    s += weight_quanh[n];
  }
  check_econd11( s , <= 0 , mcerr );
  if( s != 1.0)
  {
    for( n=0; n<qatomh; n++)
    {
      weight_quanh[n] /= s;
    }
  }
  for( n=0; n<qatomh; n++)
  {
    weight_massh[n]=weight_quanh[n] * atomh[n]->A();
  }
  s = 0.0;
  for( n=0; n<qatomh; n++)
  {
    s += weight_massh[n];
  }
  check_econd11( s , <= 0 , mcerr );
  if( s != 1.0)
  {
    for( n=0; n<qatomh; n++)
    {
      weight_massh[n] /= s;
    }
  }
  double mean_ratio_Z_to_Ahh = 0.0; 
  for( n=0; n<qatomh; n++)
  {
    Z_meanh += atomh[n]->Z() * weight_quanh[n];
    A_meanh += atomh[n]->A() * weight_quanh[n];
    inv_A_meanh += (1.0 / atomh[n]->A()) * weight_quanh[n];
    mean_ratio_Z_to_Ahh += atomh[n]->Z() / atomh[n]->A() *  weight_massh[n];
  } 
  mean_ratio_Z_to_Ah = Z_meanh / A_meanh;
  //mcout<<"mean_ratio_Z_to_Ahh="<<mean_ratio_Z_to_Ahh
  //     <<"mean_ratio_Z_to_Ah="<<mean_ratio_Z_to_Ah<<'\n';
  NumberOfElectronsInGramh = mean_ratio_Z_to_Ah* (gram/mole) * Avogadro;

}

// four atoms
AtomMixDef::AtomMixDef(const String& fatom_not1, double  fweight_quan1,
		       const String& fatom_not2, double  fweight_quan2,
		       const String& fatom_not3, double  fweight_quan3,
		       const String& fatom_not4, double  fweight_quan4):
  qatomh(4), atomh(4), 
  weight_quanh(4),
  weight_massh(4), 
  Z_meanh(0.0), A_meanh(0.0), inv_A_meanh(0.0),
  mean_ratio_Z_to_Ah(0.0)
{ 
  mfunnamep("AtomMixDef::AtomMixDef(...)");
  DynLinArr< String > fatom_not(4);
  fatom_not[0] = fatom_not1;
  fatom_not[1] = fatom_not2;
  fatom_not[2] = fatom_not3;
  fatom_not[3] = fatom_not4;

  long n, k;

  for( k=0; k<qatomh; k++)
  {
    AtomDef* ad = AtomDef::get_AtomDef(fatom_not[k]);
    if(ad == NULL)
    {
      funnw.ehdr(mcerr);
      mcerr<<"can not find atom with notation "<<fatom_not[k]
	   <<"\nIn particular, check the sequance of initialization\n";
      spexit(mcerr);
    }
    atomh[k].put(ad);
  }
  weight_quanh[0] = fweight_quan1;
  weight_quanh[1] = fweight_quan2;
  weight_quanh[2] = fweight_quan3;
  weight_quanh[3] = fweight_quan4;
  double s = 0.0;
  for( n=0; n<qatomh; n++)
  {
    check_econd11( weight_quanh[n] , <= 0 , mcerr );
    s += weight_quanh[n];
  }
  check_econd11( s , <= 0 , mcerr );
  if( s != 1.0)
  {
    for( n=0; n<qatomh; n++)
    {
      weight_quanh[n] /= s;
    }
  }
  for( n=0; n<qatomh; n++)
  {
    weight_massh[n]=weight_quanh[n] * atomh[n]->A();
  }
  s = 0.0;
  for( n=0; n<qatomh; n++)
  {
    s += weight_massh[n];
  }
  check_econd11( s , <= 0 , mcerr );
  if( s != 1.0)
  {
    for( n=0; n<qatomh; n++)
    {
      weight_massh[n] /= s;
    }
  }
  double mean_ratio_Z_to_Ahh = 0.0; 
  for( n=0; n<qatomh; n++)
  {
    Z_meanh += atomh[n]->Z() * weight_quanh[n];
    A_meanh += atomh[n]->A() * weight_quanh[n];
    inv_A_meanh += (1.0 / atomh[n]->A()) * weight_quanh[n];
    mean_ratio_Z_to_Ahh += atomh[n]->Z() / atomh[n]->A() *  weight_massh[n];
  } 
  mean_ratio_Z_to_Ah = Z_meanh / A_meanh;
  //mcout<<"mean_ratio_Z_to_Ahh="<<mean_ratio_Z_to_Ahh
  //     <<"mean_ratio_Z_to_Ah="<<mean_ratio_Z_to_Ah<<'\n';
  NumberOfElectronsInGramh = mean_ratio_Z_to_Ah * (gram/mole) * Avogadro;
}

void AtomMixDef::print(ostream & file) const
{
  file<<(*this);
}

ostream & operator << (ostream & file, const AtomMixDef& f)
{
  mfunname("ostream & operator << (ostream & file, const AtomMixDef& f)");
  Ifile<<"AtomMixDef\n";
  indn.n+=2;
  Ifile<<"Z_mean()="<<setw(3)<<f.Z_mean()
       <<" A_mean()/(gram/mole)="<<f.A_mean()/(gram/mole)<<'\n';
  //  Ifile<<"total Z ="<<setw(3) << f.qatom() * f.Z_mean()
  //     <<" total A /(gram/mole)="<< f.qatom() * f.A_mean()/(gram/mole)<<'\n';
  Ifile<<"inv_A_mean()*(gram/mole)="<<f.inv_A_mean()*(gram/mole)<<'\n';
  Ifile<<"mean_ratio_Z_to_A()*(gram/mole)="<<f.mean_ratio_Z_to_A()*(gram/mole)
       <<'\n';
  Ifile<<"NumberOfElectronsInGram()="<<f.NumberOfElectronsInGram()<<'\n';
  // Here above the mass unit is defined, therefore there is no need to divide
  // by gram.
  Iprintn(file, f.qatom());
  long n;
  indn.n+=2;
  for( n=0; n<f.qatom(); n++)
  {
    //Ifile<<"n="<<n<<'\n';
    //Ifile<<"atom(n)="<<f.atom(n);
    Ifile<<"n="<<n<<" atom(n)->notation="<<f.atom(n)->notation()
	 <<'\n';
    indn.n+=2;
    Ifile<<" weight_quan(n)="<<f.weight_quan(n)
	 <<" weight_mass(n)="<<f.weight_mass(n)<<'\n';
    indn.n-=2;
  }
  indn.n-=2;


  indn.n-=2;
  return file;
}


/*
HepAList <atom_def> atom_def::cont;
// This call should be before all atom_def
// otherwise they are all lost.

atom_def Hydrogen(  "Hydrogen",   "H",   1, 1.00794 * gram/mole);
atom_def Helium(    "Helium",     "He",  2, 4.002602 * gram/mole);
atom_def Lithium(   "Lithium",    "Li",  3, 6.941 * gram/mole);
atom_def Beryllium( "Beryllium",  "Be",  4, 9.012182 * gram/mole);
atom_def Boron(     "Boron",      "B",   5, 10.811 * gram/mole);
atom_def Carbon(    "Carbon",     "C",   6, 12.011 * gram/mole);
atom_def Nitrogen(  "Nitrogen",   "N",   7, 14.00674 * gram/mole);
atom_def Oxygen(    "Oxygen",     "O",   8, 15.9994 * gram/mole);
atom_def Fluorine(  "Fluorine",   "F",   9, 18.9984032 * gram/mole);
atom_def Neon(      "Neon",       "Ne", 10, 20.1797 * gram/mole);
atom_def Sodium(    "Sodium",     "Na", 11, 22.989768 * gram/mole);
atom_def Magnesium( "Magnesium",  "Mg", 12, 24.3050 * gram/mole);
atom_def Aluminium( "Aluminium",  "Al", 13, 26981539 * gram/mole);
atom_def Silicon(   "Silicon",    "Si", 14, 28.0855 * gram/mole);
atom_def Phosphor(  "Phosphor",   "P",  15, 30.973762 * gram/mole);
atom_def Sulfur(    "Sulfur",     "S",  16, 32.066 * gram/mole);
atom_def Chlorine(  "Chlorine",   "Cl", 17, 35.066 * gram/mole);
atom_def Argon(     "Argon",      "Ar", 18, 39.948 * gram/mole);
atom_def Krypton(   "Krypton",    "Kr", 36, 83.80 * gram/mole);
atom_def Xenon(     "Xenon",      "Xe", 54, 131.29 * gram/mole);
atom_def Uranium(   "Uranium",    "U",  92, 238.0289 * gram/mole);
*/
