#include <iomanip>
#include "wcpplib/matter/GasDef.h"
#include "wcpplib/util/FunNameStack.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"

GasDef::GasDef(void):MatterDef(), pressureh(0.0), qmolech(0) 
{
}

GasDef::GasDef(const String& fname, const String& fnotation,
	       long fqmolec, const DynLinArr< String >& fmolec_not,
	       const DynLinArr< double >& fweight_quan_molec,
	       double fpressure, double ftemperature, double fdensity):
  pressureh(fpressure),
  qmolech(fqmolec), molech(fqmolec),
  weight_quan_molech(fqmolec),
  weight_mass_molech(fqmolec)
{
  mfunname("GasDef::GasDef(...many molecules...)");

  long n, k;
  for( k=0; k<fqmolec; k++) // finding pointers to all molec. by notations
  {
    //mcout<<"fqmolec="<<fqmolec<<" fmolec_not[k]="<<fmolec_not[k]
    //	 <<" fweight_quan_molec[k]="<<fweight_quan_molec[k]<<'\n';
    MoleculeDef* amd = MoleculeDef::get_MoleculeDef( fmolec_not[k] );
    check_econd11a(amd , == NULL , 
		   "No molecula with such notation: "<<fmolec_not[k]<<'\n', 
		   mcerr)
    molech[k].put(amd);
    /*
    AbsListNode<MoleculeDef*>* an=NULL;
    while( (an = MoleculeDef::logbook.get_next_node(an)) != NULL)
    { 
      if(an->el->notation() == fmolec_not[k])
      {
	molech[k].put(an->el);
	goto mark1;
      }
    }
    */
    if(amd == NULL)
    {
      mcerr<<"can not find molecule with notation "<<fmolec_not[k]
	   <<"\nIn particular, check the sequance of initialization\n";
      spexit(mcerr);
    }
  }
  double s = 0.0;
  for( n=0; n<fqmolec; n++)
  {
    weight_quan_molech[n] = fweight_quan_molec[n]; 
    check_econd11( weight_quan_molech[n] , <= 0 , mcerr );
    s += weight_quan_molech[n];
  }
  check_econd11( s , <= 0 , mcerr );
  if( s != 1.0)
  {
    for( n=0; n<fqmolec; n++)
    {
      weight_quan_molech[n] /= s;
    }
  }
  for( n=0; n<fqmolec; n++)
  {
    weight_mass_molech[n]=weight_quan_molech[n] * molech[n]->A_total();
  }
  s = 0.0;
  for( n=0; n<fqmolec; n++)
  {
    s += weight_mass_molech[n];
  }
  check_econd11( s , <= 0 , mcerr );
  if( s != 1.0)
  {
    for( n=0; n<fqmolec; n++)
    {
      weight_mass_molech[n] /= s;
    }
  }

  long qat=0;
  DynLinArr< String > fatom_not(1000);  
  DynLinArr< double > weight_qa(1000, 0.0);
  for( k=0; k<fqmolec; k++)
  {
    for( n=0; n<molech[k]->qatom(); n++)
    {
      /*
	This is originally designed to avoid duplication of an atom
	in the list if it presents twice in different moleculas.
	But it appears that the same atom in different moleculas
	can have different features related to position and sensitivity
	of external shell. In particular it affects on ionization. 
	This difference can be used in inherited and 
	related classes. Therefore such reduction of the list can produce 
	problems. Therefore this is excluded by commenting off this passage,
	and also by commenting off mark2.
	for(i=0; i<qat; i++)
	{
	if(molech[k]->atom(n)->notation() == fatom_not[i])
	{
	weight_qa[i] += fweight_quan_molec[k] * molech[k]->weight_quan(n);
	goto mark2;
	}
	}
      */
      fatom_not[qat] = molech[k]->atom(n)->notation();
      weight_qa[qat] = fweight_quan_molec[k] * molech[k]->qatom_ps(n);
      //mcout<<"qat="<<qat<<" fatom_not[qat]="<<fatom_not[qat]
      //	   <<" weight_qa[qat]="<<weight_qa[qat]<<'\n';
      qat++;
      //mark2: ;
    }
  }
  if(fdensity < 0.0)
    fdensity=gasdensity(ftemperature, fpressure,
			molech,
			weight_quan_molech,
			qmolech);
  verify(fname, fnotation);
  
  {
    *((MatterDef*) this) = MatterDef
      (fname, fnotation,
       qat, fatom_not,
       weight_qa, 
       fdensity, ftemperature);
  }
}

GasDef::GasDef(const String& fname, const String& fnotation,
	       long fqmolec, const DynLinArr< String >& fmolec_not,
	       const DynLinArr< double >& fweight_volume_molec,
	       double fpressure, double ftemperature, 
	       int s1, int s2) 
// s1 and s2 are to distunguish the constructor
{
  mfunname("GasDef::GasDef(...many molecules... Waals)");
  DynLinArr< MoleculeDef* > amolec(fqmolec);
  long n;
  int s_have_Waals=0;
  for( n=0; n<fqmolec; n++)
  {
    //Iprint2n(mcout, fqmolec, n);
    amolec[n] = MoleculeDef::get_MoleculeDef( fmolec_not[n] );
    check_econd11a(amolec[n]  , == NULL , 
		   "No molecula with such notation: "<<fmolec_not[n]<<'\n', 
		   mcerr)
      //amolec[n]->print(mcout);
    VanDerVaals* aw = amolec[n]->awls().get();
    if(aw != NULL)
      s_have_Waals=1;
  }
  /*
  if(s_have_Waals = 0) // there is not information at all, use ideal case
  {
    *this=GasDef(fname, fnotation,
		 fqmolec, fmolec_not,
		 fweight_volume_molec,
		 fpressure, ftemperature);
  }
  else
  {
  */
    // first it needs to normilize volumes to total unity
  DynLinArr< double > fw(fqmolec); // normalized volume weights
  double s = 0.0;
  for( n=0; n<fqmolec; n++)
  {
    s += fweight_volume_molec[n];
  }
  check_econd11( s , <= 0 , mcerr );
  for( n=0; n<fqmolec; n++)
  {
    fw[n] = fweight_volume_molec[n]/s;
  }

  // calculate number of molecules or moles and mass of each component  
  DynLinArr< double > fweight_quan_molec(fqmolec);
  double mass_t = 0.0;
  double ridberg = k_Boltzmann * Avogadro;  // more precise
  for( n=0; n<fqmolec; n++)
  {
    VanDerVaals* aw = amolec[n]->awls().get();
    if(aw == NULL)  // ideal gas case
    {
      //mcout<<"ideal gas case\n";
      fweight_quan_molec[n] = fw[n] * fpressure /
	(ridberg * ftemperature); // number of moles according to ideal gas
      double ms = fweight_quan_molec[n] * amolec[n]->A_total();
      //Iprint2n(mcout, fweight_quan_molec[n], ms/gram);
      mass_t += ms;
    }
    else
    {
      //mcout<<"Waals gas case\n";
      int s_not_single;
      double number_of_moles = fw[n] * 
	1.0 / aw->volume_of_mole(ftemperature,  // relative to T_k 
				 fpressure,
				 s_not_single);
      check_econd11(s_not_single, == 1 , mcerr);
      fweight_quan_molec[n] = number_of_moles;
      double ms = fweight_quan_molec[n] * amolec[n]->A_total();
      //Iprint2n(mcout, fweight_quan_molec[n], ms/gram);
      mass_t += ms;
    }
  }
  double density_t = mass_t; 
  *this=GasDef(fname, fnotation,
	       fqmolec, fmolec_not,
	       fweight_quan_molec,
	       fpressure, ftemperature, density_t);
  //}
}

GasDef::GasDef(const String& fname, const String& fnotation,
	       const String& fmolec_not,
	       double fpressure, double ftemperature, double fdensity)
{
  mfunname("GasDef::GasDef(...1 molecule...)");
  DynLinArr< String > fmolec_noth(1, fmolec_not);
  DynLinArr< double > fweight_quan_molec(1, 1.0);
  {
    *this=GasDef(fname, fnotation,
		 1, fmolec_noth,
		 fweight_quan_molec,
		 fpressure, ftemperature, fdensity);
  }
}

GasDef::GasDef(const String& fname, const String& fnotation,
	       const String& fmolec_not,
	       double fpressure, double ftemperature, int s1, int s2)
{
  mfunname("GasDef::GasDef(...1 molecule...)");
  DynLinArr< String > fmolec_noth(1, fmolec_not);
  DynLinArr< double > fweight_volume_molec(1, 1.0);
  {
    *this=GasDef(fname, fnotation,
		 1, fmolec_noth,
		 fweight_volume_molec,
		 fpressure, ftemperature, s1, s2);
  }
}

GasDef::GasDef(const String& fname, const String& fnotation,
	       const String& fmolec_not1, double fweight_quan_molec1,
	       const String& fmolec_not2, double fweight_quan_molec2,
	       double fpressure, double ftemperature, double fdensity)
{
  mfunname("GasDef::GasDef(...2 molecules...)");
  DynLinArr< String > fmolec_noth(2);
  DynLinArr< double > fweight_quan_molec(2, 0.0);
  fmolec_noth[0] = fmolec_not1;
  fmolec_noth[1] = fmolec_not2;
  fweight_quan_molec[0] = fweight_quan_molec1;
  fweight_quan_molec[1] = fweight_quan_molec2;
  {
    *this=GasDef(fname, fnotation,
		 2, fmolec_noth,
		 fweight_quan_molec,
		 fpressure, ftemperature, fdensity);
  }
}

GasDef::GasDef(const String& fname, const String& fnotation,
	       const String& fmolec_not1, double fweight_volume_molec1,
	       const String& fmolec_not2, double fweight_volume_molec2,
	       double fpressure, double ftemperature, int s1, int s2)
{
  mfunname("GasDef::GasDef(...2 molecules...)");
  DynLinArr< String > fmolec_noth(2);
  DynLinArr< double > fweight_volume_molec(2, 0.0);
  fmolec_noth[0] = fmolec_not1;
  fmolec_noth[1] = fmolec_not2;
  fweight_volume_molec[0] = fweight_volume_molec1;
  fweight_volume_molec[1] = fweight_volume_molec2;
  {
    *this=GasDef(fname, fnotation,
		 2, fmolec_noth,
		 fweight_volume_molec,
		 fpressure, ftemperature,  s1, s2);
  }
}

GasDef::GasDef(const String& fname, const String& fnotation,
	       const String& fmolec_not1, double fweight_quan_molec1,
	       const String& fmolec_not2, double fweight_quan_molec2,
	       const String& fmolec_not3, double fweight_quan_molec3,
	       double fpressure, double ftemperature, double fdensity)
{
  mfunname("GasDef::GasDef(...3 molecules...)");
  DynLinArr< String > fmolec_noth(3);
  DynLinArr< double > fweight_quan_molec(3, 0.0);
  fmolec_noth[0] = fmolec_not1;
  fmolec_noth[1] = fmolec_not2;
  fmolec_noth[2] = fmolec_not3;
  fweight_quan_molec[0] = fweight_quan_molec1;
  fweight_quan_molec[1] = fweight_quan_molec2;
  fweight_quan_molec[2] = fweight_quan_molec3;
  {
    *this=GasDef(fname, fnotation,
		 3, fmolec_noth,
		 fweight_quan_molec,
		 fpressure, ftemperature, fdensity);
  }
}

GasDef::GasDef(const String& fname, const String& fnotation,
	       const String& fmolec_not1, double fweight_volume_molec1,
	       const String& fmolec_not2, double fweight_volume_molec2,
	       const String& fmolec_not3, double fweight_volume_molec3,
	       double fpressure, double ftemperature, int s1, int s2)
{
  mfunname("GasDef::GasDef(...3 molecules...)");
  DynLinArr< String > fmolec_noth(3);
  DynLinArr< double > fweight_volume_molec(3, 0.0);
  fmolec_noth[0] = fmolec_not1;
  fmolec_noth[1] = fmolec_not2;
  fmolec_noth[2] = fmolec_not3;
  fweight_volume_molec[0] = fweight_volume_molec1;
  fweight_volume_molec[1] = fweight_volume_molec2;
  fweight_volume_molec[2] = fweight_volume_molec3;
  {
    *this=GasDef(fname, fnotation,
		 3, fmolec_noth,
		 fweight_volume_molec,
		 fpressure, ftemperature, s1, s2);
  }
}

GasDef::GasDef(const String& fname, const String& fnotation,
	       const GasDef& gd, 
	       double fpressure, double ftemperature, double fdensity)
{
  mfunname("GasDef::GasDef( another GasDef with different pres)");
  long fqmolec = gd.qmolec(); 
  DynLinArr< String > fmolec_not(fqmolec);
  DynLinArr< double > fweight_quan_molec(fqmolec);
  long n;
  for( n=0; n<fqmolec; n++)
  {
    fmolec_not[n] = gd.molec(n)->notation();
    fweight_quan_molec[n] = gd.weight_quan_molec(n);
  }
  *this=GasDef(fname, fnotation,
	       fqmolec, fmolec_not,
	       fweight_quan_molec,
	       fpressure, ftemperature, fdensity);

}

double GasDef::Z_mean_molec(void) const // mean charge of molecula
{
  mfunname("double GasDef::Z_mean_molec(void) const ");
  double s = 0.0;
  long n;
  for(n=0; n<qmolech; n++)
  {
    s += molech[n]->Z_total() * weight_quan_molech[n];
  }
  return s;
}


void GasDef::print(ostream & file, int l) const
{
  file<<(*this);
}

const double mm_rt_st_in_atmosphere = 760;
// This corresponds to 133.322 pascal in one mm 
//( 101325 pascal in one atmosphere )

ostream & operator << (ostream & file, const GasDef& f)
{
  mfunname("ostream & operator << (ostream & file, const GasDef& f)");
  Ifile<<"GasDef: \n";
  indn.n+=2;
  indn.n+=2;
  file << ((MatterDef&)f);
  indn.n-=2;
  Ifile<<"pressure/atmosphere="<<f.pressure()/atmosphere
       <<" pressure/atmosphere * mm_rt_st_in_atmosphere = "
       <<f.pressure()/atmosphere * mm_rt_st_in_atmosphere<<'\n';
  Ifile<<"Z_mean_molec="<<f.Z_mean_molec()<<'\n';

  file<<"qmolec()="<<f.qmolec()<<'\n';
  long n;
  indn.n+=2;
  for( n=0; n<f.qmolec(); n++)
  {
    //Ifile<<"n="<<n<<'\n';
    //Ifile<<"atom(n)="<<f.atom(n);
    Ifile<<"n="<<n<<" molec(n)->notation="<<f.molec(n)->notation()
	 <<'\n';
    indn.n+=2;
    Ifile<<"weight_quan_molec(n)="<<f.weight_quan_molec(n)
	 <<" weight_mass_molec(n)="<<f.weight_mass_molec(n)<<'\n';
    Ifile<<"Z_total="<<f.molec(n)->Z_total()
	 <<" A_total/(gram/mole)="<<f.molec(n)->A_total()/(gram/mole)<<'\n';
    indn.n-=2;
  }
  indn.n-=2;
  indn.n-=2;
  return file;
}

double gasdensity(double temperature, double pressure,
		  DynLinArr< ProtPtr<MoleculeDef> >molec,
		  DynLinArr< double > weight_quan_molec,
		  long qmolec)
{
  mfunname("double gasdensity(...)");
  double sw=0.0;
  double sa=0.0;
  long n;
  for(n=0; n<qmolec; n++)
  {
    sa += weight_quan_molec[n] * molec[n]->A_total();
    sw += weight_quan_molec[n];
  }
  //double ridberg=8.314 * (joule/(kelvin*mole));  // for debug
  double ridberg = k_Boltzmann * Avogadro;  // more precise
  //mcout<<"ridberg/(joule/(kelvin*mole)) ="
  //     << ridberg/(joule/(kelvin*mole))<<'\n';
  return sa * pressure /(ridberg * temperature * sw );
}


