#include <fstream>
#include <iomanip>
#include "wcpplib/clhep_units/WSystemOfUnits.h"
#include "wcpplib/math/tline.h"
#include "heed++/code/HeedMatterDef.h"

/*
2003, I. Smirnov
*/

HeedMatterDef::HeedMatterDef(void): 
  eldens_cm_3(0.0), eldens(0.0), xeldens(0.0),
  wpla(0.0), radiation_length(0.0), Rutherford_const(0.0), W(0.0), F(0.0)
{;}

/*
HeedMatterDef::HeedMatterDef
(PassivePtr< MatterDef > fmatter, DynLinArr< PassivePtr<AtomPhotoAbsCS> > fapacs,
 double fW, double fF):
  matter(fmatter), apacs(fapacs), W(fW), F(fF)
{
  mfunnamep("HeedMatterDef::HeedMatterDef(...)");
  check_econd11(matter->qatom() , != 0 , mcerr); 
  check_econd12(matter->qatom() , != , apacs.get_qel() , mcerr);
  long q = matter->qatom();
  long n;
  for( n=0; n<q; n++)
  {
    check_econd12(matter->atom(n)->Z() , != , apacs[n]->get_Z() , mcerr);
  }    
}  
*/
HeedMatterDef::HeedMatterDef
(EnergyMesh* fenergy_mesh,
 MatterDef* amatter, 
 AtomPhotoAbsCS* faapacs[],  // array of size corresponding matter
 double fW, double fF):
  W(fW), F(fF),
  energy_mesh(fenergy_mesh)
{
  mfunname("HeedMatterDef::HeedMatterDef(...)");
  matter.put(amatter);
  check_econd11(matter->qatom() , <= 0 , mcerr); 
  long q = matter->qatom();
  apacs.put_qel(q);
  long n;
  for( n=0; n<q; n++)
  {
    apacs[n].put(faapacs[n]);
    check_econd12(matter->atom(n)->Z() , != , apacs[n]->get_Z() , mcerr);
  } 
  check_econd11(F, == 0.0, mcerr);
  if(W == 0.0)
  {
#ifdef CALC_W_USING_CHARGES
    double mean_I = 0.0;
    double d = 0.0;
    long n;
    for(n=0; n<q; n++)
    {
      mean_I += matter->weight_quan(n) * apacs[n]->get_Z() *
	apacs[n]->get_I_min();
      d += matter->weight_quan(n) * apacs[n]->get_Z();
    }
    W = coef_I_to_W * mean_I/d;
#else
    double mean_I = 0.0;
    long n;
    for(n=0; n<q; n++)
    {
      mean_I += matter->weight_quan(n) * apacs[n]->get_I_min();
    }
    W = coef_I_to_W * mean_I;
#endif
  }
  inite_HeedMatterDef();
}  

HeedMatterDef::HeedMatterDef
(EnergyMesh* fenergy_mesh,
 GasDef* agas, 
 MolecPhotoAbsCS* fampacs[],  //array of size corresponding gas
 double fW, double fF):
  W(fW), F(fF),
  energy_mesh(fenergy_mesh)
{
  mfunname("HeedMatterDef::HeedMatterDef(...)");
  matter.put(agas);
  check_econd11(agas->qmolec() , <= 0 , mcerr); 
  //check_econd12(agas->qmolec() , != , fampacs.get_qel() , mcerr); 
  long qat = agas->qatom();
  apacs.put_qel(qat);
  long qmol = agas->qmolec();
  long nmol;
  long nat = 0;
  for(nmol=0; nmol<qmol; nmol++)
  {
    check_econd12(agas->molec(nmol)->tqatom() , != , 
		  fampacs[nmol]->get_qatom(),
		  mcerr);
    long qa = agas->molec(nmol)->qatom();  //quantity of different atoms in mol
    long na;
    for(na=0; na<qa; na++)
    {
      apacs[nat].put(fampacs[nmol]->get_atom(na).getver());
      check_econd12( apacs[nat]->get_Z() , != , 
		     agas->molec(nmol)->atom(na)->Z(), mcerr );
      nat++;
    }
  }
  if(F == 0.0)
  {
#ifdef CALC_W_USING_CHARGES
    double u = 0.0;
    double d = 0.0;
    long n;
    for(n=0; n<qmol; n++)
    {
      u += agas->weight_quan_molec(n) * fampacs[n]->get_total_Z() *
	fampacs[n]->get_F();
      d += agas->weight_quan_molec(n) * fampacs[n]->get_total_Z();
    }
    F = u/d;
#else
    long n;
    for(n=0; n<qmol; n++)
    {
      F += agas->weight_quan_molec(n) * fampacs[n]->get_F();
    }
#endif
  }

  if(W == 0.0)
  {
#ifdef CALC_W_USING_CHARGES
    double u = 0.0;
    double d = 0.0;
    long n;
    for(n=0; n<qmol; n++)
    {
      u += agas->weight_quan_molec(n) * fampacs[n]->get_total_Z() *
	fampacs[n]->get_W();
      d += agas->weight_quan_molec(n) * fampacs[n]->get_total_Z();
    }
    W = u/d;
#else
    long n;
    for(n=0; n<qmol; n++)
    {
      W += agas->weight_quan_molec(n) * fampacs[n]->get_W();
    }
#endif
  }
  inite_HeedMatterDef();
}

HeedMatterDef::HeedMatterDef
(EnergyMesh* fenergy_mesh,
 const String& gas_notation, 
 MolecPhotoAbsCS* fampacs[],  //array of size corresponding gas
 double fW, double fF):
  W(fW), F(fF), 
 energy_mesh(fenergy_mesh)
{
  mfunnamep("HeedMatterDef::HeedMatterDef(...)");
  MatterDef* amat = MatterDef::get_MatterDef(gas_notation);
  GasDef* agas = dynamic_cast< GasDef* >(amat);
  if(agas == NULL)
  {
    funnw.ehdr(mcerr);    
    mcerr<<"notation supplied as the gas notation is not appear "
	 <<"to be related to gas \n";
    mcerr<<"gas_notation="<<gas_notation<<'\n';
    spexit(mcerr);
  }
    
  matter.put(agas);
  check_econd11(agas->qmolec() , <= 0 , mcerr); 
  //check_econd12(agas->qmolec() , != , fampacs.get_qel() , mcerr); 
  long qat = agas->qatom();
  apacs.put_qel(qat);
  long qmol = agas->qmolec();
  long nmol;
  long nat = 0;
  for(nmol=0; nmol<qmol; nmol++)
  {
    check_econd12(agas->molec(nmol)->tqatom() , != , 
		  fampacs[nmol]->get_qatom(),
		  mcerr);
    long qa = agas->molec(nmol)->qatom();  //quantity of different atoms in mol
    long na;
    for(na=0; na<qa; na++)
    {
      apacs[nat].put(fampacs[nmol]->get_atom(na).getver());
      check_econd12( apacs[nat]->get_Z() , != , 
		     agas->molec(nmol)->atom(na)->Z(), mcerr );
      nat++;
    }
  }
  if(F == 0.0)
  {
#ifdef CALC_W_USING_CHARGES
    double u = 0.0;
    double d = 0.0;
    long n;
    for(n=0; n<qmol; n++)
    {
      u += agas->weight_quan_molec(n) * fampacs[n]->get_total_Z() *
	fampacs[n]->get_F();
      d += agas->weight_quan_molec(n) * fampacs[n]->get_total_Z();
    }
    F = u/d;
#else
    long n;
    for(n=0; n<qmol; n++)
    {
      F += agas->weight_quan_molec(n) * fampacs[n]->get_F();
    }
#endif
  }

  if(W == 0.0)
  {
#ifdef CALC_W_USING_CHARGES
    double u = 0.0;
    double d = 0.0;
    long n;
    for(n=0; n<qmol; n++)
    {
      u += agas->weight_quan_molec(n) * fampacs[n]->get_total_Z() *
	fampacs[n]->get_W();
      d += agas->weight_quan_molec(n) * fampacs[n]->get_total_Z();
    }
    W = u/d;
#else
    long n;
    for(n=0; n<qmol; n++)
    {
      W += agas->weight_quan_molec(n) * fampacs[n]->get_W();
    }
#endif
  }
  inite_HeedMatterDef();
}

void HeedMatterDef::inite_HeedMatterDef(void)
{
  mfunname("void HeedMatterDef::inite_HeedMatterDef(void)");
  //mcout<<"inite_HeedMatterDef is called\n";
  //Iprintn(mcout, matter->Z_mean());
  //Iprintn(mcout, matter->A_mean()/(gram/mole));
  //Iprintn(mcout, matter->density()/(gram/cm3) );
  //Iprintn(mcout, C1_MEV_CM);
  //Iprintn(mcout, pow( C1_MEV_CM , 3.0));
  eldens_cm_3 = matter->Z_mean() / (matter->A_mean()/(gram/mole)) * AVOGADRO * 
    matter->density()/(gram/cm3) ;
  eldens = eldens_cm_3  / ( pow( C1_MEV_CM , 3.0) );
  //eldens = matter->Z_mean() / (matter->A_mean()/(gram/mole)) * AVOGADRO * 
  //  matter->density()/(gram/cm3) / ( pow( C1_MEV_CM , 3.0) );
  //matter->density()/(gram/cm3) / (pow(5.07,3)*1.0e30);
  xeldens= eldens * C1_MEV_CM;
  //xeldens= eldens*5.07e10;
  wpla = eldens * 4.0 * M_PI / (ELMAS*FSCON);

  radiation_length = 0.0;
  double rms = 0.0;
  long n;
  long qat = matter->qatom();
  for(n=0; n < qat; n++)
  {
    rms += matter->atom(n)->A() * matter->weight_quan(n);
  }
  rms = rms / (gram/mole);

  DynLinArr< double > RLenAt(qat);
  DynLinArr< double > RuthAt(qat);
  for(n = 0; n < qat; n++)
  {
    RLenAt[n] = 716.4 * matter->atom(n)->A() / (gram/mole) /
      (matter->atom(n)->Z() * (matter->atom(n)->Z() + 1) 
       * log(287 / sqrt(double( matter->atom(n)->Z()    ))));
    RuthAt[n] = 4.0 * M_PI * matter->atom(n)->Z() * matter->atom(n)->Z() 
      * ELRAD * ELRAD * ELMAS * ELMAS;
  }
  DynLinArr< double > rm(qat);
  for(n = 0; n < qat; n++)
  {
    rm[n] = matter->atom(n)->A() / (gram/mole) * matter->weight_quan(n) / rms;
  }
  for(n = 0; n < qat; n++)
  {
    radiation_length += rm[n] / RLenAt[n];
  }
  radiation_length = 1.0 / ( matter->density()/(gram/cm3) * radiation_length);

  Rutherford_const = 0.0;
  for(n = 0; n < qat; n++)
  {
    Rutherford_const += matter->weight_quan(n) * RuthAt[n];
  }
  Rutherford_const *= matter->density()/(gram/cm3) * AVOGADRO /
    (matter->A_mean() / (gram/mole) );

  min_ioniz_pot = DBL_MAX;
  for(n=0; n < qat; n++)
  {
    if(min_ioniz_pot > apacs[n]->get_I_min())
      min_ioniz_pot = apacs[n]->get_I_min();
  }
  long qe = energy_mesh->get_q();
  long ne;
  ACS.put_qel(qe);
  ICS.put_qel(qe);
  epsip.put_qel(qe);
  epsi1.put_qel(qe);
  epsi2.put_qel(qe);
  int na;
  for(ne=0; ne<qe; ne++)
  {
    double e1 = energy_mesh->get_e(ne);
    double e2 = energy_mesh->get_e(ne+1);
    double sa = 0.0;
    double si = 0.0;
    for(na=0; na < qat; na++)
    {
      double t;
      //mcout<<"
      sa += matter->weight_quan(na) *
	(t=apacs[na]->get_integral_ACS(e1, e2))/(e2 - e1);
      //Iprint4n(mcout, ne, na, e1, e2);
      //Iprint2n(mcout, t, (e2 - e1));
      //Iprint2n(mcout, matter->weight_quan(na), t);
      check_econd11a(t , < 0 , "ACS: ne="<<ne
		     <<" e1="<<e1<<" e2="<<e2<<" na="<<na<<'\n',  mcerr);
      if(s_use_mixture_thresholds == 1)
      {
	si += matter->weight_quan(na) *
	  (t=apacs[na]->get_integral_TICS(e1, e2, min_ioniz_pot))/(e2 - e1);
      }
      else
      {
	si += matter->weight_quan(na) *
	  (t=apacs[na]->get_integral_ICS(e1, e2))/(e2 - e1);
      }
      check_econd11a(t , < 0 , "ICS: ne="<<ne
		     <<" e1="<<e1<<" e2="<<e2<<" na="<<na<<'\n',  mcerr);
    }
    ACS[ne] = sa; 
    check_econd11a(ACS[ne] , < 0 , "ne="<<ne<<'\n',  mcerr);
    ICS[ne] = si;
    check_econd11a(ICS[ne] , < 0 , "ne="<<ne<<'\n',  mcerr);
    //Iprint3n(mcout, ne, ACS[ne], ICS[ne]);

    double ec  = energy_mesh->get_ec(ne);
    double ec2 = ec * ec;
    epsip[ne]  = -wpla/ec2;

    epsi2[ne] = sa * C1_MEV2_MBN / ec * eldens / matter->Z_mean();  
  }
  /*
  // for debug:  replace the epsi2 by data from old version in order to check
  // math
  double temp[150] ={ 
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.1419910E-03,
 0.1338962E-03,
 0.1262628E-03,
 0.1190646E-03,
 0.1122768E-03,
 0.1058759E-03,
 0.9984001E-04,
 0.0000000E+00,
 0.1064728E-04,
 0.4732443E-03,
 0.4491931E-03,
 0.4265132E-03,
 0.4051262E-03,
 0.3849585E-03,
 0.3650968E-03,
 0.2199173E-04,
 0.7821930E-03,
 0.9424096E-03,
 0.9632859E-03,
 0.9491465E-03,
 0.9140529E-03,
 0.8654244E-03,
 0.8039676E-03,
 0.7347972E-03,
 0.6604797E-03,
 0.5804613E-03,
 0.5002352E-03,
 0.4177351E-03,
 0.3385186E-03,
 0.2581498E-03,
 0.1818544E-03,
 0.1073203E-03,
 0.6411548E-04,
 0.1476657E-04,
 0.1349197E-04,
 0.1287257E-04,
 0.1256007E-04,
 0.1235696E-04,
 0.1213073E-04,
 0.1182153E-04,
 0.1141035E-04,
 0.1090044E-04,
 0.1030713E-04,
 0.9650292E-05,
 0.8951113E-05,
 0.8232955E-05,
 0.7513849E-05,
 0.6808758E-05,
 0.6129448E-05,
 0.5484643E-05,
 0.4880457E-05,
 0.4320401E-05,
 0.3806222E-05,
 0.3338363E-05,
 0.2915556E-05,
 0.2536449E-05,
 0.2198645E-05,
 0.1899332E-05,
 0.1635490E-05,
 0.1404015E-05,
 0.1201637E-05,
 0.1025526E-05,
 0.8729276E-06,
 0.7411496E-06,
 0.8294041E-05,
 0.6746710E-05,
 0.5531698E-05,
 0.4631210E-05,
 0.4243355E-05,
 0.3546543E-05,
 0.2964335E-05,
 0.2475729E-05,
 0.2064629E-05,
 0.1718602E-05,
 0.1427703E-05,
 0.1183485E-05,
 0.9789117E-06,
 0.8079501E-06,
 0.6654361E-06,
 0.5469414E-06,
 0.4486697E-06,
 0.3673680E-06,
 0.3002568E-06,
 0.2450022E-06,
 0.1995937E-06,
 0.1623658E-06,
 0.1319047E-06,
 0.1070249E-06,
 0.8673433E-07,
 0.7021037E-07,
 0.5677791E-07,
 0.4587307E-07,
 0.3703099E-07,
 0.2986956E-07,
 0.2407540E-07,
 0.1939200E-07,
 0.1560976E-07,
 0.1255782E-07,
 0.1009702E-07,
 0.8114254E-08,
 0.6517703E-08,
 0.5232907E-08,
 0.4199575E-08,
 0.3368916E-08,
 0.2701507E-08,
 0.2165508E-08,
 0.1735230E-08,
 0.1389859E-08,
 0.9804396E-08,
 0.8122287E-08,
 0.6700766E-08,
 0.5507273E-08,
 0.4510964E-08,
 0.3683489E-08,
 0.2999349E-08,
 0.2436012E-08,
 0.1973836E-08,
 0.1595902E-08,
 0.1287780E-08,
 0.1037258E-08,
 0.8340705E-09,
 0.6696468E-09,
 0.5368663E-09,
 0.4298432E-09,
 0.3437312E-09,
 0.2745560E-09,
 0.2190689E-09,
 0.1746221E-09,
 0.1390643E-09,
 0.1106516E-09,
 0.8797314E-10,
 0.6989009E-10,
 0.5548491E-10,
 0.4401974E-10,
 0.3490204E-10,
 0.2765677E-10,
 0.2190359E-10,
 0.1733826E-10,
 0.1371784E-10};
  */
    /*  
  double temp[150] ={ 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.0000000E+00,
 0.1585396E-03,
 0.1472240E-03,
 0.1367159E-03,
 0.1269579E-03,
 0.1178964E-03,
 0.0000000E+00,
 0.6491638E-03,
 0.6064963E-03,
 0.5668742E-03,
 0.5300801E-03,
 0.4959122E-03,
 0.4627692E-03,
 0.7492764E-03,
 0.9366248E-03,
 0.9609964E-03,
 0.9356895E-03,
 0.8772041E-03,
 0.8076888E-03,
 0.7208660E-03,
 0.6244075E-03,
 0.5228503E-03,
 0.4191598E-03,
 0.3189717E-03,
 0.2189420E-03,
 0.1240326E-03,
 0.6306416E-04,
 0.1426123E-04,
 0.1306481E-04,
 0.1258360E-04,
 0.1232541E-04,
 0.1202579E-04,
 0.1158000E-04,
 0.1097223E-04,
 0.1022733E-04,
 0.9384479E-05,
 0.8488719E-05,
 0.7581212E-05,
 0.6693487E-05,
 0.5849020E-05,
 0.5063761E-05,
 0.4347229E-05,
 0.3703417E-05,
 0.3132855E-05,
 0.2633045E-05,
 0.2200008E-05,
 0.1828220E-05,
 0.1511601E-05,
 0.1243809E-05,
 0.1018738E-05,
 0.8309066E-06,
 0.6750423E-06,
 0.6989221E-05,
 0.5438153E-05,
 0.4332854E-05,
 0.3803051E-05,
 0.3033325E-05,
 0.2416732E-05,
 0.1920795E-05,
 0.1521996E-05,
 0.1201950E-05,
 0.9459229E-06,
 0.7418824E-06,
 0.5799263E-06,
 0.4518989E-06,
 0.3510858E-06,
 0.2719994E-06,
 0.2101837E-06,
 0.1620297E-06,
 0.1246423E-06,
 0.9569555E-07,
 0.7333256E-07,
 0.5610460E-07,
 0.4286134E-07,
 0.3270069E-07,
 0.2491863E-07,
 0.1896770E-07,
 0.1442350E-07,
 0.1095794E-07,
 0.8318077E-08,
 0.6309297E-08,
 0.4782196E-08,
 0.3622294E-08,
 0.2741993E-08,
 0.2074390E-08,
 0.1568389E-08,
 0.1033191E-07,
 0.8155243E-08,
 0.6394435E-08,
 0.4984566E-08,
 0.3865533E-08,
 0.2984020E-08,
 0.2294152E-08,
 0.1757347E-08,
 0.1341745E-08,
 0.1021420E-08,
 0.7755047E-09,
 0.5873820E-09,
 0.4439255E-09,
 0.3348409E-09,
 0.2521058E-09,
 0.1895007E-09,
 0.1422269E-09,
 0.1065989E-09,
 0.7979461E-10,
 0.5966064E-10,
 0.4455909E-10,
 0.3324720E-10,
 0.2478432E-10,
 0.1846001E-10,
 0.1373875E-10,
 0.1021758E-10,
 0.7593745E-11,
 0.5640174E-11,
 0.4186744E-11,
 0.3106167E-11,
 0.2303322E-11,
 0.1707186E-11,
 0.1264784E-11,
 0.9366401E-12,
 0.6933657E-12,
 0.5130927E-12,
 0.3795619E-12,
 0.2806937E-12,
 0.2075171E-12,
 0.1533750E-12,
 0.1133293E-12,
 0.8371894E-13,
 0.6183092E-13,
 0.4565565E-13,
 0.3370507E-13,
 0.2487790E-13,
 0.1835926E-13,
 0.1354641E-13,
 0.9993670E-14,
 0.7371604E-14,
 0.5436751E-14,
 0.4009229E-14,
 0.2956173E-14,
 0.2179464E-14,
 0.1606660E-14,
		      0.1184283E-14};
    */
  /*
  for(ne=0; ne<qe; ne++)
  {
    epsi2[ne] = temp[ne];
  }
  */





  // To do next loop we need all epsi2
  for(ne=0; ne<qe; ne++)
  {
    // double e1 = energy_mesh->get_e(ne);
    // double e2 = energy_mesh->get_e(ne+1);
    double ec = energy_mesh->get_ec(ne);
    double ec2=ec * ec;
    double s=0;
    long m;  // index for integration of energy
    for(m=0; m<qe; m++)
    {
      double em1 = energy_mesh->get_e(m);
      double em2 = energy_mesh->get_e(m+1);
      double ecm = energy_mesh->get_ec(m);
      if(m != ne)
      {
	s+= epsi2[m] * ecm * (em2 - em1)/
	  (ecm * ecm - ec2);
      }
      else
      {
	double ee1=(em1 + ecm)/2.0;
	double ee2=(em2 + ecm)/2.0;
	double ep1, ep2;  // extrapolated values to points ee1 and ee2
	if(m == 0)
	  ep1=epsi2[m] + (ee1 - ecm)*
	    (epsi2[m+1]-epsi2[m])/
	    (energy_mesh->get_ec(m+1) - ecm);
	else
	  ep1 = epsi2[m-1] + (ee1 - energy_mesh->get_ec(m-1))*
	    (epsi2[m] - epsi2[m-1])/
	    (ecm - energy_mesh->get_ec(m-1));
	if(m < qe - 1)
	  ep2 = epsi2[m] + (ee2 - ecm) *
	    (epsi2[m+1] - epsi2[m])/
	    (energy_mesh->get_ec(m+1) - ecm);
	else
	  ep2 = epsi2[m] + (ee2 - ecm) *
	    (epsi2[m] - epsi2[m-1])/
	    (ecm - energy_mesh->get_ec(m-1));
	
	s = s + ep1 * ee1 * (ecm - em1)/(ee1 * ee1 - ec2);
	s = s + ep2 * ee2 * (em2 - ecm)/(ee2 * ee2 - ec2);
      }
    }
    epsi1[ne]=(2.0/M_PI)*s;
  }
}

void HeedMatterDef::replace_epsi12(const String& file_name)
{
  mfunnamep("void HeedMatterDef::replace_epsi12(const String& file_name)");

#ifdef USE_STLSTRING
  std::ifstream file(file_name.c_str());
#else
  std::ifstream file(hist_file_name);
#endif
  if (!file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << file_name << std::endl;
    spexit(mcerr);
  } else {
    mcout << "file " << file_name << " is opened" << std::endl;
  }
  long qe=0;  // number of points in input mesh
  file >> qe;
  check_econd11(qe , <= 2 , mcerr);
  
  DynLinArr< double > ener(qe);
  DynLinArr< double > eps1(qe);
  DynLinArr< double > eps2(qe);

  long ne;
  for( ne = 0; ne < qe; ne++)
  {
    file >> ener[ne] >> eps1[ne] >> eps2[ne];
    check_econd11( eps2[ne] , < 0.0 , mcerr);
    if(ne > 0 )
    {
      check_econd12( ener[ne] , < , ener[ne-1] , mcerr);
    }
  }

  PointCoorMesh< double, DynLinArr< double > > pcmd
    ( qe, &(ener) );
  double emin = ener[0] - 0.5 * (ener[1] - ener[0]);
  double emax = ener[qe-1] +  0.5 * (ener[qe-1] - ener[qe-2]);

  //Iprint2n(mcout, emin, emax);
  qe = energy_mesh->get_q();  // changing to our
  for( ne = 0; ne < qe; ne++)
  {
    double ec = energy_mesh->get_ec(ne);
    epsi1[ne] = t_value_straight_point_ar< double , DynLinArr< double >,
      PointCoorMesh< double, DynLinArr< double > > >
      (pcmd,  // dimension q 
       eps1,                  // dimension q-1
       ec,
       0,
       1, emin,
       1, emax);
    epsi2[ne] = t_value_straight_point_ar< double , DynLinArr< double >,
      PointCoorMesh< double, DynLinArr< double > > >
      (pcmd,  // dimension q 
       eps2,                  // dimension q-1
       ec,
       1,
       1, emin,
       1, emax);
    //Iprint3n(mcout, ec, epsi1[ne], epsi2[ne]);
  }
}

  

void HeedMatterDef::print(std::ostream& file, int l) const 
{
  if (l <= 0) return;
  Ifile << "HeedMatterDef:\n";
  indn.n += 2;
  matter->print(file, 1);
  if (l >=2) {
    long q = matter->qatom();
    long n;
    Ifile << "Printing " << q << " photoabsorption cross sections:\n";
    indn.n += 2;
    for (n = 0; n < q; n++) {
      apacs[n]->print(file, l - 1);
    }
    indn.n -= 2;
  }
  Iprintan(file, eldens_cm_3, "1/cm^3");
  Iprintan(file, eldens, "MeV^3");
  Iprintan(file, xeldens, "MeV^2/cm");
  Iprintn(file, wpla);
  Iprintn(file, radiation_length);
  Iprintan(file, Rutherford_const, "1/cm^3");
  Iprintn(file, W);
  Iprintn(file, F);
  Iprintn(file, min_ioniz_pot);
  Iprintn(file, energy_mesh->get_q());
  if (l >= 2) {
    long qe = energy_mesh->get_q();
    long ne;
    indn.n += 2;
    Ifile << " ne       energy      ACS(Mb)      ICS(Mb) ACS(1/MeV^2) ICS(1/MeV^2)       epsip       epsi1       epsi2   (1+epsi1)^2+epsi2^2\n";
    for (ne = 0; ne < qe; ne++) {
      //double et = pow( energy_mesh->get_ec(ne), 2.0);
      Ifile << std::setw(3) << ne << ' ' 
            << std::setw(12) << energy_mesh->get_e(ne) << ' '
	    << std::setw(12) << ACS[ne] << ' '
	    << std::setw(12) << ICS[ne] << ' '
	    << std::setw(12) << ACS[ne]*C1_MEV2_MBN << ' '
	    << std::setw(12) << ICS[ne]*C1_MEV2_MBN << ' '
	    << std::setw(12) << epsip[ne] << ' '
	    << std::setw(12) << epsi1[ne] << ' '
	    // << std::setw(12) << epsip[ne] * et << ' '
	    // << std::setw(12) << epsi1[ne] * et << ' '
	    << std::setw(12) << epsi2[ne] << ' '
	    << std::setw(12) << pow( (1+epsi1[ne]), 2.0) + pow(epsi2[ne], 2.0) 
	    << " \n";
    }
    indn.n -= 2;
  }
  indn.n -= 2;
}


//HeedMatterDef HeedGasHydrogen2(GasHydrogen2, Hydrogen_for_H2_PACS,
//			       37.0e-6, standard_factor_Fano);
