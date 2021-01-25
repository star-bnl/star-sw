/* 
 *  $Id: StHyperNuclei.hh,v 1.2 2021/01/25 11:19:23 jwebb Exp $ 
 *
 *  $Log: StHyperNuclei.hh,v $
 *  Revision 1.2  2021/01/25 11:19:23  jwebb
 *  Ensure that an instance of the particle data is available.
 *
 *  Revision 1.1  2021/01/13 12:16:39  jwebb
 *
 *  Definitions for H4-Lambda, He4-Lambda and He5-Lambda hypernuclei.
 *
 *
 */
#ifndef StHyperNuclei_hh                                                                                                                                                                                   
#define StHyperNuclei_hh     

#include "StIon.hh"
#include "StarPDGEncoding.hh"
#include "SystemOfUnits.h"


class StH4Lambda : public StIon {
public:
    static StH4Lambda* instance() {
      return &mH4Lambda;
    };
    
private:
    static StH4Lambda mH4Lambda;
    
    StH4Lambda(const string  &aName="H4Lambda",  
		  double mass =3.92727 *GeV,
		  double width=0.0*MeV, /* TODO */
		  double charge=+1 *eplus,
		  int              iSpin=0,
		  int              iParity=0,
		  int              iConjugation=0,
		  int              iIsospin=0,   
		  int              iIsospinZ=0, 
		  int              gParity=0,
		  const string  &  pType="hypernucleus",
		  int              lepton=0,
		  int              baryon=4,
                  int              encoding=hid(1,3,1),
		  bool             stable=false,
    	       double           lifetime=2.632e-10) : StIon(aName,mass,width,charge,iSpin,iParity,iConjugation,iIsospin,iIsospinZ,gParity,pType,lepton,baryon,encoding,stable,lifetime)
  { /* nada */ }
};
class StHe4Lambda : public StIon {
public:
    static StHe4Lambda* instance() {
      return &mHe4Lambda;
    };
    
private:
    static StHe4Lambda mHe4Lambda;
    
    StHe4Lambda(const string  &aName="He4Lambda",  
		  double mass =3.92168 *GeV,
		  double width=0.0*MeV, /* TODO */
		  double charge=+2 *eplus,
		  int              iSpin=0,
		  int              iParity=0,
		  int              iConjugation=0,
		  int              iIsospin=0,   
		  int              iIsospinZ=0, 
		  int              gParity=0,
		  const string  &  pType="hypernucleus",
		  int              lepton=0,
		  int              baryon=4,
   		  int              encoding=hid(2,3,1),
		  bool             stable=false,
		double           lifetime=2.632e-10) : StIon(aName,mass,width,charge,iSpin,iParity,iConjugation,iIsospin,iIsospinZ,gParity,pType,lepton,baryon,encoding,stable,lifetime)
  { /* nada */ }
};
class StHe5Lambda : public StIon {
public:
    static StHe5Lambda* instance() {
      return &mHe5Lambda;
    };
    
private:
    static StHe5Lambda mHe5Lambda;
    
    StHe5Lambda(const string  &aName="He5Lambda",  
		  double mass =4.83978 *GeV,
		  double width=0.0*MeV, /* TODO */
		  double charge=+3 *eplus,
		  int              iSpin=0,
		  int              iParity=0,
		  int              iConjugation=0,
		  int              iIsospin=0,   
		  int              iIsospinZ=0, 
		  int              gParity=0,
		  const string  &  pType="hypernucleus",
		  int              lepton=0,
		  int              baryon=4,
 		  int              encoding=hid(2,4,1),
		  bool             stable=false,
		double           lifetime=2.632e-10) : StIon(aName,mass,width,charge,iSpin,iParity,iConjugation,iIsospin,iIsospinZ,gParity,pType,lepton,baryon,encoding,stable,lifetime)
  { /* nada */ }
};

#endif
