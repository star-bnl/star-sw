///////////////////////////////////////////////////////////////////////////
//
//    Copyright 2010
//
//    This file is part of starlight.
//
//    starlight is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    starlight is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with starlight. If not, see <http://www.gnu.org/licenses/>.
//
///////////////////////////////////////////////////////////////////////////
//
// File and Version Information:
// $Rev::                             $: revision of last commit
// $Author: jwebb $: author of last commit
// $Date: 2012/11/27 22:27:32 $: date of last commit
//
// Description:
//
//
//
///////////////////////////////////////////////////////////////////////////

#include <iostream>
#include <fstream>
#include <cmath>
#include <vector>


#include "starlightconstants.h"
#include "gammagammasingle.h"
#ifdef ENABLE_PYTHIA
#include "PythiaStarlight.h"
#endif

using namespace std;


//______________________________________________________________________________
Gammagammasingle::Gammagammasingle(inputParameters& input, beamBeamSystem& bbsystem)
: eventChannel(input, bbsystem)
{
#ifdef ENABLE_PYTHIA
  pythia = new pythiaStarlight();

  std::cout << "Initialising pythia" << std::endl;
  //TODO: remove the env var stupidity...	
  const char* pythiaPath = getenv("PYTHIADIR");
  if(pythiaPath != 0)
    {
      char path[128];
      sprintf(path, "%s/xmldoc\0", pythiaPath);
      pythia->init(std::string(path));
    }
  else
    {
      std::cerr << "ERROR: Trying to initialise pythia but cannot find the PYTHIADIR environment variable, please set it accordingly" << std::endl;
    }
#endif
  //Initialize randomgenerator with our seed.
  _randy.SetSeed(input.randomSeed());
  cout<<"Randy in Single Meson construction: "<<_randy.Rndom()<<endl;
  //Storing inputparameters into protected members for use
  _GGsingInputnumw=input.nmbWBins();
  _GGsingInputnumy=input.nmbRapidityBins();
  _GGsingInputpidtest=input.prodParticleType();
  _GGsingInputGamma_em=input.beamLorentzGamma();
  cout<<"SINGLE MESON pid test: "<<_GGsingInputpidtest<<endl;
  //reading in luminosity tables
  read();
  //Now calculating crosssection
  singleCrossSection();
}


//______________________________________________________________________________
Gammagammasingle::~Gammagammasingle()
{ }


//______________________________________________________________________________
void Gammagammasingle::singleCrossSection()
{
  //This function carries out a delta function cross-section calculation. For reference, see STAR Note 243, Eq. 8
  //Multiply all _Farray[] by _f_max
  double _sigmaSum=0.,remainw=0.;//_remainwd=0.;
  int ivalw =0;//_ivalwd;
  //calculate the differential cross section and place in the sigma table
  _wdelta=getMass();
  for(int i=0;i<_GGsingInputnumw;i++){
    for(int j=0;j<_GGsingInputnumy;j++){
      //Change 8 to 4 in the following equation, to fix integration of a delta function.
      //Now, it matches the standard literature(cf. Eq. 67 of G.Baur et al., Phys. Rep. 364, 359(2002).
      //STAR Note 243 gives the incorrect 4.
      _sigmax[i][j]=(getSpin()*2.+1.)*4*starlightConstants::pi*starlightConstants::pi*getWidth()/
	(getMass()*getMass()*getMass())*_f_max*_Farray[i][j]*starlightConstants::hbarc*starlightConstants::hbarc/100.;
    }
  }
  //find the index, i,for the value of w just less than the mass because we want to use the value from the sigma table that has w=mass

  for(int i=0;i<_GGsingInputnumw;i++){
    if(getMass()>_Warray[i]) ivalw=i;
  }

  remainw = (getMass()-_Warray[ivalw])/(_Warray[ivalw+1]-_Warray[ivalw+1]-_Warray[ivalw]);
  _ivalwd = ivalw;
  _remainwd = remainw;
  //if we are interested rho pairs at threshold, the just set sigma to 100nb
  switch(_GGsingInputpidtest){
  case starlightConstants::ZOVERZ03:
    _sigmaSum =0.;
    for(int j=0;j<_GGsingInputnumy-1;j++){
                        _sigmaSum = _sigmaSum +2.0*(_Yarray[j+1]-_Yarray[j])*
			  100.0E-9*(.1/getMass())*((1.-remainw)*_f_max*
						   (_Farray[ivalw][j]+_Farray[ivalw][j])/2.+remainw*_f_max*
						   (_Farray[ivalw+1][j]+_Farray[ivalw+1][j+1])/2.);
    }
    break;
  default:
    //Sum to find the total cross-section
    //The two iss to account for the fact that we integrate over just 1/2 of the rapidity range
    _sigmaSum =0.;
    for(int j =0;j<_GGsingInputnumy-1;j++){
                        _sigmaSum = _sigmaSum+2.*
			  (_Yarray[j+1]-_Yarray[j])*((1.-remainw)*
						   (_sigmax[ivalw][j]+_sigmax[ivalw][j+1])/2.+remainw*
						   (_sigmax[ivalw+1][j]+_sigmax[ivalw+1][j+1])/2.);
    }
  }
  cout <<"The total cross-section is: "<<_sigmaSum<<" barns."<<endl;
  return;
}


//______________________________________________________________________________
void Gammagammasingle::pickw(double &w)
{
  //This function picks a w for the 2-photon calculation. 
  double sgf=0.,signorm=0.,x=0.,remainarea=0.,remainw=0.,a=0.,b=0.,c=0.;
  int ivalw=0;
  
  double * _sigofw;
  double * sgfint;
  _sigofw = new double[starlightLimits::MAXWBINS];
  sgfint = new double[starlightLimits::MAXYBINS];
 
  if(_wdelta != 0){
    w=_wdelta;
    ivalw=_ivalwd;
    remainw=_remainwd;
  }
  else{
    //deal with the case where sigma is an array
    //_sigofw is simga integrated over y using a linear interpolation
    //sigint is the integral of sgfint, normalized
    
    //integrate sigma down to a function of just w
    for(int i=0;i<_GGsingInputnumw;i++){
      _sigofw[i]=0.;
      for(int j=0;j<_GGsingInputnumy-1;j++){
	_sigofw[i] = _sigofw[i]+(_Yarray[j+1]-_Yarray[j])*(_sigmax[i][j+1]+_sigmax[i][j])/2.;
      }
    }
    //calculate the unnormalized sgfint array
    sgfint[0]=0.;
    for(int i=0;i<_GGsingInputnumw-1;i++){
      sgf=(_sigofw[i+1]+_sigofw[i])*(_Warray[i+1]-_Warray[i])/2.;
      sgfint[i+1]=sgfint[i]+sgf;
    }
    //normalize sgfint array
    signorm=sgfint[_GGsingInputnumw-1];
    
    for(int i=0;i<_GGsingInputnumw;i++){
      sgfint[i]=sgfint[i]/signorm;
    }
    //pick a random number
    x = _randy.Rndom();//random()/(RAND_MAX+1.0);
    //compare x and sgfint to find the ivalue which is just less than the random number x
    for(int i=0;i<_GGsingInputnumw;i++){
      if(x > sgfint[i]) ivalw=i;
    }
    //remainder above ivalw
    remainarea = x - sgfint[ivalw];
    
    //figure out what point corresponds to the excess area in remainarea
    c = -remainarea*signorm/(_Warray[ivalw+1]-_Warray[ivalw]);
    b = _sigofw[ivalw];
    a = (_sigofw[ivalw+1]-_sigofw[ivalw])/2.;
    if(a==0.){
      remainw = -c/b;
    }
    else{
      remainw = (-b+sqrt(b*b-4.*a*c))/(2.*a);
    }
    _ivalwd = ivalw;
    _remainwd = remainw;
    //calculate the w value
    w = _Warray[ivalw]+(_Warray[ivalw+1]-_Warray[ivalw])*remainw;
    }

  delete[] _sigofw;
  delete[] sgfint;
}


//______________________________________________________________________________
void Gammagammasingle::picky(double &y)
{
  double * sigofy;
  double * sgfint;
  sigofy = new double[starlightLimits::MAXYBINS];
  sgfint = new double[starlightLimits::MAXYBINS];
  
  double remainw =0.,remainarea=0.,remainy=0.,a=0.,b=0.,c=0.,sgf=0.,signorm=0.,x=0.;
  int ivalw=0,ivaly=0;
  
  ivalw=_ivalwd;
  remainw=_remainwd;
  //average over two colums to get y array
  for(int j=0;j<_GGsingInputnumy;j++){
    sigofy[j]=_sigmax[ivalw][j]+(_sigmax[ivalw+1][j]-_sigmax[ivalw][j])*remainw;
  }
  //calculate the unnormalized sgfint
  
  sgfint[0]=0.;
  for(int j=0;j<_GGsingInputnumy-1;j++){
    sgf = (sigofy[j+1]+sigofy[j])/2.;
    sgfint[j+1]=sgfint[j]+sgf*(_Yarray[j+1]-_Yarray[j]);
  }
  
  //normalize the sgfint array
  signorm = sgfint[_GGsingInputnumy-1];
  
  for(int j=0;j<_GGsingInputnumy;j++){
    sgfint[j]=sgfint[j]/signorm;
  }
  //pick a random number
  x = _randy.Rndom();//random()/(RAND_MAX+1.0);
  //compare x and sgfint to find the ivalue which is just less then the random number x
  for(int i=0;i<_GGsingInputnumy;i++){
    if(x > sgfint[i]) 
      ivaly = i;
  }
  //remainder above ivaly
  remainarea = x - sgfint[ivaly];
  //figure what point corresponds to the leftover area in remainarea
  c = -remainarea*signorm/(_Yarray[ivaly+1]-_Yarray[ivaly]);
  b = sigofy[ivaly];
  a = (sigofy[ivaly+1]-sigofy[ivaly])/2.;
  if(a==0.){
    remainy = -c/b;
  }
  else{
    remainy = (-b + sqrt(b*b-4.*a*c))/(2.*a);
  }
  //calculate the y value
  y = _Yarray[ivaly]+(_Yarray[ivaly+1]-_Yarray[ivaly])*remainy;
  delete[] sigofy;
  delete[] sgfint;
}


//______________________________________________________________________________
void Gammagammasingle::parentMomentum(double w,double y,double &E,double &px,double &py,double &pz)
{
  //this function calculates px,py,pz,and E given w and y
  double anglepp1=0.,anglepp2=0.,pp1=0.,pp2=0.,E1=0.,E2=0.,signpx=0.,pt=0.;
  
  //E1 and E2 are for the 2 photons in the CM frame
  E1 = w*exp(y)/2.;
  E2 = w*exp(-y)/2.;
  //pz = E1-E2;
  //calculate px and py
  //to get x and y components-- phi is random between 0 and 2*pi
  anglepp1 = _randy.Rndom();//random()/(RAND_MAX+1.0);
  anglepp2 = _randy.Rndom();//random()/(RAND_MAX+1.0);
  
  pp1 = pp(E1);
  pp2 = pp(E2);
  px = pp1*cos(2.*starlightConstants::pi*anglepp1)+pp2*cos(2.*starlightConstants::pi*anglepp2);
  py = pp1*sin(2.*starlightConstants::pi*anglepp1)+pp2*sin(2.*starlightConstants::pi*anglepp2);
  //Compute vector sum Pt=Pt1+Pt2 to find pt for the produced particle
  pt = sqrt(px*px+py*py);
  //W is the mass of the produced particle (not necessarily on-mass-shell).Now compute its energy and pz
  E = sqrt(w*w+pt*pt)*cosh(y);
  pz= sqrt(w*w+pt*pt)*sinh(y);
  signpx = _randy.Rndom();//random()/(RAND_MAX+1.0);
  //pick the z direction
  if(signpx > 0.5) 
    pz = -pz;	
}


//______________________________________________________________________________
double Gammagammasingle::pp(double E)
{
  //  will probably have to pass in beambeamsys? that way we can do beam1.formFactor(t) or beam2..., careful with the way sergey did it for asymmetry
  //  returns on random draw from pp(E) distribution
      
  double ereds =0.,Cm=0.,Coef=0.,x=0.,pp=0.,test=0.,u=0.;
  double singleformfactorCm=0.,singleformfactorpp1=0.,singleformfactorpp2=0.;
  int satisfy =0;
        
  ereds = (E/_GGsingInputGamma_em)*(E/_GGsingInputGamma_em);
  Cm = sqrt(3.)*E/_GGsingInputGamma_em;
  //the amplitude of the p_t spectrum at the maximum
  singleformfactorCm=_bbs.beam1().formFactor(Cm*Cm+ereds);
  //Doing this once and then storing it as a double, which we square later...SYMMETRY?using beam1 for now.
  Coef = 3.0*(singleformfactorCm*singleformfactorCm*Cm*Cm*Cm)/((2.*(starlightConstants::pi)*(ereds+Cm*Cm))*(2.*(starlightConstants::pi)*(ereds+Cm*Cm)));
        
  //pick a test value pp, and find the amplitude there
  x = _randy.Rndom();//random()/(RAND_MAX+1.0);
  pp = x*5.*starlightConstants::hbarc/_bbs.beam1().nuclearRadius(); //Will use nucleus #1, there should be two for symmetry//nextline
  singleformfactorpp1=_bbs.beam1().formFactor(pp*pp+ereds);
  test = (singleformfactorpp1*singleformfactorpp1)*pp*pp*pp/((2.*starlightConstants::pi*(ereds+pp*pp))*(2.*starlightConstants::pi*(ereds+pp*pp)));

  while(satisfy==0){
    u = _randy.Rndom();//random()/(RAND_MAX+1.0);
    if(u*Coef <= test){
      satisfy =1;
    }
    else{
      x =_randy.Rndom();//random()/(RAND_MAX+1.0);
      pp = 5*starlightConstants::hbarc/_bbs.beam1().nuclearRadius()*x;
      singleformfactorpp2=_bbs.beam1().formFactor(pp*pp+ereds);//Symmetry
      test = (singleformfactorpp2*singleformfactorpp2)*pp*pp*pp/(2.*starlightConstants::pi*(ereds+pp*pp)*2.*starlightConstants::pi*(ereds+pp*pp));
    }
  }
  return pp;
}


//______________________________________________________________________________
void Gammagammasingle::twoBodyDecay(starlightConstants::particleTypeEnum &ipid,double /*E*/,double W,double px0,double py0,double pz0,double &px1,double &py1,double &pz1,double &px2,double &py2,double &pz2,int &iFbadevent)
{
  //     This routine decays a particle into two particles of mass mdec,
  //     taking spin into account
  
  double mdec=0.,E1=0.,E2=0.;
  double pmag,ytest=0.;
  double phi,theta,xtest,dndtheta,Ecm;
  double  betax,betay,betaz;
  
  //    set the mass of the daughter particles
  switch(_GGsingInputpidtest){ 
  case starlightConstants::ZOVERZ03:
  case starlightConstants::F2:	
    mdec = starlightConstants::pionChargedMass;
    break;
  case starlightConstants::F2PRIME:
    //  decays 50% to K+/K-, 50% to K_0's
    ytest = _randy.Rndom();
    if(ytest >= 0.5){
      mdec = starlightConstants::kaonChargedMass;
    }
    else{
      mdec = 0.493677;
    }
    break;
  default :
    cout<<"No default mass selected for single photon-photon particle, expect errant results"<<endl;
  }
  
  //Calculating the momentum's magnitude
  //add switch for rho pairs at threshold and everything else.
  switch(_GGsingInputpidtest){
  case starlightConstants::ZOVERZ03:	//the rho pairs produced at threshold
    pmag = sqrt(getMass()*getMass()/4. - mdec*mdec);
    break;
  default :
    if(W < 2*mdec){
      cout<<" ERROR: W="<<W<<endl;
      iFbadevent = 1;
      return;
    }
    pmag = sqrt(W*W/4. - mdec*mdec);
  }
  //     pick an orientation, based on the spin
  //      phi has a flat distribution in 2*pi
  phi = _randy.Rndom()*2.*starlightConstants::pi; //(random()/(RAND_MAX+1.0))* 2.*starlightConstants::pi;
  
  //     find theta, the angle between one of the outgoing particles and
  //    the beamline, in the frame of the two photons
  //this will depend on spin, F2,F2' and z/z03 all have spin 2, all other photonphoton-single mesons are handled by jetset
  //Applies to spin2 mesons.
 L300td:
  theta = starlightConstants::pi*_randy.Rndom();
  xtest = _randy.Rndom();
  dndtheta = sin(theta)*sin(theta)*sin(theta)*sin(theta)*sin(theta);
  if(xtest > dndtheta)
    goto L300td;

  //     compute unboosted momenta
  px1 = sin(theta)*cos(phi)*pmag;
  py1 = sin(theta)*sin(phi)*pmag;
  pz1 = cos(theta)*pmag;
  px2 = -px1;
  py2 = -py1;
  pz2 = -pz1;
  //        compute energies
  //Changed mass to W 11/9/2000 SRK
  Ecm = sqrt(W*W+px0*px0+py0*py0+pz0*pz0);
  E1 = sqrt(mdec*mdec+px1*px1+py1*py1+pz1*pz1);
  E2 = sqrt(mdec*mdec+px2*px2+py2*py2+pz2*pz2);

  //     Lorentz transform into the lab frame
  // betax,betay,betaz are the boost of the complete system
  betax = -(px0/Ecm);
  betay = -(py0/Ecm);
  betaz = -(pz0/Ecm);
  
  transform (betax,betay,betaz,E1,px1,py1,pz1,iFbadevent);
  transform (betax,betay,betaz,E2,px2,py2,pz2,iFbadevent);
  
  
  if(iFbadevent == 1)
    return;
  
  //       change particle id from that of parent to that of daughters

  switch(_GGsingInputpidtest){
    //These decay into a pi+ pi- pair
  case starlightConstants::ZOVERZ03:
  case starlightConstants::F2:
    ipid=starlightConstants::PION;
    break;
  case starlightConstants::F2PRIME:
    if( ytest >= 0.5 )
      {
	//Decays 50/50 into k+ k- or k_s k_l
	ipid=starlightConstants::KAONCHARGE;	
      }
    else
      {
	ipid=starlightConstants::KAONNEUTRAL;
      }	
    break;
  default:
    cout<<"Rethink the daughter particles"<<endl;
  }
}


//______________________________________________________________________________
starlightConstants::event Gammagammasingle::produceEvent(int &/*ievent*/)
{
  // Not in use anymore, default event struct returned
  return starlightConstants::event();
}


//______________________________________________________________________________
// fix it ... lost functionality 
//starlightConstants::event Gammagammasingle::produceEvent(int &ievent)
upcEvent Gammagammasingle::produceEvent()
{
  //	 cout << "NOT IMPLEMENTED!" << endl;
	 
  //	 return upcEvent();
	 int ievent = 0;

  //    returns the vector with the decay particles inside.
  //	onedecayparticle single;
  starlightConstants::event single;
  double comenergy = 0.;
  double rapidity = 0.;
  double parentE = 0.;
  double parentmomx=0.,parentmomy=0.,parentmomz=0.;
  int iFbadevent=0;
  starlightConstants::particleTypeEnum ipid = starlightConstants::UNKNOWN;
  double px2=0.,px1=0.,py2=0.,py1=0.,pz2=0.,pz1=0.;
  double px3=0.,px4=0.,py3=0.,py4=0.,pz3=0.,pz4=0.;
  //  double theta=0.,phi=0.;//angles from jetset
  double xtest=0.,ztest=0.;
 

  //this function decays particles and writes events to a file
  //zeroing out the event structure
  single._numberOfTracks=0;
  for(int i=0;i<4;i++){
    single.px[i]=0.;
    single.py[i]=0.;
    single.pz[i]=0.;
    single._fsParticle[i]=starlightConstants::UNKNOWN;
    single._charge[i]=0;
  }
  
  pickw(comenergy);
  picky(rapidity);
  parentMomentum(comenergy,rapidity,parentE,parentmomx,parentmomy,parentmomz);
  
  switch(_GGsingInputpidtest){
  case starlightConstants::ZOVERZ03:
    //Decays into two pairs.
    parentmomx=parentmomx/2.;
    parentmomy=parentmomy/2.;
    parentmomz=parentmomz/2.;
    //Pair #1	
    twoBodyDecay(ipid,parentE,comenergy,parentmomx,parentmomy,parentmomz,px1,py1,pz1,px2,py2,pz2,iFbadevent);
    //Pair #2
    twoBodyDecay(ipid,parentE,comenergy,parentmomx,parentmomy,parentmomz,px3,py3,pz3,px4,py4,pz4,iFbadevent);
    //Now add them to vectors to be written out later.
		
    single._numberOfTracks=4;//number of tracks per event
    if (iFbadevent==0){
      xtest = _randy.Rndom();//random()/(RAND_MAX+1.0);
      ztest = _randy.Rndom();
      //Assigning charges randomly.
      if (xtest<0.5){
	single._charge[0]=1;//q1=1;
	single._charge[1]=-1;//q2=-1;
      }
      else{
	single._charge[0]=1;//q1=-1;
	single._charge[1]=-1;//q2=1;
      }
      if (ztest<0.5){
	single._charge[2]=1;//q3=1;
	single._charge[3]=-1;//q4=-1;
      }
      else{
	single._charge[2]=-1;//q3=-1;
	single._charge[3]=1;//q4=1;
      }
      //Track #1
      single.px[0]=px1;
      single.py[0]=py1;
      single.pz[0]=pz1;
      single._fsParticle[0]=ipid;
      //Track #2                                                                                                                      
      single.px[1]=px2;
      single.py[1]=py2;
      single.pz[1]=pz2;
      single._fsParticle[1]=ipid;
      //Track #3
      single.px[2]=px3;
      single.py[2]=py3;
      single.pz[2]=pz3;
      single._fsParticle[2]=ipid;
      //Track #4
      single.px[3]=px4;
      single.py[3]=py4;
      single.pz[3]=pz4;
      single._fsParticle[3]=ipid;
      
      ievent=ievent+1;
    }	
    
    break;
  case starlightConstants::F2:
  case starlightConstants::F2PRIME:
    twoBodyDecay(ipid,parentE,comenergy,parentmomx,parentmomy,parentmomz,px1,py1,pz1,px2,py2,pz2,iFbadevent);
    
    single._numberOfTracks=2;
    if (iFbadevent==0){
      xtest = _randy.Rndom();//random()/(RAND_MAX+1.0);
      if (xtest<0.5){
	single._charge[0]=1;//q1=1;
	single._charge[1]=-1;//q2=-1;
      }
      else{
	single._charge[0]=-1;//q1=-1;
	single._charge[1]=1;//q2=1;
      }	
      //Track #1
      single.px[0]=px1;
      single.py[0]=py1;
      single.pz[0]=pz1;
      single._fsParticle[0]=ipid; 
      //Track #2
      single.px[1]=px2;
      single.py[1]=py2;
      single.pz[1]=pz2;
      single._fsParticle[1]=ipid;
      ievent=ievent+1;
    }
    break;
  default:
    //This will be jetset stuff...just need to add link to jetset program
   #ifdef ENABLE_PYTHIA    
    //cout << "Submitting "<<_GGsingInputpidtest<<" to jetset to decay."<<endl;
    //calculate theta and phi of the particle being submitted to jetset.
    //We do not need to do this anymore?  Pythia takes pid,px,py,pz,e, and m
    //thephi(comenergy,parentmomx,parentmomy,parentmomz,parentE,theta,phi);
    ///lu1ent(0,pid,E,theta,phi);
    //Lets begin to link Pythia8 into starlight.  Probably best to do this in main.cpp
    //However, we only do pythia here for the time being.  Lets be self contained in case
    //We want to remove it later or whatever other reason.
    /*#include "Pythia.h"
      using namespace Pythia8;
      // Generator; shorthand for event.
      Pythia pythia;
      Event& event = pythia.event;
      // Key requirement: switch off ProcessLevel, and thereby also PartonLevel.
      pythia.readString("ProcessLevel:all = off");
      //When limittau0 is on, only particles with tau0<tau0max are decayed.
      pythia.readString("ParticleDecays:limitTau0 = on");
      //Default tau0 max is 10mm/c, we are setting it to 1mm/c
      pythia.readString("ParticleDecays:tau0Max = 1");
      // Provide printout of initial information.
      pythia.settings.listChanged();//Useful for earlier stages.
      // Initialize.
      pythia.init();*/
    
    //Reseting the event.
    
    Pythia8::Event &event = pythia->getPythia()->event;
    event.reset();
    //Adding an event to pythia
    //Adding Particles information (id,status,color,anticolor,px,py,pz,energy,restmass)
    double restmass=0.;
    restmass=getMass();
    double tempx=0.,tempy=0.,tempz=0.;
    event.append(_GGsingInputpidtest,1,0,0,parentmomx,parentmomy,parentmomz,parentE,restmass);
    
    // Generate events. Quit if failure.
    
    if (!pythia->getPythia()->next()) {
      cout << " Event generation aborted prematurely, owing to error! Gammagammasingle::produceevent\n";
      break;
    }
    single._vertx[0]=0.;
    single._verty[0]=0.;
    single._vertz[0]=0.;
    single._numberOfVertices=1;
    
    single._numberOfTracks=(event.size()-1);
    for(int j=1;j<event.size();j++){//skipping event[0], this just outputs the mother as if the beam/system
      int b=j-1;//start counting at 0 for arrays in single
      single._charge[b]=1;//Charge should be returned on the id
      single.px[b]=event[j].px();
      single.py[b]=event[j].py();
      single.pz[b]=event[j].pz();
      single._fsParticle[b]=event[j].id();
      single._mother1[b]=event[j]._mother1();
      single._mother2[b]=event[j]._mother2();
      single._daughter1[b]=event[j]._daughter1();
      single._daughter2[b]=event[j]._daughter2();
      //might have to change the type for _fsParticle since pythia could return something we do not have defined.
      if(!(event[j].xProd()==tempx&&event[j].yProd()==tempy&&event[j].zProd()==tempz)){
	single._numberOfVertices++;
	single._vertx[single._numberOfVertices-1]=event[j].xProd();
	single._verty[single._numberOfVertices-1]=event[j].yProd();
	single._vertz[single._numberOfVertices-1]=event[j].zProd();
	tempx=event[j].xProd();
	tempy=event[j].yProd();
	tempz=event[j].zProd();
      }
    }
    ievent=ievent+1;
    #else
    break;
    #endif
  }

  return upcEvent(single);
}


//______________________________________________________________________________
void Gammagammasingle::thephi(double W,double px,double py,double pz,double E,double &theta,double &phi)
{
  //     This subroutine calculates angles for channels decayed by jetset.
  //    subroutine thephi(W,px,py,pz,E,theta,phi)
  E = sqrt (W*W+px*px+py*py+pz*pz);

  theta = acos(pz/sqrt(px*px+py*py+pz*pz));
  phi = acos(px/sqrt(px*px+py*py));
  
  if ((px == 0)  && (py == 0))
    phi = 0.;
  if (py < 0)
    phi = 2*starlightConstants::pi - phi;
}


//______________________________________________________________________________
double Gammagammasingle::getMass()
{
  double singlemass=0.;
  switch(_GGsingInputpidtest){
  case starlightConstants::ETA:
    singlemass=0.54730;
    break;
  case starlightConstants::ETAPRIME:
    singlemass=0.95777;
    break;
  case starlightConstants::ETAC:
    singlemass=2.980;
    break;
  case starlightConstants::F0:
    singlemass=0.980;
    break;
  case starlightConstants::F2:
    singlemass=1.2754;
    break;
  case starlightConstants::A2:
    singlemass=1.318;
    break;
  case starlightConstants::F2PRIME:
    singlemass=1.525;
    break;
  case starlightConstants::ZOVERZ03:
    singlemass=1.540;
    break;
  default:
    cout<<"Not a recognized single particle, Gammagammasingle::getmass(), mass = 0."<<endl;
  }
  return singlemass;
}


//______________________________________________________________________________
double Gammagammasingle::getWidth()
{
  double singlewidth=0.;
  switch(_GGsingInputpidtest){
  case starlightConstants::ETA:
    singlewidth=1.E-6;
    break;
  case starlightConstants::ETAPRIME:
    singlewidth=5.E-6;
    break;
  case starlightConstants::ETAC:
    singlewidth=6.3E-6;
    break;
  case starlightConstants::F0:
    singlewidth=0.56E-6;
    break;
  case starlightConstants::F2:
    singlewidth=2.6E-6;
    break;
  case starlightConstants::A2:
    singlewidth=1.04E-6;
    break;
  case starlightConstants::F2PRIME:
    singlewidth=0.1E-6;
    break;
  case starlightConstants::ZOVERZ03:
    singlewidth=0.1E-6;
    break;
  default:
    cout<<"Not a recognized single particle, Gammagammasingle::getwidth(), width = 0."<<endl;
  }
  return singlewidth; 
}


//______________________________________________________________________________
double Gammagammasingle::getSpin()
{
  double singlespin=0.5;
  switch(_GGsingInputpidtest){
  case starlightConstants::ETA:
    singlespin=0.0;
    break;
  case starlightConstants::ETAPRIME:
    singlespin=0.0;
    break;
  case starlightConstants::ETAC:
    singlespin=0.0;
    break;
  case starlightConstants::F0:
    singlespin=0.0;
    break;
  case starlightConstants::F2:
    singlespin=2.0;
    break;
  case starlightConstants::A2:
    singlespin=2.0;
    break;
  case starlightConstants::F2PRIME:
    singlespin=2.0;
    break;
  case starlightConstants::ZOVERZ03:
    singlespin=2.0;
    break;
  default:
    cout<<"Not a recognized single particle, Gammagammasingle::getspin(), spin = 0."<<endl;
  }
  return singlespin;
}



