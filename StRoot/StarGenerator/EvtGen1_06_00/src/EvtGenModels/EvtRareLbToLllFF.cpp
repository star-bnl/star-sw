// Include files 



// local

#include "EvtGenModels/EvtRareLbToLllFF.hh"
#include "EvtGenBase/EvtVector4R.hh" 
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenBase/EvtIdSet.hh"


//-----------------------------------------------------------------------------
// Implementation file for class : EvtRareLbToLllFF
//
// 2013-11-25 : Thomas Blake
//-----------------------------------------------------------------------------

//=============================================================================
// Standard constructor, initializes variables
//=============================================================================

EvtRareLbToLllFF::FormFactorDependence::FormFactorDependence()  : 
  a0_( 0 ) , a2_( 0 ), a4_( 0 ), al_( 0 ), ap_( 0 ) {} 

EvtRareLbToLllFF::FormFactorDependence::FormFactorDependence( const double al, 
                                                             const double ap ) :
  a0_( 0 ) , a2_( 0 ), a4_( 0 ), al_( al ), ap_( ap ) {} 

EvtRareLbToLllFF::FormFactorDependence::FormFactorDependence( const double a0, 
                                                             const double a2, 
                                                             const double a4, 
                                                             const double al, 
                                                             const double ap ) : 
  a0_( a0 ) , a2_( a2 ), a4_( a4 ), al_( al ), ap_( ap ) {} 

 EvtRareLbToLllFF::FormFactorDependence::FormFactorDependence( const  EvtRareLbToLllFF::FormFactorDependence& other ) : 
   a0_( other.a0_ ), a2_( other.a2_ ), a4_( other.a4_ ), al_( other.al_ ), ap_( other.ap_ ){} 


EvtRareLbToLllFF::FormFactorDependence* EvtRareLbToLllFF::FormFactorDependence::clone() const {
  return new  EvtRareLbToLllFF::FormFactorDependence( a0_, a2_, a4_, al_, ap_ );
}

EvtRareLbToLllFF::FormFactorSet::FormFactorSet(){}

EvtRareLbToLllFF::FormFactorSet::~FormFactorSet(){}

EvtRareLbToLllFF::FormFactorSet::FormFactorSet( const EvtRareLbToLllFF::FormFactorSet& other ) : 
  F1( other.F1 ), F2( other.F2 ), 
  F3( other.F3 ), F4( other.F4 ), 
  G1( other.G1 ), G2( other.G2 ), 
  G3( other.G3 ), G4( other.G4 ),
  H1( other.H1 ), H2( other.H2 ), 
  H3( other.H3 ), H4( other.H4 ), 
  H5( other.H5 ), H6( other.H6 ) {}

void EvtRareLbToLllFF::FormFactorDependence::param( const double al, 
                                                   const double ap )
{
  al_ = al;
  ap_ = ap;
}

void EvtRareLbToLllFF::FormFactorDependence::param( const double a0, 
                                                   const double a2, 
                                                   const double a4, 
                                                   const double al, 
                                                   const double ap )
{
  a0_ = a0;
  a2_ = a2;
  a4_ = a4;
  al_ = al;
  ap_ = ap;
}

EvtRareLbToLllFF::EvtRareLbToLllFF() : EvtRareLbToLllFFBase()  
  {}


void EvtRareLbToLllFF::init()
{
  // Parameters for Lambda0
  EvtRareLbToLllFF::FormFactorSet* L1115 = new EvtRareLbToLllFF::FormFactorSet();  
  L1115->F1.param( 1.21, 0.319, -0.0177, 0.387, 0.372 );
  L1115->F2.param( -0.202, -0.219, 0.0103, 0.387, 0.372 );
  L1115->F3.param( -0.0615, 0.00102, -0.00139, 0.387, 0.372 );
  L1115->F4.param( 0.387, 0.372 );
  L1115->G1.param( 0.927, 0.104 , -0.00553, 0.387, 0.372 );
  L1115->G2.param( -0.236, -0.233, 0.0110 , 0.387, 0.372 );
  L1115->G3.param( 0.0756, 0.0195, -0.00115, 0.387, 0.372 );
  L1115->G4.param( 0.387, 0.372 );
  L1115->H1.param( 0.936, 0.0722, -0.00643, 0.387, 0.372 );
  L1115->H2.param( 0.227, 0.265, -0.0101, 0.387, 0.372 );
  L1115->H3.param( -0.0757, -0.0195, 0.00116, 0.387, 0.372 );
  L1115->H4.param( -0.0174, -0.00986, -0.000524, 0.387, 0.372 );
  L1115->H5.param( 0.387, 0.372 );
  L1115->H6.param( 0.387, 0.372 );
  
  // Parameters for Lambda(Lambda(1520)0) 
  EvtRareLbToLllFF::FormFactorSet* L1520 = new EvtRareLbToLllFF::FormFactorSet();
  L1520->F1.param( -1.66, -0.295, 0.00924, 0.333, 0.308 );
  L1520->F2.param( 0.544, 0.194, -0.00420, 0.333, 0.308 );
  L1520->F3.param( 0.126, 0.00799, -0.000635, 0.333, 0.308 );
  L1520->F4.param( -0.0330, -0.00977, 0.00211, 0.303, 0.308 );
  L1520->G1.param( -0.964, -0.100, 0.00264, 0.333, 0.308 );
  L1520->G2.param( 0.625, 0.219, -0.00508, 0.333, 0.308 );
  L1520->G3.param( -0.183, -0.0380, 0.00351, 0.333, 0.308 );
  L1520->G4.param( 0.0530, 0.0161, -0.00221, 0.333, 0.308 );
  L1520->H1.param( -1.08, -0.0732, 0.00464, 0.333, 0.308 );
  L1520->H2.param( -0.507, -0.246, 0.00309, 0.333, 0.308 );
  L1520->H3.param(  0.187, 0.0295, -0.00107, 0.333, 0.308 );
  L1520->H4.param( 0.0772, 0.0267, -0.00217, 0.333, 0.308 );
  L1520->H5.param( -0.0517, -0.0173, 0.00259, 0.333, 0.308 );
  L1520->H6.param( 0.0206, 0.00679, -0.000220, 0.333, 0.308 );

  FFMap_[ EvtPDL::getId("Lambda0").getId() ]            = L1115;
  FFMap_[ EvtPDL::getId("anti-Lambda0").getId() ]       = L1115;
  FFMap_[ EvtPDL::getId("Lambda(1520)0").getId() ]      = L1520;
  FFMap_[ EvtPDL::getId("anti-Lambda(1520)0").getId() ] = L1520;

  EvtGenReport(EVTGEN_INFO,"EvtGen") << " EvtRareLbToLll is using form factors from arXiv:1108.6129 " << std::endl;
}

//=============================================================================
// Destructor
//=============================================================================

EvtRareLbToLllFF::~EvtRareLbToLllFF() {
  
  std::map< int, FormFactorSet* >::iterator it;
  
  for ( it = FFMap_.begin(); it != FFMap_.end(); ++it )
  {
    if ( it->second ) delete (it->second);
  }
  
  FFMap_.clear();
}

//=============================================================================




double EvtRareLbToLllFF::func( const double p, EvtRareLbToLllFF::FormFactorDependence& dep )
{
  static const double mq     = 0.2848;
  static const double mtilde = 1.122;
  
  const double asq = 0.5*(  dep.al_*dep.al_ + dep.ap_*dep.ap_ );
  const double psq = p*p;

  return ( dep.a0_ + dep.a2_*psq + dep.a4_*psq*psq )*exp( -(3.*mq*mq*psq)/(2.*mtilde*mtilde*asq ) );
}



void EvtRareLbToLllFF::DiracFF( EvtParticle* parent, 
                               EvtParticle* lambda,
                               EvtRareLbToLllFF::FormFactorSet& dep,
                               EvtRareLbToLllFF::FormFactors& FF )
{
  const double M  = lambda->mass();
  const double MB = parent->mass();
  
  const double vdotv = calculateVdotV( parent, lambda );
  const double p     = lambda->getP4().d3mag();

  FF.F_[0] = func( p, dep.F1 );
  FF.F_[1] = func( p, dep.F2 );
  FF.F_[2] = func( p, dep.F3 );
  
  FF.G_[0] = func( p, dep.G1 );
  FF.G_[1] = func( p, dep.G2 );
  FF.G_[2] = func( p, dep.G3 );

  const double H1 = func( p, dep.H1 );
  const double H2 = func( p, dep.H2 );
  const double H3 = func( p, dep.H3 );
  const double H4 = func( p, dep.H4 );

  if ( isNatural( lambda ) )
  {
    FF.FT_[0] = -( MB + M )*H1 - ( MB - M*vdotv )*H2 - ( MB*vdotv - M )*H3;
    FF.FT_[1] = MB*H1 + ( MB - M )*H2 + ( MB*vdotv - M )*H4;
    FF.FT_[2] = M*H1 + ( MB - M )*H3 - ( MB - M*vdotv )*H4;
    
    FF.GT_[0] = ( MB - M )*H1 - M*( 1. - vdotv )*H2 - MB*(1. - vdotv )*H3;
    FF.GT_[1] = MB*H1 - M*H2 -MB*H3;
    FF.GT_[2] = M*H1 + M*H2 + MB*H3;
  }
  else 
  { 
    FF.FT_[0] = ( MB - M )*H1 - ( MB - M*vdotv )*H2 - ( MB*vdotv - M )*H3;
    FF.FT_[1] = MB*H1 - ( MB + M )*H2 + ( MB*vdotv - M )*H4;
    FF.FT_[2] = M*H1 - ( MB + M )*H3 - ( MB - M*vdotv )*H4;
    
    FF.GT_[0] = -( MB + M )*H1 + M*( 1. + vdotv )*H2 + MB*(1. + vdotv )*H3;
    FF.GT_[1] = MB*H1 - M*H2 -MB*H3;
    FF.GT_[2] = M*H1 - M*H2 - MB*H3;
  }
}

void EvtRareLbToLllFF::RaritaSchwingerFF( EvtParticle* parent, 
                                         EvtParticle* lambda,
                                         EvtRareLbToLllFF::FormFactorSet& FFset,
                                         EvtRareLbToLllFF::FormFactors&   FF )
{ 
  const double M  = lambda->mass();
  const double MB = parent->mass();
  
  const double vdotv = calculateVdotV( parent, lambda );
  const double p     = lambda->getP4().d3mag();

  FF.F_[0] = func( p, FFset.F1 );
  FF.F_[1] = func( p, FFset.F2 );
  FF.F_[2] = func( p, FFset.F3 );
  FF.F_[3] = func( p, FFset.F4 );
  
  FF.G_[0] = func( p, FFset.G1 );
  FF.G_[1] = func( p, FFset.G2 );
  FF.G_[2] = func( p, FFset.G3 );
  FF.G_[3] = func( p, FFset.G4 );

  const double H1 = func( p, FFset.H1 );
  const double H2 = func( p, FFset.H2 );
  const double H3 = func( p, FFset.H3 );
  const double H4 = func( p, FFset.H4 );
  const double H5 = func( p, FFset.H5 );
  const double H6 = func( p, FFset.H6 );
  
  if ( isNatural( lambda ) )
  {
    FF.FT_[0] = -( MB + M )*H1 - ( MB - M*vdotv )*H2 - ( MB*vdotv - M )*H3 - MB*H5;
    FF.FT_[1] = MB*H1 + ( MB - M )*H2 + ( MB*vdotv - M )*H4 - MB*H6;
    FF.FT_[2] = M*H1 + ( MB - M )*H3 - ( MB - M*vdotv )*H4;
    FF.FT_[3] = (MB - M)*H5 + (MB - M*vdotv)*H6;
    
    FF.GT_[0] = ( MB - M )*H1 - M*( 1. - vdotv )*H2 - MB*(1. - vdotv )*H3 + MB*H5 + M*H6;
    FF.GT_[1] = MB*H1 - M*H2 - MB*H3;
    FF.GT_[2] = M*H1 + M*H2 + MB*H3 - M*H6;
    FF.GT_[3] = (MB + M)*H5 + M*(1. + vdotv)*H6;
  }
  else 
  {
    FF.FT_[0] = ( MB - M )*H1 - ( MB - M*vdotv )*H2 - ( MB*vdotv - M )*H3 - MB*H5;
    FF.FT_[1] = MB*H1 - ( MB + M )*H2 + ( MB*vdotv - M )*H4 - MB*H6;
    FF.FT_[2] = M*H1 - ( MB + M )*H3 - ( MB - M*vdotv )*H4;
    FF.FT_[3] = -(MB + M)*H5 + (MB - M*vdotv)*H6;
    
    FF.GT_[0] = -( MB + M )*H1 + M*( 1. + vdotv )*H2 + MB*(1. + vdotv )*H3 + MB*H5 + M*H6;
    FF.GT_[1] = MB*H1 - M*H2 - MB*H3;
    FF.GT_[2] = M*H1 - M*H2 - MB*H3 - M*H6;
    FF.GT_[3] = -(MB - M)*H5 - M*(1. - vdotv)*H6;
  }
}

void EvtRareLbToLllFF::getFF( EvtParticle* parent, 
                             EvtParticle* lambda,
                             EvtRareLbToLllFF::FormFactors& FF )
{
  // Find the FF information for this particle, start by setting all to zero
  FF.areZero();
  
  // Are the FF's for the particle known? 
  std::map< int, EvtRareLbToLllFF::FormFactorSet* >::iterator it 
    = FFMap_.find( lambda->getId().getId() );
  
  if ( it == FFMap_.end() ) {
    EvtGenReport(EVTGEN_ERROR,"EvtGen") << " EvtRareLbToLll does not contain FF for " 
                            << lambda->getId() << std::endl;
    return ;
  }
  
  // Split by spin 1/2, spin 3/2
  const int spin = EvtPDL::getSpinType(lambda->getId());
  
  if ( EvtSpinType::DIRAC == spin ) {
    DiracFF(parent, lambda, *(it->second), FF );
  } 
  else if ( spin == EvtSpinType::RARITASCHWINGER ) {
    RaritaSchwingerFF(parent, lambda, *(it->second), FF );
  }
  else 
  { 
    EvtGenReport(EVTGEN_ERROR,"EvtGen") << " EvtRareLbToLll expects DIRAC or RARITASWINGER daughter " << std::endl;// should add a warning here 
    return ;
  }
  
  
  return ;
}

