#ifndef EVTRARELBTOLLLFF_HH 
#define EVTRARELBTOLLLFF_HH 1

// Include files

/** @class EvtRareLbToLllFF EvtRareLbToLllFF.hh EvtGenModels/EvtRareLbToLllFF.hh
 *  
 *
 *  @author Thomas Blake
 *  @date   2013-11-26
 */

#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtIdSet.hh"
#include "EvtGenModels/EvtRareLbToLllFFBase.hh"

#include <string>
#include <map> 

class EvtRareLbToLllFF : public EvtRareLbToLllFFBase{

public: 
  
  class FormFactorDependence
  {
  public:
    FormFactorDependence();
    
    FormFactorDependence( const double al, 
                          const double ap );
    
    FormFactorDependence( const double a0, 
                          const double a2, 
                          const double a4, 
                          const double al, 
                          const double ap ) ;

    FormFactorDependence( const FormFactorDependence& other );
    
    FormFactorDependence* clone() const ;

    void param( const double al, 
                const double ap ) ;
    
    void param( const double a0, 
                const double a2, 
                const double a4, 
                const double al, 
                const double ap );
    
                
    double a0_;
    double a2_;
    double a4_;
    double al_;
    double ap_;
  };  

  class FormFactorSet 
  {
  public:
    FormFactorSet() ;
    
    FormFactorSet( const FormFactorSet& other );
    
    virtual ~FormFactorSet();

    EvtRareLbToLllFF::FormFactorDependence F1;
    EvtRareLbToLllFF::FormFactorDependence F2;
    EvtRareLbToLllFF::FormFactorDependence F3;
    EvtRareLbToLllFF::FormFactorDependence F4;
    
    EvtRareLbToLllFF::FormFactorDependence G1;
    EvtRareLbToLllFF::FormFactorDependence G2;
    EvtRareLbToLllFF::FormFactorDependence G3;
    EvtRareLbToLllFF::FormFactorDependence G4;
    
    EvtRareLbToLllFF::FormFactorDependence H1;
    EvtRareLbToLllFF::FormFactorDependence H2;
    EvtRareLbToLllFF::FormFactorDependence H3;
    EvtRareLbToLllFF::FormFactorDependence H4;
    EvtRareLbToLllFF::FormFactorDependence H5;
    EvtRareLbToLllFF::FormFactorDependence H6;
  };


  /// Standard constructor
  EvtRareLbToLllFF( ); 

  virtual ~EvtRareLbToLllFF( ); ///< Destructor
  
  void init() ;
  
  void getFF( EvtParticle* parent, 
              EvtParticle* lambda, 
              EvtRareLbToLllFFBase::FormFactors& FF );


protected:
  
  
private:
  double func( const double p, EvtRareLbToLllFF::FormFactorDependence& dep );
  
  std::map< int, EvtRareLbToLllFF::FormFactorSet* > FFMap_;
  
  void DiracFF( EvtParticle* parent, 
                EvtParticle* lambda, 
                EvtRareLbToLllFF::FormFactorSet& FFset, 
                EvtRareLbToLllFF::FormFactors&   FF );

  void RaritaSchwingerFF( EvtParticle* parent, 
                          EvtParticle* lambda, 
                          EvtRareLbToLllFF::FormFactorSet& FFset, 
                          EvtRareLbToLllFF::FormFactors&   FF );

  
};

#endif // EVTRARELBTOLLLFF_HH
