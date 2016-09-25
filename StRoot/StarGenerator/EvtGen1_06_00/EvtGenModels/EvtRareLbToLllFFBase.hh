#ifndef EVTRARELBTOLLLFFBASE_HH 
#define EVTRARELBTOLLLFFBASE_HH 1

// Include files

/** @class 
 *  
 *
 *  @author Michal Kreps
 *  @date   2014-10-20
 */

#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtIdSet.hh"

#include <string>
#include <map> 

class EvtRareLbToLllFFBase {

public:

  class FormFactors 
  {
  public: 
    FormFactors() ;
    
    virtual ~FormFactors() {} ;
    
    void areZero() ;

    double  F_[4];
    double  G_[4];
    double FT_[4];
    double GT_[4];    
  };

  virtual void init() = 0 ;
  
  virtual void getFF( EvtParticle* parent, 
              EvtParticle* lambda, 
              EvtRareLbToLllFFBase::FormFactors& FF ) = 0;
 
  bool isNatural( EvtParticle* lambda ) ;

  EvtRareLbToLllFFBase( ); 
  virtual ~EvtRareLbToLllFFBase( ) {};

protected:

  double calculateVdotV( EvtParticle* parent, EvtParticle* lambda ) const ;
  double calculateVdotV(EvtParticle*, EvtParticle*, double qsq) const;

  EvtIdSet natural_;

};

#endif
