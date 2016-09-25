#ifndef EVTRARELBTOLLLFFLQCD_HH 
#define EVTRARELBTOLLLFFLQCD_HH 1

// Include files

/** @class EvtRareLbToLllFF EvtRareLbToLllFF.hh EvtGenModels/EvtRareLbToLllFF.hh
 *  
 *
 *  @author Michal Kreps
 *  @date   2016-04-19
 *  @date   2014-10-23
 */

#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtIdSet.hh"
#include "EvtGenModels/EvtRareLbToLllFFBase.hh"

#include <string>
#include <map> 

class EvtRareLbToLllFFlQCD : public EvtRareLbToLllFFBase{

public: 
  
  /// Standard constructor
  EvtRareLbToLllFFlQCD( ); 

  virtual ~EvtRareLbToLllFFlQCD( ); ///< Destructor
  
  void init() ;
  
  void getFF( EvtParticle* parent, 
              EvtParticle* lambda, 
              EvtRareLbToLllFFBase::FormFactors& FF );


protected:
  
  
private:
  double formFactorParametrization(double q2, double a0, double a1, double pole);
  double zvar(double q2);

  double fconsts[3][3];
  double gconsts[3][3];
  double hconsts[3][3];
  double htildaconsts[3][3];

  double t0;
  double tplus;
};

#endif // EVTRARELBTOLLLFF_HH
