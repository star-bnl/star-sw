#ifndef EVTRARELBTOLLLFFGUTSCHE_HH 
#define EVTRARELBTOLLLFFGUTSCHE_HH 1

// Include files

/** @class EvtRareLbToLllFF EvtRareLbToLllFF.hh EvtGenModels/EvtRareLbToLllFF.hh
 *  
 *
 *  @author Michal Kreps
 *  @date   2014-10-21
 */

#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtIdSet.hh"
#include "EvtGenModels/EvtRareLbToLllFFBase.hh"

#include <string>
#include <map> 

class EvtRareLbToLllFFGutsche : public EvtRareLbToLllFFBase{

public: 
  
  /// Standard constructor
  EvtRareLbToLllFFGutsche( ); 

  virtual ~EvtRareLbToLllFFGutsche( ); ///< Destructor
  
  void init() ;
  
  void getFF( EvtParticle* parent, 
              EvtParticle* lambda, 
              EvtRareLbToLllFFBase::FormFactors& FF );


protected:
  
  
private:
  double formFactorParametrization(double s, double f0, double a, double b);

  double fVconsts[3][3];
  double fAconsts[3][3];
  double fTVconsts[3][3];
  double fTAconsts[3][3];
  
  static EvtIdSet fParents;
  static EvtIdSet fDaughters;
};

#endif // EVTRARELBTOLLLFF_HH
