#ifndef EVTRARELBTOLLLWC_HH 
#define EVTRARELBTOLLLWC_HH 1

// Include files

#include "EvtGenBase/EvtComplex.hh"

/** @class EvtRareLbToLllWC EvtRareLbToLllWC.hh EvtGenModels/EvtRareLbToLllWC.hh
 *  
 *  Implementation of wilson coefficient calculation  
 *
 *  @author Thomas Blake
 *  @date   2013-11-27
 */


class EvtRareLbToLllWC {
public: 
  /// Standard constructor
  EvtRareLbToLllWC( ); 
  virtual ~EvtRareLbToLllWC( ); ///< Destructor
  
  EvtComplex GetC7Eff( const double q2 ) const;
  EvtComplex GetC9Eff( const double q2, const bool btod=false) const;
  EvtComplex GetC10Eff( const double q2 ) const ;
  
};

#endif // 
