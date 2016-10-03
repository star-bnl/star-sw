#ifndef EVTRARELBTOLLL_HH 
#define EVTRARELBTOLLL_HH 1

// Include files

/** @class EvtRareLbToLll EvtRareLbToLll.hh EvtGenModels/EvtRareLbToLll.hh
 *  
 *
 *  @author Thomas Blake
 *  @date   2013-11-27
 */

#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtDecayAmp.hh"
#include "EvtGenBase/EvtAmp.hh"

#include "EvtGenModels/EvtRareLbToLllFFBase.hh"
#include "EvtGenModels/EvtRareLbToLllWC.hh"

class EvtRareLbToLll : public  EvtDecayAmp {

public: 
  /// Standard constructor
  EvtRareLbToLll( ); 

  virtual ~EvtRareLbToLll( ); ///< Destructor

  virtual std::string getName() ;
  
  virtual EvtDecayBase* clone();

  virtual void init();
  
  virtual void initProbMax();
  
  virtual void decay( EvtParticle *parent );

protected:
  
  void calcAmp( EvtAmp& amp, EvtParticle *parent ) ;
  
  void HadronicAmp( EvtParticle* parent, 
                    EvtParticle* lambda, 
                    EvtVector4C* T,
                    const  int i, 
                    const  int j );
  
  void HadronicAmpRS( EvtParticle* parent, 
                      EvtParticle* lambda, 
                      EvtVector4C* T,
                      const  int i, 
                      const  int j );

  bool isParticle( EvtParticle* parent ) const ;
  

private:

  double m_maxProbability; 
  
  EvtRareLbToLllFFBase* ffmodel_;
  EvtRareLbToLllWC* wcmodel_;
  
};
#endif // 
