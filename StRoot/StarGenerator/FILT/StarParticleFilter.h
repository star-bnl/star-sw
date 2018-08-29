#ifndef __StarParticleFilter_h__
#define __StarParticleFilter_h__

#include <vector>

/*!
  \class StarParticleFilter
  \brief Filter which requires one or more particles in the final state of the event record
 */

#include "StarFilterMaker.h"

class StarParticleFilter : public StarFilterMaker
{
public:
  StarParticleFilter( const char* name = "partfilt" );
  virtual ~StarParticleFilter(){ /* nada */ }

  int Filter( StarGenEvent *event = 0 );

  /// Require a particle of type pdgid within kinematic cuts specified.  If multiple
  /// triggers are set, they will be OR'd together.
  /// @param _pdgid ID of the particle according to PDG standards
  /// @param _ptmn Minimum pT of particle [GeV]
  /// @param _ptmx Maximum pT of particle [GeV].  If < _ptmn, no maximum is set.
  /// @param _etamn Minimum eta of particle
  /// @param _etamx Maximum eta of particle 
  void AddTrigger( int _pdgid, double _ptmn=0, double _ptmx=-1, double _etamn=-1, double _etamx=1 );

private:
protected:

  struct Trigger_t {
    int    pdgid;
    double ptmn;
    double ptmx;
    double etamn;
    double etamx;
    Trigger_t() :
      pdgid(0), 
      ptmn (0.), 
      ptmx (0.), 
      etamn(0.), 
      etamx(0.) 
    { };
    Trigger_t( int _pdgid, double _ptmn, double _ptmx, double _etamn, double _etamx ) : 
      pdgid(_pdgid), 
      ptmn (_ptmn), 
      ptmx (_ptmx), 
      etamn(_etamn), 
      etamx(_etamx) 
    {
    };
  };
  vector<Trigger_t> mTriggers;


#if 0 // we dont really need, but this triggers cons to create dictionary
  ClassDef(StarParticleFilter,1);
#endif

};

#endif
