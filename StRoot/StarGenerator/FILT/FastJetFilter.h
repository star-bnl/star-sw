#ifndef __FastJetFilter_h__
#define __FastJetFilter_h__

#include <vector>

/*!
  \class FastJetFilter
  \brief Filter which requires one or more particles in the final state of the event record
 */

#include "StarGenerator/FILT/StarFilterMaker.h"

#ifndef __CINT__
#include <fastjet/PseudoJet.hh>
#include <fastjet/ClusterSequence.hh>
#include <fastjet/JetDefinition.hh>
#include <fastjet/Selector.hh>
#endif

class FastJetFilter : public StarFilterMaker
{
public:
  FastJetFilter( const char* name = "partfilt" );
  virtual ~FastJetFilter(){ /* nada */ }

  int Init();

  int Filter( StarGenEvent *event = 0 );

  /// Require a particle of type pdgid within kinematic cuts specified.  If multiple
  /// triggers are set, they will be OR'd together.
  /// @param _pdgid ID of the particle according to PDG standards
  /// @param _ptmn Minimum pT of particle [GeV]
  /// @param _ptmx Maximum pT of particle [GeV].  If < _ptmn, no maximum is set.
  /// @param _etamn Minimum eta of particle
  /// @param _etamx Maximum eta of particle 
  /// @param _pdgidParent The PDG id of the parent particle (0 accepts any)
  void AddTrigger( int _pdgid, double _ptmn=0, double _ptmx=-1, double _etamn=-1, double _etamx=1, int _pdgidParent=0 );

private:
protected:

  struct Trigger_t {
    int    pdgid;
    double ptmn;
    double ptmx;
    double etamn;
    double etamx;
    int    pdgid_parent;
    Trigger_t() :
      pdgid(0), 
      ptmn (0.), 
      ptmx (0.), 
      etamn(0.), 
      etamx(0.),
      pdgid_parent(0)
    { };
    Trigger_t( int _pdgid, double _ptmn, double _ptmx, double _etamn, double _etamx, int _pdgid_parent ) : 
      pdgid(_pdgid), 
      ptmn (_ptmn), 
      ptmx (_ptmx), 
      etamn(_etamn), 
      etamx(_etamx),
      pdgid_parent(_pdgid_parent)
    {
    };
  };
  vector<Trigger_t> mTriggers;

#ifndef __CINT__
  fastjet::Strategy            strategy = fastjet::Best;
  fastjet::RecombinationScheme recombScheme = fastjet::E_scheme; //Change as you need
  fastjet::JetDefinition*      jetdefinition = nullptr;
  fastjet::JetAlgorithm        algorithm;
#endif

#if 1 // we dont really need, but this triggers cons to create dictionary
  ClassDef(FastJetFilter,0);
#endif

};

#endif
