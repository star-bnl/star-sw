#ifndef _OutputPolicyRivet_h_
#define _OutputPolicyRivet_h_

#include <HepMC3/GenEvent.h>
#include <HepMC3/GenParticle.h>
#include <HepMC3/GenVertex.h>
#include <Rivet/AnalysisHandler.hh>
#include <OutputPolicyNone.h>
#include "CRMChepevt.h"
#include "CRMChepmc3.h"
class CRMCoptions;

class OutputPolicyRivet : public OutputPolicyNone
{
public:
  OutputPolicyRivet();
  void InitOutput(const CRMCoptions& cfg) override;
  void FillEvent(const CRMCoptions& cfg, const int nEvent) override;
  void CloseOutput(const CRMCoptions& cfg) override;
private:
  CRMChepevt<HepMC3::GenParticlePtr,
	     HepMC3::GenVertexPtr,
	     HepMC3::FourVector,
	     HepMC3::GenEvent> _hepevt;
  CRMChepmc3 _hepmc3;
  HepMC3::GenEvent _event;
  Rivet::AnalysisHandler _handler;
  bool _is_init = false;
};


#endif

