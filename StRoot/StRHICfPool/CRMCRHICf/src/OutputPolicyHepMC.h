#ifndef _OutputPolicyHepMC_h_
#define _OutputPolicyHepMC_h_

#include <HepMC/GenEvent.h>
#include <HepMC/GenParticle.h>
#include <HepMC/GenVertex.h>
#include "OutputPolicyNone.h"
#include "CRMChepevt.h"
#include "CRMCstat.h"

class CRMCoptions;

namespace HepMC {
  class IO_GenEvent;
}

#include <boost/iostreams/filtering_stream.hpp>


class OutputPolicyHepMC : public OutputPolicyNone {

 public:
  OutputPolicyHepMC();

  void InitOutput(const CRMCoptions& cfg) override;
  void FillEvent(const CRMCoptions& cfg, const int nEvent) override;
  void CloseOutput(const CRMCoptions& cfg) override;

  void PrintTestEvent(const CRMCoptions& cfg) override;
 private:
  boost::iostreams::filtering_ostream *fOut;
  CRMChepevt<HepMC::GenParticle*,
	     HepMC::GenVertex*,
	     HepMC::FourVector,
	     HepMC::GenEvent> hepevt;
  HepMC::IO_GenEvent* ascii_out;

 protected:
  Stat<double> _eta;
  Stat<double> _pt;
  Stat<double> _pz;
  Stat<double> _e;
  Stat<int>    _m;
  Stat<int>    _mid;
};


#endif

