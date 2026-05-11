#include <OutputPolicyRivet.h>

#include <CRMCoptions.h>
#include <CRMCinterface.h>
#include <CRMCconfig.h>
#include <Rivet/Rivet.hh>

//--------------------------------------------------------------------
OutputPolicyRivet::OutputPolicyRivet()
{}

//--------------------------------------------------------------------
void OutputPolicyRivet::InitOutput(const CRMCoptions& cfg)
{
  _hepmc3.init(cfg);

  for (auto p : cfg.GetRivetSearch()) {
    Rivet::addAnalysisLibPath (p);
    Rivet::addAnalysisDataPath(p);
  }
  for (auto p : cfg.GetRivetPreloads())
    _handler.readData(p);
  
  for (auto a : cfg.GetRivetAnalyses()) 
    _handler.addAnalysis(a);
}

//--------------------------------------------------------------------
void OutputPolicyRivet::FillEvent(const CRMCoptions& cfg, const int nEvent)
{
  if (!_hepevt.convert(_event)) 
    throw std::runtime_error("!!!Could not read next event");

  _hepmc3.fillInEvent(cfg, nEvent, _event);

  if (not _is_init) _handler.init(_event);

  _is_init = true;
  _handler.analyze(_event);
}

//--------------------------------------------------------------------
void OutputPolicyRivet::CloseOutput(const CRMCoptions& cfg)
{
  _handler.finalize();
  _handler.writeData(cfg.GetOutputFileName());
}


//--------------------------------------------------------------------
//
// EOF
//
