#include <OutputPolicyNone.h>

#include <CRMCoptions.h>
#include <CRMCinterface.h>

#include <iomanip>
#include <iostream>

#include <CRMCconfig.h> //cmake generated


OutputPolicyNone::OutputPolicyNone()
{
}


void
OutputPolicyNone::InitOutput(const CRMCoptions& cfg)
{
}


void
OutputPolicyNone::FillEvent(const CRMCoptions& cfg, const int nEvent)
{
}

void 
OutputPolicyNone::FillRHICfEvent(const CRMCoptions& cfg, const int nEvent, int& passEventNum)
{
}

void
OutputPolicyNone::CloseOutput(const CRMCoptions& cfg)
{
}

void
OutputPolicyNone::PrintCrossSections(const CRMCoptions& cfg)
{
  std::ostream& o = std::cout;
  o.setf(std::ios::showpoint);
  o.setf(std::ios::fixed);
  o.precision(3);

  o << "\n          >> Test output <<\n\n"
    << "  Total hp Cross Section (mb):      " << gCRMC_data.sigtot << "\n"
    << "  Elastic hp Cross Section (mb):    " << gCRMC_data.sigela << "\n"
    << "  Inel. hp Cross Section (mb) :     " << gCRMC_data.sigine << "\n";
  if (cfg.GetProjectileId()>10000 && cfg.GetTargetId()>10000) {    
    o << "  Inel. AA Cross Section (mb):   " << gCRMC_data.sigineaa << "\n"
      << "  Elastic AA Cross Section (mb): " << gCRMC_data.sigelaaa << "\n"
      << "  Total AA Cross Section (mb):   " << gCRMC_data.sigtotaa << "\n" ;
  } else if (cfg.GetTargetId()>10000) {
    o << "  Inel. hA Cross Section (mb):   " << gCRMC_data.sigineaa << "\n"
      << "  Elastic hA Cross Section (mb): " << gCRMC_data.sigelaaa << "\n"
      << "  Total hA Cross Section (mb):   " << gCRMC_data.sigtotaa << "\n" ;
  }
}
