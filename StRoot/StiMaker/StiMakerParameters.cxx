#include "StiMakerParameters.h"

ClassImp(StiMakerParameters)

StiMakerParameters::StiMakerParameters()
  : useEmc(false),
    useEemc(false),
    useSvt(false),
    useSsd(false),
    useTpc(true),
    useFtpc(false),
    useResidualCalculator(false),
    useMcAsRec(false),
    useGui(false),
    doSimulation(false),
    doAssociation(false),
    doMiniMcEvent(false),
    doDst(false),
    doStEventOutput(false),
    doStEventInput(false),
    doPlots(false)
{}

StiMakerParameters::~StiMakerParameters()
{}

ostream& operator<<(ostream& os, const StiMakerParameters&pars)
{
  cout << "StiMakerParameters :" <<endl
       << "                          Use EMC :" << pars.useEmc  <<endl
       << "                         Use EEMC :" << pars.useEemc <<endl
       << "                          Use SVT :" << pars.useSvt  <<endl
       << "                          Use SSD :" << pars.useSsd  <<endl
       << "                          Use TPC :" << pars.useTpc  <<endl
       << "                         Use FTPC :" << pars.useFtpc <<endl
       << "            UseResidualCalculator :" << pars.useResidualCalculator << endl
       << "                          Use GUI :" << pars.useGui  <<endl
       << "Use MC HITS As Reconstructed Hits :" << pars.useMcAsRec << endl
       << "                     doSimulation :" << pars.doSimulation <<endl
       << "                    doAssociation :" << pars.doAssociation <<endl
       << "                    doMiniMcEvent :" << pars.doMiniMcEvent <<endl
       << "                            doDst :" << pars.doDst <<endl
       << "                  doStEventOutput :" << pars.doStEventOutput <<endl
       << "                   doStEventInput :" << pars.doStEventInput   <<endl
       << "                          doPlots :" << pars.doPlots << endl;
  return os;
}

