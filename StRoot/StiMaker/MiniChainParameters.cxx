#include "MiniChainParameters.h"

ClassImp(MiniChainParameters)

MiniChainParameters::MiniChainParameters()
  : useEmc(false),
    useEemc(false),
    useSvt(true),
    useSsd(false),
    useTpc(true),
    useFtpc(false),
    useGui(false),
    doSimulation(false),
    doAssociation(false),
    doMiniMcEvent(false),
    doDst(false),
    doStEventOutput(false),
    doStEventInput(false)
{}

MiniChainParameters::~MiniChainParameters()
{}

ostream& operator<<(ostream& os, const MiniChainParameters&pars)
{
  cout << "MiniChainParameters :" <<endl
       << "             Use EMC :" << pars.useEmc  <<endl
       << "            Use EEMC :" << pars.useEemc <<endl
       << "             Use SVT :" << pars.useSvt  <<endl
       << "             Use SSD :" << pars.useSsd  <<endl
       << "             Use TPC :" << pars.useTpc  <<endl
       << "            Use FTPC :" << pars.useFtpc <<endl
       << "        doSimulation :" << pars.doSimulation <<endl
       << "       doAssociation :" << pars.doAssociation <<endl
       << "       doMiniMcEvent :" << pars.doMiniMcEvent <<endl
       << "               doDst :" << pars.doDst <<endl
       << "     doStEventOutput :" << pars.doStEventOutput <<endl
       << "      doStEventInput :" << pars.doStEventInput   <<endl;
  return os;
}

