#include "StiMakerParameters.h"

ClassImp(StiMakerParameters)

StiMakerParameters::StiMakerParameters()
  : useEmc(false),
    useEemc(false),
    useSvt(false),
    useSsd(false),
    useTpc(true),
    useFtpc(false),
    usePixel(false),
    activeEmc(false),
    activeEemc(false),
    activeSvt(false),
    activeSsd(false),
    activeTpc(true),
    activeFtpc(false),
    activePixel(false),
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
{
	trackerInputFile  = "trackerInputFile.dat";
	fitterInputFile   = "fitterInputFile.dat";
	emcInputFile      = "emcInputFile.dat";
	eemcInputFile     = "eemcInputFile.dat";
	svtInputFile      = "svtInputFile.dat";
	ssdInputFile      = "ssdInputFile.dat";
	tpcInputFile      = "tpcInputFile.dat";
	ftpcInputFile     = "ftpcInputFile .dat";
	pixelInputFile    = "pixelInputFile.dat";
}

StiMakerParameters::~StiMakerParameters()
{}

ostream& operator<<(ostream& os, const StiMakerParameters&pars)
{
  cout << "StiMakerParameters :" <<endl
       << "                Detector Geometry :" << endl
       << "                          Use EMC :" << pars.useEmc  <<endl
       << "                         Use EEMC :" << pars.useEemc <<endl
       << "                          Use SVT :" << pars.useSvt  <<endl
       << "                          Use SSD :" << pars.useSsd  <<endl
       << "                          Use TPC :" << pars.useTpc  <<endl
       << "                         Use FTPC :" << pars.useFtpc <<endl  
       << "                        Use Pixel :" << pars.usePixel <<endl
       << "                      Hit Loading :" << endl
       << "                       Active EMC :" << pars.activeEmc  <<endl
       << "                      Active EEMC :" << pars.activeEemc <<endl
       << "                       Active SVT :" << pars.activeSvt  <<endl
       << "                       Active SSD :" << pars.activeSsd  <<endl
       << "                       Active TPC :" << pars.activeTpc  <<endl
       << "                      Active FTPC :" << pars.activeFtpc <<endl
       << "                     Active Pixel :" << pars.activePixel <<endl
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

