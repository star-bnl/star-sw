#include <CRMC.h>
#include <CRMCinterface.h>
#include <CRMCoptions.h>
#include <OutputPolicyNone.h>

#include <iomanip>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <stdlib.h>
#include <string>
#include <cstdio>
#include <ctime>
#include <climits>

#include "TString.h"

#include <CRMCconfig.h> //cmake generated

using namespace std;




CRMC::CRMC(const CRMCoptions& cfg,
	   OutputPolicyNone& output)
  : fCfg(cfg), fOutput(output) {
}



bool
CRMC::init()
{
  setbuf(stdout, 0); // set output to unbuffered
  

  if (fInterface.init(fCfg.GetHEModel()) != 1)
    return false;
  
  // open FORTRAN IO at first call
  //call here variable settings from c++ interface
  fInterface.crmc_init(fCfg.GetSqrts(),
                       fCfg.GetSeed(),
                       fCfg.GetHEModel(),
                       fCfg.ProduceTables(),
                       fCfg.GetTypout(),
                       fCfg.GetParamFileName().c_str(),
                       fCfg.GetOutputFileName().c_str(),
                       fCfg.GetOutputFileName().size());
  //init models with set variables

  TString runType = fCfg.GetRHICfRunType();
  runType.ToUpper();
  // int CollisionEventNum = fCfg.GetNCollision();
  int CollisionEventNum = INT_MAX;
  if(runType != "ALL"){CollisionEventNum = INT_MAX;}

  fInterface.crmc_set(CollisionEventNum,
                      fCfg.GetProjectileMomentum(),
                      fCfg.GetTargetMomentum(),
                      fCfg.GetProjectileId(),
                      fCfg.GetTargetId());

  fOutput.InitOutput(fCfg);
  //fFilter.Init(fCfg.GetFilter());
  return true;
}



bool
CRMC::run()
{
  const time_t timer_start = time(NULL);
  
  std::cout.precision(10);

  int eventNum = fCfg.GetNCollision();
  int passEventNum = 0;
  int iColl = 0;

  while(1){
    // cleanup vectors
    gCRMC_data.Clean();

    if ((iColl+1) % 1000 == 0 || (fCfg.GetProjectileId()+fCfg.GetTargetId()>400 && (iColl+1) %10== 0))
      cout << " ==[crmc]==> Collision number " << iColl+1 << endl;

    // loop over collisions
    fInterface.crmc_generate(fCfg.GetTypout(),iColl+1,
                             gCRMC_data.fNParticles,
                             gCRMC_data.fImpactParameter,
                             gCRMC_data.fPartId[0],
                             gCRMC_data.fPartPx[0],
                             gCRMC_data.fPartPy[0],
                             gCRMC_data.fPartPz[0],
                             gCRMC_data.fPartEnergy[0],
                             gCRMC_data.fPartMass[0],
                             gCRMC_data.fPartStatus[0]);
    
    gCRMC_data.sigtot = double(hadr5_.sigtot);
    gCRMC_data.sigine = double(hadr5_.sigine);
    gCRMC_data.sigela = double(hadr5_.sigela);
    gCRMC_data.sigdd = double(hadr5_.sigdd);
    gCRMC_data.sigsd = double(hadr5_.sigsd);
    gCRMC_data.sloela = double(hadr5_.sloela);
    gCRMC_data.sigtotaa = double(hadr5_.sigtotaa);
    gCRMC_data.sigineaa = double(hadr5_.sigineaa);
    gCRMC_data.sigelaaa = double(hadr5_.sigelaaa);
    gCRMC_data.npjevt = cevt_.npjevt;
    gCRMC_data.ntgevt = cevt_.ntgevt;
    gCRMC_data.kolevt = cevt_.kolevt;
    gCRMC_data.kohevt = cevt_.kohevt;
    gCRMC_data.npnevt = cevt_.npnevt;
    gCRMC_data.ntnevt = cevt_.ntnevt;
    gCRMC_data.nppevt = cevt_.nppevt;
    gCRMC_data.ntpevt = cevt_.ntpevt;
    gCRMC_data.nglevt = cevt_.nglevt;
    gCRMC_data.ng1evt = c2evt_.ng1evt;
    gCRMC_data.ng2evt = c2evt_.ng2evt;
    gCRMC_data.bimevt = double(cevt_.bimevt);
    gCRMC_data.phievt = double(cevt_.phievt);
    gCRMC_data.fglevt = double(c2evt_.fglevt);
    gCRMC_data.typevt = int(c2evt_.typevt);

    fOutput.FillRHICfEvent(fCfg, iColl, passEventNum);
    iColl++;
    if(eventNum == passEventNum){break;}
  }

  std::cout.precision(2);

  cout << " \n succesfully processed " << passEventNum << " events in " << iColl << " collision"
       << ( iColl > 1 ? "s \n" : " \n" ) ;

  const time_t timer_stop = time(NULL);
  const double realTime = difftime(timer_stop, timer_start);
  //const double cpuTime = watch.CpuTime();

  cout << " in " << realTime << " sec, "
       << ((double)realTime/iColl) << " sec/collision, with "
    //<< (cpuTime/realTime*100) << "% cpu usage. \n" 
       << endl;

  return true;
}

bool
CRMC::finish()
{
  if      (fCfg.IsCSMode()) fOutput.PrintCrossSections(fCfg);
  else if (fCfg.IsTest())   fOutput.PrintTestEvent(fCfg);
  
  fOutput.CloseOutput(fCfg);
  return true;
}

