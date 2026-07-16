#include <CRMCoptions.h>
#include <CRMCinterface.h>
#include <CRMCRHICfOption.h>
#include <CRMCRHICfEventOutput.h>
#include <CRMCconfig.h> 

#include <climits>
#include <iostream>
#include <fstream>
#include <cmath>

using namespace std;

int main(int argc, char **argv)
{
    const CRMCRHICfOption options(argc, argv);

    CRMCinterface interface;
    if(interface.init(options.GetHEModel()) != 1){return 2;}

    string outFileName = options.GetOutputFileName()+".hepmc3";

    // open FORTRAN IO at first call
    //call here variable settings from c++ interface
    interface.crmc_init(double(510.),
                        options.GetSeed(),
                        options.GetHEModel(),
                        int(1),
                        int(0),
                        options.GetParamFileName().c_str(),
                        outFileName.c_str(),
                        outFileName.size());
                        
    //init models with set variables
    int maxCollNum = INT_MAX;
    // crmc_set(totalEventNum, protectile p, target p, protectile Idx, target Idx)
    interface.crmc_set(maxCollNum, 
                       double(255.),
                       double(-255.),
                       int(1),
                       int(1)); 

    CRMCRHICfEventOutput* output = new CRMCRHICfEventOutput();
    output -> InitOutput(options);

    int eventNum = options.GetNCollision();
    int passEventNum = 0;
    int collNum = 0;

    const time_t timer_start = time(NULL);

    // Main event loop 
    while(1){
        // cleanup vectors
        gCRMC_data.Clean();

        if((collNum+1) % 1000 == 0){cout << " ==[crmc]==> Collision number " << collNum+1 << endl;}
        
        // loop over collisions
        interface.crmc_generate(int(0),
                                collNum+1,
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

        output -> FillRHICfEvent(options, collNum, passEventNum);
        collNum++;

        if(eventNum == passEventNum){break;}
    }

    output -> CloseOutput();

    std::cout.precision(2);
    const time_t timer_stop = time(NULL);
    const double realTime = difftime(timer_stop, timer_start);
    cout << "CRMCRHICf summary: " << passEventNum << " RHICf events in " << collNum << " collisions" << endl;
    cout << "                   " << realTime << " sec, " << ((double)realTime/collNum) << " sec/collision" << endl;
    cout << "CRMCRHICf Terminate..." << endl;

    return 0;
}
