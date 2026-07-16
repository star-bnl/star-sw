#include <CRMCconfig.h>
#include "CRMCRHICfOption.h"

#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <math.h>

#include <tclap/CmdLine.h>

using namespace std;

CRMCRHICfOption::CRMCRHICfOption(int argc, char **argv)
    : fNCollision(500)
    , fSeed(0)
    , fHEModel(0)
    , fParamFileName("crmc.param ")
    , fOutputFileName("")
    , fRHICfRunType("ALL")
    , fJobIndex("")
{
  ParseOptions(argc, argv);
}

void CRMCRHICfOption::ParseOptions(int argc, char **argv)
{
    std::ostringstream vers;
    vers << CRMC_VERSION_MAJOR << "." <<CRMC_VERSION_MINOR << "." << CRMC_VERSION_PATCH ;
    std::string version = vers.str();
        
    TCLAP::CmdLine cmd("Options for CRMC", ' ', version);

    TCLAP::ValueArg<int> seed("s", "seed", "random seed between 0 and 1e9 (default: random)", false, 0, "int");
    cmd.add(seed);

    TCLAP::ValueArg<int> number("n", "number", "number of collisions (default: 500)", false, 500, "int");
    cmd.add(number);

    TCLAP::ValueArg<int> model("m", "model", "model [0=EPOS.LHC-R (default), 1=EPOS.no.had.resc, 6=Sibyll_2.3e, 7=QGSJETII-04, 13=QGSJETIII-01]", false, 0, "int");
    cmd.add(model);

    TCLAP::ValueArg<string> runType("r", "runType", "RHICf Run Type", false, "", "string");
    cmd.add(runType);

    TCLAP::ValueArg<string> jobIndex("j", "jobIndex", "Specific job index", false, "", "string");
    cmd.add(jobIndex);

    try{cmd.parse(argc, argv);}
    catch (std::exception &e){
        std::cout << e.what() << std::endl;
        exit(EXIT_FAILURE);
    }

    if(number.isSet()){fNCollision = number.getValue();}
    if(model.isSet()){
        fHEModel = model.getValue();
        if(GetModelIdx() == -1){
            cerr << " No support model (-1) for CRMCRHICf simulation, terminate.." << endl;
            exit(1);
        }
    }
    if(runType.isSet()){
        fRHICfRunType = runType.getValue();
        if(GetRHICfRunType() == RunType::NON){
            cout << " Invalid Run Type, force to Run type = ALL"<< endl;
            fRHICfRunType = "ALL";
        }   
    }

    if (jobIndex.isSet()){fJobIndex = jobIndex.getValue();}


    // check if random seed was provided, otherwise generate one
        // parameter readout
    if (seed.isSet()){
        fSeed = seed.getValue();
        if(fSeed < 0){
        cerr << " Seed is negative: " << fSeed << endl;
        exit(1);
        }
        if(fSeed > 1e9){
        cerr << " Seed too large (>1e9): " << fSeed << endl;
        exit(1);
        }
    }
    if (fSeed == 0){
        ifstream urandom("/dev/urandom", ios::in | ios::binary);
        urandom.read((char *)&fSeed, sizeof(fSeed) / sizeof(char));
        urandom.close();
        fSeed = abs(fSeed) % 999999999;
    }

    DumpConfig();
}

void CRMCRHICfOption::DumpConfig() const
{
    cout << "\n          >> CRMC with RHICf Interface <<\n\n";
    cout << "  p+p @ sqrt(s) = 510 GeV" << "\n\n";

    cout << "  seed:                       " << fSeed << "\n";
    cout << "  number of collisions:       " << fNCollision << "\n"
         << "  parameter file name:        " << fParamFileName << "\n";
    cout << "  output file format:         " << "RHICfEvent" << endl;
    cout << "  output file name:           " << GetOutputFileName() << "\n";
    cout << "  HE model:                   " << fHEModel << ", " << GetModelName() << endl;;
    cout << "  RHICf Run Type              " << GetRHICfRunTypeName() << endl;
    cout << "  Job Index                   " << GetJobIndex() <<"\n" << endl;

    cout.setf(ios::showpoint);
    cout.setf(ios::fixed);
    cout.precision(3);
}

string CRMCRHICfOption::GetOutputFileName() const
{
    TString outputPath = getenv("PWD");
    TString RunTypeName = GetRHICfRunTypeName();
    TString modelName = GetModelName();
    TString jobTime = GetTime();    
    TString jobIndex = GetJobIndex();
    if(jobIndex != ""){jobIndex = "_" + jobIndex;}
    TString outputName = outputPath +"/crmc_"+ modelName +"_"+ RunTypeName +"_"+ jobTime + jobIndex +".RHICfSimGenerator";
    return outputName.Data();
}

TString CRMCRHICfOption::GetModelName() const
{
    switch (fHEModel){
        case 13: return "QGSJETIII01"; // QGSJETIII-01
        case 6: return "SIBYLL"; // SIBYLL 2.3e
        case 0: return "EPOSLHCR"; // EPOS-LHC-R
        case 1: return "EPOSLHCR_FAST"; // EPOS-LHC-R (Fast)
        case 7: return "QGSJETII04"; // QGSJETII-04
    }
    return "non";
}

TString CRMCRHICfOption::GetRHICfRunTypeName() const
{
    TString name = fRHICfRunType;
    name.ToUpper();
    if(name.Index("TL")==-1 && name.Index("TS")==-1 && name.Index("TOP")==-1 && name.Index("ALL")==-1){
        return "";
    }
    return name;
}

int CRMCRHICfOption::GetModelIdx() const
{
    switch (fHEModel){
        case 13: return 1; // QGSJETIII-01
        case 6: return 2; // SIBYLL 2.3e
        case 0: return 3; // EPOS-LHC-R
        case 1: return 4; // EPOS-LHC-R (Fast)
        case 7: return 5; // QGSJETII-04
    }
    return -1;
}

int CRMCRHICfOption::GetRHICfRunType() const
{
    if(GetRHICfRunTypeName()=="TL"){return RunType::TL;}
    else if(GetRHICfRunTypeName()=="TS"){return RunType::TS;}
    else if(GetRHICfRunTypeName()=="TOP"){return RunType::TOP;}
    else if(GetRHICfRunTypeName()=="ALL"){return RunType::ALL;}
    return RunType::NON;
}

TString CRMCRHICfOption::GetTime() const
{
    time_t timer;
    timer = time(NULL); 
    struct tm* t = localtime(&timer); 
    int date = (t->tm_year -100)*10000 + (t->tm_mon+1)*100 + t->tm_mday;
    int time = t->tm_hour*10000 + t->tm_min*100 + t->tm_sec;
    TString jobTime = Form("%i%i", date, time);
    return jobTime;
}