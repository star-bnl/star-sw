#include "RHICfSimUtil.hh"

RHICfSimUtil* RHICfSimUtil::mInstance = nullptr;

RHICfSimUtil* RHICfSimUtil::GetRHICfSimUtil(int num, char** par){
    if (mInstance != nullptr){return mInstance;}
    return new RHICfSimUtil(num, par);
}

RHICfSimUtil::RHICfSimUtil(int num, char** par) 
: mInput("")
{
    mInstance = this;

    if(!mSimOptions){
        mSimOptions = new RHICfSimOptions();
        mSimOptions -> SetInputOption(num, par);
    }

    mRandom = new TRandom3(0);
}

RHICfSimUtil::~RHICfSimUtil()
{
}

Long_t RHICfSimUtil::GenSeed()
{
    // generate the seed from 0 to Long_t max value
    return mRandom -> Integer(LONG_MAX);
}

TString RHICfSimUtil::GetGDMLFile()
{
    if(!mSimOptions){return "";}
    TString path = mSimOptions->GetOptString("GEOMETRYDIR");
    TString runtype = mSimOptions->GetOptString("RUNTYPE");
    TString gdmlFile = "";
    if(runtype == "TS"){gdmlFile = path+"/full_ts.gdml";}
    else if(runtype == "TL"){gdmlFile = path+"/full_tl.gdml";}
    else if(runtype == "TOP"){gdmlFile = path+"/full_top.gdml";}
    else{
        cout << "RHICfSimUtil::GetGDMLFile() -- Error: Runtype = " << runtype << " is not validate" << endl;
        return "";
    }

    ifstream inFile(gdmlFile.Data());
    if(!(inFile.is_open())){return "";}

    vector<TString> lineVec;

    std::string linetmp;
    while(std::getline(inFile, linetmp)){
        TString line = linetmp;

        if(line.Index("<!ENTITY gwd") != -1){
            line = "<!ENTITY gwd \"" + path + "\">";
            lineVec.push_back(line);
        }
        else{
            lineVec.push_back(line);
        }
    }
    inFile.close();

    ofstream outFile(gdmlFile.Data());
    for(int i=0; i<lineVec.size(); i++){
        outFile << lineVec[i] << endl;
    }
    outFile.close();

    return gdmlFile;
}

Bool_t RHICfSimUtil::IsStarSimMode()
{
    if(!mSimOptions){return 0;}
    TString mode = mSimOptions -> GetOptString("MODE");
    mode.ToUpper();
    if(mode == "STARSIM"){return 1;}
    return 0;
}

Bool_t RHICfSimUtil::IsSingleGenMode()
{
    if(!mSimOptions){return 0;}
    TString mode = mSimOptions -> GetOptString("MODE");
    mode.ToUpper();
    if(mode == "SINGLEGEN"){return 1;}
    return 0;
}

TString RHICfSimUtil::GetProcessName(int procId)
{
    if(procId == 101){return "NonDiffraction";}
    if(procId == 103 || procId == 104){return "SingleDiffraction";}
    if(procId == 105){return "DoubleDiffraction";}
    if(procId == 106){return "CentralDiffraction";}
    if(procId == 102){return "Elestic";}
    return "Non";
}   

Double_t RHICfSimUtil::GetRandomGaus(double mean, double sigma){return mRandom -> Gaus(mean, sigma);}
Double_t RHICfSimUtil::GetRandomUniform(double bound1, double bound2){return mRandom -> Uniform(bound1, bound2);}
