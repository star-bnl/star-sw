#include "RHICfSimOptions.hh"

#include "TFile.h"
#include "TTree.h"
#include "StRHICfSimPar.h"
#include "StRHICfSimDst.h"
#include "StRHICfSimEvent.h"

#include "TSystem.h"
#include "TSystemFile.h"
#include "TSystemDirectory.h"

RHICfSimOptions::RHICfSimOptions() 
{
    mRequiredPar.clear();
    mOptInt.clear();
    mOptDouble.clear();
    mOptBool.clear();
    mOptString.clear();

    Init();
}

RHICfSimOptions::~RHICfSimOptions()
{
}

void RHICfSimOptions::Init()
{
    // List of Required parameters
    mRequiredPar.push_back(make_pair("m", "mode")); // input mode : STARsim, HepMC 
    mRequiredPar.push_back(make_pair("i", "input")); // input file : StarSim.root, HepMC.txt
    mRequiredPar.push_back(make_pair("n", "eventnum")); // total number of events
    mRequiredPar.push_back(make_pair("o", "outpath")); // output file path
    mRequiredPar.push_back(make_pair("r", "runtype")); // RHICf run tpye : TS, TL, TOP
    mRequiredPar.push_back(make_pair("g", "geometrydir")); // Geometry dir path
    mRequiredPar.push_back(make_pair("t", "tabledir")); // Table dir path
}

void RHICfSimOptions::SetInputOption(int num,char** par)
{   
    mInputParNum = num;
    mInputPar = par;
    if(num == 2){
        TString par = mInputPar[1];
        ParsingFile(par);
    }
    else if(num > 2){
        ParsingParm();
    }

    // checking whether the required parameters are registered
    for(int i=0; i<mRequiredPar.size(); i++){
        TString requiredParName = mRequiredPar[i].second;
        if(!CheckOpt(requiredParName)){
            cout << "RHICfSimOptions::SetInputOption() -- Option: " << requiredParName << " is not set." << endl;

            // Force to default required parameters
            if(requiredParName == "geometrydir"){
                AddOpt(requiredParName, GetDirPath(requiredParName));
            }
            if(requiredParName == "tabledir"){
                AddOpt(requiredParName, GetDirPath(requiredParName));
            }
            if(requiredParName == "mode"){
                AddOpt(requiredParName, TString("STARSIM"));
                cout << "RHICfSimOptions::SetInputOption() -- " << requiredParName << " is to be default mode. STARSIM" << endl;
            }
            if(requiredParName == "outpath"){
                TString currentPath = gSystem -> pwd();
                AddOpt(requiredParName, currentPath);
                cout << "RHICfSimOptions::SetInputOption() -- " << requiredParName << " is to be current path. " << currentPath << endl;
            }
            if(requiredParName == "runtype"){
                TString mode = GetOptString("MODE");
                if(mode == "STARSIM"){
                    TString inputFile = GetOptString("INPUT");
                    inputFile = GetExistROOTFile(inputFile); // Check the one RHICfSimDst.root file or STAR job
                    AddOpt("AUTOINPUT", inputFile);

                    TFile* file = new TFile(inputFile, "READ");
                    TTree* tree = (TTree*)file -> Get("StRHICfSimDst");
                    StRHICfSimDst* simDst = new StRHICfSimDst();
                    simDst -> ReadDstArray(tree);
                    int eventNum = tree -> GetEntries();
                    if(!CheckOpt("EVENTNUM")){AddOpt("eventNum", eventNum);}
                    
                    tree -> GetEntry(0);
                    StRHICfSimEvent* simEvent = simDst -> GetSimEvent();
                    int runtype = simEvent -> GetRHICfRunType();
                    TString runtypeName = "";
                    if(runtype == rTLtype){runtypeName = "TL";}
                    if(runtype == rTStype){runtypeName = "TS";}
                    if(runtype == rTOPtype){runtypeName = "TOP";}
                    AddOpt("runtype", runtypeName);

                    // Output file name
                    TString outputFile = inputFile;
                    if(CheckOpt("OUTPATH")){
                        TObjArray* tokens = outputFile.Tokenize("/");
                        outputFile = ((TObjString *)tokens -> At(tokens->GetEntries()-1)) -> GetString();
                        TString outPath = GetOptString("OUTPATH");
                        outPath = (outPath[outPath.Sizeof()-2] == '/')? outPath : outPath+"/";
                        outputFile = outPath + outputFile;
                    }
                    outputFile.ReplaceAll(".RHICfSimDst.root", ".rhicfsim.RHICfSimDst.root");
                    AddOpt("OUTPUT", outputFile);
                    cout << "RHICfSimOptions::SetInputOption()  -- OUTPUT: " << outputFile << endl;

                    if(CheckOpt("runtype")){
                        cout << "RHICfSimOptions::SetInputOption() -- " << requiredParName << " has found. " << GetOptString("RUNTYPE") << endl; 
                        delete simEvent;
                        delete simDst;
                        delete tree;
                        delete file;
                    }
                }
                else{
                    cout << "RHICfSimOptions::SetInputOption() -- Warning: " << requiredParName << " is not set." << endl;
                }
            }
        }
    }

    PrintOpt();
}
void RHICfSimOptions::SetParm(int num, char** par)
{
    mInputParNum = num;
    mInputPar = par;

    ParsingParm();
    PrintOpt();
}

void RHICfSimOptions::AddOpt(TString name, int value)
{
    if(CheckOpt(name)){
        cout << "RHICfSimOptions::AddOpt() -- " << name << " already existing.." << endl;
        return;
    }
    mOptInt.push_back(make_pair(name, value));
}

void RHICfSimOptions::AddOpt(TString name, double value)
{
    if(CheckOpt(name)){
        cout << "RHICfSimOptions::AddOpt() -- "<< name << " already existing.." << endl;
        return;
    }
    mOptDouble.push_back(make_pair(name, value));
}

void RHICfSimOptions::AddOpt(TString name, bool value)
{
    if(CheckOpt(name)){
        cout << "RHICfSimOptions::AddOpt() -- "<< name << " already existing.." << endl;
        return;
    }
    mOptBool.push_back(make_pair(name, value));
}

void RHICfSimOptions::AddOpt(TString name, TString value)
{
    if(CheckOpt(name)){
        cout << "RHICfSimOptions::AddOpt() -- " << name << " already existing.." << endl;
        return;
    }
    mOptString.push_back(make_pair(name, value));
}

int RHICfSimOptions::GetOptInt(TString name)
{
    name.ToUpper();
    for(int i=0; i<mOptInt.size(); i++){
        TString optName = mOptInt[i].first;
        optName.ToUpper();
        if(name == optName){return mOptInt[i].second;}
    }
    cout << "RHICfSimOptions::GetOptInt() -- " << name << " is not exist !!!" << endl;
    return -999;
}

double RHICfSimOptions::GetOptDouble(TString name)
{
    name.ToUpper();
    for(int i=0; i<mOptDouble.size(); i++){
        TString optName = mOptDouble[i].first;
        optName.ToUpper();
        if(name == optName){return mOptDouble[i].second;}
    }

    cout << "RHICfSimOptions::GetOptDouble() -- " << name << " is not exist !!!" << endl;
    return -999.;
}

bool RHICfSimOptions::GetOptBool(TString name)
{
    name.ToUpper();
    for(int i=0; i<mOptBool.size(); i++){
        TString optName = mOptBool[i].first;
        optName.ToUpper();
        if(name == optName){return mOptBool[i].second;}
    }

    cout << "RHICfSimOptions::GetOptBool() -- " << name << " is not exist !!!" << endl;
    return 0;
}

TString RHICfSimOptions::GetOptString(TString name)
{
    name.ToUpper();
    for(int i=0; i<mOptString.size(); i++){
        TString optName = mOptString[i].first;
        optName.ToUpper();
        if(name == optName){return mOptString[i].second;}
    }

    cout << "RHICfSimOptions::GetOptString() -- " << name << " is not exist !!!" << endl;
    return "";
}

bool RHICfSimOptions::CheckOpt(TString name)
{
    name.ToUpper();
    TString optName;
    for(int i=0; i<mOptInt.size(); i++){
        optName = mOptInt[i].first;
        optName.ToUpper();
        if(name == optName){return true;}
    }
    for(int i=0; i<mOptDouble.size(); i++){
        optName = mOptDouble[i].first;
        optName.ToUpper();
        if(name == optName){return true;}
    }
    for(int i=0; i<mOptBool.size(); i++){
        optName = mOptBool[i].first;
        optName.ToUpper();
        if(name == optName){return true;}
    }
    for(int i=0; i<mOptString.size(); i++){
        optName = mOptString[i].first;
        optName.ToUpper();
        if(name == optName){return true;}
    }
    return false;
}

void RHICfSimOptions::PrintOpt()
{
    cout << "RHICfSimOptions::PrintOpt() ================================================================" << endl;
    TString optName;
    for(int i=0; i<mOptInt.size(); i++){
        optName = mOptInt[i].first;
        optName.ToUpper();
        cout << "Option: " << optName << " = " << mOptInt[i].second << endl;
    }
    for(int i=0; i<mOptDouble.size(); i++){
        optName = mOptDouble[i].first;
        optName.ToUpper();
        cout << "Option: " << optName << " = " << mOptDouble[i].second << endl;
    }
    for(int i=0; i<mOptBool.size(); i++){
        optName = mOptBool[i].first;
        optName.ToUpper();
        cout << "Option: " << optName << " = " << mOptBool[i].second << endl;
    }
    for(int i=0; i<mOptString.size(); i++){
        optName = mOptString[i].first;
        optName.ToUpper();
        cout << "Option: " << optName << " = " << mOptString[i].second << endl;
    }
    cout << "============================================================================================" << endl;
}

void RHICfSimOptions::ParsingFile(TString par)
{
    if(par.Index(".txt") != -1 || par.Index(".dat") != -1 || par.Index(".par") != -1){
        std::string const file = par.Data();
        std::ifstream inputStream(file.c_str());
        std::string line;
        TObjArray *tokens;
        while(getline(inputStream, line)){
            tokens = TString(line).Tokenize(" ");
            if(tokens->GetEntries() == 0){continue;}

            vector<TString> word;
            for(int i=0; i<tokens->GetEntries(); i++){
                TString value = ((TObjString *) tokens -> At(i)) -> GetString();
                word.push_back(value);
            }

            // case1: "name" "value" "comment etc.."
            if(word[0][0] == '*' || word[0][0] == '#' || word[0][0] == '!'){continue;}
            if(tokens->GetEntries() < 2){continue;}
            if(word[1][0] != '=' && (!(word[1][0] == '/' && word[1][1] == '/' && word[1].Sizeof() > 2) || word[1][0] != '#')){
                AddStringByType(word[0], word[1]);
                ParsingFile(word[1]);
            }

            // case2: "name" "=" "value" 
            if(tokens->GetEntries() < 3){continue;}
            if(word[1][0] == '=' && (!(word[2][0] == '/' && word[2][1] == '/' && word[2].Sizeof() > 2) || word[2][0] != '#')){
                AddStringByType(word[0], word[2]);
                ParsingFile(word[2]);
            }
        } 
    }
}

void RHICfSimOptions::ParsingParm()
{
    TString parName = "";
    TString parValue = "";
    for(int i=1; i<mInputParNum; i++){
        TString par = TString(mInputPar[i]);

        int pos = -1;
        pos = par.Index("-");
        if(pos != -1){
            parName = par.ReplaceAll("-", "");
            parValue = TString(mInputPar[i+1]);
            if(parValue[0] != '-'){
                int findRequiredParIdx = -1;
                for(int i=0; i<mRequiredPar.size(); i++){
                    if(parName == TString(mRequiredPar[i].first)){
                        findRequiredParIdx = i;
                        break;
                    }
                }
                if(findRequiredParIdx != -1){
                    parName = mRequiredPar[findRequiredParIdx].second;
                }
                AddStringByType(parName, parValue);
                ParsingFile(parValue);
            }
            i = i+1;
        }
    }
}

void RHICfSimOptions::AddStringByType(TString name, TString val)
{
    TString tmp = val;
    tmp.ToUpper();

    if(tmp.IsFloat()){
        if(tmp.IsDec()){
            int valueInt = tmp.Atoi();
            AddOpt(name, valueInt); // int
        }
        else{
            double valueFloat = tmp.Atof();
            AddOpt(name, valueFloat); // double
        }
    }
    else if(tmp.IsBin() || tmp == "TRUE" || tmp == "FALSE"){
        bool valueBool = (tmp.IsBin())? tmp.Atoi() : ((tmp=="TRUE")? 1 : 0);
        AddOpt(name, valueBool); // bool
    }
    else{
        AddOpt(name, val); // string
    } 
}

TString RHICfSimOptions::GetDirPath(TString type)
{
    TString typeName = (type == "geometrydir")? "Geometry" : "Table";

    // Find a Geometry and table directory
    TString currentPath = gSystem -> pwd();
    TObjArray *tokens = currentPath.Tokenize("/");
    TString directory = "";

    TList *listOfDirs;
    TObject *objDir;
    for(int i=0; i<tokens->GetEntries()-3; i++){
        TString dirPath = "";
        for(int j=0; j<tokens->GetEntries()-i; j++){
            dirPath = dirPath+"/"+ ((TObjString *) tokens -> At(j)) -> GetString();
        }
        TSystemDirectory dir("dir", dirPath);
        listOfDirs = dir.GetListOfFiles();
        TIter next(listOfDirs);

        while((objDir = next())){
            TSystemFile* dirPtr = dynamic_cast<TSystemFile*>(objDir);
            if(dirPtr && dirPtr->IsDirectory()){
                TString dirName = dirPtr->GetName();
                if(dirName.Index("geometry") != -1 && type == "geometrydir"){
                    directory = dirPath+"/geometry";
                }
                if(dirName.Index("tables") != -1 && type == "tabledir"){
                    directory = dirPath+"/tables";
                }
            }
        }

        if(directory != ""){
            cout << "RHICfSimOptions::GetDirPath() -- " << typeName << " was found. " << directory << endl;
            return directory;
        }
    }
}

TString RHICfSimOptions::GetExistROOTFile(TString input)
{
    if(input.Index(".RHICfSimDst.root") != -1){return input;}
    TString findFileName = input + ".RHICfSimDst.root";

    TString currentPath = gSystem -> pwd();
    currentPath.ReplaceAll("/RHICfSTARSim/build", "");

    cout << "RHICfSimOptions::GetExistROOTFile() -- Find a RHICfSimDst file in " << currentPath << endl;

    TSystemDirectory dir("dir", currentPath);
    TList *listOfDirs = dir.GetListOfFiles();
    
    TObject *objDir;
    TIter next(listOfDirs);
    while((objDir = next())){
        TSystemFile* dirPtr = dynamic_cast<TSystemFile*>(objDir);
        if(dirPtr && !dirPtr->IsDirectory()){
            TString fileName = dirPtr->GetName();
            if(fileName.Index(findFileName) != -1){
                fileName = currentPath +"/"+ fileName;
                cout << "RHICfSimOptions::GetExistROOTFile() -- RHICfSimDst file found " << fileName << endl;
                return fileName;
            }
        }
    }

    cout << "RHICfSimOptions::GetExistROOTFile() -- Can not find a " <<  findFileName << endl;
    return "";
}