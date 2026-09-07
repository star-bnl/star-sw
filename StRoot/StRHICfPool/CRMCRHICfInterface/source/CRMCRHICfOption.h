#ifndef _CRMCRHICfOption_h_
#define _CRMCRHICfOption_h_

#include <string>
#include "TString.h"

class CRMCRHICfOption
{
    public:
        enum RunType{
            TL = 0,
            TS = 1, 
            TOP = 2,
            ALL = 3,
            NON = -1
        };

    public:
        CRMCRHICfOption();
        CRMCRHICfOption(int argc, char** argv);
        virtual ~CRMCRHICfOption() {}

        void DumpConfig() const;

        std::string GetOutputFileName() const;

        TString GetModelName() const; 
        TString GetRHICfRunTypeName() const; 
        int GetModelIdx() const;
        int GetRHICfRunType() const;

        TString GetTime() const;

        const std::string GetJobIndex() const { return fJobIndex;}
        const std::string& GetParamFileName() const { return fParamFileName; }

        int GetHEModel() const { return fHEModel; }
        int GetSeed() const { return fSeed; }
        int GetNCollision() const { return fNCollision; }


    protected:
        int fNCollision;
        int fSeed;
        int fHEModel;

        std::string fParamFileName;
        std::string fOutputFileName;
        std::string fRHICfRunType;
        std::string fJobIndex;

    private:
        void ParseOptions(int argc, char** argv);

};

#endif
