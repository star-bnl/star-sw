//-------------------------------------------------
// For StTpcEvalMaker
//-------------------------------------------------
// author: milton toy
// additions: manuel cbs
//-------------------------------------------------
// header file for class StTpcEvalOutput
//-------------------------------------------------

#ifndef StTpcEvalOutput_H
#define StTpcEvalOutput_H

#include "StObject.h"

class TFile; 

class StTpcEvalOutput : public StObject {
public:
    StTpcEvalOutput();
    void Open(); 
    void Write(); 
    void Close(); 
    TFile* mOutputFile; //!
private:
    ClassDef(StTpcEvalOutput,1)
};

#endif
