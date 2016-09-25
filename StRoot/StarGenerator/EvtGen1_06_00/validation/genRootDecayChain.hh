#ifndef GENROOT_DECAYCHAIN_HH
#define GENROOT_DECAYCHAIN_HH

#include "TFile.h"
#include "TTree.h"
#include "TCanvas.h"

#include <string>

class EvtParticle;

class genRootDecayChain {

public:

    genRootDecayChain(const std::string& decayFileName,
		      const std::string& rootFileName,
		      const std::string& parentName,
		      int nEvents,
		      bool storeMtmXYZ = false);

    ~genRootDecayChain();

    void run();

protected:

    void initTree();
    void generateEvents();
    void storeDaughterInfo(EvtParticle* theParticle);
    void storeTreeInfo(EvtParticle* theParticle);
    void writeTree();

private:

    std::string _decayFileName;
    std::string _rootFileName;
    std::string _parentName;
    int _nEvents;
    bool _storeMtmXYZ;

    TFile* _theFile;
    TTree* _theTree;
    TCanvas* _theCanvas;

    int _eventId;
    int _PDGId;
    int _dVtx;
    int _pVtx;
    int _daug;
    double _p;
    double _E;
    double _pL;
    double _EL;
    double _m;
    double _px;
    double _py;
    double _pz;
    double _pxL;
    double _pyL;
    double _pzL;
    double _t;

    int _vertexNo;

};


#endif
