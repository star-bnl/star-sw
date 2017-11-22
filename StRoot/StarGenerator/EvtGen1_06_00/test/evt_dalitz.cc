/*
 * Generate the kinematics of a Dalitz decay 
 * according to a decay file. 
 */

#include <stdio.h>
#include <unistd.h>
#include <fstream>
#include "EvtGenBase/EvtSimpleRandomEngine.hh"
#include "EvtGenBase/EvtMTRandomEngine.hh"
#include "EvtGenBase/EvtRandom.hh"
#include "EvtGenBase/EvtVector4R.hh"
#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtParticleFactory.hh"
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGen/EvtGen.hh"
#include "EvtGenBase/EvtAbsRadCorr.hh"
#include "EvtGenBase/EvtDecayBase.hh"

#ifdef EVTGEN_EXTERNAL
#include "EvtGenExternal/EvtExternalGenList.hh"
#endif

#include <cstdlib>
#include <list>
#include <string>

using std::endl;
using std::fstream;
using std::ifstream;
using std::ofstream;

void usage(const char *cmd)
{
  printf("Usage: %s -d <decayfile> -m <mother> -n <nevents> -f <datfile> -r <rootfile>\n",cmd);
}


int main(int argc, char* argv[]) {

  std::string mother("B+");
  int N = 10;
  std::string datfile("output.dat");
  std::string decfile =("../DECAY_2010.DEC");

  // Get options

  extern char* optarg; 
  int c;
  while ( (c = getopt(argc, argv, "d:m:n:f:")) != EOF ) {
      switch (c) {
      case 'm': mother = std::string(optarg); break;
      case 'n': N = atoi(optarg); break;
      case 'f': datfile = std::string(optarg); break; 
      case 'd': decfile = std::string(optarg); break; 
      }
  }

  // Initialize EvtGen

  // Define the random number generator
  EvtRandomEngine* eng = 0;

#ifdef EVTGEN_CPP11
  // Use the Mersenne-Twister generator (C++11 only)
  eng = new EvtMTRandomEngine();
#else
  eng = new EvtSimpleRandomEngine();
#endif

  EvtRandom::setRandomEngine(eng);

  EvtAbsRadCorr* radCorrEngine = 0;
  std::list<EvtDecayBase*> extraModels;

#ifdef EVTGEN_EXTERNAL
  EvtExternalGenList genList;
  radCorrEngine = genList.getPhotosModel();
  extraModels = genList.getListOfModels();
#endif

  EvtGen myGenerator(decfile.c_str(),"../evt.pdl",eng,
		     radCorrEngine, &extraModels);

  // Initialize decay

  EvtId id = EvtPDL::getId(mother);

  // generate

  ofstream df(datfile.c_str());

  int count = 1;
  do {
    EvtVector4R p_init(EvtPDL::getMass(id),0.0,0.0,0.0);
    EvtParticle *root_part=EvtParticleFactory::particleFactory(id,p_init);
    root_part->setDiagonalSpinDensity();      
    myGenerator.generateDecay(root_part);

    int nDaug = root_part->getNDaug();
    if (nDaug == 3) {

      EvtVector4R p0 = root_part->getDaug(0)->getP4Lab();
      EvtVector4R p1 = root_part->getDaug(1)->getP4Lab();
      EvtVector4R p2 = root_part->getDaug(2)->getP4Lab();
      double qAB = (p0+p1).mass2();
      double qBC = (p1+p2).mass2();
      double qCA = (p2+p0).mass2();

      df << qAB << " " << qBC << " " << qCA << " " << endl;
    }

    root_part->deleteTree();

  } while(count++ < N);

  delete eng;

}

// Macro for displaying resutls
/*
TTree* readTree(const char* file, int N) 
{
  double qAB,qBC,qCA;
  TTree* t = new TTree();
  t->Branch("qAB",&qAB,"qAB/D");
  t->Branch("qBC",&qBC,"qBC/D");
  t->Branch("qCA",&qCA,"qCA/D");
  
  ifstream f(file);

  int i=0;
  while(i < N) {
    i++;
    f >> qAB >> qBC >> qCA;
    t->Fill();
  }

  return t;
}
*/




