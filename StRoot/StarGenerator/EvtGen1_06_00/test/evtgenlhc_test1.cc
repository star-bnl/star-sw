//#@# Dalitz plot for D0 --> K- pi+ pi0 decay:
//#@# 1: Mass(K-, pi+)
//#@# 2: Mass(pi+,pi0)
//
//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package developed jointly
//      for the BaBar and CLEO collaborations.  If you use all or part
//      of it, please give an appropriate acknowledgement.
//
// Copyright Information: See EvtGen/COPYRIGHT
//      Copyright (C) 1998      Caltech, UCSB
//
// Module: testEvtGen.cc
//
//  Description:
//
//     This program invokes the EvtGen event generator package
//     for testing various decay models that are implemented.
//
// Modification history:
//
//    DJL/RYD     Sometime long ago         Module created
//
//------------------------------------------------------------------------
//
//
//
#include "EvtGenBase/EvtComplex.hh"
#include "EvtGenBase/EvtTensor4C.hh"
#include "EvtGenBase/EvtVector4C.hh"
#include "EvtGenBase/EvtVector4R.hh"
#include "EvtGenBase/EvtVectorParticle.hh"
#include "EvtGenBase/EvtDiracSpinor.hh"
#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtKine.hh"
#include "EvtGenBase/EvtGammaMatrix.hh"
#include "EvtGenBase/EvtRandom.hh"
#include "EvtGenBase/EvtRandomEngine.hh"
#include "EvtGenBase/EvtDecayTable.hh"
#include "EvtGenBase/EvtReport.hh"
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenBase/EvtStdHep.hh"
#include "EvtGenBase/EvtSecondary.hh"
#include "EvtGenBase/EvtConst.hh"
#include "EvtGen/EvtGen.hh"
#include "EvtGenBase/EvtParticleFactory.hh"
#include "EvtGenBase/EvtSimpleRandomEngine.hh"
#include "EvtGenBase/EvtMTRandomEngine.hh"
#include "EvtGenBase/EvtIdSet.hh"
#include "EvtGenBase/EvtParser.hh"

#include "EvtGenBase/EvtAbsRadCorr.hh"
#include "EvtGenBase/EvtDecayBase.hh"

#ifdef EVTGEN_EXTERNAL
#include "EvtGenExternal/EvtExternalGenList.hh"
#endif

#include <cstdio>
#include <fstream>
#include <sstream>
#include <cmath>
#include <string>
#include <vector>
#include <cstdlib>

#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "TApplication.h"
#include "TROOT.h"

using std::vector;

void runFile(int nevent,char* fname,EvtGen& myGenerator);
void runPrint(int nevent,char* fname,EvtGen& myGenerator);
void runFileVpho(int nevent,char* fname,EvtGen& myGenerator);
void runTest1(int nevent,EvtGen& myGenerator);
void runTest2(int nevent,EvtGen& myGenerator);
void runOmega(int nevent,EvtGen& myGenerator);
void runChi1Kstar(int nevent,EvtGen& myGenerator);
void runPi0Dalitz(int nevent,EvtGen& myGenerator);
void runMix(int nevent,EvtGen& myGenerator);
void runBMix(int nevent,EvtGen& myGenerator,std::string userFile,std::string rootFile);
void runDDalitz(int nevent,EvtGen& myGenerator);
void runPiPiCPT(int nevent,EvtGen& myGenerator);
void runPiPiPiPi(int nevent,EvtGen& myGenerator);
void runD2Pi(int nevent,EvtGen& myGenerator);
void runJetsetTab3(int nevent,EvtGen& myGenerator);
void runHelAmp(int nevent,EvtGen& myGenerator,std::string userFile,std::string rootFile);
void runHelAmp2(int nevent,EvtGen& myGenerator);
void runJpsiKs(int nevent,EvtGen& myGenerator);
void runDump(int nevent,EvtGen& myGenerator);
void runD1(int nevent,EvtGen& myGenerator);
void runGenericCont(int nevent,EvtGen& myGenerator);
void runPiPiPi(int nevent,EvtGen& myGenerator);
void runBHadronic(int nevent,EvtGen& myGenerator);
void runSingleB(int nevent,EvtGen& myGenerator);
void runA2Pi(int nevent,EvtGen& myGenerator);
void runAlias();
void runRepeat(int nevent);
void runPhotos(int nevent,EvtGen& myGenerator);
void runTrackMult(int nevent,EvtGen& myGenerator);
void runGeneric(int neventOrig,EvtGen& myGenerator,
		std::string listfile);
void runFinalStates(int nevent,EvtGen& myGenerator);
std::vector<std::string> findFinalState(EvtParticle *p);
void runKstarnunu(int nevent,EvtGen& myGenerator);
void runBsmix(int nevent,EvtGen& myGenerator);
void runTauTauPiPi(int nevent,EvtGen& myGenerator);
void runTauTauEE(int nevent,EvtGen& myGenerator);
void runTauTau2Pi2Pi(int nevent,EvtGen& myGenerator);
void runTauTau3Pi3Pi(int nevent,EvtGen& myGenerator);
void runJPsiKstar(int nevent,EvtGen& myGenerator,int modeInt);
void runSVVCPLH(int nevent,EvtGen& myGenerator);
void runSVSCPLH(int nevent,EvtGen& myGenerator);
void runSSDCP(int nevent,EvtGen& myGenerator);
void runKstarstargamma(int nevent,EvtGen& myGenerator);
void runDSTARPI(int nevent,EvtGen& myGenerator);
void runETACPHIPHI(int nevent,EvtGen& myGenerator);
void runVVPiPi(int nevent,EvtGen& myGenerator);
void runSVVHelAmp(int nevent,EvtGen& myGenerator);
void runSVVHelAmp2(int nevent,EvtGen& myGenerator);
void runPartWave(int nevent,EvtGen& myGenerator);
void runPartWave2(int nevent,EvtGen& myGenerator);
void runTwoBody(int nevent,EvtGen& myGenerator,std::string decfile,std::string rootFile);
void runPiPi(int nevent,EvtGen& myGenerator);
void runA1Pi(int nevent,EvtGen& myGenerator);
void runCPTest(int nevent,EvtGen& myGenerator);
void runSemic(int nevent,EvtGen& myGenerator);
void runKstarll(int nevent,EvtGen& myGenerator);
void runKll(int nevent,EvtGen& myGenerator);
void runHll(int nevent,EvtGen& myGenerator, char* mode);
void runVectorIsr(int nevent,EvtGen& myGenerator);
void runBsquark(int nevent,EvtGen& myGenerator);
void runK3gamma(int nevent,EvtGen& myGenerator);
void runLambda(int nevent,EvtGen& myGenerator);
void runBtoXsgamma(int nevent,EvtGen& myGenerator);
void runBtoK1273gamma(int nevent,EvtGen& myGenerator);
void runCheckRotBoost();
void runMassCheck(int nevent, EvtGen& myGenerator, int partnum);                                                                  
void runJpsiPolarization(int nevent, EvtGen& myGenerator);
void runDDK(int nevent, EvtGen &myGenerator);

int countInclusive(std::string name, EvtParticle *root,TH1F* mom=0,TH1F* mass=0 );
int countInclusiveParent(std::string name, EvtParticle *root,EvtIdSet setIds,
			 TH1F* mom=0);
int countInclusiveSubTree(std::string name, EvtParticle *root,EvtIdSet setIds,
			 TH1F* mom=0);
void runBaryonic(int nEvent, EvtGen& myGenerator);


int main(int argc, char* argv[]){

  // Define the random number generator
  EvtRandomEngine* myRandomEngine = 0;

#ifdef EVTGEN_CPP11
  // Use the Mersenne-Twister generator (C++11 only)
  myRandomEngine = new EvtMTRandomEngine();
#else
  myRandomEngine = new EvtSimpleRandomEngine();
#endif

  if (!TROOT::Initialized()) {
    static TROOT root("RooTuple", "RooTuple ROOT in EvtGen");
  }
  if (argc==1){

    EvtVector4R p(0.0,1.0,0.0,0.0);
    EvtVector4R k(0.0,0.0,1.0,0.0);

    EvtTensor4C T=dual(EvtGenFunctions::directProd(p,k));

    EvtGenReport(EVTGEN_INFO,"EvtGen") << "p:"<<p<<std::endl;
    EvtGenReport(EVTGEN_INFO,"EvtGen") << "k:"<<k<<std::endl;
    EvtGenReport(EVTGEN_INFO,"EvtGen") << "T=dual(directProd(p,k)):"<<T<<std::endl;
    EvtGenReport(EVTGEN_INFO,"EvtGen") << "T03:"<<T.get(0,3)<<std::endl;
    return 1;
  }

  EvtAbsRadCorr* radCorrEngine = 0;
  std::list<EvtDecayBase*> extraModels;

#ifdef EVTGEN_EXTERNAL
  EvtExternalGenList genList;
  radCorrEngine = genList.getPhotosModel();
  extraModels = genList.getListOfModels();
#endif

  EvtGen myGenerator("../DECAY_2010.DEC", "../evt.pdl", myRandomEngine,
		     radCorrEngine, &extraModels);

  if (!strcmp(argv[1],"file")) {
    int nevent=atoi(argv[2]);
    runFile(nevent,argv[3],myGenerator);
  }

  if (!strcmp(argv[1],"print")) {
    int nevent=atoi(argv[2]);
    runPrint(nevent,argv[3],myGenerator);
  }

  if (!strcmp(argv[1],"filevpho")) {
    int nevent=atoi(argv[2]);
    runFileVpho(nevent,argv[3],myGenerator);
  }

  if (!strcmp(argv[1],"test1")) {
    int nevent=atoi(argv[2]);
    runTest1(nevent,myGenerator);
  }

  if (!strcmp(argv[1],"chi1kstar")) {
    int nevent=atoi(argv[2]);
    runChi1Kstar(nevent,myGenerator);
  }

  if (!strcmp(argv[1],"test2")) {
    int nevent=atoi(argv[2]);
    runTest2(nevent,myGenerator);
  }

  if (!strcmp(argv[1],"omega")) {
    int nevent=atoi(argv[2]);
    runOmega(nevent,myGenerator);
  }

  if (!strcmp(argv[1],"alias")) {
    runAlias();
  }

  if (!strcmp(argv[1],"repeat")) {
    int nevent=atoi(argv[2]);
    runRepeat(nevent);
  }

  if (!strcmp(argv[1],"photos")) {
    int nevent=atoi(argv[2]);
    runPhotos(nevent,myGenerator);
  }

  if (!strcmp(argv[1],"trackmult")) {
    int nevent=atoi(argv[2]);
    runTrackMult(nevent,myGenerator);
  }

  if (!strcmp(argv[1],"generic")) {
    int nevent=atoi(argv[2]);
    std::string listfile("");
    if ( argc==4) listfile=argv[3];
    runGeneric(nevent,myGenerator,listfile);
  }

  if (!strcmp(argv[1],"finalstates")) {
    int nevent=atoi(argv[2]);
    runFinalStates(nevent,myGenerator);
  }

  if (!strcmp(argv[1],"kstarnunu")) {
    int nevent=atoi(argv[2]);
    runKstarnunu(nevent,myGenerator);
  }

  if (!strcmp(argv[1],"bsmix")) {
    int nevent=atoi(argv[2]);
    runBsmix(nevent,myGenerator);
  }

  if (!strcmp(argv[1],"BtoXsgamma")) {
    int nevent=atoi(argv[2]);
    runBtoXsgamma(nevent,myGenerator);
  }

  if (!strcmp(argv[1],"BtoK1273gamma")) {
    int nevent=atoi(argv[2]);
    runBtoK1273gamma(nevent,myGenerator);
  }

  if (!strcmp(argv[1],"pi0dalitz")) {
    int nevent=atoi(argv[2]);
    runPi0Dalitz(nevent,myGenerator);
  }

  if (!strcmp(argv[1],"ddalitz")) {
    int nevent=atoi(argv[2]);
    runDDalitz(nevent,myGenerator);
  }


  if (!strcmp(argv[1],"kstarll")) {
    int nevent=atoi(argv[2]);
    runKstarll(nevent, myGenerator);
  }
  if (!strcmp(argv[1],"kll")) {
    int nevent=atoi(argv[2]);
    runKll(nevent, myGenerator);
  }
  if (!strcmp(argv[1],"hll")) {
    int nevent=atoi(argv[2]);
    runHll(nevent, myGenerator, argv[3]);
  }

  if (!strcmp(argv[1],"vectorisr")) {
    int nevent=atoi(argv[2]);
    runVectorIsr(nevent, myGenerator);
  }


  if (!strcmp(argv[1],"bsquark")) {
    int nevent=atoi(argv[2]);
    runBsquark(nevent, myGenerator);
  }


  if (!strcmp(argv[1],"k3gamma")) {
    int nevent=atoi(argv[2]);
    runK3gamma(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"lambda")) {
    int nevent=atoi(argv[2]);
    runLambda(nevent, myGenerator);
  }


  if (!strcmp(argv[1],"tautaupipi")) {
    int nevent=atoi(argv[2]);
    runTauTauPiPi(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"tautauee")) {
    int nevent=atoi(argv[2]);
    runTauTauEE(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"tautau2pi2pi")) {
    int nevent=atoi(argv[2]);
    runTauTau2Pi2Pi(nevent, myGenerator);
  }
  if (!strcmp(argv[1],"tautau3pi3pi")) {
    int nevent=atoi(argv[2]);
    runTauTau3Pi3Pi(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"jpsikstar")) {
    int nevent=atoi(argv[2]);
    int modeInt=atoi(argv[3]);
    runJPsiKstar(nevent, myGenerator,modeInt);
  }


  if (!strcmp(argv[1],"svvcplh")) {
    int nevent=atoi(argv[2]);
    runSVVCPLH(nevent, myGenerator);
  }


  if (!strcmp(argv[1],"svscplh")) {
    int nevent=atoi(argv[2]);
    runSVSCPLH(nevent, myGenerator);
  }


  if (!strcmp(argv[1],"ssdcp")) {
    int nevent=atoi(argv[2]);
    runSSDCP(nevent, myGenerator);
  }


  if (!strcmp(argv[1],"kstarstargamma")) {
    int nevent=atoi(argv[2]);
    runKstarstargamma(nevent, myGenerator);
  }


  if (!strcmp(argv[1],"dstarpi")) {
    int nevent=atoi(argv[2]);
    runDSTARPI(nevent, myGenerator);
  }


  if (!strcmp(argv[1],"etacphiphi")) {
    int nevent=atoi(argv[2]);
    runETACPHIPHI(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"vvpipi")) {
    int nevent=atoi(argv[2]);
    runVVPiPi(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"svvhelamp")) {
    int nevent=atoi(argv[2]);
    runSVVHelAmp(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"partwave")) {
    int nevent=atoi(argv[2]);
    runPartWave(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"partwave2")) {
    int nevent=atoi(argv[2]);
    runPartWave2(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"twobody")) {
    int nevent=atoi(argv[2]);
    runTwoBody(nevent, myGenerator, argv[3], argv[4]);
  }

  if (!strcmp(argv[1],"pipipi")) {
    int nevent=atoi(argv[2]);
    runPiPiPi(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"bhadronic")) {
    int nevent=atoi(argv[2]);
    runBHadronic(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"singleb")) {
    int nevent=atoi(argv[2]);
    runSingleB(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"pipi")) {
    int nevent=atoi(argv[2]);
    runPiPi(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"pipipipi")) {
    int nevent=atoi(argv[2]);
    runPiPiPiPi(nevent, myGenerator);
  }
     
  if (!strcmp(argv[1],"a2pi")) {
    int nevent=atoi(argv[2]);
    runA2Pi(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"helamp")) {
    int nevent=atoi(argv[2]);
    runHelAmp(nevent, myGenerator, argv[3], argv[4]);
  }


  if (!strcmp(argv[1],"helamp2")) {
    int nevent=atoi(argv[2]);
    runHelAmp2(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"d2pi")) {
    int nevent=atoi(argv[2]);
    runD2Pi(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"a1pi")) {
    int nevent=atoi(argv[2]);
    runA1Pi(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"cptest")) {
    int nevent=atoi(argv[2]);
    runCPTest(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"pipicpt")) {
    int nevent=atoi(argv[2]);
    runPiPiCPT(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"jpsiks")) {
    int nevent=atoi(argv[2]);
    runJpsiKs(nevent, myGenerator);
  }
  
  if (!strcmp(argv[1],"dump")) {
    int nevent=atoi(argv[2]);
    runDump(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"genericcont")) {
    int nevent=atoi(argv[2]);
    runGenericCont(nevent, myGenerator);
  }


  if (!strcmp(argv[1],"d1")) {
    int nevent=atoi(argv[2]);
    runD1(nevent, myGenerator);
  }
  
  if (!strcmp(argv[1],"mix")) {
    int nevent=atoi(argv[2]);
    runMix(nevent, myGenerator);
  }

  if (!strcmp(argv[1],"bmix")) {
    int nevent=atoi(argv[2]);
    runBMix(nevent, myGenerator, argv[3], argv[4]);
  }
  
  if (!strcmp(argv[1],"semic")) {
    int nevent=atoi(argv[2]);
    runSemic(nevent,myGenerator);
  }

  if (!strcmp(argv[1],"ddk")) {
     int nevent=atoi(argv[2]);
     runDDK(nevent,myGenerator);
   }

  if (!strcmp(argv[1],"checkmass")) {
     int nevent=atoi(argv[2]);
     int partnum=atoi(argv[3]);
     runMassCheck(nevent,myGenerator,partnum);
   }
                                                                             
  if (!strcmp(argv[1],"jpsipolarization")) {
     int nevent=atoi(argv[2]);
     runJpsiPolarization(nevent,myGenerator);
   }

  //*******************************************************
  //test of the rotations and boosts performed in EvtGen.
  // Added by Lange and Ryd Jan 5,2000.
  //*******************************************************

  if (!strcmp(argv[1],"checkrotboost")) {
    runCheckRotBoost();
  }
  
  if ( !strcmp(argv[1], "baryonic") )
  {
      runBaryonic( atoi(argv[2]), myGenerator );
  }

  delete myRandomEngine;
  return 0;


}

void runFile(int nevent,char* fname, EvtGen &myGenerator) {

  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));

  int count;
  
  char udecay_name[100];
  
  strcpy(udecay_name,fname);
  
  myGenerator.readUDecay(udecay_name);
  
  count=1;
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(UPS4,
							       p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}

void runPrint(int nevent,char* fname, EvtGen &myGenerator) {

  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));

  int count;
  
  char udecay_name[100];
  
  strcpy(udecay_name,fname);
  
  myGenerator.readUDecay(udecay_name);
  
  count=1;
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(UPS4,
							       p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);

    root_part->printTree();

    root_part->deleteTree();  
    
  }while (count++<nevent);
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}

void runFileVpho(int nevent,char* fname, EvtGen &myGenerator) {

  static EvtId VPHO=EvtPDL::getId(std::string("vpho"));
  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));

  int count;
  
  char udecay_name[100];
  
  strcpy(udecay_name,fname);
  
  myGenerator.readUDecay(udecay_name);
  
  count=1;
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(VPHO,
							       p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}

///////////////

void runJpsiPolarization(int nevent, EvtGen &myGenerator) {

   static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
   static EvtId JPSI=EvtPDL::getId(std::string("J/psi"));
                                                                             
   int count;
                                                                             
   myGenerator.readUDecay("exampleFiles/GENERIC.DEC");
   myGenerator.readUDecay("exampleFiles/JPSITOLL.DEC");
   TFile *file=new TFile("jpsipolar.root", "RECREATE");
                                                                             
   TH1F* coshel  = new TH1F("h1","cos hel",50,-1.0,1.0); 
   TH1F* coshelHigh  = new TH1F("h2","cos hel pstar gt 1.1",50,-1.0,1.0); 
   TH1F* coshelLow  = new TH1F("h3","cos hel pstar lt 1.1",50,-1.0,1.0); 
                                                                             
   count=1;
                                                                             
   do{
                                                                             
     EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
     EvtParticle* root_part=EvtParticleFactory::particleFactory(UPS4,
                                                                p_init);
                                                                             
     root_part->setVectorSpinDensity();      
     myGenerator.generateDecay(root_part);
     EvtParticle *p = root_part;
                                                                             
     do{
       EvtId type=p->getId();
       if (p->getId()==JPSI) { 
         EvtVector4R p4psi=p->getP4Lab();
         EvtVector4R p4Daug=p->getDaug(0)->getP4Lab();
         double dcostheta=EvtDecayAngle(p_init,p4psi,p4Daug);
         coshel->Fill(dcostheta);
         if ( p4psi.d3mag() > 1.1 ) {
           coshelHigh->Fill(dcostheta);
         }
         else{
           coshelLow->Fill(dcostheta);
         }
       }
       p=p->nextIter(root_part);
     }while(p!=0);
                                                                             
     root_part->deleteTree();  
   }while (count++<nevent);
                                                                             
   file->Write(); file->Close();

   EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}
                                                                              
                                                                             
void runMassCheck(int nevent, EvtGen &myGenerator, 
              int partnum) {
                                                                             
   int count;
   static EvtId myPart=EvtPDL::evtIdFromStdHep(partnum);
   TFile *file=new TFile("checkmass.root", "RECREATE");
                                                                             
   TH1F* mass  = new TH1F("h1","Mass",500,0.0,2.5); 

   count=1;
   do{
     mass->Fill(EvtPDL::getMass(myPart));
   }while (count++<nevent);
   file->Write(); file->Close();

  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}

void runPi0Dalitz(int nevent, EvtGen &myGenerator) {

  static EvtId PI0=EvtPDL::getId(std::string("pi0"));
  
  TFile *file=new TFile("pi0dalitz.root", "RECREATE");
  
  TH1F* q2  = new TH1F("h1","q2",50,0.0,0.02); 
  
  int count;
  

  myGenerator.readUDecay("exampleFiles/PI0DALITZ.DEC");

  count=1;
    
  do{
    

      EvtVector4R p_init(EvtPDL::getMass(PI0),0.0,0.0,0.0);
     
      EvtParticle* root_part=EvtParticleFactory::particleFactory(PI0,
								 p_init);

      root_part->setDiagonalSpinDensity();      

      myGenerator.generateDecay(root_part);

      EvtVector4R ep=root_part->getDaug(0)->getP4Lab();
      EvtVector4R em=root_part->getDaug(1)->getP4Lab();
      EvtVector4R gamma=root_part->getDaug(2)->getP4Lab();

      q2->Fill( (ep+em).mass2() );
      //      EvtGenReport(EVTGEN_INFO,"EvtGen") << ep << em << gamma <<std::endl;
      root_part->deleteTree();  

    }while (count++<nevent);
  
    file->Write(); file->Close();

    EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}



//*******************************************************************************
void runTest1(int nevent, EvtGen &myGenerator) {
  //  TFile *file=new TFile("test1.root", "RECREATE");
  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  
  //  int first=0;
  //  char **second;
  //  TApplication *theApp = new TApplication("App", &first, second);
  TFile *file = new TFile("test1.root", "RECREATE", "Example");
  TH1F * costhetaB  = new TH1F("hcosthetaB","costhetaB",50,-1.0,1.0); 
  TH1F* phiB = new TH1F("hphiB","phiB",50,-EvtConst::pi,EvtConst::pi); 
  TH1F* Elep = new TH1F("hEl","E?l!",50,0.0,2.5); 
  TH1F* q2 = new TH1F("hq2","q^2!",44,0.0,11.0); 
  TH1F* ctv = new TH1F("hctv","ctv",50,-1.0,1.0); 
  TH1F* chi_low_ctv = new TH1F("hcostv1","[h] for cos[Q]?V!\"L#0",50,0.0,EvtConst::twoPi); 
  TH1F* chi_high_ctv = new TH1F("hcostv2","[h] for cos[Q]?V!\"G#0",50,0.0,EvtConst::twoPi); 
  TH1F* dt  = new TH1F("hdt","dt",50,-5.0,5.0); 
  
  
  int count;
  
  EvtVector4R p4b0,p4b0b,p4dstar,p4e,p4nu,p4d,p4pi,p4pip,p4pim;
  char udecay_name[100];
  
  strcpy(udecay_name,"exampleFiles/TEST1.DEC");
  
  //    EvtGen myGenerator(decay_name,pdttable_name,myRandomEngine);
  
  myGenerator.readUDecay(udecay_name);
  double costhetaV;
  
  count=1;
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(UPS4,
							       p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    
    p4b0=root_part->getDaug(0)->getP4Lab();
    p4b0b=root_part->getDaug(1)->getP4Lab();
    
    p4dstar=root_part->getDaug(0)->getDaug(0)->getP4Lab();
    p4e=root_part->getDaug(0)->getDaug(1)->getP4Lab();
    p4nu=root_part->getDaug(0)->getDaug(2)->getP4Lab();
    
    p4d=root_part->getDaug(0)->getDaug(0)->getDaug(0)->getP4Lab();
    p4pi=root_part->getDaug(0)->getDaug(0)->getDaug(1)->getP4Lab();
    
    p4pip=root_part->getDaug(1)->getDaug(0)->getP4Lab();
    p4pim=root_part->getDaug(1)->getDaug(1)->getP4Lab();
    
    
    costhetaB->Fill(p4b0.get(3)/p4b0.d3mag());
    phiB->Fill(atan2(p4b0.get(1),p4b0.get(2)));
    Elep->Fill(p4b0*p4e/p4b0.mass());
    q2->Fill((p4e+p4nu).mass2());
    dt->Fill(root_part->getDaug(1)->getLifetime()-root_part->getDaug(0)->getLifetime());
    costhetaV=EvtDecayAngle(p4b0,p4d+p4pi,p4d);
    ctv->Fill(costhetaV);
    if (costhetaV<0.0){
      chi_low_ctv->Fill(EvtDecayAngleChi(p4b0,p4d,p4pi,p4e,p4nu));
    }
    else{
      chi_high_ctv->Fill(EvtDecayAngleChi(p4b0,p4d,p4pi,p4e,p4nu));
    }
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  file->Write();
  file->Close();
  //  delete theApp;
  //  hfile.write();

  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}

//*******************************************************************************
void runDDK(int nevent, EvtGen &myGenerator) {
  //  TFile *file=new TFile("test1.root", "RECREATE");
  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  
  int count;
  
  char udecay_name[100];
  
  strcpy(udecay_name,"exampleFiles/GENERIC.DEC");
  
  myGenerator.readUDecay(udecay_name);
  
  count=1;

  static EvtId kp=EvtPDL::getId(std::string("K+"));
  static EvtId km=EvtPDL::getId(std::string("K-"));
  static EvtId ks=EvtPDL::getId(std::string("K_S0"));
  static EvtId kl=EvtPDL::getId(std::string("K_L0"));
  static EvtId k0=EvtPDL::getId(std::string("K0"));
  static EvtId kb=EvtPDL::getId(std::string("anti-K0"));

  static EvtId d0=EvtPDL::getId(std::string("D0"));
  static EvtId dp=EvtPDL::getId(std::string("D+"));
  static EvtId dm=EvtPDL::getId(std::string("D-"));
  static EvtId db=EvtPDL::getId(std::string("anti-D0"));

  static EvtIdSet theKs(kp,km,ks,kl,k0,kb);
  static EvtIdSet theDs(d0,dp,dm,db);

  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));
  static EvtId BP=EvtPDL::getId(std::string("B+"));
  static EvtId BM=EvtPDL::getId(std::string("B-"));
  
  static EvtIdSet theBs(B0B,B0,BP,BM);

  int nDDK=0;
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(UPS4,
							       p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);

    EvtParticle *theB01=root_part->getDaug(0);
    EvtParticle *theB02=root_part->getDaug(1);

    int nD=0;
    int nK=0;

    EvtParticle *p=theB01;
    do{
      EvtId type=p->getId();
      EvtId typePar=p->getParent()->getId();
      if (theDs.contains(type)) nD++;
      if (theKs.contains(type) && theBs.contains(typePar)) nK++;
      p=p->nextIter(theB01);
    }while(p!=0);
    if ( nD==2 && nK==1 ) nDDK++;

    nD=0;
    nK=0;

    p=theB02;
    do{
      EvtId type=p->getId();
      EvtId typePar=p->getParent()->getId();
      if (theDs.contains(type)) nD++;
      if (theKs.contains(type) && theBs.contains(typePar)) nK++;
      p=p->nextIter(theB02);
    }while(p!=0);
    if ( nD==2 && nK==1 ) nDDK++;
    
    root_part->deleteTree();  
    
  }while (count++<nevent);

  EvtGenReport(EVTGEN_INFO,"EvtGen") << nDDK << " " << (count-1) << " " << nDDK/float(2*(count-1)) << std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}

//*******************************************************************************

void runTest2(int nevent, EvtGen &myGenerator) {

  TFile *file=new TFile("test2.root", "RECREATE");
  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  TH1F* costhetaB  = new TH1F("h1","cos[Q]?B!",50,-1.0,1.0); 
  TH1F* phiB  = new TH1F("h2","[f]?B!",50,-EvtConst::pi,
					EvtConst::pi); 
  
  TH1F* dt  = new TH1F("h3","[D]t",100,-5.0,5.0); 
  TH1F* costhetaJpsi  = new TH1F("h4","cos[Q]?J/[y]!",
						50,-1.0,1.0);
  TH1F* costhetaKstar = new TH1F("h5","cos[Q]?K*!",
						50,-1.0,1.0); 
  TH1F* chi = new TH1F("h6","[h]",50,0.0,EvtConst::twoPi); 
  TH1F* chi1 = new TH1F("h26","[h] [D]t\"L#0",50,0.0,
				       EvtConst::twoPi); 
  TH1F* chi2 = new TH1F("h27","[h] [D]t\"G#0",50,0.0,
				       EvtConst::twoPi); 
  
  TH1F* costhetaomega  = new TH1F("h7","costhetaomega",50,-1.0,1.0);
  TH1F* costhetaomega1  = new TH1F("h20","costhetaomega1",50,-1.0,1.0);
  TH1F* costhetaomega2  = new TH1F("h21","costhetaomega2",50,-1.0,1.0);
  TH1F* costhetaomega3  = new TH1F("h22","costhetaomega3",50,-1.0,1.0);
  TH1F* omegaamp  = new TH1F("h8","omegaamp",50,0.0,0.05);
  TH1F* omegaamp1  = new TH1F("h9","omegaamp1",50,0.0,0.05);
  TH1F* omegaamp2  = new TH1F("h10","omegaamp2",50,0.0,0.05);
  TH1F* omegaamp3  = new TH1F("h11","omegaamp3",50,0.0,0.05);
  
  TH2F* chi1vscoskstarl = 
    new TH2F("h30","[h] vs. cos[Q]?J/[y]! [D]t\"L#0",
		    20,0.0,EvtConst::twoPi,20,-1.0,1.0); 
  
  TH2F* chi1vscoskstarg = 
    new TH2F("h31","[h] vs. cos[Q]?J/[y]! [D]t\"G#0",
		    20,0.0,EvtConst::twoPi,20,-1.0,1.0); 
  
  int count=1;
  
  EvtVector4R p4_b0,p4_b0b,p4_psi,p4_kstar,p4_mup,p4_mum;
  EvtVector4R p4_kz,p4_pi0,p4_pi1,p4_pi2,p4_pi3,p4_omega;
  EvtVector4R p4_pi1_omega,p4_pi2_omega,p4_pi3_omega;
  char udecay_name[100];

  strcpy(udecay_name,"exampleFiles/TEST2.DEC");
  //EvtGen myGenerator(decay_name,pdttable_name,myRandomEngine);
  myGenerator.readUDecay(udecay_name);
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(UPS4,
							       p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    p4_b0=root_part->getDaug(0)->getP4Lab();
    p4_b0b=root_part->getDaug(1)->getP4Lab();
    p4_psi=root_part->getDaug(1)->getDaug(0)->getP4Lab();
    p4_kstar=root_part->getDaug(1)->getDaug(1)->getP4Lab();
    p4_mup=root_part->getDaug(1)->getDaug(0)->getDaug(0)->getP4Lab();
    p4_mum=root_part->getDaug(1)->getDaug(0)->getDaug(1)->getP4Lab();
    p4_kz=root_part->getDaug(1)->getDaug(1)->getDaug(0)->getP4Lab();
    p4_pi0=root_part->getDaug(1)->getDaug(1)->getDaug(1)->getP4Lab();
    
    p4_omega=root_part->getDaug(0)->getDaug(0)->getP4Lab();
    p4_pi1=root_part->getDaug(0)->getDaug(0)->getDaug(0)->getP4Lab();
    p4_pi2=root_part->getDaug(0)->getDaug(0)->getDaug(1)->getP4Lab();
    p4_pi3=root_part->getDaug(0)->getDaug(0)->getDaug(2)->getP4Lab();
    
    //get momentum in the omega restframe
    p4_pi1_omega=root_part->getDaug(0)->getDaug(0)->getDaug(0)->getP4();
    p4_pi2_omega=root_part->getDaug(0)->getDaug(0)->getDaug(1)->getP4();
    p4_pi3_omega=root_part->getDaug(0)->getDaug(0)->getDaug(2)->getP4();
    
    EvtVector3R p3_perp=cross(EvtVector3R(p4_pi2_omega.get(0),
					  p4_pi2_omega.get(1),
					  p4_pi2_omega.get(2)),
			      EvtVector3R(p4_pi3_omega.get(0),
					  p4_pi3_omega.get(1),
					  p4_pi3_omega.get(2)));
    
    EvtVector4R p4_perp(p3_perp.d3mag(),p3_perp.get(0),
			p3_perp.get(1),p3_perp.get(2));
    
    root_part->getDaug(0)->getDaug(0)->getDaug(0)->setP4(p4_perp);
    
    p4_perp=root_part->getDaug(0)->getDaug(0)->getDaug(0)->getP4Lab();
    
    EvtVector4R p4_perpprime=p4_omega-p4_perp;
    
    double d_omegaamp=EvtVector3R(p4_pi1_omega.get(0),
				  p4_pi1_omega.get(1),
				  p4_pi1_omega.get(2))*p3_perp;
    
    d_omegaamp*=d_omegaamp;
    d_omegaamp*=20.0;
    
    double d_dt=root_part->getDaug(1)->getLifetime()-
      root_part->getDaug(0)->getLifetime();
    double d_costhetaJpsi=EvtDecayAngle(p4_b0b,p4_mup+p4_mum,p4_mup);
    double d_costhetaKstar=EvtDecayAngle(p4_b0b,p4_pi0+p4_kz,p4_pi0);
    double d_chi=EvtDecayAngleChi(p4_b0b,p4_pi0,p4_kz,p4_mup, p4_mum );
    costhetaB->Fill(p4_b0.get(3)/p4_b0.d3mag());
    phiB->Fill(atan2(p4_b0.get(1),p4_b0.get(2)));
    
    dt->Fill(d_dt);
    costhetaJpsi->Fill(d_costhetaJpsi);
    costhetaKstar->Fill(d_costhetaKstar);
    chi->Fill(d_chi);
    
    if (d_dt<0.0) {
      chi1->Fill(d_chi);
      chi1vscoskstarl->Fill(d_chi,d_costhetaJpsi,1.0);
    }
    
    if (d_dt>0.0) {
      chi2->Fill(d_chi);
      chi1vscoskstarg->Fill(d_chi,d_costhetaJpsi,1.0);
    }
    
    double d_costhetaomega=EvtDecayAngle(p4_b0b,p4_perp+p4_perpprime,p4_perp);
    
    
    costhetaomega->Fill(d_costhetaomega); 
    if (d_omegaamp<0.001) costhetaomega1->Fill(d_costhetaomega);
    if (d_omegaamp>0.02) costhetaomega2->Fill(d_costhetaomega);
    if (std::fabs(d_omegaamp-0.015)<0.005) costhetaomega3->Fill(d_costhetaomega);
    
    omegaamp->Fill(d_omegaamp);
    if (d_costhetaomega<-0.5) omegaamp1->Fill(d_omegaamp);
    if (d_costhetaomega>0.5) omegaamp2->Fill(d_omegaamp);
    if (std::fabs(d_costhetaomega)<0.5) omegaamp3->Fill(d_omegaamp);
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}



void runOmega(int nevent, EvtGen &myGenerator) {

  TFile *file=new TFile("omega.root", "RECREATE");
  static EvtId OMEGA=EvtPDL::getId(std::string("omega"));
  
  TH2F* dalitz = new TH2F("h1","E1 vs E2",50,0.0,0.5,50,0.0,0.5); 

  int count=1;

  char udecay_name[100];

  
  strcpy(udecay_name,"exampleFiles/OMEGA.DEC");
  myGenerator.readUDecay(udecay_name);
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMeanMass(OMEGA),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(OMEGA,
							       p_init);
    //root_part->setDiagonalSpinDensity();      
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    

    dalitz->Fill(root_part->getDaug(0)->getP4().get(0),
		       root_part->getDaug(1)->getP4().get(0),1.0);
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}




void runChi1Kstar(int nevent, EvtGen &myGenerator) {

  TFile *file=new TFile("chi1kstar.root", "RECREATE");
  static EvtId B0=EvtPDL::getId(std::string("B0"));
  
  TH1F* costhetaChi1  = new TH1F("h1","cos[Q]?J/[x]!",
						50,-1.0,1.0);
  TH1F* costhetaKstar = new TH1F("h2","cos[Q]?K*!",
						50,-1.0,1.0); 
  TH1F* chi = new TH1F("h3","[x]",50,-EvtConst::pi,EvtConst::pi); 

  
  int count=1;
  
  EvtVector4R p4_b,p4_chi,p4_kstar,p4_gamma,p4_psi,p4_k,p4_p;

  char udecay_name[100];

  strcpy(udecay_name,"exampleFiles/CHI1KSTAR.DEC");

  myGenerator.readUDecay(udecay_name);
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(B0),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(B0,
							       p_init);
    myGenerator.generateDecay(root_part);
    
    p4_b=root_part->getP4Lab();
    p4_chi=root_part->getDaug(0)->getP4Lab();
    p4_kstar=root_part->getDaug(1)->getP4Lab();
    p4_psi=root_part->getDaug(0)->getDaug(0)->getP4Lab();
    p4_gamma=root_part->getDaug(0)->getDaug(1)->getP4Lab();
    p4_k=root_part->getDaug(1)->getDaug(0)->getP4Lab();
    p4_p=root_part->getDaug(1)->getDaug(1)->getP4Lab();
    
    double d_costhetaChi1=EvtDecayAngle(p4_b,p4_chi,p4_psi);
    double d_costhetaKstar=EvtDecayAngle(p4_b,p4_kstar,p4_k);
    double d_chi=EvtDecayAngleChi(p4_b,p4_k,p4_p,p4_psi, p4_gamma );

    if (d_chi>EvtConst::pi) d_chi-=EvtConst::twoPi;



    costhetaChi1->Fill(d_costhetaChi1);
    costhetaKstar->Fill(d_costhetaKstar);
    chi->Fill(d_chi);
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}

void runAlias() {
  
  EvtId idpip=EvtPDL::getId(std::string("pi+"));
  EvtPDL::alias(idpip,std::string("my_pi+"));
  EvtId myidpip=EvtPDL::getId(std::string("my_pi+"));
  
  EvtId idpim=EvtPDL::getId(std::string("pi-"));
  EvtPDL::alias(idpim,std::string("my_pi-"));
  EvtId myidpim=EvtPDL::getId(std::string("my_pi-"));
  
  EvtId idpi0=EvtPDL::getId(std::string("pi0"));
  EvtPDL::alias(idpi0,std::string("my_pi0"));
  EvtId myidpi0=EvtPDL::getId(std::string("my_pi0"));
  
  
  
  EvtPDL::aliasChgConj(myidpip,myidpim);
  
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Id    pi+:" << idpip << std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Id    pi-:" << idpim << std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Id    pi0:" << idpi0 << std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Id my_pi+:" << myidpip << std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Id my_pi-:" << myidpim << std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Id my_pi0:" << myidpi0 << std::endl;
  
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Chg conj    pi+:" << 
    EvtPDL::chargeConj(idpip) << std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Chg conj    pi-:" << 
    EvtPDL::chargeConj(idpim) << std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Chg conj    pi0:" << 
    EvtPDL::chargeConj(idpi0) << std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Chg conj my_pi+:" << 
    EvtPDL::chargeConj(myidpip) << std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Chg conj my_pi-:" << 
    EvtPDL::chargeConj(myidpim) << std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Chg conj my_pi0:" << 
    EvtPDL::chargeConj(myidpi0) << std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
} 
  

void runRepeat(int nevent) {
  int i;
  
  for(i=0;i<nevent;i++){
    
    EvtDecayTable::getInstance()->readDecayFile(std::string("../DECAY_2010.DEC"));
    
  }
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}


void runPhotos(int nevent, EvtGen &myGenerator) {

  static EvtId PSI=EvtPDL::getId(std::string("J/psi"));

  TFile *file=new TFile("photos.root", "RECREATE");
  
  TH1F* mee  = new TH1F("h1","mee",60,3.0,3.12); 
  
  int count=1;

  EvtVector4R e1,e2;
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/PHOTOS.DEC");
  //EvtGen myGenerator(decay_name,pdttable_name,myRandomEngine);
  myGenerator.readUDecay(udecay_name);
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(PSI),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(PSI,
							       p_init);
    root_part->setDiagonalSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    e1=root_part->getDaug(0)->getP4Lab();
    e2=root_part->getDaug(1)->getP4Lab();
    
    mee->Fill( (e1+e2).mass() );
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}

void runFinalStates(int nevent, EvtGen &myGenerator) {

    //Parse the table of particles to find..


  
  EvtParser parser;
  parser.read(std::string("exampleFiles/finalstates.list"));
  
  std::vector< std::string > dList[20];
  int dListNum[20];
  std::vector< std::string > *dListItem = 0;
  std::string dListName[20];
  int ik,lk;
  std::string tk="";
  int tline=-1;
  std::string parent;
  for(ik=0;ik<parser.getNToken();ik++){
    lk=tline;
    tline=parser.getLineofToken(ik);
    tk=parser.getToken(ik);

    if ( lk!=tline&&tline>2) {
      if ( tline>1) {
	dList[tline-3]=*dListItem;
	dListItem=new std::vector<std::string>;
	dListNum[tline-3]=0;
	dListName[tline-2]=parser.getToken(ik);
      }
    }
    else{
      if ( tline==1 ) {
	//This is the parent particle name
	parent=parser.getToken(ik);
	dListItem=new std::vector<std::string>;
      }
      else{
	//This is one of the daughters
	if ( tline!=2 || (lk==tline) ) {
	  dListItem->push_back(parser.getToken(ik));
	}
	if ( tline==2 && (lk!=tline) ) {
	  dListName[tline-2]=parser.getToken(ik);
	}
      }
    }
  }
  dList[tline-2]=*dListItem;
  dListNum[tline-2]=0;


  static EvtId parId=EvtPDL::getId(parent);
  int count=0;
  do{
    
    if (count==1000*(count/1000)) {
      //if (count==1*(count/1)) {
      EvtGenReport(EVTGEN_INFO,"EvtGen") << "Event:"<< count << std::endl;
      //EvtGenReport(EVTGEN_INFO,"EvtGen") << HepRandom::getTheSeed()<<std::endl;
    }
    EvtVector4R p_init(EvtPDL::getMass(parId),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(parId,
							       p_init);
    if ( parent=="Upsilon(4S)") {
      root_part->setVectorSpinDensity();      
    }
    else{
      root_part->setDiagonalSpinDensity();      
    }  
  
  
    myGenerator.generateDecay(root_part);

    EvtParticle* p = root_part;
    
    std::vector<std::string> fs=findFinalState(p);

    int j;
    for(j=0; j<(tline-1); j++) {
      std::vector<std::string> temp=dList[j];
      if ( temp.size() == fs.size() ) {
	bool foundIt=true;
	unsigned int k, l;
	std::vector<bool> alreadyUsed(temp.size());
	for(k=0; k<temp.size(); k++) {
	  bool foundThisOne=false;
	  for(l=0; l<temp.size(); l++) {
	    if ( k==0 ) alreadyUsed[l]=false;
	    if ( foundThisOne||alreadyUsed[l] ) continue;
	    if ( temp[k]==fs[l] ) { 
	      alreadyUsed[l]=true; 
	      foundThisOne=true; 
	      //	      EvtGenReport(EVTGEN_INFO,"EvtGen") << "found daughter " << k << " " << l << std::endl;
	    }
	  }
	  if ( !foundThisOne ) foundIt=false;
	}
	if ( foundIt ) {//EvtGenReport(EVTGEN_INFO,"EvtGen") << "found a cand \n"; (histo1[j])->Fill(0.5);
	  //EvtGenReport(EVTGEN_INFO,"EvtGen") << "found one " << j << std::endl; 
	dListNum[j]++;}
      }
    }

    

    root_part->deleteTree();
    count++;
  }while(count<nevent);
  int j;
  for (j=0; j<(tline-1) ; j++) EvtGenReport(EVTGEN_INFO,"EvtGen") << dListName[j].c_str() << " " << j << " " << dListNum[j] << " " << count << " "<< (dListNum[j]/(1.0*count)) << std::endl;

  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}

std::vector<std::string> findFinalState( EvtParticle *tree){

  EvtParticle *p=tree;
  std::vector<std::string> fs;
  static EvtId ep=EvtPDL::getId(std::string("e+"));
  static EvtId em=EvtPDL::getId(std::string("e-"));
  static EvtId kp=EvtPDL::getId(std::string("K+"));
  static EvtId km=EvtPDL::getId(std::string("K-"));
  static EvtId mup=EvtPDL::getId(std::string("mu+"));
  static EvtId mum=EvtPDL::getId(std::string("mu-"));
  static EvtId pip=EvtPDL::getId(std::string("pi+"));
  static EvtId pim=EvtPDL::getId(std::string("pi-"));
  static EvtId pi0=EvtPDL::getId(std::string("pi0"));
  static EvtId pr=EvtPDL::getId(std::string("p+"));
  static EvtId apr=EvtPDL::getId(std::string("anti-p-"));
  static EvtId ne=EvtPDL::getId(std::string("n0"));
  static EvtId ane=EvtPDL::getId(std::string("anti-n0"));


  do{
    EvtId type=p->getId();

    if (type==ep) fs.push_back(std::string("e+"));
    if (type==em) fs.push_back(std::string("e-"));
    if (type==mup) fs.push_back(std::string("mu+"));
    if (type==mum) fs.push_back(std::string("mu-"));
    if (type==kp) fs.push_back(std::string("K+"));
    if (type==km) fs.push_back(std::string("K-"));
    if (type==pip) fs.push_back(std::string("pi+"));
    if (type==pim) fs.push_back(std::string("pi-"));
    if (type==pi0) fs.push_back(std::string("pi0"));
    if (type==pr) fs.push_back(std::string("p+"));
    if (type==apr) fs.push_back(std::string("anti-p-"));
    if (type==ne) fs.push_back(std::string("n0"));
    if (type==ane) fs.push_back(std::string("anti-n0"));

    p=p->nextIter();
    
  }while(p!=0);

  return fs;

}
void runTrackMult(int nevent, EvtGen &myGenerator) {

  TFile *file=new TFile("trackmult.root","RECREATE");

  TH1F* trackAll = new TH1F("trackAll","trackAll",12,1.0,25.0); 
  TH1F* trackNoSL = new TH1F("trackNoSL","trackNoSL",12,1.0,25.0); 
  TH1F* track1SL = new TH1F("track1SL","track1SL",12,1.0,25.0); 
  TH1F* track2SL = new TH1F("track2SL","track2SL",12,1.0,25.0); 

  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  
  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));
  static EvtId BP=EvtPDL::getId(std::string("B+"));
  static EvtId BM=EvtPDL::getId(std::string("B-"));

  //look for these tracks in generic events
  static EvtId ep=EvtPDL::getId(std::string("e+"));
  static EvtId em=EvtPDL::getId(std::string("e-"));
  static EvtId mup=EvtPDL::getId(std::string("mu+"));
  static EvtId mum=EvtPDL::getId(std::string("mu-"));
  static EvtId pip=EvtPDL::getId(std::string("pi+"));
  static EvtId pim=EvtPDL::getId(std::string("pi-"));
  static EvtId kp=EvtPDL::getId(std::string("K+"));
  static EvtId km=EvtPDL::getId(std::string("K-"));
  static EvtId pp=EvtPDL::getId(std::string("p+"));
  static EvtId pm=EvtPDL::getId(std::string("anti-p-"));

  static EvtIdSet theTracks(ep,em,mup,mum,pip,pim,kp,km,pp,pm);
  static EvtIdSet theLeptons(ep,em,mup,mum);
  static EvtIdSet theBs(B0,B0B,BP,BM);

  int count=1;
  
  EvtParticle *p;
  
  myGenerator.readUDecay("exampleFiles/GENERIC.DEC");

  int totTracks=0;
  int totTracksNoSL=0;
  int totTracks1SL=0;
  int totTracks2SL=0;
  int totNoSL=0;
  int tot1SL=0;
  int tot2SL=0;

  do{
    
    int evTracks=0;

    if (count==1000*(count/1000)) {
      EvtGenReport(EVTGEN_INFO,"EvtGen") << count << std::endl;
      //EvtGenReport(EVTGEN_INFO,"EvtGen") << HepRandom::getTheSeed()<<std::endl;
    }
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(UPS4,
							       p_init);
    root_part->setVectorSpinDensity();      
    
    
    myGenerator.generateDecay(root_part);
    
    p=root_part;

    int howManySL=0;

    do{
      if (theTracks.contains(p->getId())) { 
	totTracks+=1;
	evTracks+=1;
      }
      if ( theLeptons.contains(p->getId())) {
	if ( p->getParent() ) {
	  if ( theBs.contains(p->getParent()->getId())) howManySL+=1;
	}
      }
      p=p->nextIter(root_part);
    
    }while(p!=0);

    //Now need to figure out which histogram to book
    trackAll->Fill(evTracks);
    if ( howManySL==0 ) {trackNoSL->Fill(evTracks); totNoSL+=1; totTracksNoSL+=evTracks;}
    if ( howManySL==1 ) {track1SL->Fill(evTracks); tot1SL+=1; totTracks1SL+=evTracks;}
    if ( howManySL==2 ) {track2SL->Fill(evTracks); tot2SL+=1; totTracks2SL+=evTracks;}

    root_part->deleteTree();
  }while (count++<nevent);
      
  double aveMulti=float(totTracks)/float(nevent);
  double aveMultiNoSL=float(totTracksNoSL)/float(totNoSL);
  double aveMulti1SL=float(totTracks1SL)/float(tot1SL);
  double aveMulti2SL=float(totTracks2SL)/float(tot2SL);
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Your average multiplicity="<<aveMulti<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Your average multiplicity for no B->semileptonic events="<<aveMultiNoSL<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Your average multiplicity for 1 B->semileptonic events="<<aveMulti1SL<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Your average multiplicity for 2 B->semileptonic events="<<aveMulti2SL<<std::endl;
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}


void runGeneric(int neventOrig, EvtGen &myGenerator, 
		std::string listfile) {

  int nevent=abs(neventOrig);

    //Parse the table of particles to find..


    TFile *file=new TFile("generic.root","RECREATE");
    
    static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
    static EvtId VPHO=EvtPDL::getId(std::string("vpho"));

    static EvtId B0=EvtPDL::getId(std::string("B0"));
    static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));
    static EvtId BP=EvtPDL::getId(std::string("B+"));
    static EvtId BM=EvtPDL::getId(std::string("B-"));

    static EvtIdSet theBs(B0B,B0,BP,BM);
    static EvtIdSet theB0B(B0B);
    static EvtIdSet theB0(B0);
    static EvtIdSet theBP(BP);
    static EvtIdSet theBM(BM);

    static EvtId D0=EvtPDL::getId(std::string("D0"));
    static EvtId D0B=EvtPDL::getId(std::string("anti-D0"));
    static EvtId DP=EvtPDL::getId(std::string("D+"));
    static EvtId DM=EvtPDL::getId(std::string("D-"));
 
    static EvtIdSet theDs(D0B,D0,DP,DM);
    static EvtIdSet theD0B(D0B);
    static EvtIdSet theD0(D0);
    static EvtIdSet theDP(DP);
    static EvtIdSet theDM(DM);

    int count;

    int stable_list[1];
    stable_list[0]=-1;

    EvtParticle *p;

    char udecay_name[100];

    strcpy(udecay_name,"exampleFiles/GENERIC.DEC");
    myGenerator.readUDecay(udecay_name);

    if (listfile!="") {
      EvtParser parser;
      if (parser.read(listfile)!=0){
	EvtGenReport(EVTGEN_ERROR,"EvtGen") << "Will terminate."<<std::endl;
	exit(-1);
      }
      
      
      std::vector<TH1F*> histo1(parser.getNToken());
      std::vector<TH1F*> histo2(parser.getNToken());
      std::vector<TH1F*> massHisto(parser.getNToken());
	
      int ik;
      std::string tk,tkname;
      for(ik=0;ik<(parser.getNToken()/2);ik++){
	tk=parser.getToken(2*ik);
	tkname=parser.getToken(1+2*ik);
	histo1[ik] = new TH1F(tkname.c_str(),tkname.c_str(), 30,0.0,3.0);
	char *directName;
	directName=new char[(strlen(tkname.c_str())+8)];
	directName = strcpy(directName,tkname.c_str());
	directName = strcat(directName,"Direct");
	histo2[ik] = new TH1F(directName,directName,
			      30,0.0,3.0); 
	delete directName;

	char *massName;
	massName=new char[(strlen(tkname.c_str())+4)];
	massName = strcpy(massName,tkname.c_str());
	massName = strcat(massName,"Mass");
	massHisto[ik] = new TH1F(massName,massName, 3000,0.0,5.0);
	delete massName;
      }
    
      count=1;
      std::vector<int> temp(parser.getNToken()/2,0);
      std::vector<int> tempB(parser.getNToken()/2,0);
      std::vector<int> tempB0B(parser.getNToken()/2,0);
      std::vector<int> tempB0(parser.getNToken()/2,0);
      std::vector<int> tempBP(parser.getNToken()/2,0);
      std::vector<int> tempBM(parser.getNToken()/2,0);
      std::vector<int> tempD(parser.getNToken()/2,0);
      std::vector<int> tempD0B(parser.getNToken()/2,0);
      std::vector<int> tempD0(parser.getNToken()/2,0);
      std::vector<int> tempDP(parser.getNToken()/2,0);
      std::vector<int> tempDM(parser.getNToken()/2,0);
      
      do{
	//EvtGenReport(EVTGEN_INFO,"EvtGen") << "new event\n";
	if (count==1000*(count/1000)) {
	  EvtGenReport(EVTGEN_INFO,"EvtGen") << count << std::endl;
	  //EvtGenReport(EVTGEN_INFO,"EvtGen") << HepRandom::getTheSeed()<<std::endl;
	}
	EvtVector4R p_init(sqrt(EvtPDL::getMass(UPS4)*EvtPDL::getMass(UPS4)+5.9*5.9),0.0,0.0,5.9);
	
	EvtParticle* root_part=0;
	if ( neventOrig > 0 ) {
	  root_part=EvtParticleFactory::particleFactory(UPS4,p_init);
	}
	else{
	  root_part=EvtParticleFactory::particleFactory(VPHO, p_init);
	}

	root_part->setVectorSpinDensity();      
	
	myGenerator.generateDecay(root_part);
	
	p=root_part;

	//EvtStdHep stdhep;
	//stdhep.init();
	//root_part->makeStdHep(stdhep);
	//EvtGenReport(EVTGEN_INFO,"EvtGen") <<stdhep<<std::endl;
	//EvtGenReport(EVTGEN_INFO,"EvtGen") <<secondary<<std::endl;
	std::string token;
	int itok;
	for(itok=0;itok<(parser.getNToken()/2);itok++){
	  
	  token=parser.getToken(2*itok);
	  //temp[itok]+=countInclusive(token,root_part);
	  temp[itok]+=countInclusive(token,root_part,histo1[itok],massHisto[itok]);
	  tempB[itok]+=countInclusiveParent(token,root_part,theBs,histo2[itok]);
	  tempB0[itok]+=countInclusiveSubTree(token,root_part,theB0);
	  tempB0B[itok]+=countInclusiveSubTree(token,root_part,theB0B);
	  tempBP[itok]+=countInclusiveSubTree(token,root_part,theBP);
	  tempBM[itok]+=countInclusiveSubTree(token,root_part,theBM);
	  //	  tempD[itok]+=countInclusiveParent(token,root_part,theDs);
	  //	  tempD0[itok]+=countInclusiveSubTree(token,root_part,theD0);
	  //	  tempD0B[itok]+=countInclusiveSubTree(token,root_part,theD0B);
	  //	  tempDP[itok]+=countInclusiveSubTree(token,root_part,theDP);
	  //	  tempDM[itok]+=countInclusiveSubTree(token,root_part,theDM);
	  
	  
	}
	
	//	numd0+=countInclusive("D0",root_part);
	//	numd0b+=countInclusive("anti-D0",root_part);
	//	numdp+=countInclusive("D+",root_part);
	//	numdm+=countInclusive("D-",root_part);
	
	//	root_part->printTree();
	root_part->deleteTree();  
      }while (count++<nevent);
      
      int itok;
      std::string token;
      for(itok=0;itok<(parser.getNToken()/2);itok++){
	
	token=parser.getToken(2*itok);
	float br=0.5*float(temp[itok])/float(nevent);
	EvtGenReport(EVTGEN_INFO,"EvtGen") << "Found "<<temp[itok]<<" "<<token.c_str() 
	     << " in " << nevent << " events. Average number of "
	     << token.c_str()<<" per B meson="<<br<<std::endl;
	
	br=0.5*float(tempB[itok])/float(nevent);
	EvtGenReport(EVTGEN_INFO,"EvtGen") << "Found "<<tempB[itok]<<" "<<token.c_str() 
	     << " produced directly in decays of B mesons avg. br.fr.="
	     <<br<<std::endl;
	br=2.0*float(tempB0[itok])/float(nevent);
	EvtGenReport(EVTGEN_INFO,"EvtGen") << "Found "<<tempB0[itok]<<" "<<token.c_str()
	     << " in decay tree of B0, br.fr.="<<br<<std::endl;
	br=2.0*float(tempB0B[itok])/float(nevent);
	EvtGenReport(EVTGEN_INFO,"EvtGen") << "Found "<<tempB0B[itok]<<" "<<token.c_str() 
	     << " in decay tree of anti-B0, br.fr.="<<br<<std::endl;
	br=2.0*float(tempBP[itok])/float(nevent);
	EvtGenReport(EVTGEN_INFO,"EvtGen") << "Found "<<tempBP[itok]<<" "<<token.c_str() 
	     << " in decay tree of B+, br.fr.="<<br<<std::endl;
	br=2.0*float(tempBM[itok])/float(nevent);
	EvtGenReport(EVTGEN_INFO,"EvtGen") << "Found "<<tempBM[itok]<<" "<<token.c_str() 
	     << " in decay tree of B-,  br.fr.="<<br<<std::endl;
	
	//	br=0.5*float(tempD[itok])/float(numd0+numd0b+numdm+numdp);
	//	EvtGenReport(EVTGEN_INFO,"EvtGen") << "Found "<<tempD[itok]<<" "<<token 
	//	     << " produced directly in decays of D mesons avg. br.fr.="
	//	     <<br<<std::endl;
	//	br=2.0*float(tempD0[itok])/float(numd0);
	//	EvtGenReport(EVTGEN_INFO,"EvtGen") << "Found "<<tempD0[itok]<<" "<<token 
	//	     << " in decay of D0,  br.fr.="<<br<<std::endl;
	//	br=2.0*float(tempD0B[itok])/float(numd0b);
	//	EvtGenReport(EVTGEN_INFO,"EvtGen") << "Found "<<tempD0B[itok]<<" "<<token 
	//	     << " in decay of anti-D0, br.fr.="<<br<<std::endl;
	//	br=2.0*float(tempDP[itok])/float(numdp);
	//	EvtGenReport(EVTGEN_INFO,"EvtGen") << "Found "<<tempDP[itok]<<" "<<token 
	//	     << " in decay of D+,  br.fr.="<<br<<std::endl;
	//	br=2.0*float(tempDM[itok])/float(numdm);
	//	EvtGenReport(EVTGEN_INFO,"EvtGen") << "Found "<<tempDM[itok]<<" "<<token 
	//	     << " in decay of D-,  br.fr.="<<br<<std::endl;
	EvtGenReport(EVTGEN_INFO,"EvtGen") << "*******************************************\n";
      }
    }
    else{
      count=1;
      do{
	
	if (count==1000*(count/1000)) {
	  EvtGenReport(EVTGEN_INFO,"EvtGen") << count << std::endl;
	  //EvtGenReport(EVTGEN_INFO,"EvtGen") << HepRandom::getTheSeed()<<std::endl;
	}
	EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
	

	EvtParticle *root_part=0;
	if ( neventOrig > 0 ) {
	  root_part=EvtParticleFactory::particleFactory(UPS4,p_init);
	}
	else{
	  root_part=EvtParticleFactory::particleFactory(VPHO, p_init);
	}

	root_part->setVectorSpinDensity();      
	
	
	myGenerator.generateDecay(root_part);
	
	p=root_part;
	
	root_part->deleteTree();  
	
      }while (count++<nevent);


    }
    file->Write(); file->Close();

  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}

void runKstarnunu(int nevent, EvtGen &myGenerator) {

  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));

  TFile *file=new TFile("kstarnunu.root", "RECREATE");
  
  TH1F* q2  = new TH1F("h1","q2",
				      50,0.0,25.0); 
  TH1F* enu = new TH1F("h2","Neutrino energy",
				      50,0.0,5.0);  
  TH1F* x = new TH1F("h3","Total neutrino energy/B mass",
				    50,0.5,0.9); 
  
  
  int count;
  
  EvtVector4R kstar,nu,nub;
  char udecay_name[100];

  strcpy(udecay_name,"exampleFiles/KSTARNUNU.DEC");
  //EvtGen myGenerator(decay_name,pdttable_name,myRandomEngine);
  myGenerator.readUDecay(udecay_name);
  
  count=1;
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(B0),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(B0,
							       p_init);
    root_part->setDiagonalSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    kstar=root_part->getDaug(0)->getP4Lab();
    nu=root_part->getDaug(1)->getP4Lab();
    nub=root_part->getDaug(2)->getP4Lab();
    
    q2->Fill( (nu+nub).mass2() );
    enu->Fill( nu.get(0) );
    x->Fill( (nu.get(0)+nub.get(0))/root_part->mass() );
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}

void runBsmix(int nevent, EvtGen &myGenerator) {

  static EvtId BS0=EvtPDL::getId(std::string("B_s0"));
  static EvtId BSB=EvtPDL::getId(std::string("anti-B_s0"));

  TFile *file=new TFile("bsmix.root", "RECREATE");
  
  TH1F* tmix  = new TH1F("h1","tmix (mm)",100,0.0,5.0); 
  TH1F* tnomix = new TH1F("h2","tnomix (mm)",100,0.0,5.0);  
  
  
  int count;
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/BSMIX.DEC");
  //EvtGen myGenerator(decay_name,pdttable_name,myRandomEngine);
  myGenerator.readUDecay(udecay_name);
  
  count=1;
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(BS0),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(BS0,
							       p_init);
    root_part->setDiagonalSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    double t=root_part->getLifetime();
    int mix=0;
    
    if (root_part->getNDaug()==1){
      if (root_part->getDaug(0)->getId()==BSB){
	mix=1;
      }
    }
    
    if (mix==0) tnomix->Fill(t);
    if (mix==1) tmix->Fill(t);
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}

void runSemic(int nevent, EvtGen &myGenerator) {

  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId VPHO=EvtPDL::getId(std::string("vpho"));
  static EvtId GAMM=EvtPDL::getId(std::string("gamma"));
  static EvtId PSI=EvtPDL::getId(std::string("J/psi"));
  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));
  static EvtId BS0=EvtPDL::getId(std::string("B_s0"));
  static EvtId BSB=EvtPDL::getId(std::string("anti-B_s0"));

  static EvtId PI0=EvtPDL::getId(std::string("pi0"));
  static EvtId PIP=EvtPDL::getId(std::string("pi+"));
  static EvtId PIM=EvtPDL::getId(std::string("pi-"));

  static EvtId EP=EvtPDL::getId(std::string("e+"));
  static EvtId KP=EvtPDL::getId(std::string("K+"));
  static EvtId MUP=EvtPDL::getId(std::string("mu+"));
  static EvtId EM=EvtPDL::getId(std::string("e-"));
  static EvtId KM=EvtPDL::getId(std::string("K-"));
  static EvtId MUM=EvtPDL::getId(std::string("mu-"));

  static EvtId DST0=EvtPDL::getId(std::string("D*0"));
  static EvtId DSTB=EvtPDL::getId(std::string("anti-D*0"));
  static EvtId DSTP=EvtPDL::getId(std::string("D*+"));
  static EvtId DSTM=EvtPDL::getId(std::string("D*-"));
  static EvtId D0=EvtPDL::getId(std::string("D0"));
  static EvtId D0B=EvtPDL::getId(std::string("anti-D0"));
  static EvtId DP=EvtPDL::getId(std::string("D+"));
  static EvtId DM=EvtPDL::getId(std::string("D-"));

  static EvtId K0L=EvtPDL::getId(std::string("K_L0"));

  static EvtId D1P1P=EvtPDL::getId(std::string("D_1+"));
  static EvtId D1P1N=EvtPDL::getId(std::string("D_1-"));
  static EvtId D1P10=EvtPDL::getId(std::string("D_10"));
  static EvtId D1P1B=EvtPDL::getId(std::string("anti-D_10"));

  static EvtId D3P2P=EvtPDL::getId(std::string("D_2*+"));
  static EvtId D3P2N=EvtPDL::getId(std::string("D_2*-"));
  static EvtId D3P20=EvtPDL::getId(std::string("D_2*0"));
  static EvtId D3P2B=EvtPDL::getId(std::string("anti-D_2*0"));

  static EvtId D3P1P=EvtPDL::getId(std::string("D'_1+"));
  static EvtId D3P1N=EvtPDL::getId(std::string("D'_1-"));
  static EvtId D3P10=EvtPDL::getId(std::string("D'_10"));
  static EvtId D3P1B=EvtPDL::getId(std::string("anti-D'_10"));

  static EvtId D3P0P=EvtPDL::getId(std::string("D_0*+"));
  static EvtId D3P0N=EvtPDL::getId(std::string("D_0*-"));
  static EvtId D3P00=EvtPDL::getId(std::string("D_0*0"));
  static EvtId D3P0B=EvtPDL::getId(std::string("anti-D_0*0"));

  static EvtId D21S0P=EvtPDL::getId(std::string("D(2S)+"));
  static EvtId D21S0N=EvtPDL::getId(std::string("D(2S)-"));
  static EvtId D21S00=EvtPDL::getId(std::string("D(2S)0"));
  static EvtId D21S0B=EvtPDL::getId(std::string("anti-D(2S)0"));

  static EvtId D23S1P=EvtPDL::getId(std::string("D*(2S)+"));
  static EvtId D23S1N=EvtPDL::getId(std::string("D*(2S)-"));
  static EvtId D23S10=EvtPDL::getId(std::string("D*(2S)0"));
  static EvtId D23S1B=EvtPDL::getId(std::string("anti-D*(2S)0"));

  static EvtIdSet radExitDstar(D23S1P,D23S1N,D23S10,D23S1B);

  
  TFile *file=new TFile("semic.root", "RECREATE");
  
  TH1F* Dpe_q2 = new TH1F("h11","q2 for B0B ->D+ e- nu",50,0.0,12.0); 
  TH1F* Dpe_elep = new TH1F("h12","Elep for B0B ->D+ e- nu",50,0.0,2.5); 
  TH1F* Dme_q2 = new TH1F("h13","q2 for B0 ->D- e+ nu",50,0.0,12.0); 
  TH1F* Dme_elep = new TH1F("h14","Elep for B0 ->D- e+ nu",50,0.0,2.5); 
  TH1F* D0e_q2 = new TH1F("h15","q2 for B- ->D0 e- nu",50,0.0,12.0); 
  TH1F* D0e_elep = new TH1F("h16","Elep for B- ->D0 e- nu",50,0.0,2.5); 
  TH1F* D0Be_q2 = new TH1F("h17","q2 for B+ ->D0B e+ nu",50,0.0,12.0); 
  TH1F* D0Be_elep = new TH1F("h18","Elep for B+ ->D0B e+ nu",50,0.0,2.5); 
  
  TH1F* Dstpe_q2 = new TH1F("h21","q2 for B0B ->D*+ e- nu",50,0.0,12.0); 
  TH1F* Dstpe_elep = new TH1F("h22","Elep for B0B ->D*+ e- nu",50,0.0,2.5); 
  TH1F* Dstme_q2 = new TH1F("h23","q2 for B0 ->D*- e+ nu",50,0.0,12.0); 
  TH1F* Dstme_elep = new TH1F("h24","Elep for B0 ->D*- e+ nu",50,0.0,2.5); 
  TH1F* Dst0e_q2 = new TH1F("h25","q2 for B- ->D*0 e- nu",50,0.0,12.0); 
  TH1F* Dst0e_elep = new TH1F("h26","Elep for B*- ->D*0 e- nu",50,0.0,2.5); 
  TH1F* Dst0Be_q2 = new TH1F("h27","q2 for B+ ->D*0B e+ nu",50,0.0,12.0); 
  TH1F* Dst0Be_elep = new TH1F("h28","Elep for B+ ->D*0B e+ nu",50,0.0,2.5); 
  
  TH1F* D1P1pe_q2 = new TH1F("h31","q2 for B0B ->1P1+ e- nu",50,0.0,12.0); 
  TH1F* D1P1pe_elep = new TH1F("h32","Elep for B0B ->1P1+ e- nu",50,0.0,2.5); 
  TH1F* D1P1me_q2 = new TH1F("h33","q2 for B0 ->1P1- e+ nu",50,0.0,12.0); 
  TH1F* D1P1me_elep = new TH1F("h34","Elep for B0 ->1P1- e+ nu",50,0.0,2.5); 
  TH1F* D1P10e_q2 = new TH1F("h35","q2 for B- ->1P10 e- nu",50,0.0,12.0); 
  TH1F* D1P10e_elep = new TH1F("h36","Elep for B*- ->1P10 e- nu",50,0.0,2.5); 
  TH1F* D1P10Be_q2 = new TH1F("h37","q2 for B+ ->1P1B e+ nu",50,0.0,12.0); 
  TH1F* D1P10Be_elep = new TH1F("h38","Elep for B+ ->1P1B e+ nu",50,0.0,2.5); 
  
  TH1F* D3P0pe_q2 = new TH1F("h41","q2 for B0B ->3P0+ e- nu",50,0.0,12.0); 
  TH1F* D3P0pe_elep = new TH1F("h42","Elep for B0B ->3P0+ e- nu",50,0.0,2.5); 
  TH1F* D3P0me_q2 = new TH1F("h43","q2 for B0 ->3P0- e+ nu",50,0.0,12.0); 
  TH1F* D3P0me_elep = new TH1F("h44","Elep for B0 ->3P0- e+ nu",50,0.0,2.5); 
  TH1F* D3P00e_q2 = new TH1F("h45","q2 for B- ->3P00 e- nu",50,0.0,12.0); 
  TH1F* D3P00e_elep = new TH1F("h46","Elep for B*- ->3P00 e- nu",50,0.0,2.5); 
  TH1F* D3P00Be_q2 = new TH1F("h47","q2 for B+ ->3P0B e+ nu",50,0.0,12.0); 
  TH1F* D3P00Be_elep = new TH1F("h48","Elep for B+ ->3P0B e+ nu",50,0.0,2.5); 
  
  TH1F* D3P1pe_q2 = new TH1F("h51","q2 for B0B ->3P1+ e- nu",50,0.0,12.0); 
  TH1F* D3P1pe_elep = new TH1F("h52","Elep for B0B ->3P1+ e- nu",50,0.0,2.5); 
  TH1F* D3P1me_q2 = new TH1F("h53","q2 for B0 ->3P1- e+ nu",50,0.0,12.0); 
  TH1F* D3P1me_elep = new TH1F("h54","Elep for B0 ->3P1- e+ nu",50,0.0,2.5); 
  TH1F* D3P10e_q2 = new TH1F("h55","q2 for B- ->3P10 e- nu",50,0.0,12.0); 
  TH1F* D3P10e_elep = new TH1F("h56","Elep for B*- ->3P10 e- nu",50,0.0,2.5); 
  TH1F* D3P10Be_q2 = new TH1F("h57","q2 for B+ ->3P1B e+ nu",50,0.0,12.0); 
  TH1F* D3P10Be_elep = new TH1F("h58","Elep for B+ ->3P1B e+ nu",50,0.0,2.5); 
  
  TH1F* D3P2pe_q2 = new TH1F("h61","q2 for B0B ->3P2+ e- nu",50,0.0,12.0); 
  TH1F* D3P2pe_elep = new TH1F("h62","Elep for B0B ->3P2+ e- nu",50,0.0,2.5); 
  TH1F* D3P2me_q2 = new TH1F("h63","q2 for B0 ->3P2- e+ nu",50,0.0,12.0); 
  TH1F* D3P2me_elep = new TH1F("h64","Elep for B0 ->3P2- e+ nu",50,0.0,2.5); 
  TH1F* D3P20e_q2 = new TH1F("h65","q2 for B- ->3P20 e- nu",50,0.0,12.0); 
  TH1F* D3P20e_elep = new TH1F("h66","Elep for B*- ->3P20 e- nu",50,0.0,2.5); 
  TH1F* D3P20Be_q2 = new TH1F("h67","q2 for B+ ->3P2B e+ nu",50,0.0,12.0); 
  TH1F* D3P20Be_elep = new TH1F("h68","Elep for B+ ->3P2B e+ nu",50,0.0,2.5); 


  TH1F* phiL = new TH1F("h69","phi",50,-3.1416,3.1416); 
  
  
  int count;
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/SEMIC.DEC");
  //EvtGen myGenerator(decay_name,pdttable_name,myRandomEngine);
  myGenerator.readUDecay(udecay_name);
  
  count=1;
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(UPS4,
							       p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    int i;
    
    for (i=0;i<2;i++){
      
      EvtId meson=root_part->getDaug(i)->getDaug(0)->getId();
      EvtId lepton=root_part->getDaug(i)->getDaug(1)->getId();
      EvtVector4R lep=root_part->getDaug(i)->getDaug(1)->getP4Lab();
      phiL->Fill(atan2(lep.get(1),lep.get(2)));
      EvtVector4R nu=root_part->getDaug(i)->getDaug(2)->getP4Lab();
      double q2=(lep+nu).mass2();
      double elep=root_part->getDaug(i)->getDaug(1)->getP4().get(0);
      if (meson==DP&&lepton==EM){
	Dpe_q2->Fill(q2);
	Dpe_elep->Fill(elep);
      }
      if (meson==DM&&lepton==EP){
	Dme_q2->Fill(q2);
	Dme_elep->Fill(elep);
      }
      if (meson==D0&&lepton==EM){
	D0e_q2->Fill(q2);
	D0e_elep->Fill(elep);
      }
      if (meson==D0B&&lepton==EP){
	D0Be_q2->Fill(q2);
	D0Be_elep->Fill(elep);
      }
      
      
      if (meson==DSTP&&lepton==EM){
	Dstpe_q2->Fill(q2);
	Dstpe_elep->Fill(elep);
      }
      if (meson==DSTM&&lepton==EP){
	Dstme_q2->Fill(q2);
	Dstme_elep->Fill(elep);
      }
      if (meson==DST0&&lepton==EM){
	Dst0e_q2->Fill(q2);
	Dst0e_elep->Fill(elep);
      }
      if (meson==DSTB&&lepton==EP){
	Dst0Be_q2->Fill(q2);
	Dst0Be_elep->Fill(elep);
      }
      
      
      if (meson==D1P1P&&lepton==EM){
	D1P1pe_q2->Fill(q2);
	D1P1pe_elep->Fill(elep);
      }
      if (meson==D1P1N&&lepton==EP){
	D1P1me_q2->Fill(q2);
	D1P1me_elep->Fill(elep);
      }
      if (meson==D1P10&&lepton==EM){
	D1P10e_q2->Fill(q2);
	D1P10e_elep->Fill(elep);
      }
      if (meson==D1P1B&&lepton==EP){
	D1P10Be_q2->Fill(q2);
	D1P10Be_elep->Fill(elep);
      }
      
      if (meson==D3P0P&&lepton==EM){
	D3P0pe_q2->Fill(q2);
	D3P0pe_elep->Fill(elep);
      }
      if (meson==D3P0N&&lepton==EP){
	D3P0me_q2->Fill(q2);
	D3P0me_elep->Fill(elep);
      }
      if (meson==D3P00&&lepton==EM){
	D3P00e_q2->Fill(q2);
	D3P00e_elep->Fill(elep);
      }
      if (meson==D3P0B&&lepton==EP){
	D3P00Be_q2->Fill(q2);
	D3P00Be_elep->Fill(elep);
      }
      
      if (meson==D3P1P&&lepton==EM){
	D3P1pe_q2->Fill(q2);
	D3P1pe_elep->Fill(elep);
      }
      if (meson==D3P1N&&lepton==EP){
	D3P1me_q2->Fill(q2);
	D3P1me_elep->Fill(elep);
      }
      if (meson==D3P10&&lepton==EM){
	D3P10e_q2->Fill(q2);
	D3P10e_elep->Fill(elep);
      }
      if (meson==D3P1B&&lepton==EP){
	D3P10Be_q2->Fill(q2);
	D3P10Be_elep->Fill(elep);
      }
      
      if (meson==D3P2P&&lepton==EM){
	D3P2pe_q2->Fill(q2);
	D3P2pe_elep->Fill(elep);
      }
      if (meson==D3P2N&&lepton==EP){
	D3P2me_q2->Fill(q2);
	D3P2me_elep->Fill(elep);
      }
      if (meson==D3P20&&lepton==EM){
	D3P20e_q2->Fill(q2);
	D3P20e_elep->Fill(elep);
      }
      if (meson==D3P2B&&lepton==EP){
	D3P20Be_q2->Fill(q2);
	D3P20Be_elep->Fill(elep);
      }
      
      
    }    
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}


void runKstarll(int nevent, EvtGen &myGenerator) {
  TFile *file=new TFile("kstkmm.root", "RECREATE");
  
  TH2F* _dalitz  = new TH2F("h1","q^2! vs Elep",
					  70,0.0,3.5,60,0.0,30.0); 
  
  
  TH1F* _ctl  = new TH1F("h2","ctl",50,-1.0,1.0); 
  
  
  TH1F* _q2  = new TH1F("h3","q2",50,0.0,25.0); 
  
  TH1F* _q2low = new TH1F("h4","q2 (low)",
					50,0.0,1.0); 
  TH1F* _q2lowlow = new TH1F("h5","q2 (lowlow)",
					   50,0.0,0.00001); 

  TH1F* _phi  = new TH1F("h6","phi",50,-EvtConst::pi,EvtConst::pi); 

  TH1F* _chi  = new TH1F("h7","chi",50,0.0,EvtConst::twoPi); 

  TH1F* _chictl  = new TH1F("h8","chictl",50,0.0,EvtConst::twoPi); 

  
  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId B0=EvtPDL::getId(std::string("B0"));
  
  int count=1;
  
  EvtVector4R kstar,l1,l2;
  EvtVector4R k,pi,b;
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/KSTARLL.DEC");
  //EvtGen myGenerator(decay_name,pdttable_name,myRandomEngine);
  
  myGenerator.readUDecay(udecay_name);
  
  std::vector<double> q2low(5);
  std::vector<double> q2high(5);
  std::vector<int> counts(5);

  //kee  
  //int n=4;
  //q2low[0]=0.0;     q2high[0]=4.5;
  //q2low[1]=4.5;     q2high[1]=8.41;
  //q2low[2]=10.24;   q2high[2]=12.96;
  //q2low[3]=14.06;   q2high[3]=30.0;
    
  //kmm  
  //int n=4;
  //q2low[0]=0.0;     q2high[0]=4.5;
  //q2low[1]=4.5;     q2high[1]=9.0;
  //q2low[2]=10.24;   q2high[2]=12.96;
  //q2low[3]=14.06;   q2high[3]=30.0;

  //K*ee  
  int n=5;
  q2low[0]=0.0;     q2high[0]=0.1;
  q2low[1]=0.1;     q2high[1]=4.5;
  q2low[2]=4.5;     q2high[2]=8.41;
  q2low[3]=10.24;   q2high[3]=12.96;
  q2low[4]=14.06;   q2high[4]=30.0;
    
  //K*mm  
  //int n=5;
  //q2low[0]=0.0;     q2high[0]=0.1;
  //q2low[1]=0.1;     q2high[1]=4.5;
  //q2low[2]=4.5;     q2high[2]=9.0;
  //q2low[3]=10.24;   q2high[3]=12.96;
  //q2low[4]=14.06;   q2high[4]=30.0;
    
  do{
    
    
    EvtVector4R p_init(EvtPDL::getMass(B0),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(B0,
							       p_init);
    root_part->setDiagonalSpinDensity();      
    
    myGenerator.generateDecay(root_part);

    //    root_part->printTree();
    //root_part->getDaug(0)->printTree();
    //root_part->getDaug(1)->printTree();
    //root_part->getDaug(2)->printTree();

    kstar=root_part->getDaug(0)->getP4Lab();
    l1=root_part->getDaug(1)->getP4Lab();
    l2=root_part->getDaug(2)->getP4Lab();
    
    b=root_part->getP4();
    k=root_part->getDaug(0)->getDaug(0)->getP4Lab();
    pi=root_part->getDaug(0)->getDaug(1)->getP4Lab();

    double qsq=(l1+l2).mass2();

    for (int j=0;j<n;j++){
      if (qsq>q2low[j]&&qsq<q2high[j]) counts[j]++;
    }
   
    _q2->Fill( (l1+l2).mass2() );
    _q2low->Fill( (l1+l2).mass2() );
    _q2lowlow->Fill( (l1+l2).mass2() );
    
    _ctl->Fill(EvtDecayAngle((l1+l2+kstar),(l1+l2),l1));
    
    _dalitz->Fill(l1.get(0),(l1+l2).mass2(),1.0);
    
    root_part->deleteTree();  

    _phi->Fill(atan2(l1.get(1),l1.get(2)));

    _chi->Fill(EvtDecayAngleChi(b,k,pi,l1,l2));

    if (EvtDecayAngle((l1+l2+kstar),(l1+l2),l1)>0){
      _chictl->Fill(EvtDecayAngleChi(b,k,pi,l1,l2));
     }
    EvtGenReport(EVTGEN_INFO,"EvtGen") << "count:"<<count<<" "<<(l1+l2).mass2()<<std::endl;
    
  }while (count++<nevent);
  
  for (int j=0;j<n;j++){
    std::cout << "["<<q2low[j]<<".."<<q2high[j]<<"]="<<counts[j]<<std::endl;
  }

  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}

void runKll(int nevent, EvtGen &myGenerator) {
  TFile *file=new TFile("ksem.root", "RECREATE");
  
  TH2F* _dalitz  = new TH2F("h1","q^2! vs Elep",
					  70,0.0,3.5,60,0.0,30.0); 
  
  
  TH1F* _ctl  = new TH1F("h2","ctl",50,-1.0,1.0); 
  
  
  TH1F* _q2  = new TH1F("h3","q2",50,0.0,25.0); 
  
  TH1F* _q2low = new TH1F("h4","q2 (low)",
					50,0.0,1.0); 
  TH1F* _q2lowlow = new TH1F("h5","q2 (lowlow)",
					   50,0.0,0.00001); 

  TH1F* _phi  = new TH1F("h6","phi",50,-EvtConst::pi,EvtConst::pi); 

  //  TH1F* _chi  = new TH1F("h7","chi",50,0.0,EvtConst::twoPi); 

  //  TH1F* _chictl  = new TH1F("h8","chictl",50,0.0,EvtConst::twoPi); 

  
  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId B0=EvtPDL::getId(std::string("B0"));
  
  int count=1;
  
  EvtVector4R k,l1,l2;
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/KLL.DEC");
  //EvtGen myGenerator(decay_name,pdttable_name,myRandomEngine);
  
  myGenerator.readUDecay(udecay_name);
  
  std::vector<double> q2low(5);
  std::vector<double> q2high(5);
  std::vector<int> counts(5);

  //kee  
  //  int n=4;
  //q2low[0]=0.0;     q2high[0]=4.5;
  //q2low[1]=4.5;     q2high[1]=8.41;
  //q2low[2]=10.24;   q2high[2]=12.96;
  //q2low[3]=14.06;   q2high[3]=30.0;
    
  //kmm  
  int n=4;
  q2low[0]=0.0;     q2high[0]=4.5;
  q2low[1]=4.5;     q2high[1]=9.0;
  q2low[2]=10.24;   q2high[2]=12.96;
  q2low[3]=14.06;   q2high[3]=30.0;

  //K*ee  
  //int n=5;
  //q2low[0]=0.0;     q2high[0]=0.1;
  //q2low[1]=0.1;     q2high[1]=4.5;
  //q2low[2]=4.5;     q2high[2]=8.41;
  //q2low[3]=10.24;   q2high[3]=12.96;
  //q2low[4]=14.06;   q2high[4]=30.0;
    
  //K*mm  
  //int n=5;
  //q2low[0]=0.0;     q2high[0]=0.1;
  //q2low[1]=0.1;     q2high[1]=4.5;
  //q2low[2]=4.5;     q2high[2]=9.0;
  //q2low[3]=10.24;   q2high[3]=12.96;
  //q2low[4]=14.06;   q2high[4]=30.0;
    
  do{
    
    
    EvtVector4R p_init(EvtPDL::getMass(B0),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(B0,
							       p_init);
    root_part->setDiagonalSpinDensity();      
    
    myGenerator.generateDecay(root_part);

    //    root_part->printTree();
    //root_part->getDaug(0)->printTree();
    //root_part->getDaug(1)->printTree();
    //root_part->getDaug(2)->printTree();

    k=root_part->getDaug(0)->getP4Lab();
    l1=root_part->getDaug(1)->getP4Lab();
    l2=root_part->getDaug(2)->getP4Lab();
    
    //b=root_part->getP4();
    //    k=root_part->getDaug(0)->getDaug(0)->getP4Lab();
    //    pi=root_part->getDaug(0)->getDaug(1)->getP4Lab();

    double qsq=(l1+l2).mass2();

    for (int j=0;j<n;j++){
      if (qsq>q2low[j]&&qsq<q2high[j]) counts[j]++;
    }
   
    _q2->Fill( (l1+l2).mass2() );
    _q2low->Fill( (l1+l2).mass2() );
    _q2lowlow->Fill( (l1+l2).mass2() );
    
    _ctl->Fill(EvtDecayAngle((l1+l2+k),(l1+l2),l1));
    
    _dalitz->Fill(l1.get(0),(l1+l2).mass2(),1.0);
    
    root_part->deleteTree();  

    _phi->Fill(atan2(l1.get(1),l1.get(2)));

    //_chi->Fill(EvtDecayAngleChi(b,k,pi,l1,l2));

    //if (EvtDecayAngle((l1+l2+kstar),(l1+l2),l1)>0){
    //  _chictl->Fill(EvtDecayAngleChi(b,k,pi,l1,l2));
    // }
    EvtGenReport(EVTGEN_INFO,"EvtGen") << "count:"<<count<<" "<<(l1+l2).mass2()<<std::endl;
    
  }while (count++<nevent);
  
  for (int j=0;j<n;j++){
    std::cout << "["<<q2low[j]<<".."<<q2high[j]<<"]="<<counts[j]<<std::endl;
  }

  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}


void runHll(int nevent, EvtGen &myGenerator, char* mode) {
  TString modename = mode;
  TString filename;
  filename = modename;
  filename = filename + "_nnlo.root";
  TFile *file=new TFile(filename, "RECREATE");
  TString decname; 
  decname += modename;
  decname.ToUpper();
  decname = "exampleFiles/" + decname + ".DEC";
  char udecay_name[100];
  strcpy(udecay_name,decname);

  TH2F* _dalitz  = new TH2F("h1","q^2! vs Elep",
					  70,0.0,3.5,60,0.0,30.0); 
  
  
  TH1F* _ctl  = new TH1F("h2","ctl",50,-1.0,1.0); 
  
  
  TH1F* _q2  = new TH1F("h3","q2",50,0.0,25.0); 
  
  TH1F* _q2low = new TH1F("h4","q2 (low)",
					50,0.0,1.0); 
  TH1F* _q2lowlow = new TH1F("h5","q2 (lowlow)",
					   50,0.0,0.00001); 

  TH1F* _phi  = new TH1F("h6","phi",50,-EvtConst::pi,EvtConst::pi); 

  TH1F* _chi  = new TH1F("h7","chi",50,0.0,EvtConst::twoPi); 

  TH1F* _chictl  = new TH1F("h8","chictl",50,0.0,EvtConst::twoPi); 
    
  EvtId B;
  if (modename == "kee" || modename == "kmm" || 
      modename == "kstksee" || modename == "kstksmm" ||
      modename == "piee" || modename == "pimm" ||
      modename == "rhoee" || modename == "rhomm")
  {
   B=EvtPDL::getId(std::string("B+"));
  }
  else
  {
   B=EvtPDL::getId(std::string("B0"));
  }
  int count = 1;
  
  EvtVector4R b,h,l1,l2;
  EvtVector4R hdaug1,hdaug2;
    
  myGenerator.readUDecay(udecay_name);
  
  std::vector<double> q2low(7);
  std::vector<double> q2high(7);
  std::vector<int> counts(7);
  int n;
  if (modename == "kee" || modename == "ksee" || 
      modename == "piee" || modename == "pi0ee" || 
      modename == "etaee" || modename == "etapee")
  {
   //kee  
   n = 6;
   q2low[0] = 0.0;     q2high[0] = 4.5;
   q2low[1] = 4.5;     q2high[1] = 8.41;
   q2low[2] = 8.41;    q2high[2] = 10.24;
   q2low[3] = 10.24;   q2high[3] = 12.96;
   q2low[4] = 12.96;   q2high[4] = 14.06;
   q2low[5] = 14.06;   q2high[5] = 30.0;
  }  
  else if (modename == "kmm" || modename == "ksmm" || 
           modename == "pimm" || modename == "pi0mm" ||
           modename == "etamm" || modename == "etapmm")
  {
   //kmm  
   n = 6;
   q2low[0] = 0.0;     q2high[0] = 4.5;
   q2low[1] = 4.5;     q2high[1] = 9.0;
   q2low[2] = 9.0;     q2high[2] = 10.24;
   q2low[3] = 10.24;   q2high[3] = 12.96;
   q2low[4] = 12.96;   q2high[4] = 14.06;
   q2low[5] = 14.06;   q2high[5] = 30.0;
  }
  else if (modename == "kstkee" || modename == "kstksee" ||
           modename == "rhoee" || modename == "rho0ee" || modename == "omegaee")
  {
   //K*ee  
   n = 7;
   q2low[0] = 0.0;     q2high[0] = 0.1;
   q2low[1] = 0.1;     q2high[1] = 4.5;
   q2low[2] = 4.5;     q2high[2] = 8.41;
   q2low[3] = 8.41;    q2high[3] = 10.24;
   q2low[4] = 10.24;   q2high[4] = 12.96;
   q2low[5] = 12.96;   q2high[5] = 14.06;
   q2low[6] = 14.06;   q2high[6] = 30.0;
  }
  else if (modename == "kstkmm" || modename == "kstksmm" ||
           modename == "rhomm" || modename == "rho0mm" || modename == "omegamm")
  {  
  //K*mm  
   n = 7;
   q2low[0] = 0.0;     q2high[0] = 0.1;
   q2low[1] = 0.1;     q2high[1] = 4.5;
   q2low[2] = 4.5;     q2high[2] = 9.0;
   q2low[3] = 9.0;     q2high[3] = 10.24;
   q2low[4] = 10.24;   q2high[4] = 12.96;
   q2low[5] = 12.96;   q2high[5] = 14.06;
   q2low[6] = 14.06;   q2high[6] = 30.0;
  }
  float q2binlow[n+1];
  for (int i = 0; i < n; i++)
  {
   q2binlow[i] = q2low[i]; 
  }
  q2binlow[n] = 30.0;

  TH1F* _q2var = new TH1F("h9", "q2var", n, q2binlow);
 
  do
  {
   EvtVector4R p_init(EvtPDL::getMass(B),0.0,0.0,0.0);
   EvtParticle* root_part = EvtParticleFactory::particleFactory(B,p_init);
   root_part->setDiagonalSpinDensity();      
   myGenerator.generateDecay(root_part);

   //    root_part->printTree();
   //root_part->getDaug(0)->printTree();
   //root_part->getDaug(1)->printTree();
   //root_part->getDaug(2)->printTree();

   h  = root_part->getDaug(0)->getP4Lab();
   l1 = root_part->getDaug(1)->getP4Lab();
   l2 = root_part->getDaug(2)->getP4Lab();
   double qsq=(l1+l2).mass2();

   for (int j = 0 ; j < n; j++)
   {
    if (qsq > q2low[j] && qsq < q2high[j]) counts[j]++;
   }
   
   _q2->Fill( (l1+l2).mass2() );
   _q2var->Fill( (l1+l2).mass2() );
   _q2low->Fill( (l1+l2).mass2() );
   _q2lowlow->Fill( (l1+l2).mass2() );
    
   _ctl->Fill(EvtDecayAngle((l1+l2+h),(l1+l2),l1));
    
   _dalitz->Fill(l1.get(0),(l1+l2).mass2(),1.0);

   _phi->Fill(atan2(l1.get(1),l1.get(2)));

   if (modename == "kstkee" || modename == "kstkmm" || 
       modename == "kstksee" || modename == "kstksmm" ||
       modename == "rhoee" || modename == "rhomm" ||
       modename == "rho0ee" || modename == "rho0mm")
   {
    b =      root_part->getP4();
    hdaug1 = root_part->getDaug(0)->getDaug(0)->getP4Lab();
    hdaug2 = root_part->getDaug(0)->getDaug(1)->getP4Lab();
    _chi->Fill(EvtDecayAngleChi(b,hdaug1,hdaug2,l1,l2));
    if (EvtDecayAngle((l1+l2+h),(l1+l2),l1) > 0)
    {
      _chictl->Fill(EvtDecayAngleChi(b,hdaug1,hdaug2,l1,l2));
    }
   }
   if (count % 1000 == 0)
   {
    EvtGenReport(EVTGEN_INFO,"EvtGen") << "count:"<<count<<" "<<(l1+l2).mass2()<<std::endl;  
   }
   root_part->deleteTree();  
  }
  while (count++ < nevent);
  
  for (int j = 0;j  < n; j++)
  {
   std::cout << "[" << q2low[j] << ".." << q2high[j] << "] = " << counts[j] << std::endl;
  }
  file->Write(); 
  file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}

void runVectorIsr(int nevent, EvtGen &myGenerator) {
  
  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId VPHO=EvtPDL::getId(std::string("vpho"));

  TFile *file=new TFile("vectorisr.root", "RECREATE");
  
  TH1F* cosv = new TH1F("h1","Cos vector in e+e- frame",
				       50,-1.0,1.0); 
  
  TH1F* cosd1 = new TH1F("h2","Cos helang  1st dau of vector",
				       50,-1.0,1.0); 

  TH1F* cosd1d1 = new TH1F("h3","Cos helang  1st dau of 1st dau",
				       50,-1.0,1.0);

  TH1F* cosd2d1 = new TH1F("h4","Cos helang  1st dau of 2nd dau",
				       50,-1.0,1.0); 
  
  TH2F* d1vsd1d1  = new TH2F("h5","Cos helangs  d1 vs d1d1",
					    20,-1.0,1.0,20,-1.0,1.0); 

  TH2F* d2vsd2d1  = new TH2F("h6","Cos helangs  d2 vs d2d1",
					    20,-1.0,1.0,20,-1.0,1.0); 

  TH2F* d1d1vsd2d1  = new TH2F("h7","Cos helangs  d1d1 vs d2d1",
					      20,-1.0,1.0,20,-1.0,1.0); 

  TH1F* chidd = new TH1F("h8","Chi - angle between decay planes",
					60,0.,360.0); 
  
  TH2F* chi12vsd1d1  = new TH2F("h9","Chi 1-2 vs d1d1",
					     30,0.,360.0,20,-1.0,1.0); 

  TH2F* chi12vsd2d1  = new TH2F("h10","Chi 1-2 vs d2d1",
					     30,0.,360.0,20,-1.0,1.0); 

  TH2F* chi21vsd1d1  = new TH2F("h11","Chi 2-1 vs d1d1",
					     30,0.,360.0,20,-1.0,1.0); 

  TH2F* chi21vsd2d1  = new TH2F("h12","Chi 2-1 vs d2d1",
					     30,0.,360.0,20,-1.0,1.0); 

  int count=1;
  char udecay_name[100];  
  EvtVector4R cm, v, d1, d2, d1d1, d1d2, d2d1, d2d2;
  
  strcpy(udecay_name,"exampleFiles/VECTORISR.DEC");
  myGenerator.readUDecay(udecay_name);
  
  do{
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(VPHO,
							       p_init);
    
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    cm=root_part->getP4Lab();
    v=root_part->getDaug(0)->getP4Lab();

    d1=root_part->getDaug(0)->getDaug(0)->getP4Lab();
    d2=root_part->getDaug(0)->getDaug(1)->getP4Lab();

    cosv->Fill(v.get(3)/v.d3mag());

    double cosdecayd1=EvtDecayAngle(cm,v,d1);
    double cosdecayd2=EvtDecayAngle(cm,v,d2);
    cosd1->Fill(cosdecayd1);

    // now get daughters of the daughters
    //
    // first daughter of first daughter
    if (root_part->getDaug(0)->getDaug(0)->getNDaug()>=2){
      d1d1=root_part->getDaug(0)->getDaug(0)->getDaug(0)->getP4Lab();
      double cosdecayd1d1=EvtDecayAngle(v,d1,d1d1);
      cosd1d1->Fill(cosdecayd1d1);
      d1vsd1d1->Fill(cosdecayd1,cosdecayd1d1,1.0);
    }
    
    // first daughter of second daughter
    if (root_part->getDaug(0)->getDaug(1)->getNDaug()>=2){
      d2d1=root_part->getDaug(0)->getDaug(1)->getDaug(0)->getP4Lab();
      double cosdecayd2d1=EvtDecayAngle(v,d2,d2d1);
      cosd2d1->Fill(cosdecayd2d1);
      d2vsd2d1->Fill(cosdecayd2,cosdecayd2d1,1.0);

      if (root_part->getDaug(0)->getDaug(0)->getNDaug()>=2){
	d1d1=root_part->getDaug(0)->getDaug(0)->getDaug(0)->getP4Lab();
	double cosdecayd1d1=EvtDecayAngle(v,d1,d1d1);
	d1d1vsd2d1->Fill(cosdecayd1d1,cosdecayd2d1,1.0);

	//second daughters of daughters 1 and 2
	d1d2=root_part->getDaug(0)->getDaug(0)->getDaug(1)->getP4Lab();
	d2d2=root_part->getDaug(0)->getDaug(1)->getDaug(1)->getP4Lab();
	double chi21=57.29578*EvtDecayAngleChi(v,d2d1,d2d2,d1d1,d1d2);
	double chi12=57.29578*EvtDecayAngleChi(v,d1d1,d1d2,d2d1,d2d2);
	chidd->Fill(chi12);

	chi12vsd1d1->Fill(chi12,cosdecayd1d1,1.0);
	chi12vsd2d1->Fill(chi12,cosdecayd2d1,1.0);

	chi21vsd1d1->Fill(chi21,cosdecayd1d1,1.0);
	chi21vsd2d1->Fill(chi21,cosdecayd2d1,1.0);

      }

    }
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}


void runBsquark(int nevent, EvtGen &myGenerator) {
  
  static EvtId VPHO=EvtPDL::getId(std::string("vpho"));
  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));

  TFile *file=new TFile("bsquark.root", "RECREATE");
  
  TH1F* elep = new TH1F("h1","Elep",50,0.0,1.5); 
  
  TH1F* q2 = new TH1F("h2","q2",50,0.0,3.0); 

  TH2F* dalitz=new TH2F("h3","q2 vs. Elep",50,0.0,1.5,50,0.0,3.0);

  TH1F* elepbar = new TH1F("h11","Elep bar",
				       50,0.0,1.5); 
  
  TH1F* q2bar = new TH1F("h12","q2 bar",
				       50,0.0,3.0); 

  TH2F* dalitzbar=new TH2F("h13","q2 vs. Elep bar",50,0.0,1.5,50,0.0,3.0);

  int count=1;
  char udecay_name[100];  
  EvtVector4R cm, v, d1, d2, d1d1, d1d2, d2d1, d2d2;
  
  strcpy(udecay_name,"exampleFiles/BSQUARK.DEC");
  myGenerator.readUDecay(udecay_name);
  
  do{
    EvtVector4R p_init(10.55,0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(VPHO,
							       p_init);
    
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);

    EvtParticle* p=root_part->nextIter();

    while (p) {

      if (p->getId()==B0) {

	EvtParticle *dstar=p->getDaug(0); 
	EvtParticle *lepton=p->getDaug(1); 
	EvtParticle *sneutrino=p->getDaug(2); 

	EvtVector4R p4dstar=dstar->getP4();
	EvtVector4R p4lepton=lepton->getP4();
	EvtVector4R p4sneutrino=sneutrino->getP4();

	elep->Fill(p4lepton.get(0));
	q2->Fill((p4lepton+p4sneutrino).mass2());

	dalitz->Fill(p4lepton.get(0),(p4lepton+p4sneutrino).mass2(),1.0);

      }

      if (p->getId()==B0B) {

	EvtParticle *dstar=p->getDaug(0); 
	EvtParticle *lepton=p->getDaug(1); 
	EvtParticle *sneutrino=p->getDaug(2); 

	EvtVector4R p4dstar=dstar->getP4();
	EvtVector4R p4lepton=lepton->getP4();
	EvtVector4R p4sneutrino=sneutrino->getP4();

	elepbar->Fill(p4lepton.get(0));
	q2bar->Fill((p4lepton+p4sneutrino).mass2());

	dalitzbar->Fill(p4lepton.get(0),(p4lepton+p4sneutrino).mass2(),1.0);

      }

      p=p->nextIter();

    }

    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}


void runK3gamma(int nevent, EvtGen &myGenerator) {
  
  static EvtId B0=EvtPDL::getId(std::string("B0"));

  TFile *file=new TFile("k3gamma.root", "RECREATE");

  TH1F* costheta = new TH1F("h1","cosTheta", 100,-1.0,1.0); 
  

  int count=1;
  char udecay_name[100];  
  EvtVector4R cm, v, d1, d2, d1d1, d1d2, d2d1, d2d2;
  
  strcpy(udecay_name,"exampleFiles/K3GAMMA.DEC");
  myGenerator.readUDecay(udecay_name);
  
  do{
    EvtVector4R p_init(EvtPDL::getMass(B0),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(B0,p_init);
    
    root_part->setDiagonalSpinDensity();      
    
    myGenerator.generateDecay(root_part);

    EvtParticle *k3=root_part->getDaug(0);
    EvtParticle *k=k3->getDaug(0);

    EvtVector4R p4b=root_part->getP4Lab();
    EvtVector4R p4k3=k3->getP4Lab();
    EvtVector4R p4k=k->getP4Lab();

    costheta->Fill(EvtDecayAngle(p4b,p4k3,p4k));

    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}


void runLambda(int nevent, EvtGen &myGenerator) {
  
  static EvtId LAMBDA=EvtPDL::getId(std::string("Lambda0"));

  TFile *file=new TFile("lambda.root", "RECREATE");

  TH1F* costheta = new TH1F("h1","cosTheta",100,-1.0,1.0); 
  

  int count=1;
  char udecay_name[100];  
  EvtVector4R cm, v, d1, d2, d1d1, d1d2, d2d1, d2d2;
  
  strcpy(udecay_name,"exampleFiles/LAMBDA.DEC");
  myGenerator.readUDecay(udecay_name);
  
  do{
    EvtVector4R p_init(EvtPDL::getMass(LAMBDA),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(LAMBDA,p_init);

    EvtSpinDensity rho;

    rho.setDim(2);
    rho.set(0,0,1.0);
    rho.set(0,1,0.0);
    rho.set(1,0,0.0);
    rho.set(1,1,0.0);

    root_part->setSpinDensityForwardHelicityBasis(rho);      
    
    myGenerator.generateDecay(root_part);

    EvtParticle *p=root_part->getDaug(0);

    EvtVector4R p4lambda=root_part->getP4Lab();
    EvtVector4R p4p=p->getP4Lab();

    costheta->Fill(p4p.get(3)/p4p.d3mag());

    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}



void runTauTauPiPi(int nevent, EvtGen &myGenerator) {

  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId VPHO=EvtPDL::getId(std::string("vpho"));
  TFile *file=new TFile("tautaupipi.root", "RECREATE");
  
  TH1F* cospi1 = new TH1F("h1","cos theta pi1",  50,-1.0,1.0); 
  TH1F* cospi2 = new TH1F("h2","cos theta pi2",  50,-1.0,1.0); 
  TH1F* costheta = new TH1F("h3","cos theta",  50,-1.0,1.0); 
  
  std::ofstream outmix;
  outmix.open("tautaupipi.dat");
  
  int count=1;
  
  EvtVector4R tau1,tau2,pi1,pi2;
  char udecay_name[100];  
  
  strcpy(udecay_name,"exampleFiles/TAUTAUPIPI.DEC");
  myGenerator.readUDecay(udecay_name);
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(VPHO,
							       p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    tau1=root_part->getDaug(0)->getP4Lab();
    tau2=root_part->getDaug(1)->getP4Lab();
    
    pi1=root_part->getDaug(0)->getDaug(0)->getP4Lab();
    pi2=root_part->getDaug(1)->getDaug(0)->getP4Lab();
    
    cospi1->Fill( EvtDecayAngle(tau1+tau2,tau1,pi1) );
    cospi2->Fill( EvtDecayAngle(tau1+tau2,tau2,pi2) );
    costheta->Fill(tau1.get(3)/tau1.d3mag());
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  
  outmix.close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}

void runTauTauEE(int nevent, EvtGen &myGenerator) {

  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId VPHO=EvtPDL::getId(std::string("vpho"));
  TFile *file=new TFile("tautauee.root", "RECREATE");
  
  TH1F* e1 = new TH1F("h1","e1",55,0.0,5.5); 
  TH1F* e2 = new TH1F("h2","e2",55,0.0,5.5); 
  TH2F* e1vse2 = new TH2F("h3","e1 vs e2",55,0.0,5.5,
					 55,0.0,5.5); 
  
  int count=1;
  char udecay_name[100];  
  
  strcpy(udecay_name,"exampleFiles/TAUTAUEE.DEC");
  myGenerator.readUDecay(udecay_name);
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(VPHO, p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    e1->Fill(root_part->getDaug(0)->getDaug(0)->getP4Lab().get(0));
    e2->Fill(root_part->getDaug(1)->getDaug(0)->getP4Lab().get(0));
    e1vse2->Fill(root_part->getDaug(0)->getDaug(0)->getP4Lab().get(0),
			root_part->getDaug(1)->getDaug(0)->getP4Lab().get(0),1.0);
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}

void runTauTau2Pi2Pi(int nevent, EvtGen &myGenerator) {

  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId VPHO=EvtPDL::getId(std::string("vpho"));
  TFile *file=new TFile("tautau2pi2pi.root", "RECREATE");
  
  TH1F* e1 = new TH1F("h1","mrho",200,0.0,2.0); 
  TH1F* e2 = new TH1F("h2","coshel",200,-1.0,1.0); 
  
  int count=1;
  char udecay_name[100];  
  
  strcpy(udecay_name,"exampleFiles/TAUTAU2PI2PI.DEC");
  myGenerator.readUDecay(udecay_name);
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(VPHO, p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);

    EvtVector4R p4tau =root_part->getDaug(0)->getP4();
    EvtVector4R p4rho =root_part->getDaug(0)->getDaug(0)->getP4()
                     + root_part->getDaug(0)->getDaug(1)->getP4(); 
    EvtVector4R p4pi = root_part->getDaug(0)->getDaug(0)->getP4();

    e1->Fill(p4rho.mass());
    double dcostheta=EvtDecayAngle(p4tau,p4rho,p4pi);
    e2->Fill(dcostheta);

    p4tau =root_part->getDaug(1)->getP4();
    p4rho =root_part->getDaug(1)->getDaug(0)->getP4()
           + root_part->getDaug(1)->getDaug(1)->getP4(); 
    p4pi = root_part->getDaug(1)->getDaug(0)->getP4();

    e1->Fill(p4rho.mass());
    dcostheta=EvtDecayAngle(p4tau,p4rho,p4pi);
    e2->Fill(dcostheta);
   
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}

void runTauTau3Pi3Pi(int nevent, EvtGen &myGenerator) {

  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId VPHO=EvtPDL::getId(std::string("vpho"));
  TFile *file=new TFile("tautau3pi3pi.root", "RECREATE");
  
  TH1F* e1 = new TH1F("h1","a1",200,0.0,2.0); 
  
  int count=1;
  char udecay_name[100];  
  
  strcpy(udecay_name,"exampleFiles/TAUTAU3PI3PI.DEC");
  myGenerator.readUDecay(udecay_name);
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(VPHO, p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);

    EvtVector4R p4tau =root_part->getDaug(0)->getP4();
    EvtVector4R p4a1 =root_part->getDaug(0)->getDaug(0)->getP4()
                    + root_part->getDaug(0)->getDaug(1)->getP4() 
                    + root_part->getDaug(0)->getDaug(2)->getP4(); 

    e1->Fill(p4a1.mass());

    p4tau =root_part->getDaug(1)->getP4();
    p4a1 =root_part->getDaug(1)->getDaug(0)->getP4()
           + root_part->getDaug(1)->getDaug(1)->getP4() 
           + root_part->getDaug(1)->getDaug(2)->getP4(); 

    e1->Fill(p4a1.mass());
   
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}


void runJPsiKstar(int nevent, EvtGen &myGenerator, int modeInt) {

  std::ofstream outmix;
  outmix.open("jpsikstar.dat");
  
  int count=1;
  
  char udecay_name[100];
  if (modeInt==0) strcpy(udecay_name,"exampleFiles/JPSIKSTAR.DEC");
  if (modeInt==1) strcpy(udecay_name,"exampleFiles/JPSIKSTAR1.DEC");
  if (modeInt==2) strcpy(udecay_name,"exampleFiles/JPSIKSTAR2.DEC");
  if (modeInt==3) strcpy(udecay_name,"exampleFiles/JPSIKSTAR3.DEC");
  if (modeInt==4) strcpy(udecay_name,"exampleFiles/JPSIKSTAR4.DEC");
  
  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId VPHO=EvtPDL::getId(std::string("vpho"));
  static EvtId GAMM=EvtPDL::getId(std::string("gamma"));
  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));
  
  myGenerator.readUDecay(udecay_name);
  
  do{

    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(UPS4,
							       p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    EvtParticle *btag,*bcp;
    
    if (root_part->getDaug(0)->getNDaug()==3){
      btag=root_part->getDaug(0);
      bcp=root_part->getDaug(1);
    }
    else{
      bcp=root_part->getDaug(0);
      btag=root_part->getDaug(1);
    }
    
    EvtId tag;
    
    if (btag->getId()==B0B){
      tag=B0;
    }
    else{
      tag=B0B;
    }
    
    EvtParticle *p_b,*p_psi,*p_kstar,*p_pi0,*p_kz,*p_ep,*p_em;
    EvtVector4R  p4_b,p4_psi,p4_kstar,p4_pi0,p4_kz,p4_ep,p4_em;
    
    p_b=bcp;
    
    p_psi=p_b->getDaug(0);
    p_kstar=p_b->getDaug(1);
    
    p_pi0=p_kstar->getDaug(0);
    p_kz=p_kstar->getDaug(1);
    
    p_ep=p_psi->getDaug(0);
    p_em=p_psi->getDaug(1);
    
    p4_b=p_b->getP4Lab();
    p4_psi=p_psi->getP4Lab();
    p4_kstar=p_kstar->getP4Lab();
    p4_pi0=p_pi0->getP4Lab();
    p4_kz=p_kz->getP4Lab();
    p4_ep=p_ep->getP4Lab();
    p4_em=p_em->getP4Lab();
    
    outmix << tag.getId() << " ";
    outmix << root_part->getDaug(0)->getLifetime() << " ";
    outmix << root_part->getDaug(1)->getLifetime() << " ";
    outmix << EvtDecayAngle(p4_b,p4_ep+p4_em,p4_ep) << " " ;
    outmix << EvtDecayAngle(p4_b,p4_pi0+p4_kz,p4_pi0) << " " ;
    outmix << EvtDecayAngleChi(p4_b,p4_pi0,p4_kz,
			       p4_ep, p4_em ) << "\n" ;
    
    root_part->deleteTree();  
    
  }while (count++<10000);
  
  outmix.close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}


void runSVVCPLH(int nevent, EvtGen &myGenerator) {

  TFile *file=new TFile("svvcplh.root", "RECREATE");
  
  TH1F* t = new TH1F("h1","t",50,0.0,5.0);
  TH1F* cospsi = new TH1F("h2","cos theta e+",50,-1.0,1.0); 
  TH1F* cosphi = new TH1F("h3","cos theta k+",50,-1.0,1.0); 
  TH1F* chi = new TH1F("h4","chi",50,0.0,2.0*EvtConst::pi); 
  
  int count=1;
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/SVVCPLH.DEC");
  myGenerator.readUDecay(udecay_name);

  static EvtId BS0=EvtPDL::getId(std::string("B_s0"));
  static EvtId BSB=EvtPDL::getId(std::string("anti-B_s0"));
  
  do{
    EvtVector4R p_init(EvtPDL::getMass(BS0),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(BS0,
							       p_init);
    root_part->setDiagonalSpinDensity();      
    
    myGenerator.generateDecay(root_part);

    EvtParticle *p_b,*p_psi,*p_phi,*p_kp,*p_km,*p_ep,*p_em;
    EvtVector4R  p4_b,p4_psi,p4_phi,p4_kp,p4_km,p4_ep,p4_em;
    
    p_b=root_part;
    
    if (p_b->getNDaug()==1) p_b=p_b->getDaug(0);
    
    p_psi=p_b->getDaug(0);
    p_phi=p_b->getDaug(1);
    
    p_kp=p_phi->getDaug(0);
    p_km=p_phi->getDaug(1);
    
    p_ep=p_psi->getDaug(0);
    p_em=p_psi->getDaug(1);
    
    p4_b=p_b->getP4Lab();
    p4_psi=p_psi->getP4Lab();
    p4_phi=p_phi->getP4Lab();
    p4_kp=p_kp->getP4Lab();
    p4_km=p_km->getP4Lab();
    p4_ep=p_ep->getP4Lab();
    p4_em=p_em->getP4Lab();
    
    t->Fill(root_part->getLifetime());
    cospsi->Fill(EvtDecayAngle(p4_b,p4_ep+p4_em,p4_ep));
    cosphi->Fill(EvtDecayAngle(p4_b,p4_kp+p4_km,p4_kp));
    chi->Fill(EvtDecayAngleChi(p4_b,p4_kp,p4_km,
				     p4_ep, p4_em ));
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}


void runSVSCPLH(int nevent, EvtGen &myGenerator) {

  TFile *file=new TFile("svscplh.root", "RECREATE");
  
  TH1F* t = new TH1F("h1","t",200,-5.0,5.0);
  TH1F* tB0tag = new TH1F("h2","dt B0 tag (ps)",200,-15.0,15.0);
  TH1F* tB0Btag = new TH1F("h3","dt B0B tag (ps)",200,-15.0,15.0);
  TH1F* ctheta = new TH1F("h4","costheta",50,-1.0,1.0); 
  
  int count=1;

  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/SVSCPLH.DEC");
  myGenerator.readUDecay(udecay_name);


  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));

  std::ofstream outmix;
  outmix.open("svscplh.dat");

  do{

    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(UPS4,
							       p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);

    
    EvtParticle *p_tag,*p_cp,*p_jpsi,*p_ep;
    EvtVector4R p4_tag,p4_cp,p4_jpsi,p4_ep;
    
    p_tag=root_part->getDaug(0);
    p_cp=root_part->getDaug(1);
    
    p_jpsi=p_cp->getDaug(0);
    p_ep=p_jpsi->getDaug(0);

    p4_tag=p_tag->getP4Lab();
    p4_cp=p_cp->getP4Lab();
    p4_jpsi=p_jpsi->getP4Lab();
    p4_ep=p_ep->getP4Lab();
    
    
    double dt=p_cp->getLifetime()-p_tag->getLifetime();
    dt=dt/(1e-12*3e11);

    t->Fill(dt);

    if (p_tag->getId()==B0){
      tB0tag->Fill(dt);
      outmix << dt << " 1"<<std::endl;
    }
    if (p_tag->getId()==B0B){
      tB0Btag->Fill(dt);
      outmix << dt << " -1"<<std::endl;
    }
    ctheta->Fill(EvtDecayAngle(p4_cp,p4_jpsi,p4_ep));

    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  outmix.close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}


void runSSDCP(int nevent, EvtGen &myGenerator) {

  TFile *file=new TFile("ssdcp.root", "RECREATE");
  
  TH1F* t = new TH1F("h1","dt",100,-15.0,15.0);
  TH1F* tB0tag = new TH1F("h2","dt B0 tag (ps)",100,-15.0,15.0);
  TH1F* tB0Btag = new TH1F("h3","dt B0B tag (ps)",100,-15.0,15.0);
  
  int count=1;

  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/SSDCP.DEC");
  myGenerator.readUDecay(udecay_name);


  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));

  std::ofstream outmix;

  do{

    EvtVector4R pinit(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle *root_part=EvtParticleFactory::particleFactory(UPS4,pinit);

    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);

    
    EvtParticle *p_tag,*p_cp,*p_jpsi;
    EvtVector4R p4_tag,p4_cp,p4_jpsi,p4_ep;
    
    p_tag=root_part->getDaug(0);
    p_cp=root_part->getDaug(1);
    
    p_jpsi=p_cp->getDaug(0);
    //p_ep=p_jpsi->getDaug(0);

    p4_tag=p_tag->getP4Lab();
    p4_cp=p_cp->getP4Lab();
    p4_jpsi=p_jpsi->getP4Lab();
    //p4_ep=p_ep->getP4Lab();
    
    
    double dt=p_cp->getLifetime()-p_tag->getLifetime();
    dt=dt/(1e-12*EvtConst::c);

    t->Fill(dt);

    if (p_tag->getId()==B0){
      tB0tag->Fill(dt);
    }
    if (p_tag->getId()==B0B){
      tB0Btag->Fill(dt);
    }
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}

void runKstarstargamma(int nevent, EvtGen &myGenerator) {

  TFile *file=new TFile("kstarstargamma.root", "RECREATE");
  
  TH1F* m = new TH1F("h1","mkpi",100,0.5,2.5);

  TH1F* ctheta = new TH1F("h2","ctheta",100,-1.0,1.0);
  
  int count=1;

  myGenerator.readUDecay("exampleFiles/KSTARSTARGAMMA.DEC");


  static EvtId B0=EvtPDL::getId(std::string("B0"));

  std::ofstream outmix;

  do{

    EvtVector4R pinit(EvtPDL::getMass(B0),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(B0,pinit);

    root_part->setDiagonalSpinDensity();      

    myGenerator.generateDecay(root_part);
    
    EvtParticle *p_kaon,*p_pion;
    EvtVector4R p4_kaon,p4_pion;
    
    p_kaon=root_part->getDaug(0);
    p_pion=root_part->getDaug(1);
    

    p4_kaon=p_kaon->getP4Lab();
    p4_pion=p_pion->getP4Lab();
    

    m->Fill((p4_kaon+p4_pion).mass());

    ctheta->Fill(EvtDecayAngle(pinit,p4_kaon+p4_pion,p4_kaon));
 
    //EvtGenReport(EVTGEN_INFO,"EvtGen") << "ctheta:"<<EvtDecayAngle(pinit,p4_kaon+p4_pion,p4_kaon)<<std::endl;
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}


void runDSTARPI(int nevent, EvtGen &myGenerator) {

  TFile *file=new TFile("dstarpi.root", "RECREATE");
  
  TH1F* t = new TH1F("h1","dt",100,-15.0,15.0);
  TH1F* tB0tagpip = new TH1F("h2","dt B0 tag pi+ (ps)",100,-15.0,15.0);
  TH1F* tB0Btagpip = new TH1F("h3","dt B0B tag pi+(ps)",100,-15.0,15.0);
  TH1F* tB0tagpim = new TH1F("h4","dt B0 tag pi- (ps)",100,-15.0,15.0);
  TH1F* tB0Btagpim = new TH1F("h5","dt B0B tag pi- (ps)",100,-15.0,15.0);
  
  int count=1;

  myGenerator.readUDecay("exampleFiles/DSTARPI.DEC");

  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));
  static EvtId PIP=EvtPDL::getId(std::string("pi+"));
  static EvtId PIM=EvtPDL::getId(std::string("pi-"));

  std::ofstream outmix;

  do{

    EvtVector4R pinit(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(UPS4,pinit);

    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);

    
    EvtParticle *p_tag,*p_cp,*p_dstar,*p_pi;
    
    p_tag=root_part->getDaug(0);
    p_cp=root_part->getDaug(1);
    
    p_dstar=p_cp->getDaug(1);
    p_pi=p_cp->getDaug(0);

    double dt=p_cp->getLifetime()-p_tag->getLifetime();
    dt=dt/(1e-12*EvtConst::c);

    t->Fill(dt);

    if (p_tag->getId()==B0){
      if (p_pi->getId()==PIP) tB0tagpip->Fill(dt);
      if (p_pi->getId()==PIM) tB0tagpim->Fill(dt);
    }
    if (p_tag->getId()==B0B){
      if (p_pi->getId()==PIP) tB0Btagpip->Fill(dt);
      if (p_pi->getId()==PIM) tB0Btagpim->Fill(dt);
    }
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}



void runETACPHIPHI(int nevent, EvtGen &myGenerator) {

  TFile *file=new TFile("etacphiphi.root", "RECREATE");
  
  TH2F* cosphi12 = new TH2F("h1","cos phi1 vs phi2",
					 50,-1.0,1.0,50,-1.0,1.0); 
  TH1F* cosphi1 = new TH1F("h2","cos phi1",50,-1.0,1.0); 
  TH1F* cosphi2 = new TH1F("h3","cos phi2",50,-1.0,1.0); 
  TH1F* chi = new TH1F("h4","chi",50,0.0,2.0*EvtConst::pi); 
  
  int count=1;
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/ETACPHIPHI.DEC");
  myGenerator.readUDecay(udecay_name);

  static EvtId ETAC=EvtPDL::getId(std::string("eta_c"));
  
  do{
    EvtVector4R p_init(EvtPDL::getMass(ETAC),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(ETAC,
							       p_init);
    root_part->setDiagonalSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    EvtParticle *p_etac,*p_phi1,*p_phi2,*p_kp1,*p_km1,*p_kp2,*p_km2;
    EvtVector4R p4_etac,p4_phi1,p4_phi2,p4_kp1,p4_km1,p4_kp2,p4_km2;
    
    p_etac=root_part;
    
    
    p_phi1=p_etac->getDaug(0);
    p_phi2=p_etac->getDaug(1);
    
    p_kp1=p_phi1->getDaug(0);
    p_km1=p_phi1->getDaug(1);

    p_kp2=p_phi2->getDaug(0);
    p_km2=p_phi2->getDaug(1);
    
    p4_etac=p_etac->getP4Lab();
    p4_phi1=p_phi1->getP4Lab();
    p4_phi2=p_phi2->getP4Lab();
    p4_kp1=p_kp1->getP4Lab();
    p4_km1=p_km1->getP4Lab();
    p4_kp2=p_kp2->getP4Lab();
    p4_km2=p_km2->getP4Lab();

    
    cosphi12->Fill(EvtDecayAngle(p4_etac,p4_phi1,p4_kp1),
                          EvtDecayAngle(p4_etac,p4_phi2,p4_kp2),1.0);

    cosphi1->Fill(EvtDecayAngle(p4_etac,p4_phi1,p4_kp1));
    cosphi2->Fill(EvtDecayAngle(p4_etac,p4_phi2,p4_kp2));
    chi->Fill(EvtDecayAngleChi(p4_etac,p4_kp1,p4_km1,
				     p4_kp2, p4_km2));
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}






void runVVPiPi(int nevent, EvtGen &myGenerator) {
  TFile *file=new TFile("vvpipi.root", "RECREATE");
  
  TH1F* cospsi = new TH1F("h1","cos theta J/psi ",50,-1.0,1.0);
  TH1F* cose = new TH1F("h2","cos theta e+ ",50,-1.0,1.0); 
  TH1F* mpipi = new TH1F("h3","m pipi ",50,0.0,1.0); 
  TH2F* cosevspsi = new TH2F("h4","cos theta e+vs cos thete J/psi ",
					    25,-1.0,1.0,25,-1.0,1.0); 
  TH1F* cose1 = new TH1F("h5","cos theta e+ 1 ",50,-1.0,1.0); 
  TH1F* cose2 = new TH1F("h6","cos theta e+ 2 ",50,-1.0,1.0); 
  
  
  int count=1;
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/VVPIPI.DEC");
  myGenerator.readUDecay(udecay_name);

  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));
  
  do{
    EvtVector4R p_init(EvtPDL::getMass(B0),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(B0,
							       p_init);
    root_part->setDiagonalSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    EvtParticle *p_b,*p_psip,*p_psi,*p_ep,*p_pi1,*p_pi2;
    EvtVector4R  p4_b,p4_psip,p4_psi,p4_ep,p4_pi1,p4_pi2;
    
    p_b=root_part;
    
    p_psip=p_b->getDaug(0);
    p_psi=p_psip->getDaug(0);
    p_pi1=p_psip->getDaug(1);
    p_pi2=p_psip->getDaug(2);
    p_ep=p_psi->getDaug(0);
    
    p4_b=p_b->getP4Lab();
    p4_psip=p_psip->getP4Lab();
    p4_psi=p_psi->getP4Lab();
    p4_pi1=p_pi1->getP4Lab();
    p4_pi2=p_pi2->getP4Lab();
    p4_ep=p_ep->getP4Lab();
    
    cospsi->Fill(EvtDecayAngle(p4_b,p4_psip,p4_psi));
    cose->Fill(EvtDecayAngle(p4_psip,p4_psi,p4_ep));
    mpipi->Fill((p4_pi1+p4_pi2).mass());
    cosevspsi->Fill(EvtDecayAngle(p4_b,p4_psip,p4_psi),
			   EvtDecayAngle(p4_psip,p4_psi,p4_ep),1.0);
    if (std::fabs(EvtDecayAngle(p4_b,p4_psip,p4_psi))>0.95){
      cose1->Fill(EvtDecayAngle(p4_psip,p4_psi,p4_ep));
    }
    if (std::fabs(EvtDecayAngle(p4_b,p4_psip,p4_psi))<0.05){
      cose2->Fill(EvtDecayAngle(p4_psip,p4_psi,p4_ep));
    }
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}


void runSVVHelAmp(int nevent, EvtGen &myGenerator) {
  TFile *file=new TFile("svvhelamp.root", "RECREATE");
  
  TH1F* cospip = new TH1F("h1","cos theta pi+",
					 50,-1.0,1.0); 
  TH1F* cospim = new TH1F("h2","cos theta pi-",
					 50,-1.0,1.0); 
  TH1F* chi = new TH1F("h3","chi pi+ to pi- in D+ direction",
				      50,0.0,EvtConst::twoPi); 
  TH1F* chicospipp = new TH1F("h4","chi pi+ to pi- in D+ direction (cospip>0)",
					     50,0.0,EvtConst::twoPi); 
  TH1F* chicospipn = new TH1F("h5","chi pi+ to pi- in D+ direction (cospip<0",
					     50,0.0,EvtConst::twoPi); 
  
  TH1F* chipp = new TH1F("h6","chi pi+ to pi- in D+ direction (cospip>0,cospim>0)",
					50,0.0,EvtConst::twoPi); 
  
  TH1F* chipn = new TH1F("h7","chi pi+ to pi- in D+ direction (cospip>0,cospim<0)",
					50,0.0,EvtConst::twoPi); 
  
  TH1F* chinp = new TH1F("h8","chi pi+ to pi- in D+ direction (cospip<0,cospim>0)",
					50,0.0,EvtConst::twoPi); 
  
  TH1F* chinn = new TH1F("h9","chi pi+ to pi- in D+ direction (cospip<0,cospim<0)",
					50,0.0,EvtConst::twoPi); 
  
  
  TH1F* chinnnn = new TH1F("h10","chi pi+ to pi- in D+ direction (cospip<-0.5,cospim<-0.5)",
					  50,0.0,EvtConst::twoPi); 
  
  int count=1;
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/SVVHELAMP.DEC");
  myGenerator.readUDecay(udecay_name);

  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));
  
  do{
    EvtVector4R p_init(EvtPDL::getMass(B0),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(B0,
							       p_init);
    root_part->setDiagonalSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    EvtParticle *p_b,*p_dstp,*p_dstm,*p_pip,*p_pim,*p_d0,*p_d0b;
    EvtVector4R p4_b,p4_dstp,p4_dstm,p4_pip,p4_pim,p4_d0,p4_d0b;
    
    p_b=root_part;
    
    p_dstp=p_b->getDaug(0);
    p_dstm=p_b->getDaug(1);
    
    p_pip=p_dstp->getDaug(1);
    p_pim=p_dstm->getDaug(1);

    p_d0=p_dstp->getDaug(0);
    p_d0b=p_dstm->getDaug(0);
    
    p4_b=p_b->getP4Lab();
    p4_dstp=p_dstp->getP4Lab();
    p4_dstm=p_dstm->getP4Lab();
    p4_pip=p_pip->getP4Lab();
    p4_pim=p_pim->getP4Lab();
    p4_d0=p_d0->getP4Lab();
    p4_d0b=p_d0b->getP4Lab();
    
    double costhpip=EvtDecayAngle(p4_b,p4_pip+p4_d0,p4_pip);
    double costhpim=EvtDecayAngle(p4_b,p4_pim+p4_d0b,p4_pim);
    double chiang=EvtDecayAngleChi(p4_b,p4_pip,p4_d0,p4_pim,p4_d0b);
    
    cospip->Fill( costhpip );
    cospim->Fill( costhpim );
    chi->Fill( chiang );
    if (costhpip>0) chicospipp->Fill( chiang );
    if (costhpip<0) chicospipn->Fill( chiang );
    
    if (costhpip>0 && costhpim>0) chipp->Fill( chiang );
    if (costhpip>0 && costhpim<0) chipn->Fill( chiang );
    if (costhpip<0 && costhpim>0) chinp->Fill( chiang );
    if (costhpip<0 && costhpim<0) chinn->Fill( chiang );
    if (costhpip<-0.5 && costhpim<-0.5) chinnnn->Fill( chiang );
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}


void runPartWave(int nevent, EvtGen &myGenerator) {

  TFile *file=new TFile("partwave.root", "RECREATE");
  
  TH1F* cospip = new TH1F("h1","cos theta pi+",
					 50,-1.0,1.0); 
  TH1F* cospim = new TH1F("h2","cos theta pi-",
					 50,-1.0,1.0); 
  TH1F* chi = new TH1F("h3","chi pi+ to pi- in D+ direction",
				      50,0.0,EvtConst::twoPi); 
  TH1F* chicospipp = new TH1F("h4","chi pi+ to pi- in D+ direction (cospip>0)",
					     50,0.0,EvtConst::twoPi); 
  TH1F* chicospipn = new TH1F("h5","chi pi+ to pi- in D+ direction (cospip<0",
					     50,0.0,EvtConst::twoPi); 
  
  TH1F* chipp = new TH1F("h6","chi pi+ to pi- in D+ direction (cospip>0,cospim>0)",
					50,0.0,EvtConst::twoPi); 
  
  TH1F* chipn = new TH1F("h7","chi pi+ to pi- in D+ direction (cospip>0,cospim<0)",
					50,0.0,EvtConst::twoPi); 
  
  TH1F* chinp = new TH1F("h8","chi pi+ to pi- in D+ direction (cospip<0,cospim>0)",
					50,0.0,EvtConst::twoPi); 
  
  TH1F* chinn = new TH1F("h9","chi pi+ to pi- in D+ direction (cospip<0,cospim<0)",
					50,0.0,EvtConst::twoPi); 
  
  
  TH1F* chinnnn = new TH1F("h10","chi pi+ to pi- in D+ direction (cospip<-0.5,cospim<-0.5)",
					  50,0.0,EvtConst::twoPi); 
  
  int count=1;
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/PARTWAVE.DEC");
  myGenerator.readUDecay(udecay_name);

  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));
  
  do{
    EvtVector4R p_init(EvtPDL::getMass(B0),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(B0,
							       p_init);
    root_part->setDiagonalSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    EvtParticle *p_b,*p_dstp,*p_dstm,*p_pip,*p_pim,*p_d0,*p_d0b;
    EvtVector4R p4_b,p4_dstp,p4_dstm,p4_pip,p4_pim,p4_d0,p4_d0b;
    
    p_b=root_part;
    
    p_dstp=p_b->getDaug(0);
    p_dstm=p_b->getDaug(1);
    
    p_pip=p_dstp->getDaug(1);
    p_pim=p_dstm->getDaug(1);

    p_d0=p_dstp->getDaug(0);
    p_d0b=p_dstm->getDaug(0);
    
    p4_b=p_b->getP4Lab();
    p4_dstp=p_dstp->getP4Lab();
    p4_dstm=p_dstm->getP4Lab();
    p4_pip=p_pip->getP4Lab();
    p4_pim=p_pim->getP4Lab();
    p4_d0=p_d0->getP4Lab();
    p4_d0b=p_d0b->getP4Lab();
    
    double costhpip=EvtDecayAngle(p4_b,p4_pip+p4_d0,p4_pip);
    double costhpim=EvtDecayAngle(p4_b,p4_pim+p4_d0b,p4_pim);
    double chiang=EvtDecayAngleChi(p4_b,p4_pip,p4_d0,p4_pim,p4_d0b);
    
    cospip->Fill( costhpip );
    cospim->Fill( costhpim );
    chi->Fill( chiang );
    if (costhpip>0) chicospipp->Fill( chiang );
    if (costhpip<0) chicospipn->Fill( chiang );
    
    if (costhpip>0 && costhpim>0) chipp->Fill( chiang );
    if (costhpip>0 && costhpim<0) chipn->Fill( chiang );
    if (costhpip<0 && costhpim>0) chinp->Fill( chiang );
    if (costhpip<0 && costhpim<0) chinn->Fill( chiang );
    if (costhpip<-0.5 && costhpim<-0.5) chinnnn->Fill( chiang );
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}

void runPartWave2(int nevent, EvtGen &myGenerator) {

  TFile file("partwave2.root", "RECREATE");
  
  TH1F* cthetapi = new TH1F("h1","cos theta pi",
					 50,-1.0,1.0); 

  TH1F* cthetapi2 = new TH1F("h2","cos theta pi (|cosrho|<0.1)",
					 50,-1.0,1.0); 


  TH1F* cthetan = new TH1F("h3","cos thetan",
					 50,-1.0,1.0); 


  //TH1F* cthetan2 = new TH1F("h4","cos thetan costhetapi>0 ",
  //					 50,-1.0,1.0); 

  TH1F* cthetarho = new TH1F("h4","cos thetarho ",
			     50,-1.0,1.0); 
  int count=1;
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/PARTWAVE2.DEC");
  myGenerator.readUDecay(udecay_name);

  static EvtId JPSI=EvtPDL::getId(std::string("J/psi"));
  static EvtId B0=EvtPDL::getId(std::string("B0"));
  
  do{
    EvtVector4R p_init(EvtPDL::getMass(B0),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(B0,
							       p_init);
    root_part->setDiagonalSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    EvtParticle *p_b,*p_jpsi,*p_rho,*p_pi1,*p_pi2;
    EvtVector4R p4_b,p4_jpsi,p4_rho,p4_pi1,p4_pi2;
    
    p_b=root_part;

    p_jpsi=root_part->getDaug(0);

    p_rho=0;

    if (p_jpsi->getDaug(0)->getNDaug()==2){
      p_rho=p_jpsi->getDaug(0);
    }
    
    if (p_jpsi->getDaug(1)->getNDaug()==2){
      p_rho=p_jpsi->getDaug(1);
    }

    assert(p_rho!=0);
    
    p_pi1=p_rho->getDaug(0);
    p_pi2=p_rho->getDaug(1);

    p4_b=p_b->getP4Lab();
    p4_jpsi=p_jpsi->getP4Lab();
    p4_rho=p_rho->getP4Lab();
    p4_pi1=p_pi1->getP4Lab();
    p4_pi2=p_pi2->getP4Lab();

    double costhetan=EvtDecayPlaneNormalAngle(p4_b,p4_jpsi,p4_pi1,p4_pi2);

    //EvtGenReport(EVTGEN_INFO,"EvtGen") << "costhetan:"<<costhetan<<std::endl;

    cthetan->Fill( costhetan );

    double costhpi=EvtDecayAngle(p4_jpsi,p4_rho,p4_pi1);

    double costhrho=EvtDecayAngle(p4_b,p4_jpsi,p4_rho);

    //EvtGenReport(EVTGEN_INFO,"EvtGen") << "costhetarho:"<<costhrho<<std::endl;

    cthetarho->Fill( costhrho );

    //if (((p4_rho.get(3)/p4_rho.d3mag()))<-0.95) cthetan2->Fill( costhetan );

    cthetapi->Fill( costhpi );
    
    if ((p4_rho.get(3)/p4_rho.d3mag())>0.9) {
      cthetapi2->Fill( costhpi );
    }
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  file.Write(); file.Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}

void runTwoBody(int nevent, EvtGen &myGenerator, std::string decFile,
		std::string rootFile) {

  TFile *file=new TFile(rootFile.c_str(), "RECREATE");
  
  int count=0;
  
  myGenerator.readUDecay(decFile.c_str());

  static EvtId B0=EvtPDL::getId(std::string("B0"));

  vector<TH1F*> histograms;
  
  do{
    EvtVector4R p_init(EvtPDL::getMass(B0),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(B0,
							       p_init);
    root_part->setDiagonalSpinDensity();      

    myGenerator.generateDecay(root_part);

    //root_part->printTree();

    myGenerator.generateDecay(root_part);
    int nhist=0;

    EvtParticle* p=root_part;
    
    do {

      int nDaug=p->getNDaug();
      
      if (!(nDaug==0||nDaug==2)) {
	EvtGenReport(EVTGEN_INFO,"EvtGen") << "nDaug="<<nDaug<<" but can only handle 0 or 2!"<<std::endl;
	abort();
      }

      if (nDaug==2){
	if (p->getParent()==0){
	  EvtVector4R p4=p->getDaug(0)->getP4();
	  double ctheta=p4.get(3)/p4.d3mag();
	  double phi=atan2(p4.get(2),p4.get(1));
	  if (count==0){
	    histograms.push_back(new TH1F("h1","cos theta",50,-1.0,1.0));
	    histograms.push_back(new TH1F("h2","phi",50,-EvtConst::pi
					  ,EvtConst::pi));
	  }
	  histograms[nhist++]->Fill(ctheta);
	  histograms[nhist++]->Fill(phi);
	}
	else{
	  double ctheta=EvtDecayAngle(p->getParent()->getP4Lab(),
				      p->getP4Lab(),
				      p->getDaug(0)->getP4Lab());
	  if (count==0){
//	    char* tmp=new char[10];
//	    std::ostrstream strm(tmp,9);
//	    strm  << (nhist+1) << '\0'<< std::endl;
//	    histograms.push_back(new TH1F(TString("h")+tmp,TString("cos theta")+tmp,50,-1.0,1.0));
        std::ostringstream strm;
        strm  << (nhist+1);
        histograms.push_back( new TH1F(TString("h") + strm.str(),
                                       TString("cos theta") + strm.str(),
                                       50,-1.0,1.0) );
	  }
	  histograms[nhist++]->Fill(ctheta);
	  if (p->getDaug(0)->getNDaug()==2) {
	    double costhetan=EvtDecayPlaneNormalAngle(p->getParent()->getP4Lab(),
						      p->getP4Lab(),
						      p->getDaug(0)->getDaug(0)->getP4Lab(),
						      p->getDaug(0)->getDaug(1)->getP4Lab());
	    if (count==0){
//	      char* tmp=new char[10];
//	      std::ostrstream strm(tmp,9);
//	      strm  << (nhist+1) << '\0'<< std::endl;
//	      histograms.push_back(new TH1F(TString("h")+tmp,TString("cos thetan")+tmp,50,-1.0,1.0));
          std::ostringstream strm;
          strm  << (nhist+1);
          histograms.push_back( new TH1F(TString("h") + strm.str(),
                                         TString("cos theta") + strm.str(),
                                         50,-1.0,1.0) );
	    }
	    histograms[nhist++]->Fill(costhetan);
	  }
	  if (p->getDaug(1)->getNDaug()==2) {
	    double costhetan=EvtDecayPlaneNormalAngle(p->getParent()->getP4Lab(),
						      p->getP4Lab(),
						      p->getDaug(1)->getDaug(0)->getP4Lab(),
						      p->getDaug(1)->getDaug(1)->getP4Lab());
	    if (count==0){
//	      char* tmp=new char[10];
//	      std::ostrstream strm(tmp,9);
//	      strm  << (nhist+1) << '\0'<< std::endl;
//	      histograms.push_back(new TH1F(TString("h")+tmp,TString("cos thetan")+tmp,50,-1.0,1.0));
          std::ostringstream strm;
          strm  << (nhist+1);
          histograms.push_back( new TH1F(TString("h") + strm.str(),
                                         TString("cos theta") + strm.str(),
                                         50,-1.0,1.0) );
	    }
	    histograms[nhist++]->Fill(costhetan);
	  }
	}
      }

      p=p->nextIter(root_part);
      
    }while(p!=0);

    root_part->deleteTree();      

  }while (count++<nevent);
  
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}


void runPiPi(int nevent, EvtGen &myGenerator) {
  std::ofstream outmix;
  outmix.open("pipi.dat");
  
  TFile *file=new TFile("pipi.root", "RECREATE");
  
  TH1F* tB0Hist = new TH1F("h1","dt in B->pipi with B0 tag",50,-5.0,5.0); 
  TH1F* tB0BHist = new TH1F("h2","dt in B->pipi with B0B tag",50,-5.0,5.0); 
  
  TH1F* tB0 = new TH1F("h3","t in B->pipi for B0 tag",25,0.0,5.0); 
  TH1F* tB0B = new TH1F("h4","t in B->pipi for B0B tag",25,0.0,5.0); 
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/PIPI.DEC");
  //EvtGen myGenerator(decay_name,pdttable_name,myRandomEngine);
  myGenerator.readUDecay(udecay_name);
  
  EvtParticle *bcp,*btag;
  
  int count=1;

  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId VPHO=EvtPDL::getId(std::string("vpho"));
  static EvtId GAMM=EvtPDL::getId(std::string("gamma"));
  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));
  
  do{
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(UPS4,
							       p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    if (root_part->getDaug(0)->getNDaug()==3){
      btag=root_part->getDaug(0);
      bcp=root_part->getDaug(1);
    }
    else{
      bcp=root_part->getDaug(0);
      btag=root_part->getDaug(1);
    }
    
    EvtId tag; //cp tag
    
    if (btag->getId()==B0B){
      tag=B0;
    }
    else{tag=B0B;
    }
    
    //  int a1=bcp->getDaug(0)->getId();    
    
    if (tag==B0) tB0Hist->Fill( bcp->getLifetime() - btag->getLifetime() );
    if (tag==B0) tB0->Fill( btag->getLifetime() );
    if (tag==B0B) tB0BHist->Fill( bcp->getLifetime() - btag->getLifetime() );
    if (tag==B0B) tB0B->Fill( btag->getLifetime() );
    
    outmix << bcp->getLifetime() << " " <<
      btag->getLifetime() << " " << tag.getId() << std::endl;
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  outmix.close();
  file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}

void runA1Pi(int nevent, EvtGen &myGenerator) {

  std::ofstream outmix;
  outmix.open("a1pi.dat");
  
  int count=1;
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/A1PI.DEC");
  myGenerator.readUDecay(udecay_name);
  
  EvtParticle *bcp,*btag;
  EvtParticle *a1,*rho0,*pi1,*pi2,*pi3,*pi4;
  EvtVector4R p4bcp,p4a1,p4rho0,p4pi1,p4pi2,p4pi3,p4pi4;
  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));
  
  do{
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(UPS4,
							       p_init);
    root_part->setVectorSpinDensity();      
    myGenerator.generateDecay(root_part);
    
    if (root_part->getDaug(0)->getNDaug()==3){
      btag=root_part->getDaug(0);
      bcp=root_part->getDaug(1);
    }
    else{
      bcp=root_part->getDaug(0);
      btag=root_part->getDaug(1);
    }
    
    a1=bcp->getDaug(0);
    pi1=bcp->getDaug(1);
    
    rho0=a1->getDaug(0);
    pi2=a1->getDaug(1);
    
    pi3=rho0->getDaug(0);
    pi4=rho0->getDaug(1);
    
    p4bcp=bcp->getP4Lab();
    p4a1=a1->getP4Lab();
    p4pi1=pi1->getP4Lab();
    
    p4rho0=rho0->getP4Lab();
    p4pi2=pi2->getP4Lab();
    
    p4pi3=pi3->getP4Lab();
    p4pi4=pi4->getP4Lab();
    
    EvtId tag; //cp tag
    
    if (btag->getId()==B0B){
      tag=B0;
    }
    else{
      tag=B0B;
    }
    
    outmix << bcp->getLifetime() << " " <<
      btag->getLifetime() << " " << 
      EvtDecayAngle(p4bcp,p4rho0+p4pi2,p4rho0) << " "<<
      EvtDecayAngle(p4a1,p4pi3+p4pi4,p4pi3) << " "<<
      EvtPDL::getStdHep(tag) << std::endl;
    
    root_part->deleteTree();  
    
  }while (count++<1000);
  
  outmix.close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
}

void runCPTest(int nevent, EvtGen &myGenerator) {

  std::ofstream outmix;
  outmix.open("cptest.dat");
  
  int count=1;
  
  static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
  static EvtId B0=EvtPDL::getId(std::string("B0"));
  static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/CPTEST.DEC");
  //EvtGen myGenerator(decay_name,pdttable_name,myRandomEngine);
  myGenerator.readUDecay(udecay_name);
  
  EvtParticle *bcp,*btag;
  
  do{
    
    EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
    
    EvtParticle* root_part=EvtParticleFactory::particleFactory(UPS4,
							       p_init);
    root_part->setVectorSpinDensity();      
    
    myGenerator.generateDecay(root_part);
    
    if (root_part->getDaug(0)->getNDaug()==3){
      btag=root_part->getDaug(0);
      bcp=root_part->getDaug(1);
    }
    else{
      bcp=root_part->getDaug(0);
      btag=root_part->getDaug(1);
    }
    
    EvtId tag; //cp tag
    
    if (btag->getId()==B0B){
      tag=B0;
    }
    else{
      tag=B0B;
    }
    
    outmix << bcp->getLifetime() << " " <<
      btag->getLifetime() << " " << tag.getId() << std::endl;
    
    root_part->deleteTree();  
    
  }while (count++<nevent);
  
  outmix.close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}


void runBtoXsgamma(int nevent, EvtGen &myGenerator) {

  static EvtId UPS4 = EvtPDL::getId(std::string("Upsilon(4S)"));
  TFile *file=new TFile("BtoXsgamma.root", "RECREATE");
  
  int count=1;
  
  EvtParticle* root_part;
  EvtVectorParticle *vector_part;
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/BTOXSGAMMA.DEC");
  
  myGenerator.readUDecay(udecay_name);
  
 // Plot kinematics for b->s,gamma
  int strangeid,antistrangeid;
  int Bmulti,bId1a,bId1b,bId2a,bId2b,b1Id,b2Id;
  do{
      vector_part = new EvtVectorParticle;
      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
      
      vector_part->init(UPS4,p_init);
      
      root_part = (EvtParticle *)vector_part;
      root_part->setVectorSpinDensity();      
      
      myGenerator.generateDecay(root_part);
      
      EvtParticle *B1 = root_part->getDaug(0);
      Bmulti= B1->getNDaug();
      if (Bmulti==1) B1 = B1->getDaug(0); 
      EvtId BId1a = B1->getDaug(0)->getId();
      bId1a = EvtPDL::getStdHep(BId1a);
      EvtId BId1b = B1->getDaug(1)->getId();
      bId1b = EvtPDL::getStdHep(BId1b);

      if (Bmulti==1) EvtGenReport(EVTGEN_INFO,"EvtGen") << "B1" << " bId1a=" << bId1a << " bId1b=" << bId1b << " ndaug=" << B1->getNDaug() << " Bid=" << EvtPDL::getStdHep(B1->getId()) << std::endl;

      EvtParticle *B2 = root_part->getDaug(1);
      Bmulti= B2->getNDaug();
      if (Bmulti==1) B2 = B2->getDaug(0); // B has a daughter which is a string
      EvtId BId2a = B2->getDaug(0)->getId();
      bId2a = EvtPDL::getStdHep(BId2a);
      EvtId BId2b = B2->getDaug(1)->getId();
      bId2b = EvtPDL::getStdHep(BId2b);

     if (Bmulti==1) EvtGenReport(EVTGEN_INFO,"EvtGen") << "B2" << " bId2a=" << bId2a << " bId2b=" << bId2b << " ndaug=" << B2->getNDaug() << " Bid=" << EvtPDL::getStdHep(B2->getId()) << std::endl;

      EvtId B1Id = B1->getId();
      b1Id = EvtPDL::getStdHep(B1Id);
      EvtId B2Id = B2->getId();
      b2Id = EvtPDL::getStdHep(B2Id);

      strangeid=0;
      antistrangeid=0;
      if ((b1Id == 511) || (b1Id == -511) || (b2Id == 511) || (b2Id == -511)) {
        strangeid=30343; antistrangeid=-30343;
      } else if ((b1Id == 521) || (b1Id == -521) || (b2Id == 521) || (b2Id == -521)) {
        strangeid=30353; antistrangeid=-30353;
      } else if ((b1Id == 531) || (b1Id == -531) || (b2Id == 531) || (b2Id == -531)) {
        strangeid=30363; antistrangeid=-30363;
      }
      EvtGenReport(EVTGEN_INFO,"EvtGen") << "bId1a "<<bId1a<<" bId1b "<<bId1b<<" bId2a "<<bId2a<<" bId2b "<<bId2b<<" for event "<<count<<std::endl;

      EvtParticle *Bpeng = 0;
      int bnum=0;
      int pengcount=0;
      if (((bId1a == strangeid) && (bId1b == 22)) || ((bId1a == antistrangeid) && (bId1b == 22))|| ((bId1b == strangeid) && (bId1a == 22)) || ((bId1b == antistrangeid) && (bId1a == 22))) {
        Bpeng = B1;
        bnum=1;
        pengcount++;
      } 
      if (((bId2a == strangeid) && (bId2b == 22)) || ((bId2a == antistrangeid) && (bId2b == 22)) || ((bId2b == strangeid) && (bId2a == 22)) || ((bId2b == antistrangeid) && (bId2a == 22))) {
        Bpeng = B2;
        bnum=2;
        pengcount++;
      }
      if (pengcount == 0) {
        Bpeng=B1;
        EvtGenReport(EVTGEN_INFO,"EvtGen") << "No penguin decay for event "<<count<<std::endl;
        bnum=0;
      } else if (pengcount == 2) {
        Bpeng=B1;
        EvtGenReport(EVTGEN_INFO,"EvtGen") << "Two penguin decays in event "<<count<<std::endl;
        bnum=0;        
      }
      Bmulti = Bpeng->getNDaug();
      EvtParticle *Xs = Bpeng->getDaug(0);
      EvtParticle *gam = Bpeng->getDaug(1);

      EvtVector4R p4Xs = Xs->getP4Lab();

      EvtId BId = Bpeng->getId();

      EvtId XsId = Xs->getId();
      int Xsmulti = Xs->getNDaug();
      EvtId gamId = gam->getId();
      
      //int bId = EvtPDL::getStdHep(BId);
      //int XId = EvtPDL::getStdHep(XsId);
      //int gId = EvtPDL::getStdHep(gamId);
      
      //float XsMass = p4Xs.mass();
      //double gmass = p4gam.mass();
      //double genergy = p4gam.get(0);
      
// debug stuff:      EvtGenReport(EVTGEN_INFO,"EvtGen") << "bnum=" << bnum << " pengcount=" << pengcount << " bId=" << bId << " Bmulti=" << Bmulti << " XsId=" << XId << " gId=" << gId << std::endl;

      //need to change this to root...I don't have the energy now
      //tuple->column("bnum", bnum);
      //tuple->column("pengcount", pengcount);
      //tuple->column("bId", bId);
      //tuple->column("Bmulti", Bmulti);
      //tuple->column("XsId", XId);
      //tuple->column("gId", gId);
      //tuple->column("XsMass", XsMass);
      //tuple->column("Xsmulti", Xsmulti, 0,"Xs", HTRange<int>(0,200));
      //tuple->column("gmass", gmass);
      //tuple->column("genergy", genergy);
      //HTValOrderedVector<int> XDaugId, XDaugNephewId;
      //HTValOrderedVector<float> XsDaugMass, XsDaugNephewMass;
      int nTot(0);
      for(int i=0;i<Xsmulti;i++){
        EvtParticle *XsDaug = Xs->getDaug(i);
        EvtVector4R p4XsDaug = XsDaug->getP4Lab();
        EvtId XsDaugId = XsDaug->getId();
        //XDaugId.push_back(EvtPDL::getStdHep(XsDaugId));
        //XsDaugMass.push_back( p4XsDaug.mass());
	
	int Daumulti = XsDaug->getNDaug();
	if(abs(EvtPDL::getStdHep(XsDaugId))==321||EvtPDL::getStdHep(XsDaugId)==310||EvtPDL::getStdHep(XsDaugId)==111||abs(EvtPDL::getStdHep(XsDaugId))==211||Daumulti==0){
	  nTot++;
	  EvtVector4R p4XsDaugNephew = XsDaug->getP4Lab();
	  EvtId XsDaugNephewId =XsDaug->getId() ;
	  //XDaugNephewId.push_back(EvtPDL::getStdHep(XsDaugId));
	  //XsDaugNephewMass.push_back( p4XsDaug.mass());

	}else if(Daumulti!=0){

	  for(int k=0;k<Daumulti;k++){
	    EvtParticle *XsDaugNephew = XsDaug->getDaug(k);
	    EvtId XsDaugNephewId = XsDaugNephew->getId();
	    int Nephmulti = XsDaugNephew->getNDaug();
	    
	    if(Nephmulti==0||abs(EvtPDL::getStdHep(XsDaugNephewId))==321||EvtPDL::getStdHep(XsDaugNephewId)==310||EvtPDL::getStdHep(XsDaugNephewId)==111||abs(EvtPDL::getStdHep(XsDaugNephewId))==211) {
	      nTot++;
	      EvtVector4R p4XsDaugNephew = XsDaugNephew->getP4Lab();
	      //XDaugNephewId.push_back(EvtPDL::getStdHep(XsDaugNephewId));
	      //XsDaugNephewMass.push_back( p4XsDaugNephew.mass());
	    }else{
	      for(int g=0;g<Nephmulti;g++){
		nTot++;
		EvtParticle *XsDaugNephewNephew = XsDaugNephew->getDaug(g);
		EvtVector4R p4XsDaugNephewNephew = XsDaugNephewNephew->getP4Lab();
		EvtId XsDaugNephewNephewId = XsDaugNephewNephew->getId();
		//XDaugNephewId.push_back(EvtPDL::getStdHep(XsDaugNephewNephewId));
		//XsDaugNephewMass.push_back( p4XsDaugNephewNephew.mass());
	      }
	    }	    
	  }
	} 
      }
      //tuple->column("XsDaugId", XDaugId,"Xsmulti", 0, "Xs");
      //tuple->column("XsDaugMass", XsDaugMass,"Xsmulti", 0, "Xs");
      
      //tuple->column("nTot", nTot, 0,"nTot", HTRange<int>(0,200));
      //tuple->column("XsDaugNephewId", XDaugNephewId,"nTot", 0, "nTot");
      //tuple->column("XsDaugNephewMass", XsDaugNephewMass,"nTot", 0, "nTot");
      
      //tuple->dumpData();
      
      root_part->deleteTree();
    
  }while (count++<nevent);
  
  file->Write(); file->Close();

  EvtGenReport(EVTGEN_INFO,"EvtGen")<<"End EvtGen. Ran on "<<nevent<<" events."<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}

void runBtoK1273gamma(int nevent, EvtGen &myGenerator) {

  static EvtId UPS4 = EvtPDL::getId(std::string("Upsilon(4S)"));
  TFile *file=new TFile("BtoK1273gamma.root", "RECREATE");  
  //HepTuple *tuple = hfile.ntuple("BtoK1273gamma", 1);
  
  int count=1;
  
  EvtParticle *root_part;
  EvtVectorParticle *vector_part;
  
  char udecay_name[100];
  strcpy(udecay_name,"exampleFiles/BTOK1273GAMMA.DEC");
  
  myGenerator.readUDecay(udecay_name);
  
 // Plot kinematics for b->s,gamma
  int strangeid,antistrangeid;
  int Bmulti,bId1a,bId1b,bId2a,bId2b,b1Id,b2Id;
  do{
      vector_part = new EvtVectorParticle;
      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
      
      vector_part->init(UPS4,p_init);
      
      root_part = (EvtParticle *)vector_part;
      root_part->setVectorSpinDensity();      
      
      myGenerator.generateDecay(root_part);
      
      EvtParticle *B1 = root_part->getDaug(0);
      Bmulti= B1->getNDaug();
      if (Bmulti==1) B1 = B1->getDaug(0); 
      EvtId BId1a = B1->getDaug(0)->getId();
      bId1a = EvtPDL::getStdHep(BId1a);
      EvtId BId1b = B1->getDaug(1)->getId();
      bId1b = EvtPDL::getStdHep(BId1b);

      if (Bmulti==1) EvtGenReport(EVTGEN_INFO,"EvtGen") << "B1" << " bId1a=" << bId1a << " bId1b=" << bId1b << " ndaug=" << B1->getNDaug() << " Bid=" << EvtPDL::getStdHep(B1->getId()) << std::endl;

      EvtParticle *B2 = root_part->getDaug(1);
      Bmulti= B2->getNDaug();
      if (Bmulti==1) B2 = B2->getDaug(0); // B has a daughter which is a string
      EvtId BId2a = B2->getDaug(0)->getId();
      bId2a = EvtPDL::getStdHep(BId2a);
      EvtId BId2b = B2->getDaug(1)->getId();
      bId2b = EvtPDL::getStdHep(BId2b);

     if (Bmulti==1) EvtGenReport(EVTGEN_INFO,"EvtGen") << "B2" << " bId2a=" << bId2a << " bId2b=" << bId2b << " ndaug=" << B2->getNDaug() << " Bid=" << EvtPDL::getStdHep(B2->getId()) << std::endl;

      EvtId B1Id = B1->getId();
      b1Id = EvtPDL::getStdHep(B1Id);
      EvtId B2Id = B2->getId();
      b2Id = EvtPDL::getStdHep(B2Id);

      strangeid=0;
      antistrangeid=0;
      if ((b1Id == 511) || (b1Id == -511) || (b2Id == 511) || (b2Id == -511)) {
        strangeid=10313; antistrangeid=-10313;
      } else if ((b1Id == 521) || (b1Id == -521) || (b2Id == 521) || (b2Id == -521)) {
        strangeid=10323; antistrangeid=-10323;
      }
      EvtGenReport(EVTGEN_INFO,"EvtGen") << "bId1a "<<bId1a<<" bId1b "<<bId1b<<" bId2a "<<bId2a<<" bId2b "<<bId2b<<" for event "<<count<<std::endl;

      EvtParticle *Bpeng = 0;
      int bnum=0;
      int pengcount=0;
      if (((bId1a == strangeid) && (bId1b == 22)) || ((bId1a == antistrangeid) && (bId1b == 22))|| ((bId1b == strangeid) && (bId1a == 22)) || ((bId1b == antistrangeid) && (bId1a == 22))) {
        Bpeng = B1;
        bnum=1;
        pengcount++;
      } 
      if (((bId2a == strangeid) && (bId2b == 22)) || ((bId2a == antistrangeid) && (bId2b == 22)) || ((bId2b == strangeid) && (bId2a == 22)) || ((bId2b == antistrangeid) && (bId2a == 22))) {
        Bpeng = B2;
        bnum=2;
        pengcount++;
      }
      if (pengcount == 0) {
        Bpeng=B1;
        EvtGenReport(EVTGEN_INFO,"EvtGen") << "No penguin decay for event "<<count<<std::endl;
        bnum=0;
      } else if (pengcount == 2) {
        Bpeng=B1;
        EvtGenReport(EVTGEN_INFO,"EvtGen") << "Two penguin decays in event "<<count<<std::endl;
        bnum=0;        
      }
      Bmulti = Bpeng->getNDaug();
      EvtParticle *Ks = Bpeng->getDaug(0);
      //EvtParticle *gam = Bpeng->getDaug(1);

      EvtVector4R p4Ks = Ks->getP4Lab();
      //const  EvtVector4R& p4gam = gam->getP4(); // gamma 4-mom in parent's rest frame

      //EvtId BId = Bpeng->getId();

      //EvtId KsId = Ks->getId();
      int Ksmulti = Ks->getNDaug();
      //EvtId gamId = gam->getId();
      
      //int bId = EvtPDL::getStdHep(BId);
      //int XId = EvtPDL::getStdHep(KsId);
      //int gId = EvtPDL::getStdHep(gamId);
      
      //double KsMass = p4Ks.mass();
      //double gmass = p4gam.mass();
      //double genergy = p4gam.get(0);
      
// debug stuff:      EvtGenReport(EVTGEN_INFO,"EvtGen") << "bnum=" << bnum << " pengcount=" << pengcount << " bId=" << bId << " Bmulti=" << Bmulti << " KsId=" << XId << " gId=" << gId << std::endl;

      //tuple->column("bnum", bnum);
      //tuple->column("pengcount", pengcount);
      //tuple->column("bId", bId);
      //tuple->column("Bmulti", Bmulti);
      //tuple->column("KsId", XId);
      //tuple->column("gId", gId);
      //tuple->column("KsMass", KsMass);
      //tuple->column("Ksmulti", Ksmulti);
      //tuple->column("gmass", gmass);
      //tuple->column("genergy", genergy);

      for(int i=0;i<Ksmulti;i++){
        EvtParticle *KsDaug = Ks->getDaug(i);
        EvtVector4R p4KsDaug = KsDaug->getP4Lab();
        EvtId KsDaugId = KsDaug->getId();
        //int XDaugId = EvtPDL::getStdHep(KsDaugId);
        //double KsDaugMass = p4KsDaug.mass();
        //tuple->column("KsDaugId", XDaugId);
        //tuple->column("KsDaugMass", KsDaugMass);
      }

      //tuple->dumpData();
      
      root_part->deleteTree();
    
  }while (count++<nevent);
  
  file->Write(); file->Close();

  EvtGenReport(EVTGEN_INFO,"EvtGen")<<"End EvtGen. Ran on "<<nevent<<" events."<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
  
}


void runCheckRotBoost(){
  
  EvtDiracSpinor sp1,sp2;
  //Generate ryd/lange random spinors.
  sp1.set(EvtComplex(1.0,-2.0),
	  EvtComplex(3.0,1.0),
	  EvtComplex(-4.5,0.5),
	  EvtComplex(0.2,-0.5));
  sp2.set(EvtComplex(0.1,-1.0),
	  EvtComplex(1.2,-0.5),
	  EvtComplex(3.6,1.8),
	  EvtComplex(-0.2,-0.6));
  
  EvtComplex s=EvtLeptonSCurrent(sp1,sp2);
  EvtComplex p=EvtLeptonPCurrent(sp1,sp2);
  EvtVector4C a=EvtLeptonACurrent(sp1,sp2);
  EvtVector4C v=EvtLeptonVCurrent(sp1,sp2);
  EvtVector4C va=EvtLeptonVACurrent(sp1,sp2);
  EvtTensor4C t=EvtLeptonTCurrent(sp1,sp2);
  
  //start with boosts...
  EvtVector4R ranBoost(2.0,0.4,-0.8,0.3);
  
  EvtDiracSpinor sp1Boost=boostTo(sp1,ranBoost);
  EvtDiracSpinor sp2Boost=boostTo(sp2,ranBoost);
  
  EvtComplex sBoost=EvtLeptonSCurrent(sp1Boost,sp2Boost);
  EvtComplex pBoost=EvtLeptonPCurrent(sp1Boost,sp2Boost);
  EvtVector4C aBoost=EvtLeptonACurrent(sp1Boost,sp2Boost);
  EvtVector4C vBoost=EvtLeptonVCurrent(sp1Boost,sp2Boost);
  EvtVector4C vaBoost=EvtLeptonVACurrent(sp1Boost,sp2Boost);
  EvtTensor4C tBoost=EvtLeptonTCurrent(sp1Boost,sp2Boost);

  EvtVector4C aDirBoost=boostTo(a,ranBoost);
  EvtVector4C vDirBoost=boostTo(v,ranBoost);
  EvtVector4C vaDirBoost=boostTo(va,ranBoost);
  EvtTensor4C tDirBoost(t);
  tDirBoost.applyBoostTo(ranBoost);
  
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Comparing after doing a random boost"<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Scalar "<<s<<" "<<sBoost<<s-sBoost<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "PseudoScalar "<<p<<" "<<pBoost<<p-pBoost<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "AxialVector "<<aDirBoost<<" "<<aBoost<<aDirBoost-aBoost<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Vector "<<vDirBoost<<" "<<vBoost<<vDirBoost-vBoost<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "V-A "<<vaDirBoost<<" "<<vaBoost<<vaDirBoost-vaBoost<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Tensor "<<tDirBoost<<" "<<tBoost<<tDirBoost-tBoost<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Done comparing after doing a random boost"<<std::endl;

  //Now do rotations...
  
  //start with boosts...
  double alpha=0.4;
  double beta=-0.61;
  double gamma=3.0;
  
  EvtDiracSpinor sp1Rot=rotateEuler(sp1,alpha,beta,gamma);
  EvtDiracSpinor sp2Rot=rotateEuler(sp2,alpha,beta,gamma);
  
  EvtComplex sRot=EvtLeptonSCurrent(sp1Rot,sp2Rot);
  EvtComplex pRot=EvtLeptonPCurrent(sp1Rot,sp2Rot);
  EvtVector4C aRot=EvtLeptonACurrent(sp1Rot,sp2Rot);
  EvtVector4C vRot=EvtLeptonVCurrent(sp1Rot,sp2Rot);
  EvtVector4C vaRot=EvtLeptonVACurrent(sp1Rot,sp2Rot);
  EvtTensor4C tRot=EvtLeptonTCurrent(sp1Rot,sp2Rot);
  
  EvtVector4C aDirRot(a);
  EvtVector4C vDirRot(v);
  EvtVector4C vaDirRot(va);
  EvtTensor4C tDirRot(t);
  aDirRot.applyRotateEuler(alpha,beta,gamma);
  vDirRot.applyRotateEuler(alpha,beta,gamma);
  vaDirRot.applyRotateEuler(alpha,beta,gamma);
  tDirRot.applyRotateEuler(alpha,beta,gamma);
  
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Comparing after doing a random rotation"<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Scalar "<<s<<" "<<sRot<<s-sRot<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "PseudoScalar "<<p<<" "<<pRot<<p-pRot<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "AxialVector "<<aDirRot<<" "<<aRot<<aDirRot-aRot<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Vector "<<vDirRot<<" "<<vRot<<vDirRot-vRot<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "V-A "<<vaDirRot<<" "<<vaRot<<vaDirRot-vaRot<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Tensor "<<tDirRot<<" "<<tRot<<tDirRot-tRot<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "Done comparing after doing a random rotation"<<std::endl;
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}

int countInclusive(std::string name, EvtParticle *root_part,TH1F* mom, TH1F* mass){

  EvtParticle *p = root_part;
  int temp=0;
  EvtId searchFor=EvtPDL::getId(name);

  do{
    EvtId type=p->getId();
    if (type==searchFor) { 
      temp+=1;
      if ( mom) mom->Fill(p->getP4Lab().d3mag());
      if ( mass) mass->Fill(p->mass());
      //if ( theBs.contains(p->getParent()->getId()) ) {
      //dirPsimom->Fill(p->getP4Lab().d3mag());
      //}	  
      //EvtGenReport(EVTGEN_INFO,"EvtGen") << "LANGE " << p->getP4Lab().d3mag() << " " << p->getP4Lab().get(3)/p->getP4Lab().d3mag() << std::endl;
    }
    
    p=p->nextIter(root_part);
    
  }while(p!=0);

  return temp;
}

int countInclusiveSubTree(std::string name, EvtParticle *root_part, 
			  EvtIdSet setIds,
			  TH1F* mom) {

  int temp=0;
  EvtParticle *p=root_part;
  do{
    if ( setIds.contains(p->getId()) ) {
      temp+=countInclusive(name,p);

    }

    //p->printTree();
    p=p->nextIter(root_part);
        
  }while(p!=0);
  //EvtGenReport(EVTGEN_INFO,"EvtGen") << "done"<<std::endl;
  return temp;
} 
   



int countInclusiveParent(std::string name, EvtParticle *root_part, 
			 EvtIdSet setIds,
			 TH1F* mom) {


  EvtParticle *p = root_part;
  int temp=0;

  EvtId searchFor=EvtPDL::getId(name);

  do{
    EvtId type=p->getId();
    if (type==searchFor) { 
      if ( p->getParent() ) {
	if ( setIds.contains(p->getParent()->getId()) ) {
	  temp+=1;
	  if ( mom) mom->Fill(p->getP4Lab().d3mag());
	}
      }
    }
    
    
    p=p->nextIter(root_part);
  }while(p!=0);
  
  return temp;

}


void runBMix(int nevent,EvtGen &myGenerator,std::string userFile,std::string rootFile) {

    TFile *file=new TFile(rootFile.c_str(), "RECREATE");

    TH1F* b0_b0 = new TH1F("h1","dt B0-B0",100,-15.0,15.0); 
    TH1F* b0b_b0b = new TH1F("h2","dt B0B-B0B",100,-15.0,15.0); 
    TH1F* b0b_b0 = new TH1F("h3","dt B0B-B0",100,-15.0,15.0); 
    TH1F* b0_b0b = new TH1F("h4","dt B0-B0B",100,-15.0,15.0); 

    int count=1;
  
    myGenerator.readUDecay(userFile.c_str());

    EvtId b0=EvtPDL::getId("B0");
    EvtId b0b=EvtPDL::getId("anti-B0");

    static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));

    std::ofstream outmix;
    TString outFileName(rootFile);
    outFileName.ReplaceAll(".root", ".dat");
    outmix.open(outFileName.Data());

    do{
      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
     
      EvtParticle *root_part=EvtParticleFactory::particleFactory(UPS4,
								 p_init);
      root_part->setVectorSpinDensity();      

      myGenerator.generateDecay(root_part);
    
      EvtId id1=root_part->getDaug(0)->getId();
      EvtId id2=root_part->getDaug(1)->getId();

      double t1=root_part->getDaug(0)->getLifetime();
      double t2=root_part->getDaug(1)->getLifetime();

      double dt=(t1-t2)/(1e-12*3e11);

      if (id1==b0&&id2==b0) { b0_b0->Fill(dt); outmix << dt << " 1"<<std::endl;}
      if (id1==b0b&&id2==b0b) {b0b_b0b->Fill(dt); outmix << dt << " 2"<<std::endl;}
      if (id1==b0b&&id2==b0) {b0b_b0->Fill(dt); outmix << dt << " 0"<<std::endl;}
      if (id1==b0&&id2==b0b) {b0_b0b->Fill(dt); outmix << dt << " 0"<<std::endl;}

      root_part->deleteTree();  

    }while (count++<nevent);

    file->Write(); file->Close();
    outmix.close();

    EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}




void runDDalitz(int nevent, EvtGen &myGenerator) {

    TFile *file=new TFile("ddalitz.root", "RECREATE");

    TH2F* dalitz  = new TH2F("h1","m^2!?[K-[p]+! vs m^2!?[K-[p]0!",
					    70,0.0,3.5,70,0.0,3.5); 
    TH2F* dalitz2  = new TH2F("h5","m^2!([p]^-![p]^0!) vs m^2!([K-[p]+!",
					    100,0.0,3.5,100,0.0,2.0); 

    TH1F* m12=new TH1F("h2","m?[K-[p]+!",100,0.0,3.0);
    TH1F* m13=new TH1F("h3","m?[K-[p]0!",100,0.0,3.0);
    TH1F* m23=new TH1F("h4","m?[[p]+[p]0!",100,0.0,2.0);


    int count;
  
    myGenerator.readUDecay("exampleFiles/DDALITZ.DEC");
    count=1;

    static EvtId D0=EvtPDL::getId(std::string("D0"));

    std::ofstream outmix;
    outmix.open("ddalitz.dat");

    
    do{
    
      EvtVector4R p_init(EvtPDL::getMass(D0),0.0,0.0,0.0);
     
      EvtParticle *root_part=EvtParticleFactory::particleFactory(D0,
								 p_init);
      root_part->setDiagonalSpinDensity();      

      myGenerator.generateDecay(root_part);

      EvtVector4R p1=root_part->getDaug(0)->getP4Lab();
      EvtVector4R p2=root_part->getDaug(1)->getP4Lab();
      EvtVector4R p3=root_part->getDaug(2)->getP4Lab();

      dalitz->Fill((p1+p2).mass2(),(p1+p3).mass2(),1.0);
      dalitz2->Fill((p1+p2).mass2(),(p2+p3).mass2(),1.0);

      m12->Fill((p1+p2).mass2());
      m13->Fill((p1+p3).mass2());
      m23->Fill((p2+p3).mass2());
      outmix << (p1+p2).mass2() << " "<<(p2+p3).mass2()<<" "<<(p1+p3).mass2()<<std::endl;
      root_part->deleteTree();  

      if(count == nevent-1) {
        std::ofstream testi("testi.dat");
        double val = m12->GetMean();
        double errval = m12->GetMeanError();
        testi << "evtgenlhc_test1  1   " << val << "   " << errval << std::endl;

        val = m23->GetMean();
        errval = m23->GetMeanError();
        testi << "evtgenlhc_test1  2   " << val << "   " << errval << std::endl;
      }

    }while (count++<nevent);
  
    file->Write(); file->Close();
    outmix.close();
    EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}


void runPiPiPi(int nevent, EvtGen &myGenerator) {

    std::ofstream outmix;
    outmix.open("pipipi.dat");

    int count;
    EvtVector4R p4pip,p4pim,p4pi0;

    myGenerator.readUDecay("exampleFiles/PIPIPI.DEC");

    count=1;
    
    static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));


    do{
    
      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
     
      EvtParticle *root_part=EvtParticleFactory::particleFactory(UPS4,
								 p_init);
      root_part->setVectorSpinDensity();      

      myGenerator.generateDecay(root_part);

      p4pip=root_part->getDaug(0)->getDaug(0)->getP4Lab();
      p4pim=root_part->getDaug(0)->getDaug(1)->getP4Lab();
      p4pi0=root_part->getDaug(0)->getDaug(2)->getP4Lab();
    
      outmix << root_part->getDaug(0)->getLifetime() << " " <<
  	root_part->getDaug(1)->getLifetime()<< " ";
      outmix  << (p4pip+p4pim).mass2()<<" "<<
	        (p4pip+p4pi0).mass2()<<std::endl;

      root_part->deleteTree();  

    }while (count++<10000);
  
    outmix.close();
    EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}


void runBHadronic(int nevent, EvtGen &myGenerator) {

    std::ofstream outmix;
    outmix.open("bhadronic.dat");

    int count;

    myGenerator.readUDecay("exampleFiles/BHADRONIC.DEC");

    static EvtId B0=EvtPDL::getId(std::string("B0"));

    count=1;
    
    do{
    

      EvtVector4R p_init(EvtPDL::getMass(B0),0.0,0.0,0.0);
     
      EvtParticle *root_part=EvtParticleFactory::particleFactory(B0,
								 p_init);
      root_part->setDiagonalSpinDensity();      

      myGenerator.generateDecay(root_part);
    
      EvtParticle *p;

      //    root_part->printTree();

      p=root_part;
 
      do{

        outmix << p->getId().getId()<<" "<< p->getP4Lab().d3mag() << std::endl;
	p=p->nextIter();

      }while(p!=0);

      root_part->deleteTree();  

    }while (count++<nevent);
  
    outmix.close();
    EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}

void runSingleB(int nevent, EvtGen &myGenerator) {

    int count;

    static EvtId B0=EvtPDL::getId(std::string("B0"));

    count=1;
    
    do{
      EvtVector4R p_init(EvtPDL::getMass(B0),0.0,0.0,0.0);
     
      EvtParticle *root_part=EvtParticleFactory::particleFactory(B0,
								 p_init);
      root_part->setDiagonalSpinDensity();      

      myGenerator.generateDecay(root_part);
    

      root_part->deleteTree();  

    }while (count++<nevent);
  
    EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";
      
}



void runPiPiPiPi(int nevent, EvtGen &myGenerator) {

    std::ofstream outmix;
    outmix.open("pipipipi.dat");

    int count;
  
    EvtVector4R p4pi1,p4pi2,p4pi3,p4pi4;


    myGenerator.readUDecay("exampleFiles/PIPIPIPI.DEC");

    count=1;

    EvtParticle *bcp,*btag;
    
    static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
    static EvtId B0=EvtPDL::getId(std::string("B0"));
    static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));

    do{
    

      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
     
      EvtParticle *root_part=EvtParticleFactory::particleFactory(UPS4,
								 p_init);
      root_part->setVectorSpinDensity();      

      myGenerator.generateDecay(root_part);

      if (root_part->getDaug(0)->getNDaug()==3){
        btag=root_part->getDaug(0);
        bcp=root_part->getDaug(1);
      }
      else{
        bcp=root_part->getDaug(0);
        btag=root_part->getDaug(1);
      }

      EvtId tag; //cp tag

      if (btag->getId()==B0B){
        tag=B0;
      }
      else{
	tag=B0B;
      }

      p4pi1=bcp->getDaug(0)->getP4Lab();
      p4pi2=bcp->getDaug(1)->getP4Lab();
      p4pi3=bcp->getDaug(2)->getP4Lab();
      p4pi4=bcp->getDaug(3)->getP4Lab();
    
      EvtVector4R p4bcp,p4rho0,p4a2;

      p4rho0=p4pi1+p4pi2;
      p4a2=p4rho0+p4pi3;
      p4bcp=p4a2+p4pi4;

      outmix << bcp->getLifetime() << " " <<
	btag->getLifetime() << " " << tag.getId() <<" " 
	     << (p4pi1+p4pi2+p4pi3).mass() << " " <<
	(p4pi1+p4pi2).mass() <<  " " <<
        EvtDecayAngle(p4bcp,p4rho0+p4pi3,p4rho0) << " "<<
        EvtDecayAngle(p4a2,p4pi1+p4pi2,p4pi1) << std::endl;

      root_part->deleteTree();  

      EvtGenReport(EVTGEN_INFO,"EvtGen") << "count:"<<count<<std::endl;

    }while (count++<1000);

    outmix.close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}

void runA2Pi(int nevent, EvtGen &myGenerator) {

    std::ofstream outmix;
    outmix.open("a2pi.dat");

    int count;

    myGenerator.readUDecay("exampleFiles/A2PI.DEC");

    EvtParticle *bcp,*btag;
    EvtParticle *a2,*rho0,*pi1,*pi2,*pi3,*pi4;
    EvtVector4R p4bcp,p4a2,p4rho0,p4pi1,p4pi2,p4pi3,p4pi4;

    count=1;

    static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
    static EvtId B0=EvtPDL::getId(std::string("B0"));
    static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));

    do{
    
      
      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
     
      EvtParticle *root_part=EvtParticleFactory::particleFactory(UPS4,
								 p_init);
      root_part->setVectorSpinDensity();      

      myGenerator.generateDecay(root_part);
    
      if (root_part->getDaug(0)->getNDaug()==3){
        btag=root_part->getDaug(0);
        bcp=root_part->getDaug(1);
      }
      else{
        bcp=root_part->getDaug(0);
        btag=root_part->getDaug(1);
      }

      a2=bcp->getDaug(0);
      pi1=bcp->getDaug(1);

      rho0=a2->getDaug(0);
      pi2=a2->getDaug(1);

      pi3=rho0->getDaug(0);
      pi4=rho0->getDaug(1);

      p4bcp=bcp->getP4Lab();
      p4a2=a2->getP4Lab();
      p4pi1=pi1->getP4Lab();

      p4rho0=rho0->getP4Lab();
      p4pi2=pi2->getP4Lab();

      p4pi3=pi3->getP4Lab();
      p4pi4=pi4->getP4Lab();

      EvtId tag; //cp tag

      if (btag->getId()==B0B){
        tag=B0;
      }
      else{
	tag=B0B;
      }


      outmix << bcp->getLifetime() << " " <<
	btag->getLifetime() << " " << 
	EvtDecayAngle(p4bcp,p4rho0+p4pi2,p4rho0) << " "<<
	EvtDecayAngle(p4a2,p4pi3+p4pi4,p4pi3) << " "<<
	EvtPDL::getStdHep(tag) << std::endl;

      root_part->deleteTree();  

    }while (count++<1000);
  
    outmix.close();

  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}


void runHelAmp(int nevent, EvtGen &myGenerator, std::string userFile, 
	       std::string rootFile) {

  TFile *file=new TFile(rootFile.c_str(), "RECREATE");

  TH1F* costheta = new TH1F("h1","costheta",100,-1.0,1.0); 
  TH1F* costheta2 = new TH1F("h2","costheta2",100,-1.0,1.0); 

    int count;

    myGenerator.readUDecay(userFile.c_str());

    count=1;

    static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
    

    do{
    
      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
     
      EvtParticle *root_part=EvtParticleFactory::particleFactory(UPS4,
								 p_init);
      root_part->setVectorSpinDensity();      

      myGenerator.generateDecay(root_part);

      EvtVector4R d14=root_part->getDaug(0)->getP4Lab();
      double c=d14.get(3)/d14.d3mag();
      costheta->Fill(c);

      EvtVector4R p=root_part->getP4Lab();
      EvtVector4R q=root_part->getDaug(0)->getP4Lab();
      EvtVector4R d=root_part->getDaug(0)->getDaug(0)->getP4Lab();

      costheta2->Fill(EvtDecayAngle(p,q,d));

      root_part->deleteTree();  

    }while (count++<nevent);
  

    file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";  

}

void runHelAmp2(int nevent, EvtGen &myGenerator) {

    TFile *file=new TFile("helamp2.root", "RECREATE");

    TH1F* costheta = new TH1F("h1","costheta",100,-1.0,1.0); 

    int count;

    myGenerator.readUDecay("exampleFiles/HELAMP2.DEC");

    count=1;

    static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
    

    do{
    
      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
     
      EvtParticle *root_part=EvtParticleFactory::particleFactory(UPS4,
								 p_init);
      root_part->setVectorSpinDensity();      

      myGenerator.generateDecay(root_part);


      EvtVector4R p=root_part->getDaug(0)->getDaug(0)->getP4Lab();
      EvtVector4R q=root_part->getDaug(0)->getDaug(0)->getDaug(0)->getP4Lab();
      EvtVector4R d=root_part->getDaug(0)->getDaug(0)->getDaug(0)->getDaug(0)->getP4Lab();

      costheta->Fill(EvtDecayAngle(p,q,d));

      root_part->deleteTree();  

    }while (count++<nevent);
  

    file->Write(); file->Close();
    EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}

void runD2Pi(int nevent, EvtGen &myGenerator) {

    TFile *file=new TFile("d2pi.root", "RECREATE");

    TH1F* cospi = new TH1F("h1","cos[Q]?[p]!",50,-1.0,1.0); 
    TH1F* ptpi = new TH1F("h2","Pt of pion",50,0.0,1.5); 
    TH1F* ppi = new TH1F("h3","P?[p]! in [Y](4S) rest frame",50,0.0,1.5); 

    int count;

    myGenerator.readUDecay("exampleFiles/D2PI.DEC");

    EvtParticle *b;
    EvtParticle *d2,*pi;

    EvtVector4R p4b,p4d2,p4pi;

    count=1;

    static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
    

    do{
    
      
      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
     
      EvtParticle *root_part=EvtParticleFactory::particleFactory(UPS4,
								 p_init);
      root_part->setVectorSpinDensity();      

      myGenerator.generateDecay(root_part);
    
      b=root_part->getDaug(0);

      d2=b->getDaug(0);
      
      pi=d2->getDaug(1);

      p4b=b->getP4Lab();
      p4d2=d2->getP4Lab();
      p4pi=pi->getP4Lab();


      cospi->Fill(EvtDecayAngle(p4b,p4d2,p4pi));
      ptpi->Fill(sqrt(p4pi.get(2)*p4pi.get(2)+p4pi.get(3)*p4pi.get(3)));
      ppi->Fill(p4pi.d3mag());


      root_part->deleteTree();  

    }while (count++<nevent);
  
    file->Write(); file->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}


void runPiPiCPT(int nevent, EvtGen &myGenerator) {

    std::ofstream outmix;
    outmix.open("pipicpt.dat");

    int count;

    myGenerator.readUDecay("exampleFiles/PIPICPT.DEC");

    EvtParticle *bcp,*btag;

    count=1;

    static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
    static EvtId B0=EvtPDL::getId(std::string("B0"));
    static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));


    do{
    
      
      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
     
      EvtParticle *root_part=EvtParticleFactory::particleFactory(UPS4,
								 p_init);
      root_part->setVectorSpinDensity();      

      myGenerator.generateDecay(root_part);
    
      if (root_part->getDaug(0)->getNDaug()==3){
        btag=root_part->getDaug(0);
        bcp=root_part->getDaug(1);
      }
      else{
        bcp=root_part->getDaug(0);
        btag=root_part->getDaug(1);
      }

      EvtId tag; //cp tag

      if (btag->getId()==B0B){
        tag=B0;
      }
      else{
	tag=B0B;
      }

      outmix << bcp->getLifetime() << " " <<
	btag->getLifetime() << " " << tag.getId() << std::endl;

      root_part->deleteTree();  

    }while (count++<10000);
  
    outmix.close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}


void runJpsiKs(int nevent, EvtGen &myGenerator) {

    std::ofstream outmix;
    outmix.open("jpsiks.dat");

    int count;

    myGenerator.readUDecay("exampleFiles/JPSIKS.DEC");

    EvtParticle *bcp,*btag;

    count=1;

    static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
    static EvtId B0=EvtPDL::getId(std::string("B0"));
    static EvtId B0B=EvtPDL::getId(std::string("anti-B0"));


    do{
    
      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
     
      EvtParticle *root_part=EvtParticleFactory::particleFactory(UPS4,
								 p_init);
      root_part->setVectorSpinDensity();      

      myGenerator.generateDecay(root_part);

      if (root_part->getDaug(0)->getNDaug()==3){
        btag=root_part->getDaug(0);
        bcp=root_part->getDaug(1);
      }
      else{
        bcp=root_part->getDaug(0);
        btag=root_part->getDaug(1);
      }

      EvtId tag; //cp tag

      if (btag->getId()==B0B){
        tag=B0;
      }
      else{
	tag=B0B;
      }

      outmix << bcp->getLifetime() << " " <<
	btag->getLifetime() << " " << tag.getId() << std::endl;

      root_part->deleteTree();  

    }while (count++<10000);
  
    outmix.close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}

void runDump(int nevent, EvtGen &myGenerator) {

    int count;


    int stable_list[1];
    stable_list[0]=-1;

  
    std::ofstream outmix;
    outmix.open("dump.dat");

    EvtParticle *p;

    myGenerator.readUDecay("exampleFiles/DUMP.DEC");

    count=1;

    static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));

    static EvtId PI0=EvtPDL::getId(std::string("pi0"));
    static EvtId PIP=EvtPDL::getId(std::string("pi+"));
    static EvtId PIM=EvtPDL::getId(std::string("pi-"));
    
    static EvtId EP=EvtPDL::getId(std::string("e+"));
    static EvtId KP=EvtPDL::getId(std::string("K+"));
    static EvtId MUP=EvtPDL::getId(std::string("mu+"));
    static EvtId EM=EvtPDL::getId(std::string("e-"));
    static EvtId KM=EvtPDL::getId(std::string("K-"));
    static EvtId MUM=EvtPDL::getId(std::string("mu-"));
    static EvtId GAMM=EvtPDL::getId(std::string("gamma"));
    static EvtId K0L=EvtPDL::getId(std::string("K_L0"));

    do{


      if (count==100*(count/100)) {
       EvtGenReport(EVTGEN_INFO,"EvtGen") << count << std::endl;
      }
      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
     
      EvtParticle *root_part=EvtParticleFactory::particleFactory(UPS4,
								 p_init);
      root_part->setVectorSpinDensity();      

      myGenerator.generateDecay(root_part);

      p=root_part;

      EvtParticle *otherb=root_part->getDaug(1);

      EvtGenReport(EVTGEN_INFO,"EvtGen") <<"Event:"<<count<<std::endl;
      root_part->printTree();

      outmix << "B"
	     <<" "<<otherb->getP4Lab().get(0)
	     <<" "<<otherb->getP4Lab().get(1)
	     <<" "<<otherb->getP4Lab().get(2)
	     <<" "<<otherb->getP4Lab().get(3)<<std::endl;

      do{


	if (p->getId()==PIP||
	    p->getId()==EP||
	    p->getId()==KP||
	    p->getId()==MUP){
	  outmix << "chg +1"
		 <<" "<<p->getP4Lab().get(1)
		 <<" "<<p->getP4Lab().get(2)
		 <<" "<<p->getP4Lab().get(3)<<std::endl;
	}

	if (p->getId()==PIM||
	    p->getId()==EM||
	    p->getId()==KM||
	    p->getId()==MUM){
	  outmix << "chg -1"
		 <<" "<<p->getP4Lab().get(1)
		 <<" "<<p->getP4Lab().get(2)
		 <<" "<<p->getP4Lab().get(3)<<std::endl;
	}

	if (p->getId()==GAMM||
	    p->getId()==K0L){
	  outmix << "neu"
		 <<" "<<p->getP4Lab().get(1)
		 <<" "<<p->getP4Lab().get(2)
		 <<" "<<p->getP4Lab().get(3)<<std::endl;
	}

	
	p=p->nextIter();

      }while(p!=0);

      outmix << "event"<<std::endl;

      root_part->deleteTree();  

    }while (count++<nevent);
  
    outmix.close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";


}

void runGenericCont(int nevent, EvtGen &myGenerator) {

    int count;

    std::ofstream outmix;
    outmix.open("genericcont.dat");

    EvtParticle *p;

    myGenerator.readUDecay("exampleFiles/GENERIC.DEC");

    count=1;
    
    static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
    static EvtId VPHO=EvtPDL::getId(std::string("vpho"));

    do{

      if (count==1000*(count/1000)) {
       EvtGenReport(EVTGEN_DEBUG,"EvtGen") << count << std::endl;

      }

      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
     
      EvtParticle *root_part=EvtParticleFactory::particleFactory(VPHO,
								 p_init);
      root_part->setVectorSpinDensity();      

      myGenerator.generateDecay(root_part);

      p=root_part;

      do{

        outmix << p->getId().getId()<<" "<< p->getP4Lab().d3mag() << std::endl;

	p=p->nextIter();

      }while(p!=0);

      //root_part->printTree();

      root_part->deleteTree();  

    }while (count++<nevent);
  
    outmix.close();

  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}


void runD1(int nevent, EvtGen &myGenerator) {

    std::ofstream outmix;
    outmix.open("d1.dat");

    int count=1;

    myGenerator.readUDecay("exampleFiles/D1.DEC");

    static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));

    do{
      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
      EvtParticle *root_part=EvtParticleFactory::particleFactory(UPS4,
								 p_init);
      root_part->setVectorSpinDensity();      

      myGenerator.generateDecay(root_part);
    
      EvtParticle *p_b,*p_d1,*p_e,*p_nu,*p_pi1,*p_dstar,*p_pi2,*p_d;
      EvtVector4R  p4_b,p4_d1,p4_e,p4_nu,p4_pi1,p4_dstar,p4_pi2,p4_d;

      // code for analyzing B->D1 e nu ; D1 -> D* pi ; D* -> D pi

      p_b=root_part->getDaug(1);

      p_d1=p_b->getDaug(0);
      p_e=p_b->getDaug(1);
      p_nu=p_b->getDaug(2);

      p_dstar=p_d1->getDaug(0);
      p_pi1=p_d1->getDaug(1);

      p_d=p_dstar->getDaug(0);
      p_pi2=p_dstar->getDaug(1);

      p4_b=p_b->getP4Lab();
      p4_d1=p_d1->getP4Lab();
      p4_e=p_e->getP4Lab();
      p4_nu=p_nu->getP4Lab();
      p4_dstar=p_dstar->getP4Lab();
      p4_pi1=p_pi1->getP4Lab();
      p4_pi2=p_pi2->getP4Lab();
      p4_d=p_d->getP4Lab();

      outmix << p4_e.get(0)<<" ";
      outmix << (p4_e+p4_nu)*(p4_e+p4_nu)<<" ";
      outmix << EvtDecayAngle(p4_b,p4_e+p4_nu,p4_e) << " " ;
      outmix << EvtDecayAngle(p4_b,p4_dstar+p4_pi1,p4_dstar) << " " ;
      outmix << EvtDecayAngle(p4_d1,p4_d+p4_pi2,p4_d) << " " ;
      outmix << EvtDecayAngleChi(p4_b,p4_e,p4_nu,
				    p4_dstar, p4_pi1 ) << "\n" ;

      root_part->deleteTree();  

      EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "count:"<<count<<std::endl;

    }while (count++<nevent);
  
    outmix.close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}


void runMix(int nevent, EvtGen &myGenerator) {

    std::ofstream outmix;
    outmix.open("mix.dat");
  
    int count=1;
  
    myGenerator.readUDecay("exampleFiles/MIX.DEC");

    static EvtId UPS4=EvtPDL::getId(std::string("Upsilon(4S)"));
    

    do{
      EvtVector4R p_init(EvtPDL::getMass(UPS4),0.0,0.0,0.0);
      EvtParticle *root_part=EvtParticleFactory::particleFactory(UPS4,
								 p_init);
      root_part->setVectorSpinDensity();      

      myGenerator.generateDecay(root_part);
    
      outmix << root_part->getDaug(0)->getId().getId() << " ";
      outmix << root_part->getDaug(0)->getLifetime() << " ";
      outmix << root_part->getDaug(1)->getId().getId() << " ";
      outmix << root_part->getDaug(1)->getLifetime() << "\n";
    
      root_part->deleteTree();  

    }while (count++<10000);

    outmix.close();
    EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}


void runBaryonic(int nEvent, EvtGen& myGenerator)
{
    TFile* f = new TFile("baryonic.root", "RECREATE");
    TH1D* q2Hist = new TH1D("q2Hist", "q square", 50, 0.0, 25.00);

    EvtId BMINUS = EvtPDL::getId("B-");
    EvtVector4R p_init(EvtPDL::getMass(BMINUS), 0.0, 0.0, 0.0);
    EvtParticle* root_part;
    myGenerator.readUDecay("exampleFiles/BARYONIC.DEC");
    
    EvtVector4R l;
    EvtVector4R p;
    EvtVector4R g;
    EvtVector4R sum;
    
    for (int i=0; i<nEvent; ++i)
    {
        root_part = EvtParticleFactory::particleFactory(BMINUS, p_init);
        root_part->setDiagonalSpinDensity();
        myGenerator.generateDecay(root_part);
        l = root_part->getDaug(0)->getP4Lab(); // lambda momentum
        p = root_part->getDaug(1)->getP4Lab(); // pBar momentum
        g = root_part->getDaug(2)->getP4Lab(); // gamma momentum
        sum = p+g;
        q2Hist->Fill( sum.mass2() );
        root_part->deleteTree();
    }
    f->Write();
    f->Close();
  EvtGenReport(EVTGEN_INFO,"EvtGen") << "SUCCESS\n";

}
