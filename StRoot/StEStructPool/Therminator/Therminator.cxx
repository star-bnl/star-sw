/**********************************************************************
 *
 * Author: Duncan Prindle
 *         (Modeled after THijing by Chunhui Han)
 *
 **********************************************************************/
#include <map>
#include "THGlobal.h"
#include "TString.h"

#include "DecayTable.h"
#include "Event.h"
#include "Integrator.h"
#include "Parser.h"
#include "Particle.h"
#include "ParticleDB.h"
#include "ParticleType.h"
#include "ReadPar.h"

#include "Therminator.h"
#include <iostream>
#include <fstream>
#include <string.h>

//>>>>> Do I still want this?
//using namespace std;

ReadPar *sRPInstance;
TString  sRPFileName;
int      sRunType; 
int      sTables;
int      sModel;
int      sIntegrateSample;

Therminator::Therminator( const char *scriptDir ) {
  sRPFileName += scriptDir;
  sRPFileName += "/therminator.in";
  init();
}

void Therminator::init() {
  mNevent = -1;
  ParticleDB   *tPartDB = new ParticleDB();
  Parser       *tParser;
  ParticleType *tType;
  
  try {
    const char *temp = sRPFileName.Data();
    sRPInstance = new ReadPar();
    sRPInstance->readFile(temp);
  }
  catch (STR tError) {
    PRINT_DEBUG_1("Caught: " << tError);
    PRINT_MESSAGE("No Par File. Generating the stub...");
    exit(3);
  }

  ReadParameters();

  tParser = new Parser(tPartDB);

   if (sTables) {
       PRINT_MESSAGE("Particle data tables from SHARE");
       tParser->ReadShare();
   } else {
       PRINT_MESSAGE("Particle data tables from old Mathematica tables");
       tParser->ReadInput();
   }

  PRINT_MESSAGE(tPartDB->GetParticleTypeCount()<<" particle types read.");
  
  for(int tPart=0;tPart<tPartDB->GetParticleTypeCount();tPart++) {
      tType = tPartDB->GetParticleType(tPart);
      PRINT_DEBUG_2("Particle number " << tType->GetNumber() << " " << tType->GetName() 
            <<" mass "<<tType->GetMass()
            <<" gamma "<<tType->GetGamma()
            <<" spin "<<tType->GetSpin()
            <<" I "<<tType->GetI()
            <<" I3 "<<tType->GetI3()
            <<" barionN "<<tType->GetBarionN()
            <<" charge "<<tType->GetCharge()
            <<" mc# "<<tType->GetPDGCode());

      double sumaBR=0.;

      if (tType->GetTable()) {
          for (int tChanIndex=0; tChanIndex<((DecayTable *) (tType->GetTable()))->GetChannelCount()+1; tChanIndex++) {
              PRINT_DEBUG_2("Channel " << tChanIndex << " " 
                << tType->GetTable()->GetDecayChannel(tChanIndex)->GetParticle1() << " " 
                << (tPartDB->GetParticleType(tType->GetTable()->GetDecayChannel(tChanIndex)->GetParticle1()))->GetName() << " " 
                << tType->GetTable()->GetDecayChannel(tChanIndex)->GetParticle2() << " " 
                << (tPartDB->GetParticleType(tType->GetTable()->GetDecayChannel(tChanIndex)->GetParticle2()))->GetName() << " ");
              if (tType->GetTable()->GetDecayChannel(tChanIndex)->Is3Particle()) {
                  PRINT_DEBUG_2(tType->GetTable()->GetDecayChannel(tChanIndex)->GetParticle3() << " " 
                      << (tPartDB->GetParticleType(tType->GetTable()->GetDecayChannel(tChanIndex)->GetParticle3()))->GetName() << " ");
              }

              PRINT_DEBUG_2(tType->GetTable()->GetDecayChannel(tChanIndex)->GetBranchingRatio() << " " 
                    << tType->GetTable()->GetDecayStep(tChanIndex));
              sumaBR+=tType->GetTable()->GetDecayChannel(tChanIndex)->GetBranchingRatio(); 
          }
          PRINT_DEBUG_2("sumaBR "<<sumaBR);
      }
    }

  calka = new Integrator(sIntegrateSample);
  calka->ReadMultInteg(tPartDB);
  if ((sRunType == 3) || (sRunType == 4)) {
      if (sRunType==4) {
          calka->Randomize();
      }

      mEvent = new Event(tPartDB, calka);
      if (sRunType == 4) {
          mEvent->Randomize();
      }
  }
}
void Therminator::ReadParameters() {
  STR tModel;
  STR tTable;
  
  try {
    tModel = sRPInstance->getPar("FreezeOutModel");
    if (tModel.Contains("SingleFreezeOut")) {
      sModel = 0;
    } else if (tModel.Contains("BlastWaveVTDelay")) {
      sModel = 6;
    } else if (tModel.Contains("BlastWaveVT")) {
      sModel = 2;
    } else if (tModel.Contains("BlastWaveVLinear")) {
      sModel = 4;
    } else if (tModel.Contains("FiniteHydro")) {
      sModel = 5;
    } else {
      PRINT_MESSAGE("Unknown model type: " << tModel.Data());
      PRINT_MESSAGE("Please provide the proper name");
      exit(0);
    }
    if (atoi(sRPInstance->getPar("Randomize").Data()) == 1) {
      sRunType = 4;
    } else {
      sRunType = 3;
    }
    tTable = sRPInstance->getPar("TableType");
    if (tTable.Contains("Mathematica")) {
      sTables = 0;
    } else if (tTable.Contains("SHARE")) {
      sTables = 1;
    } else {
      PRINT_MESSAGE("Unknown table type: " << tTable.Data());
      exit(0);
    }
    sIntegrateSample = atoi(sRPInstance->getPar("NumberOfIntegrateSamples").Data());
  }
  catch (STR tError) {
    PRINT_DEBUG_1("RunBFPW::ReadParameters - Caught exception " << tError);
    PRINT_MESSAGE("Did not find one of the neccessary parameters in the parameters file.");
    exit(0);
  }
}
void Therminator::GenerateEvent() {
  mITrack = -1;
  if (sRunType == 4) {
    mEvent->GenerateEvent();
  }
  else {
    mEvent->GenerateEvent(43212-mNevent*2);
  }
  mEvent->DecayParticles();

  const char *tmp = sRPInstance->getPar("EventOutputFile").Data();
  if (strcmp(tmp,"")) {
      writeOut();
  }

  mNevent++;
}

void Therminator::writeOut() {
  if(mNevent < 0) {
    cout <<"No event generated."<<endl;
    return;
  }
  mEvent->WriteEvent(mNevent);
}
int   Therminator::GetNParticles() {
    return mEvent->GetParticleCount();
}
// NOTE: Event::GetParticleOfCount(int i) really gets the next
//       particle unless i = 0 or 1 in which case starts at the
//       begining of the list (adding one for i = 1).
//       I don't know why it does iter--, iter++, iterr++ to go forward
//       one step.
//       In Therminator we keep track of which particle we are accessing
//       and only call Event::GetParticleOfCount when i changes.
//       As long as we are accessing in a for loop this works, but
//       be aware we don't have the random access implied by the names!!!!!
int Therminator::GetPdg(int i) {
    if (i != mITrack) {
        mITrack = i;
        mCurrParticle = mEvent->GetParticleOfCount(i);
    }
    if (mCurrParticle) {
        ParticleType *t = mCurrParticle->GetParticleType();
        return t->GetPDGCode();
    } else {
        return -99999;
    }
}
float Therminator::GetPx(int i) {
    if (i != mITrack) {
        mITrack = i;
        mCurrParticle = mEvent->GetParticleOfCount(i);
    }
    if (mCurrParticle) {
        return mCurrParticle->px;
    } else {
        return -99999;
    }
}
float Therminator::GetPy(int i) {
    if (i != mITrack) {
        mITrack = i;
        mCurrParticle = mEvent->GetParticleOfCount(i);
    }
    if (mCurrParticle) {
        return mCurrParticle->py;
    } else {
        return -99999;
    }
}
float Therminator::GetPz(int i) {
    if (i != mITrack) {
        mITrack = i;
        mCurrParticle = mEvent->GetParticleOfCount(i);
    }
    if (mCurrParticle) {
        return mCurrParticle->pz;
    } else {
        return -99999;
    }
}
float Therminator::GetVx(int i) {
    if (i != mITrack) {
        mITrack = i;
        mCurrParticle = mEvent->GetParticleOfCount(i);
    }
    if (mCurrParticle) {
        return mCurrParticle->rx*pow(10.0,-13);
    } else {
        return -99999;
    }
}
float Therminator::GetVy(int i) {
    if (i != mITrack) {
        mITrack = i;
        mCurrParticle = mEvent->GetParticleOfCount(i);
    }
    if (mCurrParticle) {
        return mCurrParticle->ry*pow(10.0,-13);
    } else {
        return -99999;
    }
}
float Therminator::GetVz(int i) {
    if (i != mITrack) {
        mITrack = i;
        mCurrParticle = mEvent->GetParticleOfCount(i);
    }
    if (mCurrParticle) {
        return mCurrParticle->rz*pow(10.0,-13);
    } else {
        return -99999;
    }
}
float Therminator::GetEnergy(int i) {
    if (i != mITrack) {
        mITrack = i;
        mCurrParticle = mEvent->GetParticleOfCount(i);
    }
    if (mCurrParticle) {
        return mCurrParticle->GetEnergy();
    } else {
        return -99999;
    }
}
float Therminator::GetMass(int i) {
    if (i != mITrack) {
        mITrack = i;
        mCurrParticle = mEvent->GetParticleOfCount(i);
    }
    if (mCurrParticle) {
        return mCurrParticle->GetMass();
    } else {
        return -99999;
    }
}
float Therminator::GetRapidity(int i) {
    if (i != mITrack) {
        mITrack = i;
        mCurrParticle = mEvent->GetParticleOfCount(i);
    }
    if (mCurrParticle) {
        return mCurrParticle->Rapidity();
    } else {
        return -99999;
    }
}
float Therminator::GetPt(int i) {
    if (i != mITrack) {
        mITrack = i;
        mCurrParticle = mEvent->GetParticleOfCount(i);
    }
    if (mCurrParticle) {
        return mCurrParticle->Pt();
    } else {
        return -99999;
    }
}
