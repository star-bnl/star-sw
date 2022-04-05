// $Id: StjMCKinMuDst.cxx,v 1.1 2008/08/22 22:10:25 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjMCKinMuDst.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include "tables/St_g2t_event_Table.h"
#include "tables/St_particle_Table.h"
#include "tables/St_g2t_pythia_Table.h"

#include <iostream>

ClassImp(StjMCKinMuDst)

using namespace std;

void StjMCKinMuDst::readIfNewEvent() const
{
  if(isNewEvent()) readNewEvent();
}

bool StjMCKinMuDst::isNewEvent() const
{
  if(_runNumber != _uDstMaker->muDst()->event()->runId()) return true;
  if(_eventId != _uDstMaker->muDst()->event()->eventId()) return true;
  return false;
}

void StjMCKinMuDst::readNewEvent() const
{
  _runNumber = _uDstMaker->muDst()->event()->runId();
  _eventId = _uDstMaker->muDst()->event()->eventId();

  _vertexZ = _uDstMaker->muDst()->event()->primaryVertexPosition().z();

  TDataSet *Event = _uDstMaker->GetDataSet("geant");
  TDataSetIter geantDstI(Event);
  St_g2t_pythia *Pg2t_pythia = (St_g2t_pythia *)geantDstI("g2t_pythia");
  g2t_pythia_st *g2t_pythia1 = Pg2t_pythia->GetTable();
  _s = g2t_pythia1->mand_s;
  _t = g2t_pythia1->mand_t;
  _u = g2t_pythia1->mand_u;
  _pt = g2t_pythia1->hard_p;
  _costh = g2t_pythia1->cos_th;
  _x1 = g2t_pythia1->bjor_1;
  _x2 = g2t_pythia1->bjor_2;

  St_g2t_event *Pg2t_event = (St_g2t_event *)geantDstI("g2t_event");
  g2t_event_st *g2t_event1 = Pg2t_event->GetTable();
  _pid = g2t_event1->subprocess_id;
}

int StjMCKinMuDst::runNumber()
{
  readIfNewEvent();
  return _runNumber;
}

int StjMCKinMuDst::eventId()
{
  readIfNewEvent();
  return _eventId;
}

double StjMCKinMuDst::vertexZ()
{
  readIfNewEvent();
  return _vertexZ;
}

double StjMCKinMuDst::s()
{
  readIfNewEvent();
  return _s;
}

double StjMCKinMuDst::t()
{
  readIfNewEvent();
  return _t;
}

double StjMCKinMuDst::u()
{
  readIfNewEvent();
  return _u;
}

double StjMCKinMuDst::pt()
{
  readIfNewEvent();
  return _pt;
}

double StjMCKinMuDst::costh()
{
  readIfNewEvent();
  return _costh;
}

double StjMCKinMuDst::x1()
{
  readIfNewEvent();
  return _x1;
}

double StjMCKinMuDst::x2()
{
  readIfNewEvent();
  return _x2;
}

int StjMCKinMuDst::pid()
{
  readIfNewEvent();
  return _pid;
}


