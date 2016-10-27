#ifndef PythiaEvent_h
#define PythiaEvent_h

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <cmath>
#include <stdlib.h>
#include <stdio.h>

#include <TObject.h>

#include "PythiaParticle.h"


class WBosMcEvent;


class PythiaEvent : public TObject
{

public:

	std::vector<PythiaParticle> tracks;
	int    I;
	int    nParticles;      //particle number in the final state
	int    ievent;
	int    genevent;
	int    subprocess;
	int    nucleon;
	double targetparton;
	double xtargparton;
	double beamparton;
	double xbeamparton;
	double thetabeamprtn;
	double pt2_hat;
	double Q2_hat;
	int    nrTracks;

   Bool_t AcceptWEvent(WBosMcEvent& wEvent);

	ClassDef(PythiaEvent, 1)
};

#endif
