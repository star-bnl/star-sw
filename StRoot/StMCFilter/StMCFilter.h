// @(#)STAR/eg:$Id: StMCFilter.h,v 1.2 2009/04/17 18:32:28 perev Exp $
// Author: V.Perev  Mar/2009
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMCFilter: base filter class for EvGen and Geant                		//
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StMCFilter
#define STAR_StMCFilter
#include <string>
class StHepParticles;
class StG3Particles;
class StGenParticles;

class StMCFilter  
{
public:
                                // ****** constructors and destructor
   StMCFilter(const char *name);
   virtual ~StMCFilter();

///		Rejection inside of EventGenerator (Pythia)
virtual int  RejectEG(const StGenParticles &ptl) const {return 0;}
///		Rejection of GEANT Tracking
virtual int  RejectGT(const StGenParticles &ptl) const {return 0;}
///		Rejection at GEANT End, No GEANT output
virtual int  RejectGE(const StGenParticles &ptl) const {return 0;}
///		Finish
virtual void Finish() const{;}

        const std::string &GetName() const { return fName;}
public:
//	static methods
static int  Select(const char *name);
static int  REJECTEG();
static int  REJECTGT();
static int  REJECTGE();
static void SetEG(void *hepEvt);
static void SetG3(void *gfKine,void *gfVert);
static int  Action(int kase, void *par1,void *par2);

static int  GetNTotEG() { return fgSelected->fCnt[0][0];}
static int  GetNTotGT() { return fgSelected->fCnt[1][0];}
static int  GetNTotGE() { return fgSelected->fCnt[2][0];}

static int  GetNRejEG() { return fgSelected->fCnt[0][1];}
static int  GetNRejGT() { return fgSelected->fCnt[1][1];}
static int  GetNRejGE() { return fgSelected->fCnt[2][1];}
static void FINISH();

//	static members
static StMCFilter     *fgSelected;
static StHepParticles *fgHepParticle;
static StG3Particles  *fgG3Particle;

protected:
std::string     fName;
char fBeg[1];
int  fCnt[3][2];
char fEnd[1];

};

#endif

