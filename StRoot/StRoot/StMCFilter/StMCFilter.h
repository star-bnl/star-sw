// @(#)STAR/eg:$Id: StMCFilter.h,v 1.10 2018/02/21 02:04:41 perev Exp $
// Author: V.Perev  Mar/2009
/*!

 \class  StMCFilter
 \author Victor Perevoztchikov
 \date   2009

  base filter class for EvGen and Geant         

*/

#ifndef STAR_StMCFilter
#define STAR_StMCFilter
#include <string>
#include <map>
class StMCFilter;  
typedef std::map<std::string, StMCFilter *> myMap_t;
class StHepParticleMaster;
class StG3ParticleMaster;
class StGenParticleMaster;

class StMCFilter  
{
 public:
                                // ****** constructors and destructor
   StMCFilter(const char *name);
   virtual ~StMCFilter();
   ///Rejection inside of EventGenerator (Pythia)
   virtual int  RejectEG(const StGenParticleMaster &ptl) const {return 0;}
   ///Rejection of GEANT Tracking
   virtual int  RejectGT(const StGenParticleMaster &ptl) const {return 0;}
   ///Rejection at GEANT End, No GEANT output
   virtual int  RejectGE(const StGenParticleMaster &ptl) const {return 0;}
   ///Finish called at the end of GEANT
   virtual void Finish() const{;}

   const std::string &GetName() const { return fName;}

   /// Return a reference to one of 10 user words
   double &User(int idx) 	{return fUser[idx];}
   /// Returns one of the 10 user words
   double  User(int idx) const 	{return fUser[idx];}

   /// Method which should be implemented in the concrete filter
   /// for configuring cuts defined w/in the filter.
   virtual void parseConfig(std::string key, float value){ /* nada */ };

   /// Returns a pointer to the selected filter
   static StMCFilter *selected(){ return fgSelected; }

 private:
   ///private static methods for Pythia & Geant3 connections
   ///Select filter by name
   static int  Select(const char *name);
   ///hidden envelope for RejectEG
   static int  REJECTEG();
   ///hidden envelope for RejectGT
   static int  REJECTGT();
   ///hidden envelope for RejectGE
   static int  REJECTGE();
   ///Define the address of common/hepevt/ from F77
   static void SetEG(void *hepEvt);
   ///Define the addresses  of gfKine & gfVert Geant3 routines
   static void SetG3(void *gfKine,void *gfVert);
   ///Routine performs connection with pythia and Geant3.
   ///it is called from F77 for EG,GT & GE init and connections
   static int  Action(int kase, void *par1,void *par2);

   static int  Config(std::string key, const float value );
   
 protected:
   ///Number of total EG events
   static int  GetNTotEG() { return fgSelected->fCnt[0][0];}
   ///Number of total GT events
   static int  GetNTotGT() { return fgSelected->fCnt[1][0];}
   ///Number of total GE events
   static int  GetNTotGE() { return fgSelected->fCnt[2][0];}
   ///Number of rejected GE events
   static int  GetNRejEG() { return fgSelected->fCnt[0][1];}
   ///Number of rejected GT events
   static int  GetNRejGT() { return fgSelected->fCnt[1][1];}
   ///Number of rejected GE events
   static int  GetNRejGE() { return fgSelected->fCnt[2][1];}
   ///Print of numbers above at the end of STARSIM 
   static void FINISH();

 private:

   ///static members
   ///pointer to selected filter
   static StMCFilter     *fgSelected;
   ///pointer to HEPEVT implementation(Pythia)
   static StHepParticleMaster *fgHepParticle;
   ///pointer to HEPEVT implementation(Geant3)
   static StG3ParticleMaster  *fgG3Particle;
   
protected:
   std::string     fName;
   char fBeg[1];
   int  fCnt[3][2];
   double fUser[20];
   char fEnd[1];
static myMap_t *mgMap;

};

#endif

