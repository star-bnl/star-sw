// $Id: StMCStack.h,v 1.1 2009/03/25 23:15:10 perev Exp $
// $Log: StMCStack.h,v $
// Revision 1.1  2009/03/25 23:15:10  perev
// New VMC maker
//
// Revision 1.3  2005/06/09 20:13:47  fisyak
// It looks like that all hits in place (calorimeters have to be check for volumeid)
//
// Revision 1.2  2005/05/03 15:42:14  fisyak
// Adjust for bfc
//
// Revision 1.1  2005/04/25 20:44:28  fisyak
// StarVMCApplication with example in macros/starVMC.C
//

#ifndef StMC_STACK_H
#define StMC_STACK_H

#include "TVirtualMCStack.h"
#include "TObjArray.h"
#include <stack>

class StarMCParticle;
class TParticle;

class StMCStack : public TVirtualMCStack {
public:
  StMCStack(int size = 0);
  virtual ~StMCStack();

  // methods
  virtual void  PushTrack(int toBeDone, int parent, int pdg,
			  double px,   double py,   double pz, double e,
			  double vx,   double vy,   double vz, double tof,
			  double polx, double poly, double polz,
			  TMCProcess mech, int& ntr,double weight,
			  int is) ;
  virtual TParticle* PopNextTrack(int& track);
  virtual TParticle* PopPrimaryForTracking(int i);
  void    Print(const char* opt=0) const;   
  void    Clear(const char* opt=0);   
   
  virtual void     SetCurrentTrack(int track)  		{fCurrentTrack = track;}
  virtual int      GetNtrack() 		const           {return fParticles->GetEntriesFast();}
  virtual int      GetNprimary() 	const           {return fNPrimary;}
  virtual TParticle* GetCurrentTrack()  const;
  virtual int      GetCurrentTrackNumber() 	const 	{return fCurrentTrack;}
  virtual int      GetCurrentTrackId() 	const     	{return GetCurrentTrackNumber()+1;}
  virtual int      GetCurrentParentTrackNumber()const;
  StarMCParticle*  GetParticle(int id) const;
//VP  TObjArray*       GetParticles() 	const        	{return fParticles;}
  StarMCParticle*  GetCurrentParticle()        		{return fCurrentParticle;}
  private:
    // data members
  std::stack<StarMCParticle*>  fStack;    //!
  TObjArray*               fParticles;
  int                      fCurrentTrack;
  int                      fNPrimary;
  StarMCParticle*            fCurrentParticle;
  ClassDef(StMCStack,0) // StMCStack
};

#endif
   

