// $Id: StarStack.h,v 1.3 2004/09/02 23:26:05 potekhin Exp $
// $Log: StarStack.h,v $
// Revision 1.3  2004/09/02 23:26:05  potekhin
// evolution
//


#ifndef STARSTACK_H
#define STARSTACK_H


// Warning this material comes from Ex02 and is not adapted from Ali

#include "StarParticle.h"
#include <TVirtualMCStack.h>

#include <stack>

class StarStack : public TVirtualMCStack
{
  public:
    StarStack(Int_t size);
    StarStack();
    virtual ~StarStack();     

    // methods

    virtual void       Init(void);
    virtual void       PushTrack(Int_t toBeDone, Int_t parent, Int_t pdg,
				 Double_t px, Double_t py, Double_t pz, Double_t e,
				 Double_t vx, Double_t vy, Double_t vz, Double_t tof,
				 Double_t polx, Double_t poly, Double_t polz,
				 TMCProcess mech, Int_t& ntr, Double_t weight,
				 Int_t is) ;

    virtual TParticle* PopNextTrack(Int_t& track);
    virtual TParticle* PopPrimaryForTracking(Int_t i);

    void Print(void)     const;   
    void PrintKine(void) const;   
    void Reset(void);
   
    // set methods
    virtual void       SetCurrentTrack(Int_t track);                           

    // get methods
    virtual Int_t      GetNtrack() const;
    virtual Int_t      GetNkine() const;
    virtual Int_t      GetNprimary() const;
    virtual TParticle* GetCurrentTrack() const;    
    virtual Int_t      GetCurrentTrackNumber() const;
    virtual Int_t      GetCurrentParentTrackNumber() const;
    StarParticle*      GetParticle(Int_t id) const;
    StarParticle*      GetKine(Int_t id)     const;

    //    virtual StarParticle* GetCurrentParticle() {return _currentParticle;}

    void               PurifyKine(void);
    void               AddToKine(StarParticle* p_) {_kine->Add(p_);}

    virtual void       SetDebug(Int_t d_) {_debug=d_;}

    //____________________________________________________________
    virtual void push(StarParticle* p_);
    virtual StarParticle* pop(void);
    virtual Bool_t empty(void);

    virtual void SetInterest(const char* processName_);

    TObjArray*   Stack(void) {return _stack;}
    TObjArray*   Kine(void)  {return _kine;}


  private:
    Int_t getLast(void);

    TObjArray*                 _stack;
    TObjArray*                 _kine;
    Int_t                      _currentTrack;
    Int_t                      _Nprimary;
    Int_t                      _Ntrack;
    Int_t                      _debug;
    StarParticle*              _currentPart;

    Int_t                      _interest[kMaxMCProcess];
    
    ClassDef(StarStack,1) // StarStack
};
//    std::stack<StarParticle*>  _stack;
//    StarParticle*              _currentParticle;

#endif //STARSTACK_H   
   

