// $Id: StMCApplication.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
// Geant4 ExampleN01 adapted to Virtual Monte Carlo 
//
// Class StMCApplication
// ----------------------- 
// Implementation of the TVirtualMCApplication
//
// by Ivana Hrivnacova, 21.4.2002

#ifndef STMC_APPLICATION_H
#define STMC_APPLICATION_H

#include <TVirtualMCApplication.h>


class TVirtualMCStack;
class GCall;

class StMCApplication : public TVirtualMCApplication
{
  public:
    StMCApplication(const char* name,  const char *title);
    StMCApplication();
    virtual ~StMCApplication();
  
    // methods
    void InitMC(const char *setup);
    void RunMC(Int_t nofEvents);
    void FinishRun();
 
    virtual void ConstructGeometry();
    virtual void InitGeometry();
    virtual void GeneratePrimaries();
    virtual void BeginEvent();
    virtual void BeginPrimary();
    virtual void PreTrack();
    virtual void Stepping();
    virtual void PostTrack();
    virtual void FinishPrimary();
    virtual void FinishEvent();
    virtual void Field(const Double_t* x, Double_t* b) const;


        
    void   setFileBased();
    void   SetStack(TVirtualMCStack *stack)     {fStack = stack;}

    GCall* GetFinishRun() 		        {return fFinishRun        ;}
    GCall* GetConstructGeometry()	        {return fConstructGeometry;} 
    GCall* GetInitGeometry()		        {return fInitGeometry     ;}      
    GCall* GetGeneratePrimaries()	        {return fGeneratePrimaries;} 
    GCall* GetBeginEvent()		        {return fBeginEvent       ;}	      
    GCall* GetBeginPrimary()		        {return fBeginPrimary     ;}      
    GCall* GetPreTrack()  		        {return fPreTrack         ;}
    GCall* GetStepping()  		        {return fStepping         ;}
    GCall* GetPostTrack() 		        {return fPostTrack        ;}
    GCall* GetFinishPrimary()	        {return fFinishPrimary    ;}
    GCall* GetFinishEvent()		        {return fFinishEvent      ;}      
    GCall* GetField()     		        {return fField            ;}
    void SetFinishRun		(GCall *u) 	{fFinishRun	        =u;}
    void SetConstructGeometry	(GCall *u)	{fConstructGeometry     =u;} 
    void SetInitGeometry	(GCall *u)	{fInitGeometry	        =u;}      
    void SetGeneratePrimaries	(GCall *u)	{fGeneratePrimaries     =u;} 
    void SetBeginEvent		(GCall *u)	{fBeginEvent            =u;}	      
    void SetBeginPrimary	(GCall *u)	{fBeginPrimary          =u;}      
    void SetPreTrack		(GCall *u)  	{fPreTrack              =u;}
    void SetStepping		(GCall *u)  	{fStepping              =u;}
    void SetPostTrack		(GCall *u) 	{fPostTrack             =u;}
    void SetFinishPrimary	(GCall *u)	{fFinishPrimary         =u;}
    void SetFinishEvent		(GCall *u)	{fFinishEvent           =u;}      
    void SetField		(GCall *u)      {fField                 =u;}

  private:
    // methods
    void InitUser();
  
    // data members
    TVirtualMCStack         *fStack;
    int                      fileBased;


    GCall* fUser[1];
    GCall* fConstructGeometry;
    GCall* fInitGeometry;
    GCall* fGeneratePrimaries;
    GCall* fField;
    GCall* fBeginEvent;
    GCall* fBeginPrimary;
    GCall* fPreTrack;
    GCall* fStepping;
    GCall* fPostTrack;
    GCall* fFinishPrimary;
    GCall* fFinishEvent;
    GCall* fFinishRun;
    GCall* fUserEnd[1];







    ClassDef(StMCApplication,1)  //Interface to MonteCarlo application
};

// inline functions


#endif //STMC_APPLICATION_H

