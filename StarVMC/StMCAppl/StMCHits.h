// $Id: StMCHits.h,v 1.1 2005/03/09 18:35:34 perev Exp $
//
//
// Class StMCHits
// ------------------


#ifndef STMC_Hits_H
#define STMC_Hits_H

#include "TNamed.h"
#include "TString.h"
#include "StMCPath.h"

class TObjArray;


class StMCHit  : public TObject
{
friend class StMCHits;
public:
  StMCHit(ULong64_t U);
protected:
  StMCHit();
 ~StMCHit(){};
public:
void      GetXYZ (Double_t *xyz) const;
void      GetDif (Double_t *dif) const;
void      GetDir (Double_t *dir) const;
void      GetBeg (Double_t *beg) const;
void      GetEnd (Double_t *end) const;


Double_t  GetELoss  () const {return fELoss;}
Double_t  GetStep   () const; 
Int_t     GetTrackId() const {return GetUniqueID();}
StMCPath  GetPath() const;    

void      SetBegEnd (Double_t *beg,Double_t *end);
void      SetELoss(Double_t eloss)		{fELoss=eloss;}
void      SetTrackId(Int_t id)             	{SetUniqueID(id);};

Int_t Compare(const TObject *) const;
private:
static ULong64_t Reverse(ULong64_t in);

 ULong64_t fID;
 Double_t fXYZ[3];   	// XYZ of the center track segment of hit
 Float_t  fDif[3];   	// end - beg
 Float_t  fELoss; 	// Energy loss for the hit
 ClassDef(StMCHit,0)    // Simple MC hit class

};
//
//	Container of hits
class StMCHits : public TNamed
{
public:
    StMCHits(const char *name="DefaultHits",const char *tit="");
   ~StMCHits();    
    // methods
  void AddHit(Double_t eloss,Double_t *beg,Double_t *end);
  int  GetNHits() const;
  void Sort();

  StMCHit    *GetHit (int idx);
  StMCPath    GetPath(int idx);
  StMCPath    GetPath(const StMCHit *hit);

private:
    // data members
    TObjArray *fHits;
    
    ClassDef(StMCHits,0) // 
};

#endif //STMC_Hits_H   
   

