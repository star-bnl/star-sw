// $Id: StMCHits.h,v 1.1 2011/04/01 18:55:17 perev Exp $
//
//
// Class StMCHits
// ------------------

#define STMC_Hits_H 1
#ifndef STMC_Hits_H
#define STMC_Hits_H

#include "TNamed.h"
#include "TString.h"

class TObjArray;


class StMCHit  : public TObject
{
friend class StMCHits;
public:
  StMCHit(const char *path=0);

  void SetPath(const char* path)    		{ fPath = path        ;}
  void SetTrackId(int trackId)  		{ SetUniqueID(trackId);}
  void SetCharge (int charge )  		{ ReSetBit(charge+7)  ;}
  void SetPos (const TLorentzVector &beg,const TLorentzVector &end) 
  void SetEnergy (double energy,double eloss)	{fEnergy=energy;fEloss=eloss;} 

  const char  *GetName() const {return fPath.Data();}
  const char  *GetPath() const {return fPath.Data();}    
  const float *GetPos()  const {return fPos	   ;}
  void GetPos (double xyz[3]) const;
  void GetDir (double dir[3]) const;
  void GetBegEnd (double beg[3],double end[3]) const;

double GetEnergy () const 	{return fEnergy;	}
double GetELoss  () const 	{return fELoss;		}
double GetStep   () const; 
double GetTime   () const 	{return fPos[3];	}
   int GetTrackId() const 	{return GetUniqueID();	}
   int GetCharge () const 	{return TestBits()-7;	}
const char  *GetPath() const	{return fPath.Data();	}    

private:

 TString fPath;
 Float_t fPos[4];   	// XYZ of the center track segment of hit & time
 Float_t fDif[3];   	// end - beg
 Float_t fMass		// Mass of particle produced this hit. 
 			// Could be different from TrackId track
 Float_t fEnergy; 	// Energy of particle produced this hit
 Float_t fELoss; 	// Energy loss for the hit
// ClassDef(StMCHit,0)    // Simple MC hit class

};

#endif //STMC_Hits_H   
// $Id: StMCHits.h,v 1.1 2011/04/01 18:55:17 perev Exp $

