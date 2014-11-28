/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : This Class Manage Theoretical Correlation Function
 * user shoud define the way of calculating the weight with SetThPair
 * ThCorrFctn should be pluged with AddCorrFctn 
 * this Manager inherit from CorrFctn . it shoul be pluged in an analysis
 * with analysis->AddCorrFctn(Manager)
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/
#ifndef StHbtThCFManager_hh
#define StHbtThCFManager_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/ThCorrFctn/StHbtThCorrFctnCollection.hh"

class StHbtPair;
class StHbtThPair;
class StHbtThCorrFctn;

class StHbtThCFManager : public  StHbtCorrFctn {

public:
// --- Constructor
  StHbtThCFManager();
  virtual ~StHbtThCFManager();

  virtual void AddCorrFctn(StHbtThCorrFctn*);

  virtual void SetThPair(StHbtThPair*);
  

  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual StHbtString Report();
  virtual void Finish();

protected:
  StHbtThPair* mThPair;//!
  StHbtThCorrFctnCollection mThCorrFctnColl;//!

#ifdef __ROOT__
  ClassDef(StHbtThCFManager, 1)
#endif
};

#endif
