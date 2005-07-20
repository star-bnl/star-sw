// $Id: StValiSet.h,v 1.1 2005/07/20 17:42:48 perev Exp $
// $Log: StValiSet.h,v $
// Revision 1.1  2005/07/20 17:42:48  perev
// *** empty log message ***
//
#ifndef STAR_StValiSet
#define STAR_StValiSet

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StValiSet virtual base class for Maker                          //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TDatime.h"
#include "TDataSet.h"
enum DBConst {kMinTime = 19950101, kMaxTime = 20380101};

class StValiSet : public TDataSet{
public:
   TDatime fTimeMin;
   TDatime fTimeMax;
   TDataSet *fDat;
   TString fFla;
   Int_t  fMod;
   StValiSet(const char *name="",TDataSet *parent=0);
   virtual ~StValiSet(){};
   virtual void ls(Int_t lev=1) const;
   virtual void ls(const Option_t *opt) const 		{TDataSet::ls(opt);}
           void Modified(int m=1){fMod=m;}
           int  IsModified()				{return fMod;}

ClassDef(StValiSet,1)
};


#endif
