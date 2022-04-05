/*!
 * \class StTreeMaker
 * \author Victor Perevoztchikov (perev@bnl.gov)
 *
 * Purpose : Main STAR I/O Maker to  read/write ROOT based STAR data
 *
 * <ol>
 * <li>Inherited from StIOInterface class
 * <li>Creates, writes and reads STAR data components(branches)
 * <li>Open/close of numerous input and output files
 * <li> Provides common user interface for all ROOT based I/O data.
 *   User maker access data via StMaker class. It does not
 *   know anything about existence of StTreeMaker(s)
 * </ol>
 *
 */

#ifndef STAR_StTreeMaker
#define STAR_StTreeMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTreeMaker 			                            	        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StIOInterFace.h"
#include "StTree.h"
#include "TTable.h"
#include "Ttypes.h"
struct dst_bfc_status_st  {
  Char_t   maker_name[12];    /* Truncated maker name            */
  Int_t    status;            /* Status number (warning,error)   */
};
class St_dst_bfc_status : public TTable {
 public:
  ClassDefTable(St_dst_bfc_status,dst_bfc_status_st)
  ClassDef(St_dst_bfc_status,1) //C++ container for chain/makers status 
};


class StTreeMaker : public StIOInterFace {
private:
   Int_t  fFinished;			//!noIO
   St_dst_bfc_status *fBfcStatus;	//!noIO
public:
   StTreeMaker(const Char_t *name="",const Char_t *ioFile="",const Char_t *treeName=0);
   virtual       ~StTreeMaker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual Int_t  MakeRead(const StUKey &RunEvent);
   virtual Int_t  MakeRead(){StUKey uk; return MakeRead(uk);};
   virtual Int_t  MakeWrite();
           Int_t  MakeBfcStatus();
   virtual Int_t  Finish();
   virtual Int_t  Save();
   virtual Int_t  Skip(int nskip);
   virtual void   Clear(Option_t *opt);
   virtual Int_t  Open(const Char_t *ioFile=0);
   virtual void   Close(Option_t *opt=0);
           void   UpdateTree(Int_t flag);
           void   UpdateHddr();
   virtual void   FillHistBranch(StBranch *histBr);
  

 
   StTree *GetTree(){return fTree;};
   StBranch *GetBranch(const Char_t *brName)
     {if(!fTree)return 0;return (StBranch*)fTree->Find(brName);};   

  virtual const Char_t *GetCVS() const
  {static const Char_t cvs[]="Tag $Name:  $ $Id: StTreeMaker.h,v 1.19 2014/08/06 11:43:51 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

private:
   StTree        *fTree;	//!
   ClassDef(StTreeMaker, 0)   	//general StAR IO maker
};

#endif
