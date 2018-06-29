/*!
 * \class StChain 
 *
 *                                                                     
 * Main base class to control chains for the different STAR "chains"   
 *                                                                     
 * This class :                                                        
 *   - Initialises the run default parameters                          
 *   - Provides API to Set/Get run parameters                           
 *   - Creates the support lists (TClonesArrays) for the Event structure
 *   - Creates the physics objects makers                               
 *                                                                      
 */

#ifndef STAR_StChain
#define STAR_StChain

#include <stdlib.h>
#include <stdio.h>

#ifndef StMaker_H
#include "StMaker.h"
#endif


class StEvtHddr;
class StChainOpt;
enum EChainBits {  
  kIsCalibrated = BIT(24)   // if the TObject has been created after calibration 
};


class StChain : public StMaker {
 private:
   Int_t               m_Version;    	//StChain version number
   Int_t               m_VersionDate;   //StChain version date
   Int_t               mNTotal;   	//Total   events processed
   Int_t               mNFailed;   	//Failed events processed
 protected:
   StEvtHddr          *m_EvtHddr;     	//Header of event
   StChainOpt         *mChainOpt;
 public:
                      StChain(const char *name="bfcChain", const Bool_t UseOwnHeader = kFALSE);
   virtual           ~StChain();
   virtual void       Clear(Option_t *option="");
   virtual Int_t      Finish();   // *MENU*
   virtual Int_t      Init();
   virtual Int_t      Make();
   virtual Int_t      Make(Int_t num){return IMake(num);}
   virtual Int_t      IsChain() const {return 1;}
   virtual Int_t      MakeEvent(); // *MENU*
   virtual Int_t      EventLoop(Int_t jBeg,Int_t jEnd, StMaker *outMk=0); 
   virtual Int_t      EventLoop(Int_t jEnd=1000000, StMaker *outMk=0) 	{return EventLoop(1,jEnd,outMk);}
   Int_t              GetVersion()     const 				{return m_Version;}
   Int_t              GetVersionDate() const 				{return m_VersionDate;}
   Int_t              GetNTotal()      const 				{return mNTotal;}
   Int_t              GetNFailed()     const 				{return mNFailed;}
   void               SetChainOpt(StChainOpt *opt) 			{mChainOpt=opt;}
   virtual const StChainOpt *GetChainOpt()    const;
   virtual const char *GetCVS() const 
 {static const char cvs[]="Tag $Name:  $ $Id: StChain.h,v 1.48 2018/06/29 21:46:18 smirnovd Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
   ClassDef(StChain, 0)   //StChain control class
};

#endif


// $Id: StChain.h,v 1.48 2018/06/29 21:46:18 smirnovd Exp $
// $Log: StChain.h,v $
// Revision 1.48  2018/06/29 21:46:18  smirnovd
// Revert iTPC-related changes committed on 2018-06-20 through 2018-06-28
//
// Revert "NoDead option added"
// Revert "Fill mag field more carefully"
// Revert "Assert commented out"
// Revert "Merging with TPC group code"
// Revert "Remove too strong assert"
// Revert "Restore removed by mistake line"
// Revert "Remove not used anymore file"
// Revert "iTPCheckIn"
//
// Revision 1.46  2014/08/06 11:42:55  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.45  2007/04/26 20:36:49  perev
// Some ChainOpt fixes
//
// Revision 1.44  2007/04/26 15:56:22  fisyak
// Remove non implemented GetChainOpt method (use default from StMaker)
//
// Revision 1.43  2007/04/26 03:57:14  perev
// StChainOpt interface to hide StBFChain dependency
//
// Revision 1.42  2005/08/29 21:42:21  fisyak
// switch from fBits to fStatus for StMaker control bits
//
// Revision 1.41  2002/11/26 02:16:39  perev
// EventLoop added
//
// Revision 1.40  2002/03/12 21:19:00  fisyak
// Set only one StEvtHddr as default option (due to Embedding)
//
// Revision 1.39  2002/02/02 23:31:14  jeromel
// doxygenized. Added some text for the Make() method.
//
// Revision 1.38  2001/04/10 22:32:43  perev
// Overload clean
//
// Revision 1.37  2001/04/10 21:38:49  perev
// Maki(int) --> IMake(int)
//
// Revision 1.36  2000/11/27 22:46:39  fisyak
// Introduce kIsCalibrated BIT
//
// Revision 1.35  2000/11/27 13:31:23  fisyak
// Add Production time set
//
// Revision 1.34  2000/09/27 19:34:59  fisyak
// Temporal fix to get Run/EventNo from dst
//
// Revision 1.33  2000/07/26 20:58:22  fine
// StChain::GetOption virtual dummy methods have been introduced
//
// Revision 1.32  2000/03/23 00:15:22  fine
// Adjusted to libSTAR for ROOT 2.24
//
// Revision 1.31  1999/12/03 01:24:40  fine
// Advanced timer has been introduced
//
// Revision 1.30  1999/07/15 13:56:46  perev
// cleanup
//
// Revision 1.29  1999/07/14 15:26:18  fine
// Context menu MakeEvent method has been introduced
//
// Revision 1.28  1999/07/13 02:19:33  perev
// GetCVS,StEvtHddr,etc...
//
// Revision 1.27  1999/07/11 20:40:35  perev
// Move Clear from StChain to StMaker
//
// Revision 1.26  1999/03/19 20:30:49  perev
// GetCVSTag introduced
//
// Revision 1.25  1999/03/11 01:23:58  perev
// new schema StChain
//
// Revision 1.18  1998/12/21 19:42:50  fisyak
// Move ROOT includes to non system
//
// Revision 1.17  1998/11/25 21:58:21  fisyak
// Cleanup
//
// Revision 1.16  1998/11/22 18:28:06  fisyak
// Add name of tag
//
// Revision 1.15  1998/11/19 01:23:56  fine
// StChain::MakeDoc has been introduced, StChain::MakeDoc has been fixed (see macros/bfc_doc.C macro
//
// Revision 1.14  1998/10/31 00:21:31  fisyak
// Makers take care about branches
//
// Revision 1.13  1998/10/07 18:43:59  perev
// Add Spy classes for Farm Monitor
//
// Revision 1.12  1998/10/06 18:00:27  perev
// cleanup
//
// Revision 1.11  1998/09/18 14:35:29  fisyak
// Fix makers
//
// Revision 1.10  1998/09/08 22:43:09  fisyak
// Modify St_dst_Maker to account new calling sequence
//
// Revision 1.9  1998/09/08 13:42:00  love
// new St_tpctest_Maker module
//
// Revision 1.8  1998/08/18 14:05:02  fisyak
// Add to bfc dst
//
// Revision 1.7  1998/08/07 19:34:53  fisyak
// Add St_run_Maker
//
// Revision 1.6  1998/07/20 15:08:08  fisyak
// Add tcl and tpt
//
