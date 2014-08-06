/***************************************************************************
 *
 * $Id: StHltMaker.h,v 1.2 2014/08/06 11:43:21 jeromel Exp $
 *
 * Author: L. Xue, H. Qiu, A. Tang, Jan 2011
 ***************************************************************************
 *
 * Description: Hlt Maker to propaganda online Hlt tracking and selection 
 *              information.
 *
 ***************************************************************************
 *
 * $Log: StHltMaker.h,v $
 * Revision 1.2  2014/08/06 11:43:21  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.1  2011/02/01 18:23:57  xueliang
 * *** empty log message ***
 *
 **************************************************************************/



#ifndef STAR_StHltMaker
#define STAR_StHltMaker


#include "StRTSBaseMaker.h"

namespace star {
	namespace rts {
		namespace hlt {
			class HLT_EVE;
			class HLT_TOF;
			class HLT_PVPD;
			class HLT_EMC;
			class HLT_GT;
			class HLT_PT;
			class HLT_NODE;
			class HLT_HIPT;
			class HLT_DIEP;
			class HLT_HF;
			const unsigned int NMax = 1000;  ///< number of maximun series number
		} // hlt
	} // rts
} // star

using namespace std;
using namespace star;
using namespace star::rts;
using namespace star::rts::hlt;

class StEvent;
class StHltEvent;

class StHltMaker : public StRTSBaseMaker {

	private:

		StEvent*            mStEvent;                 ///< pointer to StEvent
		StHltEvent*         mStHltEvent;              ///< pointer to StHltEvent

		unsigned int mNumHighPt;                      ///< number of HighPt tracks
		unsigned int mNumHeavyFragment;               ///< number of HeavyFragment tracks
		unsigned int mNumDielectron;                  ///< number of di-electron pairs

		int mHighPtNodeSN[NMax];              ///< series number of HighPt tracks
		int mHeavyFragmentNodeSN[NMax];       ///< series number of HeavyFragment tracks
		int mDaughter1NodeSN[NMax];           ///< series number of daughter1 electron
		int mDaughter2NodeSN[NMax];           ///< series number of daughter2 electron

	protected:

		virtual StRtsTable*              GetNextGl3();

		StHltEvent*                      GetHltEvent();     ///< create StHltEvent point

		virtual void processBank(const HLT_EVE  *bank);
		virtual void processBank(const HLT_TOF  *bank);
		virtual void processBank(const HLT_PVPD *bank);
		virtual void processBank(const HLT_EMC  *bank);
		virtual void processBank(const HLT_GT   *bank);
		virtual void processBank(const HLT_PT   *bank);
		virtual void processBank(const HLT_NODE *bank);
		virtual void processBank(const HLT_HIPT *bank);
		virtual void processBank(const HLT_DIEP *bank);
		virtual void processBank(const HLT_HF   *bank);
		
		///< process hlt banks one by one

	public:

		StHltMaker(const char *name="HLT") ;     ///< default constructor 

		virtual       ~StHltMaker();
		virtual void  Clear(Option_t *option="");
		virtual Int_t Init();
		virtual Int_t Make();
		virtual Int_t Finish();

		virtual Int_t InitRun  (int runumber); ///< Overload empty StMaker::InitRun 

		void  fillNodePointer(StHltEvent*);    ///< create pointers in track node
		void  fillHighPt(StHltEvent*);         ///< fill high pt information 
		void  fillHeavyFragment(StHltEvent*);  ///< fill heavy fragment information
		void  fillDielectron(StHltEvent*);     ///< fill di-electron information
		void  fillTriggerReason(StHltEvent*);  ///< fill triggered prticles (highpt heavyfragment di-electron) to trigger reason

		///< Displayed on session exit, leave it as-is please ...
		virtual const char *GetCVS() const {
			static const char cvs[]="Tag $Name:  $ $Id: StHltMaker.h,v 1.2 2014/08/06 11:43:21 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
			return cvs;
		}

		///< obtain the whole list of leading edge hits
		///< to obtain the published result use StMaker::GetDataSet("pp2ppRawHits");

		ClassDef(StHltMaker,0)   ///< StAF chain virtual base class for Makers

};


#endif
