/***************************************************************************
 *
 * $Id: StRchMaker.h,v 1.1 1999/02/12 00:12:33 lyons Exp $
 *
 * Author: Dan Lyons
 ***************************************************************************
 *
 * Description: RICH offline software
 *              StRchMaker.h - ROOT/STAR Maker for offline chain.
 *              Start at
 *  http://rsgi01.rhic.bnl.gov/STAR/html/comp_l/root/index2.html
 *              for more info, or at
 *  http://rsgi01.rhic.bnl.gov/star/starlib/doc/www/star.html
 *              if the other one disappears for some reason
 *
 ***************************************************************************
 *
 * $Log: StRchMaker.h,v $
 * Revision 1.1  1999/02/12 00:12:33  lyons
 * Trail version... untested
 *
 *
#ifndef ST_RCH_MAKER_HH
#define ST_RCH_MAKER_HH
 * used in first DAQ data
#endif
#define rCH_WITH_PAD_MONITOR 1

#ifndef StMaker_H
#endif
class StRichReaderInterface;
 private:
   Bool_t drawinit;

    
    StRchMaker(const char *name="rch",
		 const char *title="event/raw_data/rch");
    virtual       ~StRchMaker();
    virtual Int_t  Init();
    virtual Int_t  Make();
    virtual void   PrintInfo();
	m_Mode = mode;
	; // extra harmless semicolon for emacs tab key purposes
};

#endif // ST_RCH_MAKER_HH
	};

inline void StRchMaker::setUseMatrix(int v) {mUseMatrix = v;}

#endif 
#endif /* __ROOT__ */
