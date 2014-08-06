/*! 
 * \class StStrangeTagsMaker
 * \author Gene Van Buren, Feb 1995. Modifications: Lee Barnby and Leon Gaillard, July 2004
 * \brief A maker to fill the strangeness tags
 * 
 * Tags are written in tables and end up in the tags.root file. It is expected that tags may
 * be used, either uniquely or in combination, to select events. This maker fills the tags 
 * related to strange particles i.e. those for V0, cascade and kink candidates found during
 * event reconstruction. The numbers of candidates consistent with the masses of various
 * particles are recorded, namely; K0-short, Lambda and anti-Lambda for V0 and Xi, anti-Xi,
 * Omega and anti-Omega for cascades. There are also tags for the numbers of candidates in 
 * mass ranges above and below each particle's known mass. For kink candidates the number of 
 * positive and negative candidates are counted and no mass calculation takes place. The ranges
 * used are:
 * mean PDG mass +/- range, where 'range' is a fraction of the mean. Currently this is set to
 * 0.025. The 'above' and 'below' windows are +range to 3*range and -range to -3*range. These
 * result in windows for K0-short that are rather too narrow (+/- 12.5 MeV) so for that
 * particle all ranges are doubled. For Lamba and anti-Lambda the range have been halved. This
 * is one of the modifications introduced by LB and LG. Others were to add anti-Xi, Omega and
 * anti-Omega and kinks as well as max pt. Here is a full list of the tags with their meanings
 * (taken from the idl file in pams/global/idl/StrangeTag.idl):
 *
 *    long  NV0;             // no. v0's
 *
 *    long  NbelowK0;        // no. v0's with mass just below k0
 *    long  NK0;             // no. v0's with mass near k0
 *    long  NaboveK0;        // no. v0's with mass just above k0
 *    long  NbelowLa;        // no. v0's with mass just below lambda
 *    long  NLa;             // no. v0's with mass near lambda
 *    long  NaboveLa;        // no. v0's with mass just above lambda
 *    long  NbelowLb;        // no. v0's with mass just below lambda-bar
 *    long  NLb;             // no. v0's with mass near lambda-bar
 *    long  NaboveLb;        // no. v0's with mass just above lambda-bar
 *    long  NbelowXi;        // no. cascade with mass just below Xi
 *    long  NXi;             // no. cascade with mass near Xi
 *    long  NaboveXi;        // no. cascade with mass just above Xi
 *    long  NbelowXibar;     // no. cascade with mass just below Xi-bar
 *    long  NXibar;          // no. cascade with mass  Xi-bar
 *    long  NaboveXibar;     // no. cascade with mass just above Xi-bar
 *    long  NbelowOm;        // no. cascade with mass just below Omega
 *    long  NOm;             // no. cascade with mass near Omega
 *    long  NaboveOm;        // no. cascade with mass just above Omega
 *    long  NbelowOmbar;     // no. cascade with mass just below Omega-bar
 *    long  NOmbar;          // no. cascade with mass  Omega-bar
 *    long  NaboveOmbar;     // no. cascade with mass just above Omega-bar
 *
 *    float MaxPtK0;         // maximum pt v0s consistent with K0
 *    float MaxPtLa;         // maximum pt v0s consistent with lambda
 *    float MaxPtLb;         // maximum pt v0s consistent with lambda-bar
 *    float MaxPtXi;         // maximum pt cascade consistent with Xi
 *    float MaxPtXibar;      // maximum pt cascade consistent with Xi-bar
 *    float MaxPtOm;         // maximum pt cascade consistent with Omega
 *    float MaxPtOmbar;      // maximum pt cascade consistent with Omega-bar
 *
 *    long  NKinkPos;        // no. of positive kink candidates
 *    long  NKinkNeg;        // no. of negative kink candidates
 *
 *    float MaxPtKinkPos;    // maximum pt postive kink candidates
 *    float MaxPtKinkNeg;    // maximum pt negative kink candidates
 *
 *    float range;           // range used in defining mass windows
 *
 * $Id: StStrangeTagsMaker.h,v 1.14 2014/08/06 11:43:44 jeromel Exp $
 *
 */


#ifndef StStrangeTagsMaker_HH
#define StStrangeTagsMaker_HH

#include <Stiostream.h>
#include "TROOT.h"
#include "StMaker.h"
#include "tables/St_StrangeTag_Table.h"
class StEvent;

class StStrangeTagsMaker : public StMaker {
public:
    StStrangeTagsMaker(const char *name="StrangeTags", const char *title=0);
    ~StStrangeTagsMaker();
    
    Int_t  Init();                    // create and fills the tags
    Int_t  Make();                    // create and fills the tags
    
    StrangeTag_st* tag();             // returns pointer to the tag table
    void          printTag(ostream& = cout);
    
protected:
    void   fillTag();                 // does the actual work;
    
private:
    StrangeTag_st*  mTagTable;        //! the tag table to fill
    StEvent*        mEvent;           //! pointer to DST data

    Float_t         mRange;
    Float_t         m2Range;
    Float_t         mMasspi2;
    Float_t         mMasspr2;
    Float_t         mMassla2;
    Float_t         mMasska2;
    
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StStrangeTagsMaker.h,v 1.14 2014/08/06 11:43:44 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

    ClassDef(StStrangeTagsMaker,0)   // macro for rootcint
};

#endif
#if 0
/***************************************************************************
 *
 * $Log: StStrangeTagsMaker.h,v $
 * Revision 1.14  2014/08/06 11:43:44  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.13  2012/12/13 20:28:39  fisyak
 * suppress warning
 *
 * Revision 1.12  2008/04/02 23:46:41  fisyak
 * remove //0 after #endif
 *
 * Revision 1.11  2006/02/14 21:07:49  perev
 * WarnOff
 *
 * Revision 1.10  2004/07/28 07:47:45  lbarnby
 * Corrected typo which prevented parsing of comments: '/ *' -> '\/*'
 *
 * Revision 1.9  2004/07/27 14:29:09  lbarnby
 * Updated documentation for doxygen
 *
 * Revision 1.8  2004/07/26 16:02:33  lbarnby
 * Added Xibar, Omega(bar). Introduce max pt tags.
 *
 * Revision 1.7  2003/09/10 19:47:34  perev
 * ansi corrs
 *
 * Revision 1.6  2003/09/02 17:59:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.5  2000/01/27 19:29:50  fisyak
 * Put StrangeTag to .data
 *
 * Revision 1.4  1999/09/24 01:23:26  fisyak
 * Reduced Include Path
 *
 * Revision 1.3  1999/07/15 13:57:29  perev
 * cleanup
 *
 * Revision 1.2  1999/02/24 02:03:39  genevb
 * Add Xi vertices
 *
 * Revision 1.1  1999/02/21 23:35:12  genevb
 * Strangeness Tags Maker
 *
 **************************************************************************/
#endif
