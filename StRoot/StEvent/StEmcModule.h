/*!
 * \class StEmcModule 
 * \author Akio Ogawa, Jan 2000
 */
/***************************************************************************
 *
 * $Id: StEmcModule.h,v 2.4 2004/07/20 17:07:49 perev Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcModule.h,v $
 * Revision 2.4  2004/07/20 17:07:49  perev
 * Pavlinov corrs for TBrowser
 *
 * Revision 2.3  2002/02/22 22:56:47  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:35  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  2000/02/23 17:34:11  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcModule_hh
#define StEmcModule_hh

#include "StObject.h"
#include "StContainers.h"

class StEmcModule : public StObject {
public:
    StEmcModule();
    ~StEmcModule();
    // StEmcModule(const StEmcModule&);            use default
    // StEmcModule& operator=(const StEmcModule&); use default
    
  unsigned int numberOfHits() const;
  void    printNumberOfHits() const;        // *MENU*
  double  getEnergy(const int pri=0) const; // *MENU*

    StSPtrVecEmcRawHit&       hits();
    const StSPtrVecEmcRawHit& hits() const;

  // 15-sep-2003 by PAI
  //    virtual void  Browse(TBrowser *b);
    virtual bool  IsFolder() const;
    
private:
    StSPtrVecEmcRawHit mHits;
    ClassDef(StEmcModule,1)
 };
#endif
