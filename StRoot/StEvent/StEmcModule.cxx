/***************************************************************************
 *
 * $Id: StEmcModule.cxx,v 2.5 2011/09/06 21:33:02 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcModule.cxx,v $
 * Revision 2.5  2011/09/06 21:33:02  ullrich
 * Bug in getEnergy() corrected (Justin Stevens)
 *
 * Revision 2.4  2004/07/20 17:07:49  perev
 * Pavlinov corrs for TBrowser
 *
 * Revision 2.3  2001/04/05 04:00:48  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2000/07/28 19:49:27  akio
 * Change in Detector Id for Endcap SMD
 *
 * Revision 2.1  2000/02/23 17:34:10  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcModule.h"
#include "StEmcRawHit.h"
#include <TBrowser.h>
//#include <StAutoBrowse.h>

static const char rcsid[] = "$Id: StEmcModule.cxx,v 2.5 2011/09/06 21:33:02 ullrich Exp $";

ClassImp(StEmcModule)

StEmcModule::StEmcModule() { /* noop */ }

StEmcModule::~StEmcModule() { /* noop */ }

unsigned int
StEmcModule::numberOfHits() const {return mHits.size();}

void
StEmcModule::printNumberOfHits() const 
{
    if(numberOfHits()==0) printf(" ** no hits ** : module number is unknown **");
    else                  printf(" m %i : nhits %i\n", mHits[0]->module(), numberOfHits()); 
    return;
}

double 
StEmcModule::getEnergy(const int pri) const
{
    float eM=0., e=0.;
    int det;
    if(mHits.size()>0) {
        det = mHits[0]->detector()-kBarrelEmcTowerIdentifier + 1;
        for(unsigned int i=0; i<mHits.size(); i++) {
            e   = mHits[i]->energy();
            eM += e;
            if(pri>1) {
                int id = mHits[i]->softId(det);
                printf(" %3i hits : id %i : adc %i : e %9.4f", 
                       i+1, id, mHits[i]->adc(), e);
                if(e<=0)  printf(" !!");
                printf("\n");
            }
        }
        if(pri>0) printf("det %i : m %i : Energy %9.3f GeV/C\n", 
                         det, mHits[0]->module(), eM);
    }
    return eM;
}

const StSPtrVecEmcRawHit&
StEmcModule::hits() const { return mHits; }

StSPtrVecEmcRawHit&
StEmcModule::hits() {return mHits; }

// 15-sep-2003 by PAI
/*
 void  StEmcModule::Browse(TBrowser *b)
 {
 if(IsFolder()) StAutoBrowse::Browse(this, b);
 //  b->Add(&mHits); // no good
 //  TClass::AutoBrowse(&mHits, b); // no good
 }
 */

bool  StEmcModule::IsFolder() const
{
    if(mHits.size()>0) return kTRUE;
    else               return kFALSE;
}
