
//////////////////////////////////////////////////////////////////////
//
// $Id: StEpcCut.h,v 1.4 2005/05/23 12:35:14 suaide Exp $
//
// Author: Subhasis Chattopadhyay
//////////////////////////////////////////////////////////////////////
//
// Description: cuts for the Epc maker
//////////////////////////////////////////////////////////////////////
//

#ifndef StEpcCut_h
#define StEpcCut_h

#include <Stiostream.h>
#include <stdlib.h>
#include "Rtypes.h"


class StEpcCut
{
public:

    StEpcCut();
    virtual   ~StEpcCut();
    //
    static Float_t DeltaEta();
    static Float_t DeltaPhi();
    static Float_t RAD_SMD_E();
    static void setDeltaEta(Float_t);
    static void setDeltaPhi(Float_t);
    //
private:

    static Float_t mDeltaEta;
    static Float_t mDeltaPhi;
    static Float_t mRAD_SMD_E;
};

inline Float_t StEpcCut::DeltaEta()
{
    return mDeltaEta;
}
inline Float_t StEpcCut::DeltaPhi()
{
    return mDeltaPhi;
}
inline Float_t StEpcCut::RAD_SMD_E()
{
    return mRAD_SMD_E;
}
inline    void StEpcCut::setDeltaEta(Float_t deltaeta)
{
    mDeltaEta=deltaeta;
}
inline    void StEpcCut::setDeltaPhi(Float_t deltaphi)
{
    mDeltaPhi=deltaphi;
}

#endif
