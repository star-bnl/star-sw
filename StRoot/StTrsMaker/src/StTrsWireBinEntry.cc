/***************************************************************************
 *
 * $Id: StTrsWireBinEntry.cc,v 1.5 2003/12/24 13:44:53 fisyak Exp $
 *
 * Author: brian, May 1998 
 ***************************************************************************
 *
 * Description: Results from transport of a single StTrsMiniChargeSegment
 *
 ***************************************************************************
 *
 * $Log: StTrsWireBinEntry.cc,v $
 * Revision 1.5  2003/12/24 13:44:53  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.4  2000/07/30 02:41:07  long
 * add dx dy dz,add function d()
 *
 * Revision 1.3  2000/02/24 16:38:05  long
 * add SigmaL,SigmaT for field on
 *
 *Revision 1.3  2000/02/23  14:00:08  long
 * add SigmaL,SigmaT for field on
 * Revision 1.2  1999/01/18 10:18:08  lasiuk
 * constructor by reference
 * set functions by reference
 *
 * Revision 1.1  1998/11/10 17:12:27  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/11/03 17:29:58  lasiuk
 * added set member functions
 * added scaleNumberOfElectrons()
 *
 * Revision 1.1  1998/06/04 23:31:58  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include "StTrsWireBinEntry.hh"

#ifdef ST_NO_TEMPLATE_DEF_ARGS
StTrsWireBinEntry::StTrsWireBinEntry() { /* nopt */ }
#endif

StTrsWireBinEntry::StTrsWireBinEntry(StThreeVector<double>& x, float elec,double SigmaL,double SigmaT,double *D, int id)
    : mPosition(x), mNumberOfElectrons(elec) ,mSigmaL(SigmaL),mSigmaT(SigmaT), mId(id){/* nopt */mD[0]=D[0];mD[1]=D[1];mD[2]=D[2]; }

StTrsWireBinEntry::~StTrsWireBinEntry() {/* nopt */ }

void StTrsWireBinEntry::scaleNumberOfElectrons(float fac)
{
    mNumberOfElectrons *= fac;
}
double StTrsWireBinEntry::sigmaL() {return mSigmaL;}
double StTrsWireBinEntry::sigmaT() {return mSigmaT;}
double *   StTrsWireBinEntry::d() {return mD; }

// Non-member function
ostream& operator<<(ostream& os, const StTrsWireBinEntry& entry)
{
    return os << "WireBin(Pos:" << entry.position() << ",No.of e:" << entry.numberOfElectrons() 
	      << ",id:" << entry.id() << ")";
}
