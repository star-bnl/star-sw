/******************************************************
 *
 * $Id: StPmdHit.cxx,v 1.3 2010/05/28 17:23:33 rashmi Exp $
 * Author: Subhasis Chattopadhyay, Dec 2001
 *
 ******************************************************
 *
 *Description: This is the class for Pmd hit objects
 *
 ******************************************************
 * $Log: StPmdHit.cxx,v $
 * Revision 1.3  2010/05/28 17:23:33  rashmi
 * Added sorting routine
 *
 * Revision 1.2  2002/09/09 11:28:12  subhasis
 * ADC added
 *
 *
 ******************************************************/
#include "StPmdHit.h"

ClassImp(StPmdHit)

//__________________________________________________________
StPmdHit::StPmdHit(TArrayI *hits) : StObject() 
{
}
//__________________________________________________________
StPmdHit::StPmdHit() : StObject() 
{
  mGsuper = 0;  mSubdet = 0; 
  mRow =0;  mCol=0;
  
}

void
StPmdHit::print(ostream *os)
{
  //! Printing member function.
  *os << "Gsuper " << Gsuper();
  *os << " SubDetector " << SubDetector();
  *os << " Row " << Row();
  *os << " Column " << Column();
  *os << " Edep " << Edep();
}

ostream &operator<<(ostream &os, StPmdHit &cl)
{
  cl.print(&os); return os;
}

Int_t StPmdHit::Compare(const TObject *hit) const
{
  StPmdHit*  mhit = (StPmdHit*)hit;
  if (mAdc < mhit->mAdc) return -1;
  else if (mAdc > mhit->mAdc) return 1;
  else return 0;
}

void StPmdHit::Browse(TBrowser *b)
{
  cout << (*this) << endl;
  StObject::Browse(b);
}



