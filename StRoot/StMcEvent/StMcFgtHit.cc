/***************************************************************************
 *
 * $Id: StMcFgtHit.cc,v 2.6 2011/10/17 00:24:00 fisyak Exp $
 * $Log: StMcFgtHit.cc,v $
 * Revision 2.6  2011/10/17 00:24:00  fisyak
 * Add time of flight for hits
 *
 * Revision 2.5  2009/10/13 19:14:27  perev
 * Wei-Ming update
 *
 * Revision 2.4  2005/11/22 21:44:51  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.3  2005/09/29 01:01:10  calderon
 * Fixed bugs in printing event and hit information.
 * Format operator<< for various classes.
 *
 * Revision 2.2  2005/07/19 20:07:34  calderon
 * Addition of default constructor, including base class StMcHit constructor.
 * Bracket calls to StMemoryPool inside #ifdef.
 *
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 **************************************************************************/
#include "StMcFgtHit.hh"

static const char rcsid[] = "$Id: StMcFgtHit.cc,v 2.6 2011/10/17 00:24:00 fisyak Exp $";

ClassImp(StMcFgtHit)

ostream&  operator<<(ostream& os, const StMcFgtHit& h)
{
    os << "StMcFgtHit" << endl;
    os << *((StMcHit *) &h);
    os << "Layer           : " << h.layer()    << endl;
    os << "Quad            : " << h.quad()     << endl;
    return os;
}

ULong_t
StMcFgtHit::layer() const
{    
  ULong_t iLayer; // layer = disk in StFgtGeom

  // volumeId encoded in UPGR16  
  Int_t numbv1 = volumeId()/1000000;
  Int_t numbv2 = (volumeId()/10000)%100;
  if(numbv2 != 0) iLayer = (ULong_t) numbv1 - 1;
  else iLayer = 8;
  return iLayer; 
}

ULong_t
StMcFgtHit::quad() const
{
  ULong_t iQuad;

  // volumeId encoded in UPGR16  
  Int_t numbv1 = volumeId()/1000000;
  Int_t numbv2 = (volumeId()/10000)%100;
  if(numbv2 != 0) iQuad = (ULong_t) (numbv2 - 1);
  else iQuad = (ULong_t) (numbv1 - 1); 
  
  return iQuad;
}
//________________________________________________________________________________
void StMcFgtHit::Print(Option_t *option) const {
  cout << "FgtHit\t"; 
  StMcHit::Print();
  cout  << "\tLayer: " << layer() 
        <<  "\tQuad: " << quad() << endl;;  
}
