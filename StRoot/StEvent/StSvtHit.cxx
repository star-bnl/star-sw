/***************************************************************************
 *
 * $Id: StSvtHit.cxx,v 2.11 2004/08/06 15:37:09 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.cxx,v $
 * Revision 2.11  2004/08/06 15:37:09  fisyak
 * Add clster id
 *
 * Revision 2.10  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.9  2001/08/07 20:50:57  caines
 * Implement better packing of hardware and charge values
 *
 * Revision 2.8  2001/04/05 04:00:56  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.7  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
 * Revision 2.6  2000/06/01 21:39:03  ullrich
 * Added member mFlag and access member flag() and setFlag().
 *
 * Revision 2.5  1999/12/13 20:16:19  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.4  1999/11/11 11:03:55  ullrich
 * Inlined layer(), sector() and ladder().
 *
 * Revision 2.3  1999/11/09 19:35:20  ullrich
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
 * Revision 2.2  1999/11/04 21:40:55  ullrich
 * Added missing default constructor
 *
 * Revision 2.1  1999/10/28 22:26:41  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:43  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include "StSvtHit.h"
#include "StTrack.h"
#include "tables/St_dst_point_Table.h"

static const char rcsid[] = "$Id: StSvtHit.cxx,v 2.11 2004/08/06 15:37:09 fisyak Exp $";

ClassImp(StSvtHit)
    
StMemoryPool StSvtHit::mPool(sizeof(StSvtHit));

StSvtHit::StSvtHit() { /* noop */ }

StSvtHit::StSvtHit(const StThreeVectorF& p,
                   const StThreeVectorF& e,
                   unsigned int hw, float q, unsigned char c)
    : StHit(p, e, hw, q, c)
{ /* noop */ }

StSvtHit::StSvtHit(const dst_point_st& pt)
{
    //
    // Unpack charge and status flag
    //
    const unsigned int iflag = pt.charge/(1L<<17);    
    const unsigned int svtq  = pt.charge - iflag*(1L<<17); 
    // mPeak is private to SvtHit
    mPeak = (float)(svtq/(1L<<10));
    mCharge = (float)(svtq - mPeak*(1L<<10));
    mFlag = static_cast<unsigned char>(iflag);
    

    //
    // Unpack position in xyz
    //
    const float maxRange   = 22;
    const float mapFactor  = 23800;
    unsigned int svty11 = pt.position[0]/(1L<<20);
    unsigned int svtz   = pt.position[1]/(1L<<10);
    unsigned int svtx   = pt.position[0] - (1L<<20)*svty11;
    unsigned int svty10 = pt.position[1] - (1L<<10)*svtz;
    unsigned int svty   = svty11 + (1L<<10)*svty10;
    mPosition.setX(float(svtx)/mapFactor - maxRange);
    mPosition.setY(float(svty)/mapFactor - maxRange);
    mPosition.setZ(float(svtz)/mapFactor - maxRange);
    
    //
    // Unpack error on position in xyz
    //
    svty11 = pt.pos_err[0]/(1L<<20);
    svtz   = pt.pos_err[1]/(1L<<10);
    svtx   = pt.pos_err[0] - (1L<<20)*svty11;
    svty10 = pt.pos_err[1] - (1L<<10)*svtz;
    svty   = svty11 + (1L<<10)*svty10;
    mPositionError.setX(float(svtx)/(1L<<26));
    mPositionError.setY(float(svty)/(1L<<26));
    mPositionError.setZ(float(svtz)/(1L<<26));

    //
    // The hardware position stays at it is
    //
    mHardwarePosition = pt.hw_position;
    mId               = pt.cluster;
}

StSvtHit::~StSvtHit() {/* noop */}

unsigned int
StSvtHit::barrel() const { 
  
  int Index = index();

  if( Index < 0) return 0; // Something wrong

  if( Index < 64) return 1;        // Index starts at 0 for hybrid1,wafer1, 
  else if( Index < 208) return 2;  // ladder1,barrel1 and moves out 
  else if( Index < 432) return 3;  // Index=431 is the largest value
  return 0;  // Something wrong

}


unsigned int 
StSvtHit::ladder() const { 
  
  int Index = index();
  int mLadder;
  int mHybrid[3]={8,12,14};  // Hybrids on each ladder
  int mLadderTot[2]={8,12};  // Ladders on each barrel
  switch( barrel()){
  case 1:
    mLadder = Index/mHybrid[0];
    return mLadder+1;
    break;
  case 2:
    Index -= mHybrid[0]*mLadderTot[0];  // Subtract off hybrids from previous
    mLadder = Index/mHybrid[1];         // layers the div. by hybrids per lay 
    return mLadder+1;
    break;
  case 3:
    Index -= mHybrid[0]*mLadderTot[0]; // Subtract off hybrids from previous
    Index -= mHybrid[1]*mLadderTot[1]; // layers the div. by hybrids per lay
    mLadder = Index/mHybrid[2];
    return mLadder+1;
    break;
  default:
    return 0; //Something Wrong
  }


}

unsigned int
StSvtHit::layer() const {

  int Barrel = barrel();
  int Ladder = ladder();

  if( Ladder%2){
    switch( Barrel){
    case 1:
      return 2;    // Outer layers are the odd numbered ladders
      break;
    case 2:
      return 4;
      break;
    case 3:
      return 6;
      break;
    default:
      return 0;
    }
  }
   else{
     switch( Barrel){
     case 1: 
       return 1;  // Inner layers are the even ladders
       break;
     case 2:
       return 3;
       break;
     case 3:
       return 5;
       break;
     default:
       return 0;
     }
   }

  return 0;
}


unsigned int
StSvtHit::wafer() const
{
 
  int Index = index();
  int Barrel = barrel()-1;
  int Ladder = ladder()-1;
  int mHybrid[3] ={8,12,14};  // Number of hybrids per ladder
  int mLadderTot[3] ={8,12,16}; // Number of ladders per barrel
  
  for( int B=0; B<Barrel; B++){
    Index -= mHybrid[B]*mLadderTot[B]; // Sub. the hybrids from prev. barrels
  }

  for( int L=0; L<Ladder; L++){
    Index -= mHybrid[Barrel]; // Sub. hybrids from previous ladders on  barrel
  }

  return (Index/2)+1;  // Two hybrids per wafer start counting from 1

}


unsigned int
StSvtHit::hybrid() const { return ((index()%2)+1); }


float
StSvtHit::anode() const {
  float anode = mHardwarePosition >> 22;
  return (anode/4.); }  // Anode packed in quarters


float
StSvtHit::timebucket() const {
  float t = mHardwarePosition >> 13;
    t = t-(anode()*4*(1L<<9));
    t /=4.; // timebucket packed in quarters
  return t;    
}
