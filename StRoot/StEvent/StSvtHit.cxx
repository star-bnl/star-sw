/***************************************************************************
 *
 * $Id: StSvtHit.cxx,v 2.18 2009/11/23 22:20:51 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.cxx,v $
 * Revision 2.18  2009/11/23 22:20:51  ullrich
 * Minor cleanup performed, fixed compiler warnings.
 *
 * Revision 2.17  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.16  2009/11/10 00:41:03  ullrich
 * Changed print-out format and added new method shell().
 *
 * Revision 2.15  2007/09/20 20:02:32  ullrich
 * Added new members to hold and access the number of anodes and of pixels.
 *
 * Revision 2.14  2006/04/27 21:59:00  ullrich
 * Added data member and methods to deal with local positions.
 *
 * Revision 2.13  2005/07/19 21:37:56  perev
 * Cleanup
 *
 * Revision 2.12  2005/06/15 01:21:01  ullrich
 * In constructor using dst_point: set mFitFlag
 *
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

static const char rcsid[] = "$Id: StSvtHit.cxx,v 2.18 2009/11/23 22:20:51 ullrich Exp $";

ClassImp(StSvtHit)
    
StMemoryPool StSvtHit::mPool(sizeof(StSvtHit));

StSvtHit::StSvtHit()
{
    mPeak = 0;
    mAnode = 0;
    mTimebucket = 0;
    mLocalPosition[0] = 0;
    mLocalPosition[1] = 0;
    mNumberOfAnodes = 0;
    mNumberOfPixels = 0;
}

StSvtHit::StSvtHit(const StThreeVectorF& p,
                   const StThreeVectorF& e,
                   unsigned int hw, float q, unsigned char c)
    : StHit(p, e, hw, q, c)
{
    mPeak = 0;
    mAnode = 0;
    mTimebucket = 0;
    mLocalPosition[0] = 0;
    mLocalPosition[1] = 0;
    mNumberOfAnodes = 0; // need to be set through access functions
    mNumberOfPixels = 0; // need to be set through access functions
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
StSvtHit::layer() const {return layer(barrel(), ladder());} 

unsigned int
StSvtHit::layer(unsigned int Barrel, unsigned int Ladder) {
    
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
StSvtHit::localPosition(unsigned int i) const
{
    if (i<2)
        return mLocalPosition[i];
    else
        return 0;
}

void
StSvtHit::setLocalPosition(float u, float v)
{
    mLocalPosition[0] = u;
    mLocalPosition[1] = v;
}

void StSvtHit::setNumberOfAnodes(unsigned short val)
{
    mNumberOfAnodes = val;
}

void StSvtHit::setNumberOfPixels(unsigned short val)
{
    mNumberOfPixels = val;
}

int StSvtHit::numberOfAnodes() const
{
    return mNumberOfAnodes;
}

int StSvtHit::numberOfPixels() const
{
    return mNumberOfPixels;
}

float StSvtHit::peakADC() const
{
    return mPeak;
}

float StSvtHit::anode() const
{ 
    return mAnode; 
} 

unsigned int 
StSvtHit::shell(unsigned int barrel, unsigned int ladder) {
    return ((barrel == 1 && ladder <= 4) || 
            (barrel == 2 && ladder <= 6) || 
            (barrel == 3 && ladder <= 8)) ? 1 : 2;
}

unsigned int
StSvtHit::shell() const {return shell(barrel(), ladder());}


int
StSvtHit::volumeID() const {return 10000 * shell() + 1000 * layer() + 100 * wafer() + ladder();}

ostream&  operator<<(ostream& os, const StSvtHit& v)
{
    return os << Form("Svt b:%2i l:%2i w:%2i h:%1i",v.barrel(),v.ladder(), v.wafer(), v.hybrid())
	    << *((StHit *)&v)
	    << Form(" P:%8.3f",v.peakADC())
	    << Form(" Luv: %8.3f %8.3f anode %8.3f timeb %8.3f",v.localPosition(0),v.localPosition(1),v.anode(),v.timebucket());
}
void   StSvtHit::Print(Option_t *option) const {cout << *this << endl;}
