/*!
 * \file StSsdEnumerations.hh
 */
/***************************************************************************
 *
 *  StSsdEnumerations.
 *
 * Author: cr
 ***************************************************************************
 *
 * Description: SSD Enumerations
 **************************************************************************/

#ifndef STSSDENUMERATIONS_HH
#define STSSDENUMERATIONS_HH

/*!
 * \enum pedestalType 
 */
enum pedestalType {kCapacitor, kTime};
/*!
 * \enum dbSsdType 
 */
enum dbSsdType {kCalibration, kGeometry, kConditions};


class StSsdPack
{
// 		id = 100000*id_strip+10000*id_side+id_wafer
//  		id_wafer=   1000*SsdLayer+   100*(iWaf+1) + (iLad+1)
public:
  StSsdPack(int w=0) 			{ mW = w;}
  StSsdPack(int strip_number,int id_side,int id_wafer)	
  					{ mW = pack(strip_number,id_side,id_wafer);}
  StSsdPack(int iWaf,int iLad)	        { mW = pack(iWaf,iLad);}
StSsdPack &operator=(int w)		{ mW = w; return *this;}
operator int  ()const                   { return mW;}  
operator long ()const                   { return mW;}  

static int getNStrip(int w)		{ return ( w/100000)		;}
static int getNClust(int w)		{ return ( w/100000)		;}
static int getIdWaf (int w) 		{ return ( w%10000)		;}
static int getWaf   (int w)   		{ return ((w/100)%100) -70 -1	;}
static int getLad   (int w)   		{ return ( w%100)	    -1	;}
static int getSide  (int w)   		{ return ( w/10000)%10		;}
static int pack(int strip_number,int id_side,int id_wafer)
                                        {return 100000*strip_number+10000*id_side+id_wafer;}
static int pack(int iWaf,int iLad)      {return 7000 + (iWaf+1)*100 + (iLad+1);}
  					

int getNStrip()	const			{ return getNStrip(mW)		;}
int getNClust()	const			{ return getNClust(mW)		;}
int getIdWaf () const			{ return getIdWaf (mW)		;}
int getWaf ()   const			{ return getWaf   (mW)		;}
int getLad ()   const			{ return getLad   (mW)		;}
int getSide()   const			{ return getSide  (mW)		;}
protected:
int mW;
};
#endif
