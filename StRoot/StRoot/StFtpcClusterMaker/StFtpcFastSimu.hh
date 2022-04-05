// $Id: StFtpcFastSimu.hh,v 1.17 2004/02/12 19:38:46 oldi Exp $
//
// $Log: StFtpcFastSimu.hh,v $
// Revision 1.17  2004/02/12 19:38:46  oldi
// Removal of intermediate tables.
//
// Revision 1.16  2004/01/28 02:04:43  jcs
// replace all instances of StFtpcReducedPoint and StFtpcPoint with StFtpcConfMapPoint
//
// Revision 1.15  2003/10/10 12:36:15  jcs
// implement new FTPC geant volume id method
// initialize counters and arrays to zero
// replace many int,float's wiht Int_t,Float_t
//
// Revision 1.14  2002/09/16 12:43:22  jcs
// replace large statically dimensioned arrays with dynamically dimensioned arrays
//
// Revision 1.13  2001/03/19 15:52:47  jcs
// use ftpcDimensions from database
//
// Revision 1.12  2001/01/25 15:25:49  oldi
// Fix of several bugs which caused memory leaks:
//  - Some arrays were not allocated and/or deleted properly.
//  - TClonesArray seems to have a problem (it could be that I used it in a
//    wrong way in StFtpcTrackMaker form where Holm cut and pasted it).
//    I changed all occurences to TObjArray which makes the program slightly
//    slower but much more save (in terms of memory usage).
//
// Revision 1.11  2001/01/08 17:10:04  jcs
// move remaining constants from code to database
//
// Revision 1.10  2000/11/24 15:02:33  hummler
// commit changes omitted in last commit
//
// Revision 1.8  2000/09/18 14:26:49  hummler
// expand StFtpcParamReader to supply data for slow simulator as well
// introduce StFtpcGeantReader to separate g2t tables from simulator code
// implement StFtpcGeantReader in StFtpcFastSimu
//
// Revision 1.7  2000/08/03 14:39:00  hummler
// Create param reader to keep parameter tables away from cluster finder and
// fast simulator. StFtpcClusterFinder now knows nothing about tables anymore!
//
// Revision 1.6  2000/02/04 13:49:42  hummler
// upgrade ffs:
// -remove unused fspar table
// -make hit smearing gaussian with decent parameters and static rand engine
// -separate hit smearing from cluster width calculation
//
// Revision 1.5  2000/02/02 15:40:08  hummler
// make hit smearing gaussian instead of box-shaped
//
// Revision 1.4  2000/02/02 15:20:37  hummler
// correct acceptance at sector boundaries,
// take values from fcl_det
//
// Revision 1.3  2000/01/03 12:48:59  jcs
// Add CVS Id strings
//

#ifndef STAR_StFtpcFastSimu
#define STAR_StFtpcFastSimu
#include "ffs_gepoint.h"
#include "TObjArray.h"
#include "StFtpcGeantPoint.hh"
#include "StFtpcTrackMaker/StFtpcConfMapPoint.hh"

#define TRUE 1
#define FALSE 0

class RandGauss;
class StFtpcParamReader;
class StFtpcDbReader;
class StFtpcGeantReader;

class StFtpcFastSimu
{
 private:
  StFtpcParamReader *mParam;
  StFtpcDbReader    *mDb;
  StFtpcGeantReader *mGeant;
  StFtpcConfMapPoint *mPoint;
  StFtpcGeantPoint *mGeantPoint;

  Char_t mStart;  //  start of simple variables
  
  Int_t nPoints;
  Int_t nPadrows;
  Int_t * nrowmax;
  Int_t * nrow;

  Float_t Va;
  Float_t Vhm[4];
  Float_t Tbm[4];
  Float_t s_rad[4];
  Float_t s_azi[4];
  Float_t err_rad[4];
  Float_t err_azi[4];
  Float_t ri;
  Float_t ra;
  Float_t phimin;
  Float_t phisec;
  Float_t sector_phi_min;
  Float_t sector_phi_max;

  Char_t mEnd; //End of simple variables

  double myModulo(double x1, double x2)
    {
      return x1-(double)(int)(x1/x2)*x2;
    }
 public:
  StFtpcFastSimu(StFtpcGeantReader *geantReader,
		 StFtpcParamReader *paramReader,
                 StFtpcDbReader    *dbReader,
		 TObjArray *pointarray,
		 TObjArray *geantarray);
  ~StFtpcFastSimu();
  int ffs_gen_padres();
  int ffs_hit_rd();
  int ffs_hit_smear(float phi, 
		    float xi, 
		    float yi, 
		    float zi, 
		    float *xo, 
		    float *yo, 
		    float *zo,
		    float st_dev_l_hit, 
		    float st_dev_tr_hit,
		    float *st_dev_z,
		    float *st_dev_x,
		    float *st_dev_y,  
		    RandGauss *quasiRandom);
  int ffs_ini();
  int ffs_merge_tagger();
  int ffs_tag();
};

#endif
