// $Id: StFtpcFastSimu.hh,v 1.14 2002/09/16 12:43:22 jcs Exp $
//
// $Log: StFtpcFastSimu.hh,v $
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
#include "fcl_fppoint.h"
#include "TObjArray.h"
#include "StFtpcGeantPoint.hh"
#include "StFtpcReducedPoint.hh"

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
  StFtpcReducedPoint *mPoint;
  StFtpcGeantPoint *mGeantPoint;
  int nPoints;
  int nPadrows;
  int * nrowmax;
  int * nrow;

  float Va;
  float Vhm[4];
  float Tbm[4];
  float s_rad[4];
  float s_azi[4];
  float err_rad[4];
  float err_azi[4];
  float ri;
  float ra;
  float phimin;
  float phisec;
  float sector_phi_min;
  float sector_phi_max;
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
