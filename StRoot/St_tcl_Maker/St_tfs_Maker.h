// $Id: St_tfs_Maker.h,v 1.1 2007/12/28 13:47:41 fisyak Exp $
// $Log: St_tfs_Maker.h,v $
// Revision 1.1  2007/12/28 13:47:41  fisyak
// Split tcl and tfs Makers
//
// Revision 1.22  2002/02/05 22:21:56  hardtke
// Move Init code to InitRun
//
// Revision 1.21  2001/05/22 22:32:50  hardtke
// Add option for returning hits in global coordinates
//
// Revision 1.20  2000/08/22 00:17:55  hardtke
// Add ability to turn off either half of TPC:  new functions EastOff(), WestOff(), AllOn()
//
// Revision 1.19  1999/12/05 00:07:05  snelling
// Modifications made for eval option: added Histograms and NTuple support
//
// Revision 1.18  1999/11/22 23:18:46  snelling
// added Li Qun's changes to tfs
//
// Revision 1.17  1999/11/20 20:53:51  snelling
// Removed hitclus table and added entries to tphit table
//
// Revision 1.16  1999/10/07 03:24:51  snelling
// created tables dynamically, correct for TFS - TRS/DATA ipix/10
//
// Revision 1.15  1999/10/05 00:46:07  snelling
// added some histogram protections
//
// Revision 1.14  1999/10/01 22:22:25  snelling
// updated histograms
//
// Revision 1.13  1999/09/24 01:23:43  fisyak
// Reduced Include Path
//
// Revision 1.12  1999/07/15 13:58:24  perev
// cleanup
//
// Revision 1.11  1999/03/17 19:23:51  sakrejda
// unpacking of raw data into adcxyz table with an on/off switch added
//
// Revision 1.11  1999/03/17 00:10:43  snellings
// switch for the pixel translation stuff added
//
// Revision 1.10  1999/03/16 00:20:43  sakrejda
// switch for the cluster morphology stuff added
//
// Revision 1.9  1999/03/13 23:34:03  perev
// New makers
//
// Revision 1.8  1999/03/11 20:40:20  ward
// Add code for cluster morphology.
//
// Revision 1.7  1999/03/01 18:53:33  sakrejda
// hit eveluation switchable
//
// Revision 1.6  1999/02/10 20:57:40  kathy
// added histograms to Maker
//
// Revision 1.5  1998/12/16 22:19:20  fisyak
// New tfs
//
// Revision 1.4  1998/10/31 00:26:22  fisyak
// Makers take care about branches
//
// Revision 1.3  1998/10/06 18:00:48  perev
// cleanup
//
// Revision 1.2  1998/08/26 12:15:10  fisyak
// Remove asu & dsl libraries
//
// Revision 1.1  1998/07/21 00:36:46  fisyak
// tcl and tpt
//
#ifndef St_tfs_Maker_H
#define St_tfs_Maker_H
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_tfs_Maker: Wrapper for fortran tph and tcl code                   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "StMaker.h"
#include "tables/St_tpg_pad_plane_Table.h"
#include "tables/St_tcl_tpc_index_type_Table.h"
class St_tfs_fspar;
class St_tfs_fsctrl;

class St_tcl_tphit;
class St_tcl_tpc_index;
class St_tpg_pad_plane;
class St_tpg_detector;

class St_tfs_Maker : public StMaker {

 public: 

  St_tfs_Maker(const char *name="tpc_hits"): StMaker(name) {}
  virtual       ~St_tfs_Maker() {}
  virtual Int_t  InitRun(int runnumber);
  virtual Int_t  Make();
  virtual void   PrintInfo();
  virtual void   FillStEvent(St_tcl_tphit *tphit);
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: St_tfs_Maker.h,v 1.1 2007/12/28 13:47:41 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
 private:
  // define the tables used
  St_tfs_fspar*          m_tfs_fspar;   	//! TFS parameter table 
  St_tfs_fsctrl*         m_tfs_fsctrl;  	//! TFS control switches

  St_tcl_tphit*          tphit;                 //! TPC hit table
  St_tcl_tpc_index*      index;                 //! TPC evaluation table
  St_tpg_pad_plane *     m_tpg_pad_plane; 
  St_tpg_detector  *     m_tpg_detector;
  St_tcl_tpc_index_type* m_type;
  ClassDef(St_tfs_Maker, 1)       //Cint definition
};
#endif
