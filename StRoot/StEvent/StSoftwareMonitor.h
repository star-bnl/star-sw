/*!
 * \class StSoftwareMonitor 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StSoftwareMonitor.h,v 2.7 2009/11/23 16:34:07 fisyak Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSoftwareMonitor.h,v $
 * Revision 2.7  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.6  2002/11/26 02:19:11  perev
 * StEventMaker ITTF modif
 *
 * Revision 2.5  2002/02/22 22:56:50  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.4  2001/04/05 04:00:42  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.3  2000/12/08 03:53:41  ullrich
 * Prepared hooks for ToF.
 *
 * Revision 2.2  1999/11/05 11:36:04  ullrich
 * Added non-const version of methods
 *
 * Revision 2.1  1999/10/13 19:43:40  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSoftwareMonitor_hh
#define StSoftwareMonitor_hh

#include "StObject.h"

class StFtpcSoftwareMonitor;
class StRichSoftwareMonitor;
class dst_mon_soft_ftpc_st;
class dst_mon_soft_rich_st;

class StSoftwareMonitor : public StObject {
public:
    StSoftwareMonitor();
    StSoftwareMonitor(const dst_mon_soft_ftpc_st*,
                      const dst_mon_soft_rich_st*);
    StSoftwareMonitor& operator=(const StSoftwareMonitor&);
    StSoftwareMonitor(const StSoftwareMonitor&);
    virtual ~StSoftwareMonitor();
                      
    StFtpcSoftwareMonitor*         ftpc();
    const StFtpcSoftwareMonitor*   ftpc() const;
    StRichSoftwareMonitor*         rich();
    const StRichSoftwareMonitor*   rich() const;

    void setFtpcSoftwareMonitor  (StFtpcSoftwareMonitor*  );
    void setRichSoftwareMonitor  (StRichSoftwareMonitor*  );

    void setFtpcSoftwareMonitor  (dst_mon_soft_ftpc_st*   );
    void setRichSoftwareMonitor  (dst_mon_soft_rich_st*   );
   
protected:
    StFtpcSoftwareMonitor   *mFtpcMonitor;
    StRichSoftwareMonitor   *mRichMonitor;
    
    ClassDef(StSoftwareMonitor,2)
};
#endif
