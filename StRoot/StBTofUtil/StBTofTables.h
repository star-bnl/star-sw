/*******************************************************************
 *
 * $Id: StBTofTables.h,v 1.1 2009/08/28 17:22:02 dongx Exp $
 *
 *****************************************************************
 *
 * $Log: StBTofTables.h,v $
 * Revision 1.1  2009/08/28 17:22:02  dongx
 * first release
 *
 *
 *******************************************************************/
#ifndef STBTOFTABLES_H
#define STBTOFTABLES_H

#include "TObject.h"

#include "tables/St_tofTrayConfig_Table.h"
#include "tables/St_tofStatus_Table.h"

class StMaker;

/**
   \class StBTofTables
   Class to retrieve the tof tray/channel status from data base
 */
class StBTofTables : public TObject
{
protected:
    static const Int_t mNTray = 120;
    static const Int_t mNModule = 32;
    static const Int_t mNCell = 6;
    static const Int_t mNChanMax = 24000;  /// A large number for total channels

    /// Tray/Channel Status    
    UShort_t     mBTofTrayConfig[mNTray];    
    UShort_t     mBTofStatus[mNTray][mNModule][mNCell];

public:
    /// Default constructor
    StBTofTables();
    virtual ~StBTofTables();
    void    Reset();
    
    /// load status tables from data base
    void    loadTables(StMaker* anyMaker);
    
    /// function to return the tray status
    bool  trayValid(int trayId) const;
    /// function to return the channel status
    int   status(int trayId, int moduleId, int cellId) const;
    
    ClassDef(StBTofTables, 1);
        
};

#endif
