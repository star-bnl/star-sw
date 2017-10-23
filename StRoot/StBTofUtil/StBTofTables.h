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
class StMaker;
/**
   \class StBTofTables
   Class to retrieve the tof tray/channel status from data base
 */
class StBTofTables : public TObject
{
protected:
 public:
    /// Default constructor
   StBTofTables() {Reset();}
   virtual ~StBTofTables() {Reset();}
   void    Reset() {}
    
   /// load status tables from data base
   void    loadTables(StMaker */* mk */) {loadTables();}
   void    loadTables() {}
    /// function to return the channel status
    static Int_t   status(Int_t trayId, Int_t moduleId, Int_t cellId);
    
    ClassDef(StBTofTables, 1);
        
};

#endif
