/******************************************************************
 * $Id: StRichReader.h,v 1.2 2000/02/08 16:24:58 lasiuk Exp $
 *
 * Description:
 *   Reader is the common interface for any reader module for 
 *   StRichRawData project.
 *
 *******************************************************************
 * $Log: StRichReader.h,v $
 * Revision 1.2  2000/02/08 16:24:58  lasiuk
 * Remove from package next revision.  I/O handled by maker
 *
 * Revision 1.2  2000/02/08 16:24:58  lasiuk
 * Remove from package next revision.  I/O handled by maker
 *
 * Revision 1.1  2000/01/18 21:32:04  lasiuk
 * Initial Revision
 *
 *   revision history:
 *     - 7/29/1999 created the class, Alexandre Nevski.
 *
 ********************************************************************/
#ifdef NEVER
#ifndef ST_RICH_READER_H
#define ST_RICH_READER_H

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
    
    template<class Out,class In> class StRichReader {
    public:

    virtual ~StRichReader() { } 

    virtual int operator()( Out& ) = 0;

    virtual int check(In) const = 0;

  };

#ifndef ST_NO_NAMESPACES
//} 
#endif

#endif
#endif
