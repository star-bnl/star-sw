/*!
 * \file StVertexDefinitions.h
 * 
 * \author unknown
 * 
 * Numbering scheme for vertex types.
 * The definitions listed here are used in StEnumeration
 * to define the actual enumerations to be used in C/C++
 * code. The definitions here are only to allow fortran
 * code to use the same identifiers and thus maintain
 * backwards compatibility.
 * 
 */
/***************************************************************************
 *
 * $Id: StVertexDefinitions.h,v 2.3 2004/11/05 04:06:15 jeromel Exp $
 *
 * Author: unknown
 ***************************************************************************
 *
 *
 ***************************************************************************
 *
 * $Log: StVertexDefinitions.h,v $
 * Revision 2.3  2004/11/05 04:06:15  jeromel
 * doxygenized doc so defines will become cross refs in auto-documentation
 *
 * Revision 2.2  2004/11/03 18:30:16  ullrich
 * Added definitions for FTPC calibration vertices.
 *
 *
 **************************************************************************/
#ifndef StVertexDefinitions_hh
#define StVertexDefinitions_hh

#define kUndefinedVertexIdentifier           0                         /*! \def kUndefinedVertexIdentifier */
#define kEventVertexIdentifier               1                         /*! \def kEventVertexIdentifier */
#define kV0DecayIdentifier                   2                         /*! \def kV0DecayIdentifier */
#define kXiDecayIdentifier                   3                         /*! \def kXiDecayIdentifier */
#define kKinkDecayIdentifier                 4                         /*! \def kKinkDecayIdentifier */
#define kOtherTypeIdentifier                 5                         /*! \def kOtherTypeIdentifier */
#define kFtpcEastCalibrationVertexIdentifier 6                         /*! \def kFtpcEastCalibrationVertexIdentifier */
#define kFtpcWestCalibrationVertexIdentifier 7                         /*! \def kFtpcWestCalibrationVertexIdentifier */

#endif

