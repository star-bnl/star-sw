/***************************************************************************
 *
 * $Id: StVertexDefinitions.h,v 2.2 2004/11/03 18:30:16 ullrich Exp $
 *
 * Author: unknown
 ***************************************************************************
 *
 * Description: Numbering scheme for vertex types.
 *              The definitions listed here are used in StEnumeration
 *              to define the actual enumerations to be used in C/C++
 *              code. The definitions here are only to allow fortran
 *              code to use the same identifiers and thus maintain
 *              backwards compatibility.
 *
 ***************************************************************************
 *
 * $Log: StVertexDefinitions.h,v $
 * Revision 2.2  2004/11/03 18:30:16  ullrich
 * Added definitions for FTPC calibration vertices.
 *
 *
 **************************************************************************/
#ifndef StVertexDefinitions_hh
#define StVertexDefinitions_hh

#define kUndefinedVertexIdentifier           0
#define kEventVertexIdentifier               1
#define kV0DecayIdentifier                   2
#define kXiDecayIdentifier                   3
#define kKinkDecayIdentifier                 4
#define kOtherTypeIdentifier                 5
#define kFtpcEastCalibrationVertexIdentifier 6
#define kFtpcWestCalibrationVertexIdentifier 7

#endif

