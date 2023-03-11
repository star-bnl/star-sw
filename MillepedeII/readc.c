
/** \file
 *  Read from C/C++ binary files.
 *
 * \author Gero Flucke, University Hamburg, 2006
 * \author Claus Kleinwort, DESY (maintenance and developement)
 *
 *  \copyright
 *  Copyright (c) 2009 - 2019 Deutsches Elektronen-Synchroton,
 *  Member of the Helmholtz Association, (DESY), HAMBURG, GERMANY \n\n
 *  This library is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version. \n\n
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details. \n\n
 *  You should have received a copy of the GNU Library General Public
 *  License along with this program (see the file COPYING.LIB for more
 *  details); if not, write to the Free Software Foundation, Inc.,
 *  675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  C-methods to handle input of C/C++ binary files as input for
 *  the fortran **pede** program (see \ref peread).
 *  This includes macros utilising \c cfortran.h to allow direct callability
 *  from fortran.
 *
 *  \c initC() has to be called once in the beginning,
 *  followed by one or several calls to \c openC() to open one or several files.
 *  \c readC() is then called to read the records sequentially. \c resetC()
 *  allows to rewind files.
 *
 *  If compiled with preprocessor macro \c USE_SHIFT_RFIO, uses \c libRFIO,
 *  i.e. includes \c shift.h instead of \c stdio.h
 *
 *  If compiled with preprocessor macro \c USE_ZLIB, uses \c libz,
 *  enables direct reading of gzipped files.
 *
 *  Written by Gero Flucke (gero.flucke@cern.ch) in 2006/7
 *  - update on July 14th, 2008
 *  - update on October 29th, 2008: return for file number in \c readC()
 *
 *  Major updates on April 24th, 2012 by C.Kleinwort:
 *  - skip records larger than buffer size (to determine max record length)
 *  - dynamic allocation of file pointer list (no hard-coded max number of files)
 *
 *  Major update on February 26th, 2014 by C.Kleinwort:
 *  - implement reading of records containing doubles (instead of floats)
 *    indicated by negative record length.
 *
 *  Last major update on April 10th, 2019 by C.Kleinwort:
 *  - Option to close and reopen files 
 */

#ifdef USE_SHIFT_RFIO
#include <shift.h>
// or this??
// // needed?#define _LARGEFILE64_SOURCE
//#include <sys/types.h>
//#include "rfio_api.h"
#else
#include <stdio.h>
#endif
#include "cfortran.h"
#ifdef USE_ZLIB
#include <zlib.h>
#endif

/* ________ global variables used for file handling __________ */

#ifdef USE_ZLIB
gzFile **files;   ///< pointer to list of pointers to opened binary files
#else
FILE **files;     ///< pointer to list of pointers to opened binary files
#endif

unsigned int maxNumFiles;      ///< max number of files
unsigned int numAllFiles;      ///< number of opened files

/*______________________________________________________________*/
/// Initialises the 'global' variables used for file handling.
/**
 * \param[in]  nFiles  Maximal number of C binary files to use.
 */
void initC(int *nFiles) {
	maxNumFiles = *nFiles;
#ifdef USE_ZLIB
	printf(" initC: using zlib version %s\n",ZLIB_VERSION);
	files = (gzFile **) malloc(sizeof(gzFile *)*maxNumFiles);
#else
	files = (FILE **) malloc(sizeof(FILE *) * maxNumFiles);
#endif
	{
		int i = 0;
		for (; i < maxNumFiles; ++i) {
			files[i] = 0;
		}
	}
	numAllFiles = 0;
}
FCALLSCSUB1( initC, INITC, initc, PINT)

/*______________________________________________________________*/

/* void rewinC() */
/* { */
/*   /\* rewind all open files and start again with first file *\/ */

/*   unsigned int i = numAllFiles; */
/*   while (i--) rewind(files[i]); /\* postfix decrement! *\/ */
/*   fileIndex = 0; */
/* } */
/* FCALLSCSUB0(rewinC,REWINC,rewinc) */

/*______________________________________________________________*/
/// Rewind file.
/**
 * \param[in]  nFileIn  File number (1 .. maxNumFiles)
 */
void resetC(int *nFileIn) {
	int fileIndex = *nFileIn - 1; /* index of current file */
	if (fileIndex < 0)
		return; /* no file opened at all... */
#ifdef USE_ZLIB
	gzrewind(files[fileIndex]);
#else
	/* rewind(files[fileIndex]);  Does not work with rfio, so call: */
	fseek(files[fileIndex], 0L, SEEK_SET);
	clearerr(files[fileIndex]); /* These two should be the same as rewind... */
#endif
}
FCALLSCSUB1( resetC, RESETC, resetc, PINT)

/*______________________________________________________________*/
/// Close file.
/**
 * \param[in]  nFileIn  File number (1 .. maxNumFiles)
 */
void closeC(int *nFileIn) {
	int fileIndex = *nFileIn - 1; /* index of current file */
	if (fileIndex < 0)
		return; /* no file opened at all... */
#ifdef USE_ZLIB
	gzclose(files[fileIndex]);
#else
	fclose(files[fileIndex]);
#endif
        files[fileIndex] = 0;
}
FCALLSCSUB1( closeC, CLOSEC, closec, PINT)

/*______________________________________________________________*/
/// Open file.
void openC(const char *fileName, int *nFileIn, int *errorFlag)
/**
 * \param[in]  fileName  File name
 * \param[in]  nFileIn  File number (1 .. maxNumFiles) or <=0 for next one
 * \param[out] errorFlag error flag:
 *      * 0: if file opened and OK,
 *      * 1: if too many files open,
 *      * 2: if file could not be opened
 *      * 3: if file opened, but with error (can that happen?)
 */
{
	/* No return value since to be called as subroutine from fortran */

	if (!errorFlag)
		return; /* 'printout' error? */

	int fileIndex = *nFileIn - 1; /* index of specific file */
	if (fileIndex < 0) fileIndex = numAllFiles; /* next one */
        
	if (fileIndex >= maxNumFiles) {
		*errorFlag = 1;
	} else {
#ifdef USE_ZLIB
		files[fileIndex] = gzopen(fileName, "rb");
		if (!files[fileIndex]) {
			*errorFlag = 2;
		} else
#else
		files[fileIndex] = fopen(fileName, "rb");
		if (!files[fileIndex]) {
			*errorFlag = 2;
		} else if (ferror(files[fileIndex])) {
			fclose(files[fileIndex]);
			files[fileIndex] = 0;
			*errorFlag = 3;
		} else
#endif
		{
			++numAllFiles; /* We have one more opened file! */
			*errorFlag = 0;
		}
	}
}
FCALLSCSUB3( openC, OPENC, openc, STRING, PINT, PINT)

/*______________________________________________________________*/
/// Read record from file.
/**
 * \param[out]    bufferDouble  read buffer for doubles
 * \param[out]    bufferFloat   read buffer for floats
 * \param[out]    bufferInt     read buffer for integers
 * \param[in,out] lengthBuffers in: buffer length, out: number of floats/ints in records
 *                              (> buffer size: record skipped)
 * \param[in]     nFileIn       File number (1 .. maxNumFiles)
 * \param[out]    errorFlag     error flag:
 *      *  -1: pointer to a buffer or lengthBuffers are null
 *      *  -2: problem reading record length
 *      *  -4: given buffers too short for record
 *      *  -8: problem with stream or EOF reading floats
 *      * -16: problem with stream or EOF reading ints
 *      * -32: problem with stream or EOF reading doubles
 *      *  =0: reached end of file (or read empty record?!)
 *      *  =4: found floats
 *      *  =8: found doubles
 */
void readC(double *bufferDouble, float *bufferFloat, int *bufferInt,
		int *lengthBuffers, int *nFileIn, int *errorFlag) {
	/* No return value since to be called as subroutine from fortran,
	 negative *errorFlag are errors, otherwise fine.

	 *nFileIn: number of the file the record is read from,
	 starting from 1 (not 0)
	 */
	int doublePrec = 0;

	if (!errorFlag)
		return;
	*errorFlag = 0;
	int fileIndex = *nFileIn - 1; /* index of current file */
	if (fileIndex < 0)
		return; /* no file opened at all... */
	if (!bufferFloat || !bufferInt || !lengthBuffers) {
		*errorFlag = -1;
		return;
	}

	/* read length of 'record' */
	int recordLength = 0; /* becomes number of words following in file */
#ifdef USE_ZLIB
	int nCheckR = gzread(files[fileIndex], &recordLength, sizeof(recordLength));
	if (gzeof(files[fileIndex])) {
		/* gzrewind(files[fileIndex]); CHK: moved to binrwd */
		*errorFlag = 0; /* Means EOF of file. */
		return;
	}
	if (recordLength<0) {
		doublePrec = 1;
		recordLength = -recordLength;
	}
	if (sizeof(recordLength) != nCheckR) {
		printf("readC: problem reading length of record file %d\n", fileIndex);
		*errorFlag = -2;
		return;
	}

	if (recordLength/2 > *lengthBuffers) {
		/*     printf("readC: given buffers too short (%d, need > %d)\n", *lengthBuffers,
		 recordLength/2); */
		/* skip floats */
		int i=0;
		if (doublePrec) {
			for (; i< recordLength/2; ++i)
			{
				int nCheckD = gzread(files[fileIndex], bufferDouble, sizeof(bufferDouble[0]));
				if (nCheckD != sizeof(bufferDouble[0])) {
					printf("readC: problem with stream or EOF skipping doubles\n");
					*errorFlag = -32;
					return;
				}
			}
		} else {
			for (; i< recordLength/2; ++i)
			{
				int nCheckF = gzread(files[fileIndex], bufferFloat, sizeof(bufferFloat[0]));
				if (nCheckF != sizeof(bufferFloat[0])) {
					printf("readC: problem with stream or EOF skipping floats\n");
					*errorFlag = -8;
					return;
				}
			}
		}
		i=0;
		/* skip ints */
		for (; i< recordLength/2; ++i)
		{
			int nCheckI = gzread(files[fileIndex], bufferInt, sizeof(bufferInt[0]));
			if (nCheckI != sizeof(bufferInt[0])) {
				printf("readC: problem with stream or EOF skipping ints\n");
				*errorFlag = -16;
				return;
			}
		}

		*errorFlag = -4;
		*lengthBuffers = recordLength/2;
		return;
	} else {
		*lengthBuffers = recordLength/2;
	}

	/* read floats (i.e. derivatives + value + sigma) */
	if (doublePrec) {
		int nCheckD = gzread(files[fileIndex], bufferDouble, *lengthBuffers*8);
		if (nCheckD != *lengthBuffers*8) {
			printf("readC: problem with stream or EOF reading doubles\n");
			*errorFlag = -32;
			return;
		}
	} else {
		int nCheckF = gzread(files[fileIndex], bufferFloat, *lengthBuffers*4);
		if (nCheckF != *lengthBuffers*4) {
			printf("readC: problem with stream or EOF reading floats\n");
			*errorFlag = -8;
			return;
		}
		int i=0;
		for (; i< recordLength/2; ++i) bufferDouble[i] = (double) bufferFloat[i];
	}

	/* read ints (i.e. parameter labels) */
	int nCheckI = gzread(files[fileIndex], bufferInt, *lengthBuffers*4);
	if (nCheckI != *lengthBuffers*4) {
		printf("readC: problem with stream or EOF reading ints\n");
		*errorFlag = -16;
		return;
	}
#else
	size_t nCheckR = fread(&recordLength, sizeof(recordLength), 1,
			files[fileIndex]);
	if (feof(files[fileIndex])) {
		/* rewind(files[fileIndex]);  Does not work with rfio, so call: */
		/* fseek(files[fileIndex], 0L, SEEK_SET); CHK: moved to binrwd
		clearerr(files[fileIndex]); These two should be the same as rewind... */
		*errorFlag = 0; /* Means EOF of file. */
		return;
	}

	if (1 != nCheckR || ferror(files[fileIndex])) {
		printf("readC: problem reading length of record, file %d\n", fileIndex);
		*errorFlag = -2;
		return;
	}

	if (recordLength < 0) {
		doublePrec = 1;
		recordLength = -recordLength;
	}
	if (recordLength / 2 > *lengthBuffers) {
		/* printf("readC: given buffers too short (%d, need > %d)\n", *lengthBuffers,
		 recordLength/2); */
		/* skip floats */
		int i = 0;
		if (doublePrec) {
			for (; i < recordLength / 2; ++i) {
				size_t nCheckD = fread(bufferDouble, sizeof(bufferDouble[0]), 1,
						files[fileIndex]);
				if (ferror(files[fileIndex]) || feof(files[fileIndex])
						|| nCheckD != *lengthBuffers) {
					printf(
							"readC: problem with stream or EOF skipping doubles\n");
					*errorFlag = -32;
					return;
				}
			}
		} else {
			for (; i < recordLength / 2; ++i) {
				size_t nCheckF = fread(bufferFloat, sizeof(bufferFloat[0]), 1,
						files[fileIndex]);
				if (ferror(files[fileIndex]) || feof(files[fileIndex])
						|| nCheckF != *lengthBuffers) {
					printf(
							"readC: problem with stream or EOF skipping floats\n");
					*errorFlag = -8;
					return;
				}
			}
		}
		i = 0;
		/* skip ints */
		for (; i < recordLength / 2; ++i) {
			size_t nCheckI = fread(bufferInt, sizeof(bufferInt[0]), 1,
					files[fileIndex]);
			if (ferror(files[fileIndex]) || feof(files[fileIndex])
					|| nCheckI != *lengthBuffers) {
				printf("readC: problem with stream or EOF skiping ints\n");
				*errorFlag = -16;
				return;
			}
		}

		*errorFlag = -4;
		*lengthBuffers = recordLength / 2;
		return;
	} else {
		*lengthBuffers = recordLength / 2;
	}

	/* read floats (i.e. derivatives + value + sigma) */
	if (doublePrec) {
		size_t nCheckD = fread(bufferDouble, sizeof(bufferDouble[0]),
				*lengthBuffers, files[fileIndex]);
		if (ferror(files[fileIndex]) || feof(files[fileIndex])
				|| nCheckD != *lengthBuffers) {
			printf("readC: problem with stream or EOF reading doubles\n");
			*errorFlag = -32;
			return;
		}
	} else {
		size_t nCheckF = fread(bufferFloat, sizeof(bufferFloat[0]),
				*lengthBuffers, files[fileIndex]);
		if (ferror(files[fileIndex]) || feof(files[fileIndex])
				|| nCheckF != *lengthBuffers) {
			printf("readC: problem with stream or EOF reading floats\n");
			*errorFlag = -8;
			return;
		}
		int i = 0;
		for (; i < recordLength / 2; ++i)
			bufferDouble[i] = (double) bufferFloat[i];
	}
	/* read ints (i.e. parameter labels) */
	size_t nCheckI = fread(bufferInt, sizeof(bufferInt[0]), *lengthBuffers,
			files[fileIndex]);
	if (ferror(files[fileIndex]) || feof(files[fileIndex])
			|| nCheckI != *lengthBuffers) {
		printf("readC: problem with stream or EOF reading ints\n");
		*errorFlag = -16;
		return;
	}
#endif

	*errorFlag = 4 * (doublePrec + 1);
}
FCALLSCSUB6(readC,READC,readc,PDOUBLE,PFLOAT,PINT,PINT,PINT,PINT)
