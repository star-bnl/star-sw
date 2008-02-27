#ifndef _EVP_READER_HH_
#define _EVP_READER_HH_


#include <sys/types.h>
#include <sys/stat.h>

#include <evp.h>		// for some constants

#include <evpSupport.h>

#ifndef RTS_PROJECT_PP	// STAR specific...

#include <scReader.h>
#include <trgReader.h>
#include <tpcReader.h>
#include <svtReader.h>
#include <ftpReader.h>
#include <ricReader.h>
#include <tofReader.h>
#include <fpdReader.h>
#include <pmdReader.h>
#include <emcReader.h>
#include <l3Reader.h>
#include <ssdReader.h>
#include "cfgutil.h"

#  ifndef __ROOT__
#     include <../SFS/sfs_index.h>
#  else 
#     include "SFS/sfs_index.h"
#  endif
#include <../RTS_READER/rts_reader.h>


#endif

#include <pp2ppReader.h>
#include "evpReaderClass.h"

#endif
