# static char SccsId[] = " @(#)94   3.42.1.4   Makefile, gen, 6.2, 6.2.1.11   11/7/06   14:14:10";
#===============================================================================
#
#  Name: ./Makefile
#
#  Description:
#
#  Traceability:
#       Version    Date       Description
#       -------    --------   -------------------
#          1.x     07/05/94   Copyright Update
#	   3.1     12/01/95   added sammi to SUBDIRS
#	   3.2-3   04/05/96   Disabled ./tools dir.  Re-enabled ./tools dir.
#	   3.4     04/24/96   Added "man" dir to SUBDIRS
#	   3.6     07/15/97   added stk to SUBDIRS
#	   3.7	   08/31/98   added HDM_SUBDIRS, HDM_BUILD_FILES, hdm-build
#	   3.8-9   11/02/98   added NODCE_SUBDIRS, NODCE_BUILD_FILES, build-nodce
#	   3.10	   11/13/98   added Makefile.nodce
#	   3.11	   01/15/99   added tools to NODCE_SUBDIRS
#	   3.12	   01/28/99   changed make order
#	   3.13	   03/09/99   Added Makefile.macros.solaris and README to 
#			      BUILD_FILES
#	   3.14	   03/11/99   Added Makefile.macros.solaris and README to 
#			      HDM_BUILD_FILES and BUILD_FILES. Previous fix
#                             was only to NODCE_BUILD_FILES
#          3.15    04/19/99   added SUN_SUBDIRS, SUN_BUILD_FILES, sun-build
#          3.16    03/23/00   removed SUN_SUBDIRS, SUN_BUILD_FILES, sun-build
#          3.17    04/10/00   added libs rule (1853)
#          3.18    04/25/00   added idl-tidl to all rule (1853)
#          3.19    06/30/00   Alpha makefile mods (1767).
#          3.20    02/16/01   Linux makefile mods (2325).
#          3.21    12/04/01   Alpha makefile mods (2601).
#          3.22    11/30/01   Linux Mover mods (2584).
#          3.23    07/25/02   Initial 5.1 Checkin
#          3.24    07/25/02   Remove sammi directory (2814)
#          3.25    02/01/03   Initial 5.2 Checkin (2927)
#          3.26    04/03/03   Add rules for subtree builds (2927)
#          3.27    10/14/03   Added config to be built (2927)
#          3.28    10/24/03   2927 - Added build-ftplib
#          3.30    10/28/03   2927 - Added build-ftplib correctly
#          3.31    03/19/04   JAG-3808 Initial 6.1 XFS mods
#          3.32    04/16/04   JLS-4033 Added missing subdirs
#          3.33    06/28/04   BMW:4132 AIX port
#          3.34    10/20/04   JLS:4293 Port to Solaris/IRIX
#          3.35    12/10/04   DGH:4543 build_hdm extracts hdm msg catalog
#          3.36    01/10/05   KMT-4591 Add build-all 
#          3.37    01/27/05   KMT-4591 Modify build-all to add doc, stk subdirs;
#                             Add DOC_SUBDIRS, STK_SUBDIRS
#          3.38    10/11/05   BMW:4904 Added make build-fs
#          3.39    01/12/06   TST-5276 Added tools to MVR_SUBDIRS
#          3.40,42 05/25/06   JLS-5176 Changes for GPFS 
#      3.42.1.1    10/10/06   KMT-5712 Add build-src, build-doc, stk flags, 
#                             SRC_SUBDIRS; Modify SUBDIRS, BUILD_SUBDIRS. 
#                             Remove build-all, STK_SUBDIRS.
#      3.42.1.2    10/10/06   KMT-5712 Replace spaces with tabs 
#      3.42.1.3    10/13/06   KMT-5880 Really add SRC_SUBDIRS
#      3.42.1.4    11/07/06   KMT-5951 Add src to CLNT_SUBDIRS
# 
#   Notes:
#
#       This file is IBM Confidential.
#
#===============================================================================

#===============================================================================
#  The HPSS makefile system can be used to build all HPSS components,
#  extract a full HPSS source tree, or extract and build selective HPSS
#  components. The supported components include: the HPSS Client Library,
#  the HPSS Standalone Mover, the HPSS PFTP Client and the HPSS HDM
#  Server. The rules defined here, in the Makefile.rules and in other
#  makefiles, implement these functions. Some of the supported rules
#  are listed and described below. In general, the makefile system works
#  by making multiple passes over the HPSS source tree, compiling or
#  generating source code that is required by subsequent passes. The
#  passes (or compliation phases) are listed in order of execution
#  here:
#
#  Compliation passes (or phases)
#     1 - Compile & Link tools needed to build HPSS. This phase
#         is implemented by the 'bld-tool' rule.
#     2 - Build all header and XDR library code that is generate
#         from XDR or IDL files. This phase is implemented by
#         the 'xdr' rule.
#     3 - Build the HPSS Common Services Library (libhpsscs.so).
#         This phase is implemented by the 'libs-cs' rule.
#     4 - Build all other HPSS libraries. This phase is
#         implemented by the 'libs' rule.
#     5 - Lastly, build each individual HPSS component.
# 
#  Primary Component Extraction Rules:
#     build      - Extract the full HPSS code package
#                  Dependencies: build-files,
#                                build-subdirs
#     build-clnt - Extract the HPSS Client Library and supporting
#                  utilities code package
#                  Dependencies: build-clnt-files,
#                                build-clnt-subdirs
#     build-ftp  - Extract the HPSS PFTP Client code package
#                  Dependencies: build-ftp-files,
#                                build-ftp-subdirs
#     build-gpfs - Extract the HPSS/GPFS  code 
#                  Dependencies: build-gpfs-files,
#                                build-gpfs-subdirs
#     build-mvr  - Extract the HPSS Standalone Mover code package
#                  Dependencies: build-mvr-files,
#                                build-mvr-subdirs
#     build-hdm  - Extract the HPSS HDM Server code package
#                  Dependencies: build-files,
#                                build-subdirs
#
#  Primary Compilation Rules:
#     (none) - Compile & Link all HPSS components
#     all    - Compile & Link all HPSS components
#     clnt   - Compile & Link the HPSS Client Library package
#     ftp    - Compile & Link the HPSS Parallel FTP Client package
#     gpfs   - Compile & Link the HPSS GPFS package
#     mvr    - Compile & Link the HPSS Mover package
#     hdm    - Compile & Link the HPSS Data Management package
#
#  Other Extraction Rules:
#     build-bld-tool         - Extract the HPSS build tools code.
#                              Dependencies: build-bld-tool-files,
#                                            build-bld-tool-subdirs
#     ftp-build-bld-tool     - Extract the HPSS build tools code for pftp.
#                              Dependencies: ftp-build-bld-tool-files,
#                                            ftp-build-bld-tool-subdirs
#     build-clntlib          - Extract the HPSS Client Libary code
#                              only. Since the Mover and HDM packages
#                              are dependent on HPSS Client Library
#                              package, this rule is used when
#                              generating those packages.
#                              Dependencies: build-clntlib-files,
#                                            build-clntlib-subdirs
#     build-bld-tool-files   - Extract the HPSS build tools code
#                              in the current directory.
#                              List: BLD_TOOL_BUILD_FILES
#     ftp-build-bld-tool-files   - Extract the HPSS build tools code
#                              in the current directory.
#                              List: BLD_TOOL_BUILD_FILES
#     build-bld-tool-subdirs - Extract the HPSS build tools code for ftp
#                              in the specified subdirectories.
#                              List: FTP_BLD_TOOL_SUBDIRS
#     ftp-build-bld-tool-subdirs - Extract the HPSS build tools code for ftp
#                              in the specified subdirectories.
#                              List: FTP_BLD_TOOL_SUBDIRS
#     build-clntlib-files    - Extract the HPSS Client Library code
#                              in the current directory.
#                              List: CLNTLIB_BUILD_FILES
#     build-clntlib-subdirs  - Extract the HPSS Client Library code
#                              in the specified subdirectories.
#                              List: CLNTLIB_SUBDIRS
#     build-clnt-files       - Extract the HPSS Client code
#                              in the current directory.
#                              List: CLNT_BUILD_FILES
#     build-clnt-subdirs     - Extract the HPSS Client code
#                              in the specified subdirectories.
#                              List: CLNT_SUBDIRS
#     build-gpfs-files       - Extract the HPSS/GPFS code
#                              in the current directory.
#                              List: GPFS_BUILD_FILES
#     build-gpfs-subdirs     - Extract the HPSS/GPFS code
#                              in the specified subdirectories.
#                              List: GPFS_SUBDIRS
#     build-mvr-files        - Extract the HPSS Standalone Mover code
#                              in the current directory.
#                              List: MVR_BUILD_FILES
#     build-mvr-subdirs      - Extract the HPSS Standalone Mover code.
#                              in the specified subdirectories.
#                              List: MVR_SUBDIRS
#     build-hdm-files        - Extract the HPSS HDM Server code.
#                              in the current directory.
#                              List: HDM_BUILD_FILES
#     build-hdm-subdirs      - Extract the HPSS HDM Server code.
#                              in the specified subdirectories.
#                              List: HDM_SUBDIRS
#     build-ftplib-files     - Extract the HPSS Library code used by PFTP
#                              in the current directory.
#                              List: FTP_BUILD_FILES
#     build-ftplib-subdirs   - Extract the HPSS Library code used by PFTP
#                              in the specified subdirectories.
#                              List: FTP_SUBDIRS
#     build-ftp-files        - Extract the HPSS PFTP Client code.
#                              in the current directory.
#                              List: FTP_BUILD_FILES
#     build-ftp-subdirs      - Extract the HPSS PFTP Client code.
#                              in the specified subdirectories.
#                              List: FTP_SUBDIRS
#     build-fs-files         - Extract the HPSS File System Client code.
#                              in the current directory.
#                              List: FS_BUILD_FILES
#     build-fs-subdirs       - Extract the HPSS File System Client code.
#                              in the specified subdirectories.
#                              List: FS_SUBDIRS
#
#  Other Compilation Rules:
#     xdr                 - Generate HPSS code that is based 
#                           either directly or indirectly on XDR
#                           source files.
#                           Dependencies: xdr-subdirs,
#                                         makefile specific 
#     xdr-clntlib         - Generate HPSS Client Library code that
#                           is based either directly or indirectly
#                           on XDR source files.
#                           Dependencies: xdr-clntlib-subdirs,
#                                         makefile specific 
#     clntlib             - Generate HPSS Client Library code.
#                           Dependencies: clntlib-subdirs,
#					  local-lib,			
#                                         makefile specific 
#     libs-cs             - Generate the HPSS common service library
#                           Dependencies: libs-cs-subdirs,
#					  local-lib,			
#                                         makefile specific 
#     libs                - Generate HPSS common service libraries
#                           Dependencies: libs-subdirs,
#					  local-lib,			
#                                         makefile specific 
#     all-subdirs         - Generate all HPSS component
#                           in the specified subdirectories.
#                           Dependencies: local-bin
#                           List: SUBDIRS
#     xdr-subdirs         - Generate HPSS code that is based 
#                           either directly or indirectly on XDR
#                           source files in the specified
#                           subdirectories.
#                           List: XDR_SUBDIRS
#     xdr-clntlib-subdirs - Generate HPSS Client Library code that
#                           is based either directly or indirectly
#                           on XDR source files in the specified
#                           subdirectories.
#                           List: CLNTLIB_XDR_SUBDIRS
#     libs-cs-subdirs     - Generate HPSS Common Services Library code
#                           in the specified subdirectories.
#                           List: CLNTLIB_SUBDIRS
#     libs-subdirs        - Generate HPSS Library code in the
#                           specified subdirectories.
#                           List: SUBDIRS
#     clntlib-subdirs     - Generate HPSS Client Library code in the
#                           specified subdirectories.
#                           List: CLNTLIB_SUBDIRS
#     ftp-subdirs         - Generate HPSS PFTP code in the specified
#                           subdirectories.
#                           List: FTP_SUBDIRS
#     libs-ftp-subdirs    - Generate HPSS PFTP Library code in the
#                           specified subdirectories.
#                           List: FTP_SUBDIRS
#     clnt-subdirs        - Generate HPSS Client code in the specified
#                           subdirectories.
#                           List: CLNT_SUBDIRS
#     gpfs-subdirs        - Generate HPSS/GPFS code in the specified
#                           subdirectories.
#                           List: GPFS_SUBDIRS
#     mvr-subdirs         - Generate HPSS Standalone Mover code in the
#                           specified subdirectories.
#                           List: MVR_SUBDIRS
#     hdm-subdirs         - Generate HPSS HDM Server code in the
#                           specified subdirectories.
#                           List: HDM_SUBDIRS
#     fs-subdirs          - Generate HPSS File System Client code in the
#                           specified subdirectories.
#                           List: FS_SUBDIRS
#
#  Other Rules:
#     clean               - Remove all generated source and object
#                           code. Executable and library code is
#                           not removed.
#     clobber             - Remove all generated source, object
#                           excutable and library code.
#     local-lib           - If needed, create the local binary directory
#     local-bin           - If needed, create the local library directory
#     build-lib           - If needed, create the build binary directory
#     build-bin           - If needed, create the local libary directory
#
#===============================================================================

#===============================================================================
# LOCAL_ROOT       Relative location of the top-level directory (where include,
#                  lib, src, bin, etc. are located) from this directory
# ROOT_SUBPATH     Relative location of this directory from $(LOCAL_ROOT)
#                  (e.g., /src/bfs)
#===============================================================================

LOCAL_ROOT    = .
ROOT_SUBPATH  = 

#===============================================================================
# Include common macros
#===============================================================================

include $(LOCAL_ROOT)/Makefile.macros

#===============================================================================
# SUBDIRS          List of local subdirectories to perform iterative makes on
#                  (OK if blank)
# BUILD_FILES      List of configuration managed files in this local directory
# XDR_SUBDIRS      List of local subdirectories to perform iterative rpcgen on
#                  (OK if blank)
#===============================================================================
stk_aix_dirs.on    = stk
stk_aix_dirs.off   =
stk_linux_dirs.on  = stk_linux
stk_linux_dirs.off =
STK_AIX_DIRS       = $(stk_aix_dirs.$(STK_AIX_BUILD_SUBDIRS))
STK_LINUX_DIRS     = $(stk_linux_dirs.$(STK_LINUX_BUILD_SUBDIRS))

SUBDIRS       = include \
                src \
                lib \
                man \
                msg \
                config \
                $(STK_AIX_DIRS) \
                $(STK_LINUX_DIRS) \
                tools

BUILD_SUBDIRS = include \
                src \
                lib \
                man \
                msg \
                config \
                stk \
                stk_linux

BUILD_FILES   = Makefile \
		Makefile.macros \
		Makefile.macros.AIX \
		Makefile.macros.IRIX \
		Makefile.macros.LINUX \
		Makefile.macros.SOLARIS \
		Makefile.rules \
		README \
		copyright

SRC_SUBDIRS   = tools

DOC_SUBDIRS   = doc

XDR_SUBDIRS   = include \
		src 

BLD_TOOL_SUBDIRS = \
		tools

FTP_BLD_TOOL_SUBDIRS = \
		tools

CS_SUBDIRS = \
		include \
		src \
		lib

CLNTLIB_SUBDIRS = \
		include \
		src \
		lib

CLNT_SUBDIRS =  src \
		tools

FTPLIB_SUBDIRS = \
		include \
		src \
		lib

FTP_SUBDIRS = \
		include \
		src 

GPFS_SUBDIRS = \
		src

MVR_SUBDIRS =	include \
		src \
		config \
		tools

HDM_SUBDIRS =	include \
		src \
		config \
		msg \
		tools

FS_SUBDIRS =	include \
		src

#===============================================================================
# Define local rules
#===============================================================================

all:		bld-tool xdr libs local-bin all-subdirs
		@echo $(INDENT)Make $@ done

bld-tool:	bld-tool-subdirs
		@echo $(INDENT)Make $@ done

ftp-bld-tool:	ftp-bld-tool-subdirs
		@echo $(INDENT)Make $@ done

xdr:		xdr-subdirs
		@echo $(INDENT)Make $@ done

libs-cs:	libs-cs-subdirs
		@echo $(INDENT)Make $@ done

libs:		local-lib libs-cs libs-subdirs
		@echo $(INDENT)Make $@ done

libs-ftp:       libs-ftp-subdirs
		@echo $(INDENT)Make $@ done

clntlib:	local-lib bld-tool xdr-clntlib-subdirs clntlib-subdirs
		@echo $(INDENT)Make $@ done

clnt:		local-lib clntlib clnt-subdirs
		@echo $(INDENT)Make $@ done

ftp:		local-lib ftp-bld-tool xdr-ftplib-subdirs ftplib-subdirs \
			ftp-subdirs
		@echo $(INDENT)Make $@ done

gpfs:		bld-tool clntlib gpfs-subdirs
		@echo $(INDENT)Make $@ done

mvr:		bld-tool clntlib mvr-subdirs 
		@echo $(INDENT)Make $@ done

hdm:		bld-tool clntlib hdm-subdirs
		@echo $(INDENT)Make $@ done

fs:		bld-tool clntlib fs-subdirs
		@echo $(INDENT)Make $@ done

clean:		clean-subdirs
		@echo $(INDENT)Make $@ done

clobber:	clobber-subdirs
		@echo $(INDENT)Make $@ done

echo:		echo-files echo-subdirs
		@echo $(INDENT)Make $@ done

touch:		touch-files touch-subdirs
		@echo $(INDENT)Make $@ done

build:		build-files build-subdirs
		@echo $(INDENT)Make $@ done

build-src:      build-files build-bldsubdirs build-src-subdirs
		@echo $(INDENT)Make $@ done

build-doc:      build-doc-subdirs
		@echo $(INDENT)Make $@ done

build-clntlib:	build-bld-tool-subdirs build-clntlib-files \
			build-clntlib-subdirs
		@echo $(INDENT)Make $@ done

build-clnt:	build-clntlib build-clnt-files build-clnt-subdirs
		@echo $(INDENT)Make $@ done

build-ftplib:	ftp-build-bld-tool-subdirs build-ftplib-files \
			build-ftplib-subdirs
		@echo $(INDENT)Make $@ done

build-ftp:	build-ftplib build-ftp-files build-ftp-subdirs
		@echo $(INDENT)Make $@ done

build-gpfs:	build-bld-tool-subdirs build-clntlib-files \
			build-clntlib-subdirs build-gpfs-files build-gpfs-subdirs
		@echo $(INDENT)Make $@ done

build-mvr:	build-bld-tool-subdirs build-clntlib-files \
			build-clntlib-subdirs build-mvr-files build-mvr-subdirs
		@echo $(INDENT)Make $@ done

build-hdm:	build-bld-tool-subdirs build-clntlib-files \
			 build-clntlib-subdirs build-hdm-files build-hdm-subdirs
		@echo $(INDENT)Make $@ done

build-fs:	build-bld-tool-subdirs build-clntlib-files \
			 build-clntlib-subdirs build-fs-files build-fs-subdirs
		@echo $(INDENT)Make $@ done

#===============================================================================
# Include common rules
#===============================================================================

include $(LOCAL_ROOT)/Makefile.rules

# DO NOT DELETE THIS LINE -- make depend depends on it.
