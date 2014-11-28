/* Declaration of functions and data types used for MD5 sum computing
   library functions.
   Copyright (C) 1995-1997,1999,2000,2001,2004,2005,2006,2008
      Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#ifndef __ST_HYPERHASH_MD5_H
#define __ST_HYPERHASH_MD5_H

#include <string>
#include <cstdio>
#include <stdint.h>

namespace StHyperHash
{

#define ST_MD5_DIGEST_SIZE 16
#define ST_MD5_BLOCK_SIZE 64

#ifndef __GNUC_PREREQ
# if defined __GNUC__ && defined __GNUC_MINOR__
#  define __GNUC_PREREQ(maj, min)					\
  ((__GNUC__ << 16) + __GNUC_MINOR__ >= ((maj) << 16) + (min))
# else
#  define __GNUC_PREREQ(maj, min) 0
# endif
#endif

#ifndef _LIBC
# define __st_md5_buffer st_md5_buffer
# define __st_md5_finish_ctx st_md5_finish_ctx
# define __st_md5_init_ctx st_md5_init_ctx
# define __st_md5_process_block st_md5_process_block
# define __st_md5_process_bytes st_md5_process_bytes
# define __st_md5_read_ctx st_md5_read_ctx
# define __st_md5_stream st_md5_stream
#endif

/* Structure to save state of computation between the single steps.  */
struct st_md5_ctx {
    uint32_t A;
    uint32_t B;
    uint32_t C;
    uint32_t D;

    uint32_t total[2];
    uint32_t buflen;
    uint32_t buffer[32];
};

/*
 * The following three functions are build up the low level used in
 * the functions `st_md5_stream' and `st_md5_buffer'.
 */

/* Initialize structure containing state of computation.
   (RFC 1321, 3.3: Step 3)  */
extern void __st_md5_init_ctx (struct st_md5_ctx *ctx) ;

/* Starting with the result of former calls of this function (or the
   initialization function update the context for the next LEN bytes
   starting at BUFFER.
   It is necessary that LEN is a multiple of 64!!! */
extern void __st_md5_process_block (const void *buffer, size_t len,
                                    struct st_md5_ctx *ctx) ;

/* Starting with the result of former calls of this function (or the
   initialization function update the context for the next LEN bytes
   starting at BUFFER.
   It is NOT required that LEN is a multiple of 64.  */
extern void __st_md5_process_bytes (const void *buffer, size_t len,
                                    struct st_md5_ctx *ctx) ;

/* Process the remaining bytes in the buffer and put result from CTX
   in first 16 bytes following RESBUF.  The result is always in little
   endian byte order, so that a byte-wise output yields to the wanted
   ASCII representation of the message digest.  */
extern void *__st_md5_finish_ctx (struct st_md5_ctx *ctx, void *resbuf) ;


/* Put result from CTX in first 16 bytes following RESBUF.  The result is
   always in little endian byte order, so that a byte-wise output yields
   to the wanted ASCII representation of the message digest.  */
extern void *__st_md5_read_ctx (const struct st_md5_ctx *ctx, void *resbuf) ;


/* Compute MD5 message digest for bytes read from STREAM.  The
   resulting message digest number will be written into the 16 bytes
   beginning at RESBLOCK.  */
extern int __st_md5_stream (FILE *stream, void *resblock) ;

/* Compute MD5 message digest for LEN bytes beginning at BUFFER.  The
   result is always in little endian byte order, so that a byte-wise
   output yields to the wanted ASCII representation of the message
   digest.  */
extern void *__st_md5_buffer (const char *buffer, size_t len,
                              void *resblock) ;

std::string md5sum(const std::string& str);

} // namespace StHyperHash

#endif /* StHyperHashMd5.h */
