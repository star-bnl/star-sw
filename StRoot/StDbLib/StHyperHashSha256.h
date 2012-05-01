/* Declarations of functions and data types used for SHA256 and SHA224 sum
   library functions.
   Copyright (C) 2005, 2006, 2008 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef __ST_HYPERHASH_SHA256_H
#define __ST_HYPERHASH_SHA256_H

#include <cstdio>
#include <stdint.h>
#include <string>

namespace StHyperHash
{

/* Structure to save state of computation between the single steps.  */
struct st_sha256_ctx {
    uint32_t state[8];

    uint32_t total[2];
    size_t buflen;
    uint32_t buffer[32];
};

enum { ST_SHA224_DIGEST_SIZE = 224 / 8 };
enum { ST_SHA256_DIGEST_SIZE = 256 / 8 };

/* Initialize structure containing state of computation. */
extern void st_sha256_init_ctx (struct st_sha256_ctx *ctx);
extern void st_sha224_init_ctx (struct st_sha256_ctx *ctx);

/* Starting with the result of former calls of this function (or the
   initialization function update the context for the next LEN bytes
   starting at BUFFER.
   It is necessary that LEN is a multiple of 64!!! */
extern void st_sha256_process_block (const void *buffer, size_t len,
                                     struct st_sha256_ctx *ctx);

/* Starting with the result of former calls of this function (or the
   initialization function update the context for the next LEN bytes
   starting at BUFFER.
   It is NOT required that LEN is a multiple of 64.  */
extern void st_sha256_process_bytes (const void *buffer, size_t len,
                                     struct st_sha256_ctx *ctx);

/* Process the remaining bytes in the buffer and put result from CTX
   in first 32 (28) bytes following RESBUF.  The result is always in little
   endian byte order, so that a byte-wise output yields to the wanted
   ASCII representation of the message digest.  */
extern void *st_sha256_finish_ctx (struct st_sha256_ctx *ctx, void *resbuf);
extern void *st_sha224_finish_ctx (struct st_sha256_ctx *ctx, void *resbuf);


/* Put result from CTX in first 32 (28) bytes following RESBUF.  The result is
   always in little endian byte order, so that a byte-wise output yields
   to the wanted ASCII representation of the message digest.  */
extern void *st_sha256_read_ctx (const struct st_sha256_ctx *ctx, void *resbuf);
extern void *st_sha224_read_ctx (const struct st_sha256_ctx *ctx, void *resbuf);


/* Compute SHA256 (SHA224) message digest for bytes read from STREAM.  The
   resulting message digest number will be written into the 32 (28) bytes
   beginning at RESBLOCK.  */
extern int st_sha256_stream (FILE *stream, void *resblock);
extern int st_sha224_stream (FILE *stream, void *resblock);

/* Compute SHA256 (SHA224) message digest for LEN bytes beginning at BUFFER.  The
   result is always in little endian byte order, so that a byte-wise
   output yields to the wanted ASCII representation of the message
   digest.  */
extern void *st_sha256_buffer (const char *buffer, size_t len, void *resblock);
extern void *st_sha224_buffer (const char *buffer, size_t len, void *resblock);

std::string sha256sum(const std::string& str);

} // namespace StHyperHash

#endif // __ST_SHA256_H
