/*
 * RSA Data Security, Inc., MD5 message-digest algorithm
 *
 * Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
 * rights reserved.
 *
 * Implementations of these message-digest algorithms, including
 * implementations derived from the reference C code in RFC-1319, RFC-1320, and
 * RFC-1321, may be made, used, and sold without license from RSA for any
 * purpose. [https://www.ietf.org/ietf-ftp/ietf/IPR/RSA-MD-all]
 *
 * DISCLAIMER: RSA MAKES NO REPRESENTATIONS AND EXTENDS NO WARRANTIES OF ANY
 * KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, VALIDITY OF INTELLECTUAL
 * PROPERTY RIGHTS, ISSUED OR PENDING, OR THE ABSENCE OF LATENT OR OTHER
 * DEFECTS, WHETHER OR NOT DISCOVERABLE, IN CONNECTION WITH THE MD2, MD4, OR
 * MD5 ALGORITHMS.  NOTHING IN THIS GRANT OF RIGHTS SHALL BE CONSTRUED AS A
 * REPRESENTATION OR WARRANTY GIVEN BY RSA THAT THE IMPLEMENTATION OF THE
 * ALGORITHM WILL NOT INFRINGE THE INTELLECTUAL PROPERTY RIGHTS OF ANY THIRD
 * PARTY.  IN NO EVENT SHALL RSA, ITS TRUSTEES, DIRECTORS, OFFICERS, EMPLOYEES,
 * PARENTS AND AFFILIATES BE LIABLE FOR INCIDENTAL OR CONSEQUENTIAL DAMAGES OF
 * ANY KIND RESULTING FROM IMPLEMENTATION OF THIS ALGORITHM, INCLUDING ECONOMIC
 * DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, REGARDLESS OF WHETHER RSA
 * SHALL BE ADVISED, SHALL HAVE OTHER REASON TO KNOW, OR IN FACT SHALL KNOW OF
 * THE POSSIBILITY OF SUCH INJURY OR DAMAGE.
 *
 * These notices must be retained in any copies of any part of this
 * documentation and/or software.
 */

#define MD5_DIGEST_LENGTH               16

typedef struct {
  u32_t state[4];               /* state (ABCD) */
  u32_t count[2];               /* number of bits, modulo 2^64 (lsb first) */
  unsigned char buffer[64];     /* input buffer */
} MD5_t;

void MD5_init(MD5_t *);
void MD5_update(MD5_t *, const unsigned char *, size_t);
void MD5_final(MD5_t *, unsigned char digest[16]);
