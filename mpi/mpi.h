/* mpi.h
 *
 * by Michael J. Fromberger <http://www.dartmouth.edu/~sting/>
 * Developed 1998-2004.
 * Assigned to the public domain as of 2002; see README.
 *
 * Arbitrary precision integer arithmetic library
 *
 * $Id: mpi.h,v 1.1 2004/02/08 04:29:29 sting Exp $
 */
#include "mpi-config.h"

#if MP_DEBUG
#undef MP_IOFUNC
#define MP_IOFUNC 1
#endif

#include <limits.h>

#define MP_NEG 1
#define MP_ZPOS 0

#define MP_OKAY 0 /* no error, all is well */
#define MP_YES 0 /* yes (boolean result) */
#define MP_NO -1 /* no (boolean result) */
#define MP_MEM -2 /* out of memory */
#define MP_RANGE -3 /* argument out of range */
#define MP_BADARG -4 /* invalid parameter */
#define MP_UNDEF -5 /* answer is undefined */
#define MP_TOOBIG -6 /* number is too large */
#define MP_LAST_CODE MP_TOOBIG

#define MP_LT -1
#define MP_EQ 0
#define MP_GT 1

#include "mpi-types.h"

typedef struct {
#if SIZEOF_INT >= SIZEOF_PTR
  unsigned int sign : 1;
  unsigned int alloc : sizeof(int)*CHAR_BIT - 1;
#else
  mp_sign sign; /* sign of this quantity */
  mp_size alloc; /* how many digits allocated */
#endif
  mp_size used; /* how many digits used */
  mp_digit *dp; /* the digits themselves */
} mp_int;

/* Macros for accessing the mp_int internals */
#define SIGN(MP) ((MP)->sign)
#define ISNEG(MP) ((MP)->sign == MP_NEG)
#define USED(MP) ((MP)->used)
#define ALLOC(MP) ((MP)->alloc)
#define DIGITS(MP) ((MP)->dp)
#define DIGIT(MP,N) (MP)->dp[(N)]

mp_size mp_get_prec(void);
void mp_set_prec(mp_size prec);

mp_err mp_init(mp_int *mp);
INLINE mp_err mp_init_minimal(mp_int *mp)
{
  DIGITS(mp) = 0;
  return MP_OKAY;
}
mp_err mp_init_array(mp_int mp[], int count);
mp_err mp_init_size(mp_int *mp, mp_size prec);
mp_err mp_init_copy(mp_int *mp, mp_int *from);
mp_err mp_copy(mp_int *from, mp_int *to);
void mp_exch(mp_int *mp1, mp_int *mp2);
void mp_clear(mp_int *mp);
void mp_clear_array(mp_int mp[], int count);
void mp_zero(mp_int *mp);
void mp_set(mp_int *mp, mp_digit d);
mp_err mp_set_int(mp_int *mp, long z);
mp_err mp_set_uintptr(mp_int *mp, uint_ptr_t z);
mp_err mp_set_intptr(mp_int *mp, int_ptr_t z);
mp_err mp_get_uintptr(mp_int *mp, uint_ptr_t *z);
mp_err mp_get_intptr(mp_int *mp, int_ptr_t *z);
#ifdef HAVE_DOUBLE_INTPTR_T
mp_err mp_set_double_intptr(mp_int *mp, double_intptr_t z);
#endif
mp_err mp_set_word(mp_int *mp, mp_word w, int sign);

mp_err mp_add_d(mp_int *a, mp_digit d, mp_int *b);
mp_err mp_sub_d(mp_int *a, mp_digit d, mp_int *b);
mp_err mp_mul_d(mp_int *a, mp_digit d, mp_int *b);
mp_err mp_mul_2(mp_int *a, mp_int *c);
mp_err mp_div_d(mp_int *a, mp_digit d, mp_int *q, mp_digit *r);
mp_err mp_div_2(mp_int *a, mp_int *c);
mp_err mp_expt_d(mp_int *a, mp_digit d, mp_int *c);

mp_err mp_abs(mp_int *a, mp_int *b);
mp_err mp_neg(mp_int *a, mp_int *b);

mp_err mp_add(mp_int *a, mp_int *b, mp_int *c);
mp_err mp_sub(mp_int *a, mp_int *b, mp_int *c);
mp_err mp_mul(mp_int *a, mp_int *b, mp_int *c);
mp_err mp_mul_2d(mp_int *a, mp_digit d, mp_int *c);
#if MP_SQUARE
mp_err mp_sqr(mp_int *a, mp_int *b);
#else
#define mp_sqr(a, b) mp_mul(a, a, b)
#endif
mp_err mp_div(mp_int *a, mp_int *b, mp_int *q, mp_int *r);
mp_err mp_div_2d(mp_int *a, mp_digit d, mp_int *q, mp_int *r);
mp_err mp_expt(mp_int *a, mp_int *b, mp_int *c);
mp_err mp_2expt(mp_int *a, mp_digit k);
mp_err mp_sqrt(mp_int *a, mp_int *b);

#if MP_MODARITH
mp_err mp_mod(mp_int *a, mp_int *m, mp_int *c);
mp_err mp_mod_d(mp_int *a, mp_digit d, mp_digit *c);
mp_err mp_addmod(mp_int *a, mp_int *b, mp_int *m, mp_int *c);
mp_err mp_submod(mp_int *a, mp_int *b, mp_int *m, mp_int *c);
mp_err mp_mulmod(mp_int *a, mp_int *b, mp_int *m, mp_int *c);
#if MP_SQUARE
mp_err mp_sqrmod(mp_int *a, mp_int *m, mp_int *c);
#else
#define mp_sqrmod(a, m, c) mp_mulmod(a, a, m, c)
#endif
mp_err mp_exptmod(mp_int *a, mp_int *b, mp_int *m, mp_int *c);
mp_err mp_exptmod_d(mp_int *a, mp_digit d, mp_int *m, mp_int *c);
#endif

int mp_cmp_z(mp_int *a);
int mp_cmp_d(mp_int *a, mp_digit d);
int mp_cmp(mp_int *a, mp_int *b);
int mp_cmp_mag(mp_int *a, mp_int *b);
int mp_cmp_int(mp_int *a, long z);
int mp_isodd(mp_int *a);
int mp_iseven(mp_int *a);

unsigned long mp_hash(mp_int *a);

#if MP_NUMTH
mp_err mp_gcd(mp_int *a, mp_int *b, mp_int *c);
mp_err mp_lcm(mp_int *a, mp_int *b, mp_int *c);
mp_err mp_xgcd(mp_int *a, mp_int *b, mp_int *g, mp_int *x, mp_int *y);
mp_err mp_invmod(mp_int *a, mp_int *m, mp_int *c);
#endif

mp_err mp_2comp(mp_int *a, mp_int *b, mp_size dig); /* peculiar semantics */
mp_err mp_and(mp_int *a, mp_int *b, mp_int *c);
mp_err mp_or(mp_int *a, mp_int *b, mp_int *c);
mp_err mp_xor(mp_int *a, mp_int *b, mp_int *c);
mp_err mp_comp(mp_int *a, mp_int *b);
mp_err mp_trunc_comp(mp_int *a, mp_int *b, mp_digit bits);
mp_err mp_trunc(mp_int *a, mp_int *b, mp_digit bits);
mp_err mp_shift(mp_int *a, mp_int *b, int bits); /* + left, - right */
mp_err mp_bit(mp_int *a, mp_digit bit);

mp_err mp_to_double(mp_int *mp, double *d);

#if MP_IOFUNC
void mp_print(mp_int *mp, FILE *ofp);
#endif

#define BITS 1
#define BYTES CHAR_BIT

mp_err mp_read_signed_bin(mp_int *mp, unsigned char *str, size_t len);
size_t mp_signed_bin_size(mp_int *mp);
mp_err mp_to_signed_bin(mp_int *mp, unsigned char *str);

mp_err mp_read_unsigned_bin(mp_int *mp, unsigned char *str, size_t len);
size_t mp_unsigned_bin_size(mp_int *mp);
mp_err mp_to_unsigned_bin(mp_int *mp, unsigned char *str);
mp_err mp_to_unsigned_buf(mp_int *mp, unsigned char *str, size_t size);

mp_size mp_count_bits(mp_int *mp);
mp_size mp_is_pow_two(mp_int *mp);

#if MP_COMPAT_MACROS
#define mp_read_raw(mp, str, len) mp_read_signed_bin((mp), (str), (len))
#define mp_raw_size(mp) mp_signed_bin_size(mp)
#define mp_toraw(mp, str) mp_to_signed_bin((mp), (str))
#define mp_read_mag(mp, str, len) mp_read_unsigned_bin((mp), (str), (len))
#define mp_mag_size(mp) mp_unsigned_bin_size(mp)
#define mp_tomag(mp, str) mp_to_unsigned_bin((mp), (str))
#endif

mp_err mp_read_radix(mp_int *mp, unsigned char *str, int radix);
mp_size mp_radix_size(mp_int *mp, int radix);
mp_size mp_value_radix_size(mp_size num, mp_size qty, int radix);
mp_err mp_toradix(mp_int *mp, unsigned char *str, int radix);
mp_err mp_toradix_case(mp_int *mp, unsigned char *str, int radix, int low);

int mp_char2value(char ch, int r);

#define mp_tobinary(M, S) mp_toradix((M), (S), 2)
#define mp_tooctal(M, S) mp_toradix((M), (S), 8)
#define mp_todecimal(M, S) mp_toradix((M), (S), 10)
#define mp_tohex(M, S) mp_toradix((M), (S), 16)

const char *mp_strerror(mp_err ec);
