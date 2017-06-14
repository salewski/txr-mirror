/* mplogic.h
 *
 * by Michael J. Fromberger <http://www.dartmouth.edu/~sting/>
 * Developed 1998-2004.
 * Assigned to the public domain as of 2002; see README.
 *
 * Bitwise logical operations on MPI values
 *
 * $Id: mplogic.h,v 1.1 2004/02/08 04:29:29 sting Exp $
 */

#include "mpi.h"

/* The logical operations treat an mp_int as if it were a bit vector,
 * without regard to its sign (an mp_int is represented in a signed
 * magnitude format).  Values are treated as if they had an infinite
 * string of zeros left of the most-significant bit.
 */

#define MP_EVEN MP_YES
#define MP_ODD MP_NO

mp_err mpl_not(mp_int *a, mp_int *b);
mp_err mpl_and(mp_int *a, mp_int *b, mp_int *c);
mp_err mpl_or(mp_int *a, mp_int *b, mp_int *c);
mp_err mpl_xor(mp_int *a, mp_int *b, mp_int *c);

mp_err mpl_rsh(mp_int *a, mp_int *b, mp_digit d);
mp_err mpl_lsh(mp_int *a, mp_int *b, mp_digit d);

mp_err mpl_num_set(mp_int *a, int *num);
mp_err mpl_num_clear(mp_int *a, int *num);
mp_err mpl_parity(mp_int *a);
