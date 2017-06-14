/*
 * Universal. We can further tweak these by making them
 * bitfields inside the mp_int struct.
 */
typedef int mp_sign;
typedef int mp_size;

/*
 * Universal. Does not need platform configuration.
 */
typedef int mp_err;

#if HAVE_USUPERLONG_T && HAVE_ULONGLONG_T && \
    SIZEOF_SUPERLONG_T / 2 == SIZEOF_LONGLONG_T && \
    SIZEOF_PTR >= SIZEOF_LONGLONG_T
  typedef ulonglong_t mp_digit;
  typedef usuperlong_t mp_word;
  #define MP_DIGIT_SIZE SIZEOF_LONGLONG_T
  #define DIGIT_FMT "%" #SIZEOF_SUPERLONG_T "llx"
#elif HAVE_ULONGLONG_T && SIZEOF_LONGLONG_T / 2 == SIZEOF_LONG && \
      SIZEOF_PTR >= SIZEOF_LONG
  typedef unsigned long mp_digit;
  typedef ulonglong_t mp_word;
  #define MP_DIGIT_SIZE SIZEOF_LONG
  #define DIGIT_FMT "%" #SIZEOF_LONGLONG_T "lx"
#elif HAVE_ULONGLONG_T && SIZEOF_LONGLONG_T / 2 == SIZEOF_INT && \
      SIZEOF_PTR >= SIZEOF_INT
  typedef unsigned int mp_digit;
  typedef ulonglong_t mp_word;
  #define MP_DIGIT_SIZE SIZEOF_INT
  #define DIGIT_FMT "%" #SIZEOF_LONGLONG_T "lx"
#elif SIZEOF_LONG / 2 == SIZEOF_INT && SIZEOF_PTR >= SIZEOF_INT
  typedef unsigned int mp_digit;
  typedef unsigned long mp_word;
  #define MP_DIGIT_SIZE SIZEOF_INT
  #define DIGIT_FMT "%" #SIZEOF_LONG "x"
#elif SIZEOF_INT / 2 == SIZEOF_SHORT
  typedef unsigned short mp_digit;
  typedef unsigned int mp_word;
  #define MP_DIGIT_SIZE SIZEOF_SHORT
  #define DIGIT_FMT "%" #SIZEOF_INT "x"
#elif SIZEOF_SHORT == 2
  typedef unsigned char mp_digit;
  typedef unsigned short mp_word;
  #define MP_DIGIT_SIZE 1
  #define DIGIT_FMT "%" #SIZEOF_SHORT "x"
#else
  #error Failure to configure MPI types on this target platform
#endif

#define MP_DIGIT_BIT convert(int, CHAR_BIT*sizeof(mp_digit))
#define MP_DIGIT_MAX convert(mp_digit, -1)
#define MP_WORD_BIT convert(int, CHAR_BIT*sizeof(mp_word))
#define MP_WORD_MAX convert(mp_word, -1)

#define RADIX (convert(mp_word, MP_DIGIT_MAX) + 1)
