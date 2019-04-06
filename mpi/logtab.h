/*
 * A table of the logs of 2 for various bases scaled by a factor
 * and converted to integer.
 *
 * This table is used to compute output lengths for the mp_toradix
 * function.
 */

#define MP_LOG_SCALE 16384

const unsigned int s_logv_2[] = {
  0, 0, 16384, 10338,
  8192, 7057, 6339, 5837,
  5462, 5169, 4933, 4737,
  4571, 4428, 4304, 4194,
  4096, 4009, 3930, 3857,
  3791, 3731, 3675, 3622,
  3574, 3529, 3486, 3446,
  3409, 3373, 3339, 3308,
  3277, 3248, 3221, 3195,
  3170, 3146, 3122, 3100,
  3079, 3059, 3039, 3020,
  3002, 2984, 2967, 2950,
  2934, 2919, 2903, 2889,
  2875, 2861, 2847, 2834,
  2822, 2809, 2797, 2786,
  2774, 2763, 2752, 2742,
  2731
};
