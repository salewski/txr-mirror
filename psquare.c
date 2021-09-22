#include <stddef.h>
#include <wchar.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <stdio.h>
#include <math.h>
#include "config.h"
#include "lib.h"
#include "signal.h"
#include "unwind.h"
#include "psquare.h"

static void psq_reset(struct psquare *psq)
{
  double p = psq->p;

  psq->n[0] = 1;
  psq->n[1] = 2;
  psq->n[2] = 3;
  psq->n[3] = 4;
  psq->n[4] = 5;

  psq->wn[0] = 1.0;
  psq->wn[1] = 1.0 + 2.0 * p;
  psq->wn[2] = 1.0 + 4.0 * p;
  psq->wn[3] = 3.0 + 2.0 * p;
  psq->wn[4] = 5.0;

  psq->dn[1] = p / 2.0;
  psq->dn[2] = p;
  psq->dn[3] = (1.0 + p) / 2.0;
  psq->dn[4] = 1.0;
}

void psq_init(struct psquare *psq, double p)
{
  memset(psq, 0, sizeof *psq);
  psq->p = p;
  psq_reset(psq);
}

void psq_init_grouped(struct psquare *psq, double p,
                      ucnum grsize, double rate)
{
  psq_init(psq, p);
  psq->type = psq_grouped;
  psq->grsize = grsize;
  psq->rate = rate;
  psq->blend = 1.0;
}

static int dbl_cmp(const void *lp, const void *rp)
{
  double ln = *coerce(const double *, lp);
  double rn = *coerce(const double *, rp);

  if (ln < rn)
    return -1;
  if (ln > rn)
    return 1;
  return 0;
}

void psq_add_sample(struct psquare *psq, double s, val self)
{
  ucnum c = psq->count++;

  if (psq->type == psq_grouped && c >= psq->grsize) {
    psq->prev = psq_get_estimate(psq);
    psq->blending = 1;
    psq->count = 1;
    psq_reset(psq);
    c = 0;
  }

  if (c < 5) {
    psq->q[c] = s;
  } else {
    int k = 0, i;

    if (c == 5)
      qsort(psq->q, c, sizeof psq->q[0], dbl_cmp);

    if (s < psq->q[0]) {
      psq->q[0] = s;
    } else if (psq->q[4] <= s) {
      psq->q[4] = s;
      k = 3;
    } else for (k = 0; k < 4; k++) {
      if (psq->q[k] <= s && s < psq->q[k + 1])
        break;
    }

    if (psq->n[4] == INT_PTR_MAX)
      uw_throwf(numeric_error_s, lit("~a: sample capacity overflow"),
                self, nao);

    for (i = k + 1; i < 5; i++)
      psq->n[i]++;

    for (i = 0; i < 5; i++)
      psq->wn[i] += psq->dn[i];

    for (i = 1; i <= 3; i++) {
      double d = psq->wn[i] - psq->n[i];
      double ds = psq->n[i + 1] - psq->n[i];
      double dp = psq->n[i - 1] - psq->n[i];

      if ((d >= 1 && ds > 1) || (d <= -1 && dp < -1)) {
        int sgd = d < 0 ? -1 : 1;
        double qs = (psq->q[i + 1] - psq->q[i]) / ds;
        double qp = (psq->q[i - 1] - psq->q[i]) / dp;
        double q = psq->q[i] + sgd/(ds - dp)*((sgd - dp)*qs + (ds - sgd)*qp);

        if (psq->q[i - 1] < q && q < psq->q[i + 1]) {
          psq->q[i] = q;
        } else {
          if (d > 0)
            psq->q[i] += qs;
          else if (d < 0)
            psq->q[i] -= qp;
        }

        psq->n[i] += sgd;
      }
    }
  }

  if (psq->blending)
    psq->blend *= psq->rate;
}

double psq_get_estimate(struct psquare *psq)
{
  ucnum c = psq->count;
  double est;

  if (c > 2 && c < 5)
    qsort(psq->q, c, sizeof psq->q[0], dbl_cmp);

  switch (psq->count) {
  case 0:
    est = 0.0;
    break;
  case 1:
    est = psq->q[0];
    break;
  case 2:
    est = psq->q[0] + (psq->q[1] - psq->q[0]) / 2;
    break;
  case 3:
    est = psq->q[1];
    break;
  case 4:
    est = psq->q[1] + (psq->q[2] - psq->q[1]) / 2;
    break;
  default:
    est = psq->q[2];
    break;
  }

  if (!psq->blending)
    return est;

  return psq->blend * psq->prev + (1 - psq->blend) * est;
}
