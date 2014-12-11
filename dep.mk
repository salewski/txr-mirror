DEP_opt/txr.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./stream.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./parser.h $(top_srcdir)/./match.h $(top_srcdir)/./utf8.h $(top_srcdir)/./debug.h $(top_srcdir)/./syslog.h $(top_srcdir)/./eval.h $(top_srcdir)/./regex.h $(top_srcdir)/./arith.h $(top_srcdir)/./txr.h config.make
opt/txr.o: $(DEP_opt/txr.o)
DEP_opt/lex.yy.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./stream.h $(top_srcdir)/./utf8.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./hash.h $(top_srcdir)/./parser.h $(top_srcdir)/./eval.h y.tab.h config.make
opt/lex.yy.o: $(DEP_opt/lex.yy.o)
DEP_opt/y.tab.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./regex.h $(top_srcdir)/./utf8.h $(top_srcdir)/./match.h $(top_srcdir)/./hash.h $(top_srcdir)/./eval.h $(top_srcdir)/./stream.h $(top_srcdir)/./parser.h config.make
opt/y.tab.o: $(DEP_opt/y.tab.o)
DEP_opt/match.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./regex.h $(top_srcdir)/./stream.h $(top_srcdir)/./parser.h $(top_srcdir)/./txr.h $(top_srcdir)/./utf8.h $(top_srcdir)/./filter.h $(top_srcdir)/./hash.h $(top_srcdir)/./debug.h $(top_srcdir)/./eval.h $(top_srcdir)/./match.h config.make
opt/match.o: $(DEP_opt/match.o)
DEP_opt/lib.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./arith.h $(top_srcdir)/./rand.h $(top_srcdir)/./hash.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./stream.h $(top_srcdir)/./utf8.h $(top_srcdir)/./filter.h $(top_srcdir)/./eval.h $(top_srcdir)/./sysif.h $(top_srcdir)/./regex.h $(top_srcdir)/./txr.h config.make
opt/lib.o: $(DEP_opt/lib.o)
DEP_opt/regex.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./parser.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./stream.h $(top_srcdir)/./gc.h $(top_srcdir)/./regex.h $(top_srcdir)/./txr.h config.make
opt/regex.o: $(DEP_opt/regex.o)
DEP_opt/gc.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./stream.h $(top_srcdir)/./hash.h $(top_srcdir)/./txr.h $(top_srcdir)/./eval.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h config.make
opt/gc.o: $(DEP_opt/gc.o)
DEP_opt/unwind.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./stream.h $(top_srcdir)/./txr.h $(top_srcdir)/./signal.h $(top_srcdir)/./eval.h $(top_srcdir)/./parser.h $(top_srcdir)/./unwind.h config.make
opt/unwind.o: $(DEP_opt/unwind.o)
DEP_opt/stream.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./stream.h $(top_srcdir)/./utf8.h $(top_srcdir)/./eval.h $(top_srcdir)/./regex.h config.make
opt/stream.o: $(DEP_opt/stream.o)
DEP_opt/arith.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./gc.h $(top_srcdir)/./eval.h $(top_srcdir)/./arith.h config.make
opt/arith.o: $(DEP_opt/arith.o)
DEP_opt/hash.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./stream.h $(top_srcdir)/./hash.h config.make
opt/hash.o: $(DEP_opt/hash.o)
DEP_opt/utf8.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./utf8.h config.make
opt/utf8.o: $(DEP_opt/utf8.o)
DEP_opt/filter.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./hash.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./match.h $(top_srcdir)/./filter.h $(top_srcdir)/./gc.h $(top_srcdir)/./eval.h $(top_srcdir)/./stream.h config.make
opt/filter.o: $(DEP_opt/filter.o)
DEP_opt/eval.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./arith.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./regex.h $(top_srcdir)/./stream.h $(top_srcdir)/./parser.h $(top_srcdir)/./hash.h $(top_srcdir)/./debug.h $(top_srcdir)/./match.h $(top_srcdir)/./rand.h $(top_srcdir)/./txr.h $(top_srcdir)/./combi.h $(top_srcdir)/./eval.h config.make
opt/eval.o: $(DEP_opt/eval.o)
DEP_opt/rand.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./arith.h $(top_srcdir)/./rand.h $(top_srcdir)/./eval.h config.make
opt/rand.o: $(DEP_opt/rand.o)
DEP_opt/combi.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./eval.h $(top_srcdir)/./hash.h $(top_srcdir)/./combi.h config.make
opt/combi.o: $(DEP_opt/combi.o)
DEP_opt/sysif.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./stream.h $(top_srcdir)/./hash.h $(top_srcdir)/./signal.h $(top_srcdir)/./utf8.h $(top_srcdir)/./unwind.h $(top_srcdir)/./eval.h $(top_srcdir)/./sysif.h config.make
opt/sysif.o: $(DEP_opt/sysif.o)
DEP_opt/mpi-1.8.6/mpi.o := config.h $(top_srcdir)/mpi-1.8.6/mpi.h $(top_srcdir)/mpi-1.8.6/logtab.h config.make
opt/mpi-1.8.6/mpi.o: $(DEP_opt/mpi-1.8.6/mpi.o)
DEP_opt/mpi-1.8.6/mplogic.o := config.h $(top_srcdir)/mpi-1.8.6/mplogic.h config.make
opt/mpi-1.8.6/mplogic.o: $(DEP_opt/mpi-1.8.6/mplogic.o)
DEP_opt/debug.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./debug.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./stream.h $(top_srcdir)/./parser.h $(top_srcdir)/./txr.h config.make
opt/debug.o: $(DEP_opt/debug.o)
DEP_opt/syslog.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./stream.h $(top_srcdir)/./hash.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./utf8.h $(top_srcdir)/./eval.h $(top_srcdir)/./syslog.h config.make
opt/syslog.o: $(DEP_opt/syslog.o)
DEP_opt/signal.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./eval.h config.make
opt/signal.o: $(DEP_opt/signal.o)
DEP_dbg/txr.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./stream.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./parser.h $(top_srcdir)/./match.h $(top_srcdir)/./utf8.h $(top_srcdir)/./debug.h $(top_srcdir)/./syslog.h $(top_srcdir)/./eval.h $(top_srcdir)/./regex.h $(top_srcdir)/./arith.h $(top_srcdir)/./txr.h config.make
dbg/txr.o: $(DEP_dbg/txr.o)
DEP_dbg/lex.yy.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./stream.h $(top_srcdir)/./utf8.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./hash.h $(top_srcdir)/./parser.h $(top_srcdir)/./eval.h y.tab.h config.make
dbg/lex.yy.o: $(DEP_dbg/lex.yy.o)
DEP_dbg/y.tab.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./regex.h $(top_srcdir)/./utf8.h $(top_srcdir)/./match.h $(top_srcdir)/./hash.h $(top_srcdir)/./eval.h $(top_srcdir)/./stream.h $(top_srcdir)/./parser.h config.make
dbg/y.tab.o: $(DEP_dbg/y.tab.o)
DEP_dbg/match.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./regex.h $(top_srcdir)/./stream.h $(top_srcdir)/./parser.h $(top_srcdir)/./txr.h $(top_srcdir)/./utf8.h $(top_srcdir)/./filter.h $(top_srcdir)/./hash.h $(top_srcdir)/./debug.h $(top_srcdir)/./eval.h $(top_srcdir)/./match.h config.make
dbg/match.o: $(DEP_dbg/match.o)
DEP_dbg/lib.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./arith.h $(top_srcdir)/./rand.h $(top_srcdir)/./hash.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./stream.h $(top_srcdir)/./utf8.h $(top_srcdir)/./filter.h $(top_srcdir)/./eval.h $(top_srcdir)/./sysif.h $(top_srcdir)/./regex.h $(top_srcdir)/./txr.h config.make
dbg/lib.o: $(DEP_dbg/lib.o)
DEP_dbg/regex.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./parser.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./stream.h $(top_srcdir)/./gc.h $(top_srcdir)/./regex.h $(top_srcdir)/./txr.h config.make
dbg/regex.o: $(DEP_dbg/regex.o)
DEP_dbg/gc.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./stream.h $(top_srcdir)/./hash.h $(top_srcdir)/./txr.h $(top_srcdir)/./eval.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h config.make
dbg/gc.o: $(DEP_dbg/gc.o)
DEP_dbg/unwind.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./stream.h $(top_srcdir)/./txr.h $(top_srcdir)/./signal.h $(top_srcdir)/./eval.h $(top_srcdir)/./parser.h $(top_srcdir)/./unwind.h config.make
dbg/unwind.o: $(DEP_dbg/unwind.o)
DEP_dbg/stream.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./stream.h $(top_srcdir)/./utf8.h $(top_srcdir)/./eval.h $(top_srcdir)/./regex.h config.make
dbg/stream.o: $(DEP_dbg/stream.o)
DEP_dbg/arith.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./gc.h $(top_srcdir)/./eval.h $(top_srcdir)/./arith.h config.make
dbg/arith.o: $(DEP_dbg/arith.o)
DEP_dbg/hash.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./stream.h $(top_srcdir)/./hash.h config.make
dbg/hash.o: $(DEP_dbg/hash.o)
DEP_dbg/utf8.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./utf8.h config.make
dbg/utf8.o: $(DEP_dbg/utf8.o)
DEP_dbg/filter.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./hash.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./match.h $(top_srcdir)/./filter.h $(top_srcdir)/./gc.h $(top_srcdir)/./eval.h $(top_srcdir)/./stream.h config.make
dbg/filter.o: $(DEP_dbg/filter.o)
DEP_dbg/eval.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./arith.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./regex.h $(top_srcdir)/./stream.h $(top_srcdir)/./parser.h $(top_srcdir)/./hash.h $(top_srcdir)/./debug.h $(top_srcdir)/./match.h $(top_srcdir)/./rand.h $(top_srcdir)/./txr.h $(top_srcdir)/./combi.h $(top_srcdir)/./eval.h config.make
dbg/eval.o: $(DEP_dbg/eval.o)
DEP_dbg/rand.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./arith.h $(top_srcdir)/./rand.h $(top_srcdir)/./eval.h config.make
dbg/rand.o: $(DEP_dbg/rand.o)
DEP_dbg/combi.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./eval.h $(top_srcdir)/./hash.h $(top_srcdir)/./combi.h config.make
dbg/combi.o: $(DEP_dbg/combi.o)
DEP_dbg/sysif.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./stream.h $(top_srcdir)/./hash.h $(top_srcdir)/./signal.h $(top_srcdir)/./utf8.h $(top_srcdir)/./unwind.h $(top_srcdir)/./eval.h $(top_srcdir)/./sysif.h config.make
dbg/sysif.o: $(DEP_dbg/sysif.o)
DEP_dbg/mpi-1.8.6/mpi.o := config.h $(top_srcdir)/mpi-1.8.6/mpi.h $(top_srcdir)/mpi-1.8.6/logtab.h config.make
dbg/mpi-1.8.6/mpi.o: $(DEP_dbg/mpi-1.8.6/mpi.o)
DEP_dbg/mpi-1.8.6/mplogic.o := config.h $(top_srcdir)/mpi-1.8.6/mplogic.h config.make
dbg/mpi-1.8.6/mplogic.o: $(DEP_dbg/mpi-1.8.6/mplogic.o)
DEP_dbg/debug.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./debug.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./stream.h $(top_srcdir)/./parser.h $(top_srcdir)/./txr.h config.make
dbg/debug.o: $(DEP_dbg/debug.o)
DEP_dbg/syslog.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./stream.h $(top_srcdir)/./hash.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./utf8.h $(top_srcdir)/./eval.h $(top_srcdir)/./syslog.h config.make
dbg/syslog.o: $(DEP_dbg/syslog.o)
DEP_dbg/signal.o := config.h $(top_srcdir)/./lib.h $(top_srcdir)/./gc.h $(top_srcdir)/./signal.h $(top_srcdir)/./unwind.h $(top_srcdir)/./eval.h config.make
dbg/signal.o: $(DEP_dbg/signal.o)
