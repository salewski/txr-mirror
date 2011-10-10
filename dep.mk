filter.o: config.h $(top_srcdir)/lib.h $(top_srcdir)/hash.h $(top_srcdir)/unwind.h $(top_srcdir)/filter.h $(top_srcdir)/gc.h
gc.o: config.h $(top_srcdir)/lib.h $(top_srcdir)/stream.h $(top_srcdir)/hash.h $(top_srcdir)/txr.h $(top_srcdir)/gc.h
hash.o: config.h $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/unwind.h $(top_srcdir)/hash.h
lex.yy.o: config.h $(top_srcdir)/lib.h y.tab.h $(top_srcdir)/gc.h $(top_srcdir)/stream.h $(top_srcdir)/utf8.h $(top_srcdir)/unwind.h $(top_srcdir)/parser.h
lib.o: config.h $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/hash.h $(top_srcdir)/unwind.h $(top_srcdir)/stream.h $(top_srcdir)/utf8.h $(top_srcdir)/filter.h
match.o: config.h $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/unwind.h $(top_srcdir)/regex.h $(top_srcdir)/stream.h $(top_srcdir)/parser.h $(top_srcdir)/txr.h $(top_srcdir)/utf8.h $(top_srcdir)/filter.h $(top_srcdir)/match.h
regex.o: config.h $(top_srcdir)/lib.h $(top_srcdir)/unwind.h $(top_srcdir)/regex.h $(top_srcdir)/txr.h
stream.o: config.h $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/unwind.h $(top_srcdir)/stream.h $(top_srcdir)/utf8.h
txr.o: config.h $(top_srcdir)/lib.h $(top_srcdir)/stream.h $(top_srcdir)/gc.h $(top_srcdir)/unwind.h $(top_srcdir)/parser.h $(top_srcdir)/match.h $(top_srcdir)/utf8.h $(top_srcdir)/txr.h
unwind.o: config.h $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/stream.h $(top_srcdir)/txr.h $(top_srcdir)/unwind.h
utf8.o: config.h $(top_srcdir)/lib.h $(top_srcdir)/utf8.h
y.tab.o: config.h $(top_srcdir)/lib.h $(top_srcdir)/regex.h $(top_srcdir)/utf8.h $(top_srcdir)/match.h $(top_srcdir)/parser.h
