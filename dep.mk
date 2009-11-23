parser.tab.o: $(top_srcdir)/config.h $(top_srcdir)/lib.h $(top_srcdir)/regex.h $(top_srcdir)/parser.h
utf8.o: $(top_srcdir)/config.h $(top_srcdir)/lib.h $(top_srcdir)/utf8.h
lib.o: $(top_srcdir)/config.h $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/hash.h $(top_srcdir)/unwind.h $(top_srcdir)/stream.h $(top_srcdir)/utf8.h
lex.yy.o: $(top_srcdir)/config.h $(top_srcdir)/lib.h y.tab.h $(top_srcdir)/gc.h $(top_srcdir)/stream.h $(top_srcdir)/utf8.h $(top_srcdir)/parser.h
popen_getwc.o:
test-sprintf.o:
test.o:
regex.o: $(top_srcdir)/config.h $(top_srcdir)/lib.h $(top_srcdir)/unwind.h $(top_srcdir)/regex.h
y.tab.o: $(top_srcdir)/config.h $(top_srcdir)/lib.h $(top_srcdir)/regex.h $(top_srcdir)/utf8.h $(top_srcdir)/parser.h
unwind.o: $(top_srcdir)/config.h $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/stream.h $(top_srcdir)/txr.h $(top_srcdir)/unwind.h
txr.o: $(top_srcdir)/config.h $(top_srcdir)/lib.h $(top_srcdir)/stream.h $(top_srcdir)/gc.h $(top_srcdir)/unwind.h $(top_srcdir)/parser.h $(top_srcdir)/match.h $(top_srcdir)/utf8.h $(top_srcdir)/txr.h
match.o: $(top_srcdir)/config.h $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/unwind.h $(top_srcdir)/regex.h $(top_srcdir)/stream.h $(top_srcdir)/parser.h $(top_srcdir)/txr.h $(top_srcdir)/utf8.h $(top_srcdir)/match.h
stream.o: $(top_srcdir)/config.h $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/unwind.h $(top_srcdir)/stream.h $(top_srcdir)/utf8.h
gc.o: $(top_srcdir)/config.h $(top_srcdir)/lib.h $(top_srcdir)/stream.h $(top_srcdir)/hash.h $(top_srcdir)/txr.h $(top_srcdir)/gc.h
hash.o: $(top_srcdir)/config.h $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/unwind.h $(top_srcdir)/hash.h
