parser.tab.o: $(top_srcdir)/lib.h $(top_srcdir)/regex.h $(top_srcdir)/parser.h
lib.o: $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/unwind.h $(top_srcdir)/stream.h
lex.yy.o: y.tab.h $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/stream.h $(top_srcdir)/parser.h
regex.o: $(top_srcdir)/lib.h $(top_srcdir)/unwind.h $(top_srcdir)/regex.h
y.tab.o: $(top_srcdir)/lib.h $(top_srcdir)/regex.h $(top_srcdir)/parser.h
unwind.o: $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/stream.h $(top_srcdir)/txr.h $(top_srcdir)/unwind.h
txr.o: $(top_srcdir)/lib.h $(top_srcdir)/stream.h $(top_srcdir)/gc.h $(top_srcdir)/unwind.h $(top_srcdir)/parser.h $(top_srcdir)/match.h $(top_srcdir)/txr.h
match.o: $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/unwind.h $(top_srcdir)/regex.h $(top_srcdir)/stream.h $(top_srcdir)/parser.h $(top_srcdir)/txr.h $(top_srcdir)/match.h
stream.o: $(top_srcdir)/lib.h $(top_srcdir)/gc.h $(top_srcdir)/unwind.h $(top_srcdir)/stream.h
gc.o: $(top_srcdir)/lib.h $(top_srcdir)/stream.h $(top_srcdir)/txr.h $(top_srcdir)/gc.h
