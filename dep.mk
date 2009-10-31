lib.o: lib.h gc.h unwind.h stream.h
lex.yy.o: y.tab.h lib.h gc.h stream.h parser.h
regex.o: lib.h unwind.h regex.h
y.tab.o: lib.h regex.h parser.h
unwind.o: lib.h gc.h stream.h txr.h unwind.h
txr.o: lib.h stream.h gc.h unwind.h parser.h match.h txr.h
match.o: lib.h gc.h unwind.h regex.h stream.h parser.h txr.h match.h
stream.o: lib.h gc.h unwind.h stream.h
gc.o: lib.h stream.h txr.h gc.h
