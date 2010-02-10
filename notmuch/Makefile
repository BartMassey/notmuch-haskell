OBJS = Notmuch.o Notmuch.hi NOTMUCH_H_hsc.o
LIBS = -lgmime-2.4 -lz -lnsl -lgobject-2.0 -lglib-2.0 -ltalloc -lxapian
NOTMUCHLIB = /local/src/notmuch/lib/notmuch.a

notmuchtest: $(OBJS) NotmuchTest.hs
	ghc --make -o notmuchtest NotmuchTest.hs $(NOTMUCHLIB) $(LIBS)

NOTMUCH_H_hsc.c NOTMUCH_H_hsc.h: NOTMUCH_H.hsc
	hsc2hs NOTMUCH_H.hsc

NOTMUCH_H_hsc.o: NOTMUCH_H_hsc.c NOTMUCH_H_hsc.h
	ghc --make NOTMUCH_H_hsc.c

Notmuch.o Notmuch.hi: Notmuch.hs NOTMUCH_H_hsc.o
	ghc --make Notmuch.hs

clean:
	-rm -f $(OBJS) NOTMUCH_H_hsc.[ch] NOTMUCH_H_stub.[cho] \
	  NOTMUCH_H.o NOTMUCH_H.hs NOTMUCH_H.hi \
          NotmuchTest.o NotmuchTest.hi \
          notmuchtest
